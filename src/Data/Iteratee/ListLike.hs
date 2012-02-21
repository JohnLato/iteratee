{-# LANGUAGE FlexibleContexts, BangPatterns, TupleSections, ScopedTypeVariables #-}

-- |Monadic Iteratees:
-- incremental input parsers, processors and transformers
-- 
-- This module provides many basic iteratees from which more complicated
-- iteratees can be built.  In general these iteratees parallel those in
-- @Data.List@, with some additions.

module Data.Iteratee.ListLike (
  -- * Iteratees
  -- ** Iteratee Utilities
  isFinished
  ,stream2list
  ,stream2stream
  -- ** Basic Iteratees
  ,break
  ,dropWhile
  ,drop
  ,head
  ,tryHead
  ,last
  ,heads
  ,peek
  ,roll
  ,length
  ,chunkLength
  ,takeFromChunk
  -- ** Nested iteratee combinators
  ,breakE
  ,take
  ,takeUpTo
  ,takeWhile
  ,takeWhileE
  ,mapStream
  ,rigidMapStream
  ,filter
  ,group
  ,groupBy
  ,merge
  ,mergeByChunks
  -- ** Folds
  ,foldl
  ,foldl'
  ,foldl1
  ,foldl1'
  -- ** Special Folds
  ,sum
  ,product
  -- * Enumerators
  -- ** Basic enumerators
  ,enumPureNChunk
  -- ** Enumerator Combinators
  ,enumWith
  ,zip
  ,zip3
  ,zip4
  ,zip5
  ,sequence_
  ,countConsumed
  ,greedy
  -- ** Monadic functions
  ,mapM_
  ,foldM
  -- * Re-exported modules
  ,module Data.Iteratee.Iteratee
)
where

import Prelude hiding (mapM_, null, head, last, drop, dropWhile, take, takeWhile, break, foldl, foldl1, length, filter, sum, product, zip, zip3, sequence_)

import qualified Prelude as Prelude

import qualified Data.ListLike as LL
import qualified Data.ListLike.FoldableLL as FLL
import Data.Iteratee.Iteratee
import Data.Monoid
import Control.Applicative ((<$>), (<*>), (<*))
import Control.Arrow (first, (***))
import Control.Monad (liftM, mplus)
import qualified Control.Monad as CM
import Control.Monad.Trans.Class
import Data.Word (Word8)
import qualified Data.ByteString as B

-- Useful combinators for implementing iteratees and enumerators

-- | Check if a stream has received 'EOF'.
isFinished :: (Monad m, Nullable s) => Iteratee s m Bool
isFinished = icontP check
  where
  check c@(Chunk xs)
    | nullC xs    = (icontP check, c)
    | otherwise   = (idone False, c)
  check s@(EOF _) = (idone True, s)
{-# INLINE isFinished #-}

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list.
-- This iteratee returns all data from the stream *strictly*.
stream2list :: (Monad m, Nullable s, LL.ListLike s el) => Iteratee s m [el]
stream2list = liftM (concatMap LL.toList) getChunks
{-# INLINE stream2list #-}

-- |Read a stream to the end and return all of its elements as a stream.
-- This iteratee returns all data from the stream *strictly*.
stream2stream :: (Monad m, Nullable s, Monoid s) => Iteratee s m s
stream2stream = liftM mconcat getChunks
{-# INLINE stream2stream #-}


-- ------------------------------------------------------------------------
-- Parser combinators

-- |Takes an element predicate and returns the (possibly empty) prefix of
-- the stream.  None of the characters in the string satisfy the character
-- predicate.
-- If the stream is not terminated, the first character of the remaining stream
-- satisfies the predicate.
-- 
-- N.B. 'breakE' should be used in preference to @break@.
-- @break@ will retain all data until the predicate is met, which may
-- result in a space leak.
-- 
-- The analogue of @List.break@

break :: (Monad m, LL.ListLike s el) => (el -> Bool) -> Iteratee s m s
break cpred = icontP (step mempty)
  where
    step bfr c@(Chunk str)
      | LL.null str       =  (icontP (step bfr), c)
      | otherwise         =  case LL.break cpred str of
        (str', tail')
          | LL.null tail' -> (icontP (step (bfr `mappend` str)), Chunk tail')
          | otherwise     -> (idone (bfr `mappend` str'), (Chunk tail'))
    step bfr stream       =  (idone bfr, stream)
{-# INLINE break #-}


-- |Attempt to read the next element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated.
-- 
-- The analogue of @List.head@
head :: (Monad m, LL.ListLike s el) => Iteratee s m el
head = icontP step
  where
  step c@(Chunk vec)
    | LL.null vec  = (icontP step, c)
    | otherwise    = (idone (LL.head vec), (Chunk $ LL.tail vec))
  step stream      = (ierr (icontP step) (setEOF stream), stream)
{-# INLINE head #-}

-- | Similar to @head@, except it returns @Nothing@ if the stream
-- is terminated.
tryHead :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe el)
tryHead = icontP step
  where
  step c@(Chunk vec)
    | LL.null vec  = (icontP step, c)
    | otherwise    = (idone (Just $ LL.head vec), Chunk $ LL.tail vec)
  step stream      = (idone Nothing, stream)
{-# INLINE tryHead #-}

-- |Attempt to read the last element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated
-- 
-- The analogue of @List.last@
last :: (Monad m, LL.ListLike s el, Nullable s) => Iteratee s m el
last = icontP (step Nothing)
  where
  step l c@(Chunk xs)
    | nullC xs     = (icontP (step l), c)
    | otherwise    = (icontP $ step (Just $ LL.last xs), Chunk LL.empty)
  step l s@(EOF _) = case l of
    Nothing -> (ierr (icontP (step l)) (setEOF s), s)
    Just x  -> (idone x, s)
{-# INLINE last #-}


-- |Given a sequence of characters, attempt to match them against
-- the characters on the stream.  Return the count of how many
-- characters matched.  The matched characters are removed from the
-- stream.
-- For example, if the stream contains 'abd', then (heads 'abc')
-- will remove the characters 'ab' and return 2.
heads :: (Monad m, Nullable s, LL.ListLike s el, Eq el)
  => s
  -> Iteratee s m Int
heads st | nullC st = return 0
heads st = loop 0 st
  where
  loop cnt xs
    | nullC xs  = return cnt
    | otherwise = icontP (step cnt xs)
  step cnt str (Chunk xs) | nullC xs  = (icontP (step cnt str), Chunk xs)
  step cnt str stream     | nullC str = (idone cnt, stream)
  step cnt str s@(Chunk xs) =
    if LL.head str == LL.head xs
       then step (succ cnt) (LL.tail str) (Chunk $ LL.tail xs)
       else (idone cnt, s)
  step cnt _ stream         = (idone cnt, stream)
{-# INLINE heads #-}


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return @Just c@ if successful, return @Nothing@ if the stream is
-- terminated by 'EOF'.
peek :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe el)
peek = icontP step
  where
    step s@(Chunk vec)
      | LL.null vec = (icontP step, s)
      | otherwise   = (idone (Just $ LL.head vec), s)
    step stream     = (idone Nothing, stream)
{-# INLINE peek #-}

-- | Return a chunk of @t@ elements length while consuming @d@ elements
--   from the stream.  Useful for creating a 'rolling average' with
--  'convStream'.
roll
  :: (Monad m, Functor m, Nullable s, LL.ListLike s el, LL.ListLike s' s)
  => Int  -- ^ length of chunk (t)
  -> Int  -- ^ amount to consume (d)
  -> Iteratee s m s'
roll t d | t > d  = icontP step
  where
    step (Chunk vec)
      | LL.length vec >= d =
          (idone (LL.singleton $ LL.take t vec), Chunk $ LL.drop d vec)
      | LL.length vec >= t =
          (idone (LL.singleton $ LL.take t vec) <* drop (d-LL.length vec)
           ,mempty)
      | LL.null vec        = (icontP step, mempty)
      | otherwise          = (icontP (step' vec), mempty)
    step stream            = (idone LL.empty, stream)
    step' v1 (Chunk vec)   = step . Chunk $ v1 `mappend` vec
    step' v1 stream        = (idone (LL.singleton v1), stream)
roll t d = LL.singleton <$> joinI (take t stream2stream) <* drop (d-t)
  -- d is >= t, so this version works
{-# INLINE roll #-}


-- |Drop n elements of the stream, if there are that many.
-- 
-- The analogue of @List.drop@
drop :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Iteratee s m ()
drop 0  = idone ()
drop n' = icontP (step n')
  where
    step n (Chunk str)
      | LL.length str < n = (icontP (step (n - LL.length str)), mempty)
      | otherwise         = (idone (), Chunk (LL.drop n str))
    step _ stream         = (idone (), stream)
{-# INLINE drop #-}

-- |Skip all elements while the predicate is true.
-- 
-- The analogue of @List.dropWhile@
dropWhile :: (Monad m, LL.ListLike s el) => (el -> Bool) -> Iteratee s m ()
dropWhile p = icontP step
  where
    step (Chunk str)
      | LL.null left = (icontP step, mempty)
      | otherwise    = (idone (), Chunk left)
      where
        left = LL.dropWhile p str
    step stream      = (idone (), stream)
{-# INLINE dropWhile #-}


-- | Return the total length of the remaining part of the stream.
-- 
-- This forces evaluation of the entire stream.
-- 
-- The analogue of @List.length@
length :: (Monad m, Num a, LL.ListLike s el) => Iteratee s m a
length = icontP (step 0)
  where
    step !i (Chunk xs) = let newL = i + fromIntegral (LL.length xs)
                         in newL `seq` (icontP (step newL), mempty)
    step !i stream     = (idone i, stream)
{-# INLINE length #-}

-- | Get the length of the current chunk, or @Nothing@ if 'EOF'.
-- 
-- This function consumes no input.
chunkLength :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe Int)
chunkLength = icontP step
 where
  step s@(Chunk xs) = (idone (Just $ LL.length xs), s)
  step stream       = (idone Nothing, stream)
{-# INLINE chunkLength #-}

-- | Take @n@ elements from the current chunk, or the whole chunk if
-- @n@ is greater.
takeFromChunk ::
  (Monad m, Nullable s, LL.ListLike s el)
  => Int
  -> Iteratee s m s
takeFromChunk n | n <= 0 = idone empty
takeFromChunk n = icontP step
 where
  step (Chunk xs) = let (h,t) = LL.splitAt n xs in (idone h, Chunk t)
  step stream     = (idone empty, stream)
{-# INLINE takeFromChunk #-}

-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- |Takes an element predicate and an iteratee, running the iteratee
-- on all elements of the stream until the predicate is met.
-- 
-- the following rule relates @break@ to @breakE@
-- @break@ pred === @joinI@ (@breakE@ pred stream2stream)
-- 
-- @breakE@ should be used in preference to @break@ whenever possible.
breakE
  :: (LL.ListLike s el, NullPoint s, Monad m, Functor m)
  => (el -> Bool)
  -> Enumeratee s s m a
breakE cpred = go
 where
  go = eneeCheckIfDonePass (icont . step)
  step k (Chunk s)
      | LL.null s  = return (icont (step k), mempty)
      | otherwise  = case LL.break cpred s of
        (str', tail')
          | LL.null tail' -> do
              (i', _) <- k (Chunk str')
              return (go i' <* dropWhile (not . cpred), Chunk tail')
                               -- if the inner iteratee completes before
                               -- the predicate is met, elements still
                               -- need to be dropped.
          | otherwise -> (idone *** const (Chunk tail')) `liftM` k (Chunk str')
  step k stream       =  return (idone (icont k), stream)
{-# INLINE breakE #-}

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements, even if the iteratee has accepted fewer.
-- 
-- The analogue of @List.take@
take ::
  (Monad m, Nullable s, LL.ListLike s el)
  => Int   -- ^ number of elements to consume
  -> Enumeratee s s m a
take n' iter
  | n' <= 0   = return iter
  | otherwise = runIter iter onDone onCont onErr onReq
 where
  onDone x = drop n' >> idone (idone x)
  onCont k = if n' == 0 then idone (icont k)
                else icont (step n' k)
  onErr i e = ierr (take n' i) e
  onReq mb doB = ireq mb (take n' . doB)

  step n k c@(Chunk str)
      | LL.null str        = return (icont (step n k), c)
      | LL.length str <= n = (take (n - LL.length str) *** const mempty)
                             `liftM` k (Chunk str)
      | otherwise          = (idone *** const (Chunk s2)) `liftM` k (Chunk s1)
      where (s1, s2) = LL.splitAt n str
  step _n k stream       = return (idone (icont k), stream)
{-# INLINE take #-}

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of 'take' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early.
-- 
-- Iteratees composed with 'takeUpTo' will consume only enough elements to
-- reach a done state.  Any remaining data will be available in the outer
-- stream.
-- 
-- > > let iter = do
-- > h <- joinI $ takeUpTo 5 I.head
-- > t <- stream2list
-- > return (h,t)
-- > 
-- > > enumPureNChunk [1..10::Int] 3 iter >>= run >>= print
-- > (1,[2,3,4,5,6,7,8,9,10])
-- > 
-- > > enumPureNChunk [1..10::Int] 7 iter >>= run >>= print
-- > (1,[2,3,4,5,6,7,8,9,10])
-- 
-- in each case, @I.head@ consumes only one element, returning the remaining
-- 4 elements to the outer stream
takeUpTo :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Enumeratee s s m a
takeUpTo i iter
 | i <= 0    = idone iter
 | otherwise = runIter iter onDone onCont onErr onReq
  where
    onDone x = idone (idone x)
    onCont k = if i == 0 then idone (icont k)
                         else icont (step i k)
    onErr i' e = ierr (takeUpTo i i') e
    onReq mb doB = ireq mb (takeUpTo i . doB)

    step n k c@(Chunk str)
      | LL.null str       = return (icont (step n k), c)
      | LL.length str < n = first (takeUpTo (n - LL.length str))
                            `liftM` k (Chunk str)
      | otherwise         = do
         -- check to see if the inner iteratee has completed, and if so,
         -- grab any remaining stream to put it in the outer iteratee.
         -- the outer iteratee is always complete at this stage, although
         -- the inner may not be.
         let (s1, s2) = LL.splitAt n str
         (iter', preStr) <- k (Chunk s1)
         case preStr of
              (Chunk preC)
                | LL.null preC -> return (idone iter', Chunk s2)
                | otherwise    -> return (idone iter'
                                     , Chunk $ preC `LL.append` s2)
              -- this case shouldn't ever happen, except possibly
              -- with broken iteratees
              _                -> return (idone iter', preStr)
    step _ k stream       = return (idone (icont k), stream)
{-# INLINE takeUpTo #-}

-- | Takes an element predicate and returns the (possibly empty)
-- prefix of the stream. All characters
-- in the string will satisfy the character predicate. If the stream
-- is not terminated, the first character of the
-- remaining stream will not satisfy the predicate.
-- 
-- The analogue of @List.takeWhile@, see also @break@ and @takeWhileE@
takeWhile :: (Monad m, LL.ListLike s el ) => (el -> Bool) -> Iteratee s m s
takeWhile = break . (not .)
{-# INLINEABLE takeWhile #-}

-- |Takes an element predicate and an iteratee, running the iteratee
-- on all elements of the stream while the predicate is met.
-- 
-- This is preferred to @takeWhile@.
takeWhileE
 :: (LL.ListLike s el, NullPoint s, Monad m, Functor m)
 => (el -> Bool)
 -> Enumeratee s s m a
takeWhileE = breakE . (not .)
{-# INLINEABLE takeWhileE #-}

-- |Map the stream: another iteratee transformer
-- Given the stream of elements of the type @el@ and the function @(el->el')@,
-- build a nested stream of elements of the type @el'@ and apply the
-- given iteratee to it.
-- 
-- The analog of @List.map@
mapStream
  :: (Monad m
     ,LL.ListLike (s el) el
     ,LL.ListLike (s el') el'
     ,NullPoint (s el)
     ,LooseMap s el el')
  => (el -> el')
  -> Enumeratee (s el) (s el') m a
mapStream f = mapChunks (lMap f)
{-# SPECIALIZE mapStream :: Monad m => (el -> el') -> Enumeratee [el] [el'] m a #-}

-- |Map the stream rigidly.
-- 
-- Like 'mapStream', but the element type cannot change.
-- This function is necessary for @ByteString@ and similar types
-- that cannot have 'LooseMap' instances, and may be more efficient.
rigidMapStream
  :: (Monad m, LL.ListLike s el, NullPoint s)
  => (el -> el)
  -> Enumeratee s s m a
rigidMapStream f = mapChunks (LL.rigidMap f)
{-# SPECIALIZE rigidMapStream :: Monad m => (el -> el) -> Enumeratee [el] [el] m a #-}
{-# SPECIALIZE rigidMapStream :: Monad m => (Word8 -> Word8) -> Enumeratee B.ByteString B.ByteString m a #-}


-- |Creates an 'enumeratee' with only elements from the stream that
-- satisfy the predicate function.  The outer stream is completely consumed.
-- 
-- The analogue of @List.filter@
filter
  :: (Monad m, Functor m, Nullable s, LL.ListLike s el)
  => (el -> Bool)
  -> Enumeratee s s m a
filter p = convStream (LL.filter p <$> getChunk)
{-# INLINE filter #-}

-- |Creates an 'Enumeratee' in which elements from the stream are
-- grouped into @sz@-sized blocks.  The final block may be smaller
-- than \sz\.
group
  :: (LL.ListLike s el, Monad m, Nullable s)
  => Int  -- ^ size of group
  -> Enumeratee s [s] m a
group cksz iinit = icont (step 0 id iinit)
 where
  -- there are two cases to consider for performance purposes:
  --  1 - grouping lots of small chunks into bigger chunks
  --  2 - breaking large chunks into smaller pieces
  -- case 2 is easier, simply split a chunk into as many pieces as necessary
  -- and pass them to the inner iteratee as one list.  @gsplit@ does this.
  --
  -- case 1 is a bit harder, need to hold onto each chunk and coalesce them
  -- after enough have been received.  Currently using a Hughes list
  -- for this, i.e ([s] -> [s])
  --
  -- not using eneeCheckIfDone because that loses final chunks at EOF
  step sz pfxd icur (Chunk s)
    | LL.null s               = return (icont (step sz pfxd icur), Chunk s)
    | LL.length s + sz < cksz = return (icont (step (sz+LL.length s)
                                             (pfxd . (s:)) icur)
                                        , mempty)
    | otherwise               =
        let (full, rest) = gsplit . mconcat $ pfxd [s]
            pfxd'        = if LL.null rest then id else (rest:)
            onDone x  = return (idone (idone x), Chunk rest)
            onErr i e = return (ierr (icont (step (LL.length rest) pfxd' i)) e
                                , Chunk rest)
            onCont k  = (icont . step (LL.length rest) pfxd'
                         *** const (Chunk rest))
                         `liftM` k (Chunk full)
            -- since step is a monadic function, the monadic request can be
            -- inlined, saving an indirection
            onReq mb doB = mb >>= \b ->
                           step (LL.length rest) pfxd' (doB b) (Chunk rest)
        in  runIter icur onDone onCont onErr onReq
  step _ pfxd icur mErr = case pfxd [] of
                         []   -> return (idone icur, mErr)
                         rest -> ((, mErr) . idone) `liftM`
                                 enumPure1Chunk [mconcat rest] icur

  gsplit ls = case LL.splitAt cksz ls of
    (g, rest) | LL.null rest -> if LL.length g == cksz
                                   then ([g], LL.empty)
                                   else ([], g)
              | otherwise -> let (grest, leftover) = gsplit rest
                                 g' = g : grest
                             in (g', leftover)


-- | Creates an 'enumeratee' in which elements are grouped into
-- contiguous blocks that are equal according to a predicate.
-- 
-- The analogue of 'List.groupBy'
groupBy
  :: forall s el m a. (LL.ListLike s el, Monad m, Nullable s)
  => (el -> el -> Bool)
  -> Enumeratee s [s] m a
groupBy same iinit = icont $ go iinit (const True, id)
  where 
    -- As in group, need to handle grouping efficiently when we're fed
    -- many small chunks.
    -- 
    -- Move the accumulation of groups by chunks into an accumulator
    -- that runs through gsplit, which is pfx / partial here. When we
    -- get a chunk, use gsplit to retrieve any full chunks and get the
    -- carried accumulator.
    -- 
    -- At the end, "finish" the accumulator and handle the last chunk,
    -- unless the stream was entirely empty and there is no
    -- accumulator.
    go icurr pfx (Chunk s) = case gsplit pfx s of
      ([], partial)   -> return (icont $ go icurr partial, mempty)
      (full, partial) ->
        -- if the inner iteratee is done, the outer iteratee needs to be
        -- notified to terminate.
        -- if the inner iteratee is in an error state, that error should
        -- be lifted to the outer iteratee
        let onCont k = k (Chunk full) >>= \(inext, str') ->
                         case str' of
                           Chunk rest -> return (icont $ go inext partial
                                           , Chunk $ mconcat rest)
                           EOF mex -> return (icont $ go inext partial, EOF mex)
            onErr inext e = return (ierr (icont (go inext partial)) e
                                    , EOF (Just e))
            onDone :: a -> m (Iteratee s m (Iteratee [s] m a), Stream s)
            onDone a      = return (idone (idone a)
                                    , Chunk . mconcat $ snd partial [])
            onReq mb doB  = mb >>= \b -> go (doB b) pfx (Chunk s)
        in runIter icurr onDone onCont onErr onReq
    go icurr (_inpfx, pfxd) (EOF mex) = case pfxd [] of
      [] -> ((,EOF mex) . idone) `liftM` enumChunk (EOF mex) icurr
      rest -> ((,EOF mex) . idone) `liftM`
               (enumPure1Chunk [mconcat rest] icurr >>= enumChunk (EOF mex))

    -- Here, gsplit carries an accumulator consisting of a predicate
    -- "inpfx" that indicates whether a new element belongs in the
    -- growing group, and a difference list to ultimately generate the
    -- group.
    --
    -- The initial accumulator is a group that can accept anything and
    -- is empty.
    -- 
    -- New chunks are split into groups. The cases are 

    --   0. Trivially, empty chunk

    --   1. One chunk, in the currently growing group: continue the
    --   current prefix (and generate a new predicate, in case we had
    --   the initial predicate
    
    --   2. One chunk, but not in the current group: finish the
    --   current group and return a new accumulator for the
    --   newly-started gorup
    
    --   3. Multiple chunks, the first of which completes the
    --   currently growing group
    
    --   4. Multiple chunks, the first of which is a new group
    --   separate from the currently-growing group
    gsplit (inpfx, pfxd) curr = case llGroupBy same curr of
      [] -> ([], (inpfx, pfxd))
      [g0] | inpfx (LL.head g0) -> ([], (same $ LL.head g0, pfxd . (g0 :)))
           | otherwise          -> ([mconcat $ pfxd []], (same $ LL.head g0, pfxd . (g0 :)))
      (g0:grest@(_:_)) | inpfx (LL.head g0) -> let glast = Prelude.last grest
                                                   gfirst = mconcat $ (pfxd . (g0 :)) []
                                                   gdone = gfirst : Prelude.init grest
                                               in ( gdone, (same (LL.head glast), (glast :)) )
                       | otherwise -> let glast = Prelude.last grest
                                          gfirst = mconcat $ pfxd []
                                          gdone = gfirst : Prelude.init grest
                                      in ( gdone, (same (LL.head glast), (glast :)) )
    llGroupBy eq l -- Copied from Data.ListLike, avoid spurious (Eq el) constraint
      | LL.null l = []
      | otherwise = (LL.cons x ys):(llGroupBy eq zs)
        where (ys, zs) = LL.span (eq x) xs
              x = LL.head l
              xs = LL.tail l

{-# INLINE groupBy #-}

-- | @merge@ offers another way to nest iteratees: as a monad stack.
-- This allows for the possibility of interleaving data from multiple
-- streams.
-- 
-- > -- print each element from a stream of lines.
-- > logger :: (MonadIO m) => Iteratee [ByteString] m ()
-- > logger = mapM_ (liftIO . putStrLn . B.unpack)
-- >
-- > -- combine alternating lines from two sources
-- > -- To see how this was derived, follow the types from
-- > -- 'ileaveLines logger' and work outwards.
-- > run =<< enumFile 10 "file1" (joinI $ enumLinesBS $
-- >           ( enumFile 10 "file2" . joinI . enumLinesBS $ joinI
-- >                 (ileaveLines logger)) >>= run)
-- > 
-- > ileaveLines :: (Functor m, Monad m)
-- >   => Enumeratee [ByteString] [ByteString] (Iteratee [ByteString] m)
-- >        [ByteString]
-- > ileaveLines = merge (\l1 l2 ->
-- >    [B.pack "f1:\n\t" ,l1 ,B.pack "f2:\n\t" ,l2 ]
-- > 
-- > 
-- 
merge ::
  (LL.ListLike s1 el1
   ,LL.ListLike s2 el2
   ,Nullable s1
   ,Nullable s2
   ,Monad m
   ,Functor m)
  => (el1 -> el2 -> b)
  -> Enumeratee s2 b (Iteratee s1 m) a
merge f = convStream $ f <$> lift head <*> head
{-# INLINE merge #-}

-- | A version of merge which operates on chunks instead of elements.
-- 
-- mergeByChunks offers more control than 'merge'.  'merge' terminates
-- when the first stream terminates, however mergeByChunks will continue
-- until both streams are exhausted.
-- 
-- 'mergeByChunks' guarantees that both chunks passed to the merge function
-- will have the same number of elements, although that number may vary
-- between calls.
mergeByChunks ::
  (Nullable c2, Nullable c1
  ,NullPoint c2, NullPoint c1
  ,LL.ListLike c1 el1, LL.ListLike c2 el2
  ,Functor m, Monad m)
  => (c1 -> c2 -> c3)  -- ^ merge function
  -> (c1 -> c3)
  -> (c2 -> c3)
  -> Enumeratee c2 c3 (Iteratee c1 m) a
mergeByChunks f f1 f2 = unfoldConvStream iter (0 :: Int)
 where
  iter 1 = (1,) . f1 <$> lift getChunk
  iter 2 = (2,) . f2 <$> getChunk
  iter _ = do
    ml1 <- lift chunkLength
    ml2 <- chunkLength
    case (ml1, ml2) of
      (Just l1, Just l2) -> do
        let tval = min l1 l2
        c1 <- lift $ takeFromChunk tval
        c2 <- takeFromChunk tval
        return (0, f c1 c2)
      (Just _, Nothing) -> iter 1
      (Nothing, _)      -> iter 2
{-# INLINE mergeByChunks #-}

-- ------------------------------------------------------------------------
-- Folds

-- | Left-associative fold.
-- 
-- The analogue of @List.foldl@
foldl
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el)
  => (a -> el -> a)
  -> a
  -> Iteratee s m a
foldl f i = icontP (step i)
  where
    step acc c@(Chunk xs)
      | LL.null xs  = (icontP (step acc), c)
      | otherwise   = (icontP (step $ FLL.foldl f acc xs), mempty)
    step acc stream = (idone acc, stream)
{-# INLINE foldl #-}


-- | Left-associative fold that is strict in the accumulator.
-- This function should be used in preference to 'foldl' whenever possible.
-- 
-- The analogue of @List.foldl'@.
foldl'
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el)
  => (a -> el -> a)
  -> a
  -> Iteratee s m a
foldl' f i = icontP (step i)
  where
    step acc c@(Chunk xs)
      | LL.null xs  = (icontP (step acc), c)
      | otherwise   = (icontP (step $! FLL.foldl' f acc xs), mempty)
    step acc stream = (idone acc, stream)
{-# INLINE foldl' #-}

-- | Variant of foldl with no base case.  Requires at least one element
--   in the stream.
-- 
-- The analogue of @List.foldl1@.
foldl1
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el)
  => (el -> el -> el)
  -> Iteratee s m el
foldl1 f = icontP step
  where
    step c@(Chunk xs)
    -- After the first chunk, just use regular foldl.
      | LL.null xs = (icontP step, c)
      | otherwise  = (foldl f $ FLL.foldl1 f xs, mempty)
    step stream    = (ierr (icontP step) $ toException EofException
                      , stream)
{-# INLINE foldl1 #-}


-- | Strict variant of 'foldl1'.
foldl1'
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el)
  => (el -> el -> el)
  -> Iteratee s m el
foldl1' f = icontP step
  where
    step c@(Chunk xs)
    -- After the first chunk, just use regular foldl'.
      | LL.null xs = (icontP step, c)
      | otherwise  = (foldl' f $ FLL.foldl1 f xs, mempty)
    step stream    = (ierr (icontP step) $ toException EofException
                      , stream)
{-# INLINE foldl1' #-}


-- | Sum of a stream.
sum :: (Monad m, LL.ListLike s el, Num el) => Iteratee s m el
sum = icontP (step 0)
  where
    step acc c@(Chunk xs)
      | LL.null xs = (icontP (step acc), c)
      | otherwise  = (icontP (step $! acc + LL.sum xs), mempty)
    step acc str   = (idone acc, str)
{-# INLINE sum #-}


-- | Product of a stream.
product :: (Monad m, LL.ListLike s el, Num el) => Iteratee s m el
product = icontP (step 1)
  where
    step acc c@(Chunk xs)
      | LL.null xs = (icontP (step acc), c)
      | otherwise  = (icontP (step $! acc * LL.product xs), mempty)
    step acc str   = (idone acc, str)
{-# INLINE product #-}


-- ------------------------------------------------------------------------
-- Zips

-- |Enumerate two iteratees over a single stream simultaneously.
-- 
-- Compare to @List.zip@.
zip
  :: (Monad m, Nullable s, LL.ListLike s el)
  => Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a, b)
zip x0 y0 = runIter x0 (odx y0) (ocx y0) (oex y0) (orx y0)
 where
  odx yIter a      = (a, ) `liftM` yIter
  ocx yIter k      = runIter yIter (ody k) (ocy k) (oey k) (ory k)
  oex yIter i' e   = throwRec e (zip i' yIter)
  orx yIter ma doA = ireq ma $ (\xIter -> zip xIter yIter) . doA

  ody x_k b        = (,b) `liftM` icont x_k
  ocy xK yK        = icont (step xK yK)
  oey xK i' e      = throwRec e (zip (icont xK) i')
  ory xK mb doB    = ireq mb $ (zip (icont xK) . doB)

  step xK yK (Chunk xs) | nullC xs = return (icont (step xK yK), Chunk xs)
  step xK yK str   = do
    (x,xLeft) <- xK str
    (y,yLeft) <- yK str
    return (zip x y, shorter xLeft yLeft)

  shorter c1@(Chunk xs) c2@(Chunk ys)
    | LL.length xs < LL.length ys = c1
    | otherwise                   = c2
  shorter e@(EOF _)  _         = e
  shorter _          e@(EOF _) = e
  
{-# INLINE zip #-}

zip3
  :: (Monad m, Nullable s, LL.ListLike s el)
  => Iteratee s m a -> Iteratee s m b
  -> Iteratee s m c -> Iteratee s m (a, b, c)
zip3 a b c = zip a (zip b c) >>=
  \(r1, (r2, r3)) -> return (r1, r2, r3)
{-# INLINE zip3 #-}

zip4
  :: (Monad m, Nullable s, LL.ListLike s el)
  => Iteratee s m a -> Iteratee s m b
  -> Iteratee s m c -> Iteratee s m d
  -> Iteratee s m (a, b, c, d)
zip4 a b c d = zip a (zip3 b c d) >>=
  \(r1, (r2, r3, r4)) -> return (r1, r2, r3, r4)
{-# INLINE zip4 #-}

zip5
  :: (Monad m, Nullable s, LL.ListLike s el)
  => Iteratee s m a -> Iteratee s m b
  -> Iteratee s m c -> Iteratee s m d
  -> Iteratee s m e -> Iteratee s m (a, b, c, d, e)
zip5 a b c d e = zip a (zip4 b c d e) >>=
  \(r1, (r2, r3, r4, r5)) -> return (r1, r2, r3, r4, r5)
{-# INLINE zip5 #-}

-- | Enumerate over two iteratees in parallel as long as the first iteratee
-- is still consuming input.  The second iteratee will be terminated with EOF
-- when the first iteratee has completed.  An example use is to determine
-- how many elements an iteratee has consumed:
-- 
-- > snd <$> enumWith (dropWhile (<5)) length
-- 
-- Compare to @zip@
enumWith
  :: (Monad m, Nullable s, LL.ListLike s el)
  => Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a, b)
enumWith x0 y0 = runIter x0 (odx y0) (ocx y0) (oex y0) (orx y0)
 where
  odx yIter a      = (a,) `liftM` joinIM (enumEof yIter)
  ocx yIter k      = runIter yIter (ody k) (ocy k) (oey k) (ory k)
  oex yIter i' e   = throwRec e (enumWith i' yIter)
  orx yIter ma doA = ireq ma $ (\xIter -> enumWith xIter yIter) . doA

  ody x_k b        = (,b) `liftM` icont x_k
  ocy xK yK        = icont (step xK yK)
  oey xK i' e      = throwRec e (enumWith (icont xK) i')
  ory xK mb doB    = ireq mb $ (enumWith (icont xK) . doB)

  step xK yK (Chunk xs) | nullC xs = return (icont (step xK yK), Chunk xs)
  step xK yK str = do
    (x, xLeft) <- xK str
    (y,_yLeft) <- yK str
    return (enumWith x y, xLeft)
{-# INLINE enumWith #-}

-- |Enumerate a list of iteratees over a single stream simultaneously
-- and discard the results. This is a different behavior than Prelude's
-- sequence_ which runs iteratees in the list one after the other.
-- 
-- Compare to @Prelude.sequence_@.
sequence_
  :: forall el s m a. (Monad m, LL.ListLike s el, Nullable s)
  => [Iteratee s m a]
  -> Iteratee s m ()
sequence_ = check []
  where
    -- recursively checks each input iteratee to see if it's finished.
    -- all of the unfinished iteratees are run with a single chunk,
    -- then checked again.

    -- a possible inefficiency is if multiple iteratees are in the
    -- Request state (monadic action), as each request is fed to the
    -- enumerator separately.  An alternative implementation would aggregate
    -- all monadic effects (as this version aggregates all continuations)
    -- to perform them at once.
    check [] [] = idone ()
    check ks [] = icont (step ks)
    check ks (i:iters) = runIter i (\_ -> check ks iters)
                                   (onCont ks iters)
                                   (onErr ks iters)
                                   (onReq ks iters)
    onCont ks iters k  = check (k:ks) iters
    onErr ks iters i e = throwRec e (check ks (i:iters))
    onReq :: [Stream s -> m (Iteratee s m a, Stream s)]
          -> [Iteratee s m a]
          -> m b
          -> (b -> Iteratee s m a)
          -> Iteratee s m ()
    onReq ks iters mb doB = ireq mb (\b -> check ks (doB b:iters))

    step ks str = first (check []) `liftM` CM.foldM (accf str) ([], str) ks
    accf str (iS, !strs) k = ((:iS) *** flip shorter strs) `liftM` k str
      
    -- return the shorter one of two streams; errors are propagated with the
    -- priority given to the "left"
    shorter c1@(Chunk xs) c2@(Chunk ys)
      | LL.length xs < LL.length ys = c1
      | otherwise                   = c2
    shorter (EOF e1 ) (EOF e2 ) = EOF (e1 `mplus` e2)
    shorter e@(EOF _) _         = e
    shorter _         e@(EOF _) = e

-- |Transform an iteratee into one that keeps track of how much data it
-- consumes.
countConsumed :: forall a s el m n.
                 (Monad m, LL.ListLike s el, Nullable s, Integral n) =>
                 Iteratee s m a
              -> Iteratee s m (a, n)
countConsumed = check 0
  where
    newLen :: n -> s -> s -> n
    newLen n c c' = n + fromIntegral ((LL.length c) - (LL.length c'))
    check :: n -> Iteratee s m a -> Iteratee s m (a,n)
    check !n iter = runIter iter (onDone n)
                                 (onCont n)
                                 (onErr n)
                                 (onReq n)
    step !n k str@(Chunk c) = k str >>= \res -> return $ case res of
      (i, Chunk c')            -> (check (newLen n c c') i, Chunk c')
      (i, str'@(EOF (Just e))) -> (throwRec e (check (newLen n c mempty) i)
                                   , str')
      (i, str')                -> (throwRec EofException
                                            (check (newLen n c mempty) i), str')
    step n k str = first (liftM (,n)) `liftM` k str
    onDone n a  = idone (a,n)
    onCont n k  = icont (step n k)
    onErr n i e = throwRec e (check n i)
    onReq n mb doB = ireq mb (check n . doB)
{-# INLINE countConsumed #-}

-- ------------------------------------------------------------------------
-- Enumerators

-- |The pure n-chunk enumerator
-- It passes a given stream of elements to the iteratee in @n@-sized chunks.
enumPureNChunk :: (Monad m, LL.ListLike s el) => s -> Int -> Enumerator s m a
enumPureNChunk str n iter
  | LL.null str = return iter
  | n > 0       = enum' str iter
  | otherwise   = error $ "enumPureNChunk called with n==" ++ show n
  where
    enum' str' iter'
      | LL.null str' = return iter'
      | otherwise    = let (s1, s2)     = LL.splitAt n str'
                           onCont k     = k (Chunk s1) >>= enum' s2 . fst
                           onErr i' e   = return $ ierr i' e
                           onReq mb doB = mb >>= enum' str' . doB
                       in runIter iter' idoneM onCont onErr onReq
{-# INLINE enumPureNChunk #-}

-- | Convert an iteratee to a \"greedy\" version.
--
-- When a chunk is received, repeatedly run the input iteratee
-- until the entire chunk is consumed, then the outputs
-- are combined (via 'mconcat').
--
-- > > let l = [1..5::Int]
-- > > run =<< enumPure1Chunk l (joinI (take 2 stream2list))
-- > [1,2]
-- > > run =<< enumPure1Chunk l (greedy $ joinI (I.take 2 stream2list))
-- > [1,2,3,4,5]
--
-- Note that a greedy iteratee will consume the entire input chunk and force
-- the next chunk before returning a value.  A portion of the second chunk may
-- be consumed.
-- 
-- 'greedy' may be useful on the first parameter of 'convStream', e.g.
-- 
-- > convStream (greedy someIter)
--
-- to create more efficient converters.
greedy ::
 (Monad m, Functor m, LL.ListLike s el', Monoid a) =>
  Iteratee s m a
  -> Iteratee s m a
greedy iter = step1 [] iter
 where
  step1 acc i = runIter i onDone onCont onErr onReq
        where
          onDone a = idone . mconcat $ reverse (a:acc)
          onCont k = icont $ step2 acc k
          onErr iRes e = ierr (step1 acc iRes) e
          onReq mb doB = ireq mb (step1 acc . doB)
  step2 acc k = undefined 
{-
    -- no input was consumed
    | lastLen == LL.length str =
        case runIter iter (Left) (Right) (
    -- some input was consumed, and data was left over.
    -- the iteratee should never be in a Cont state here...
    | otherwise = case runIter iter Left undefined undefined undefined of
        Left a -> step (a:acc)  iter 


    | otherwise   = joinIM $ do
      i2 <- enumPure1Chunk str iter
      result <- runIter i2 (\a -> return $ Left a)
                           (\k -> return $ Right (icont k))
                           (\i' e -> undefined)
                           (\mb doB -> undefined)
      case result of
        Left (a, Chunk resS)
          | LL.null resS
              || LL.length resS == LL.length str -> return $
                         idone (mconcat $ reverse (a:acc)) (Chunk resS)
        Left (a, stream) -> return $ step (a:acc) iter stream
        Right i -> return $ fmap (mconcat . reverse . (:acc)) i
  step acc iter stream = joinIM $
    enumChunk stream (fmap (mconcat . reverse . (:acc)) iter)
-}
{-# INLINE greedy #-}

-- ------------------------------------------------------------------------
-- Monadic functions

-- | Map a monadic function over the elements of the stream and ignore the
-- result.
mapM_
  :: (Monad m, LL.ListLike s el, Nullable s)
  => (el -> m b)
  -> Iteratee s m ()
mapM_ f = icont step
  where
    step c@(Chunk xs) | LL.null xs = return (icont step, c)
    step (Chunk xs) = LL.mapM_ f xs >> return (icont step, mempty)
    step s@(EOF _)  = return (idone (), s)
{-# INLINE mapM_ #-}

-- |The analogue of @Control.Monad.foldM@
foldM
  :: (Monad m, LL.ListLike s b, Nullable s)
  => (a -> b -> m a)
  -> a
  -> Iteratee s m a
foldM f e = icont (step e)
  where
    step acc c@(Chunk xs) | LL.null xs = return (icont (step acc), c)
    step acc (Chunk xs) = CM.foldM f acc (LL.toList xs) >>= \acc' ->
                            return (icont (step acc'), mempty)
    step acc stream     = return (idone acc, stream)
{-# INLINE foldM #-}
