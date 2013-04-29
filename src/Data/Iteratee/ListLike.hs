{-# LANGUAGE FlexibleContexts
            ,BangPatterns
            ,TupleSections
            ,ScopedTypeVariables #-}

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
import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, mplus)
import qualified Control.Monad as CM
import Control.Monad.Trans.Class

-- Useful combinators for implementing iteratees and enumerators

-- | Check if a stream has received 'EOF'.
isFinished :: (Monad m, LL.ListLike s el) => Iteratee s m Bool
isFinished = icontP check
  where
  check c@(Chunk xs)
    | LL.null xs    = continueP check
    | otherwise   = ContDone False c
  check NoData    = continueP check
  check s@(EOF _) = ContDone True s
{-# INLINE isFinished #-}

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list.
-- This iteratee returns all data from the stream *strictly*.
stream2list :: (Monad m, LL.ListLike s el) => Iteratee s m [el]
stream2list = liftM (concatMap LL.toList) getChunks
{-# INLINE stream2list #-}

-- |Read a stream to the end and return all of its elements as a stream.
-- This iteratee returns all data from the stream *strictly*.
stream2stream :: (Monad m, Monoid s) => Iteratee s m s
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
    step bfr (Chunk str)  =  case LL.break cpred str of
        (str', tail')
          | LL.null tail' -> continueP (step (bfr `mappend` str))
          | otherwise     -> ContDone (bfr `mappend` str') $ Chunk tail'
    step bfr NoData       =  continueP (step bfr)
    step bfr stream       =  ContDone bfr stream
{-# INLINE break #-}


-- |Attempt to read the next element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated.
-- 
-- The analogue of @List.head@, with all the problems thereof.  Consider
-- using 'tryHead'.
head :: (Monad m, LL.ListLike s el) => Iteratee s m el
head = icontP step
  where
  step (Chunk vec)
    | LL.null vec  = continueP step
    | otherwise    = ContDone (LL.head vec) $ Chunk $ LL.tail vec
  step NoData      = continueP step
  step EOF{}   = ContErr (icontP step) (toIterException exc)
  exc = EofException "Iteratee.head"
{-# INLINE head #-}

-- | Similar to @head@, except it returns @Nothing@ if the stream
-- is terminated.
tryHead :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe el)
tryHead = icontP step
  where
  step (Chunk vec)
    | LL.null vec  = continueP step
    | otherwise    = ContDone (Just $ LL.head vec) $ Chunk $ LL.tail vec
  step NoData      = continueP step
  step str@EOF{}   = ContDone Nothing str
{-# INLINE tryHead #-}

-- |Attempt to read the last element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated
-- 
-- The analogue of @List.last@
last :: (Monad m, LL.ListLike s el) => Iteratee s m el
last = icontP (step Nothing)
  where
  step l (Chunk xs)
    | LL.null xs     = continueP (step l)
    | otherwise    = continueP $ step (Just $ LL.last xs)
  step l NoData    = continueP (step l)
  step l s@(EOF _) = case l of
    Nothing -> ContErr (icontP (step l)) (toIterException exc)
    Just x  -> ContDone x s
  exc = EofException "Iteratee.last"
{-# INLINE last #-}


-- |Given a sequence of characters, attempt to match them against
-- the characters on the stream.  Return the count of how many
-- characters matched.  The matched characters are removed from the
-- stream.
-- For example, if the stream contains 'abd', then (heads 'abc')
-- will remove the characters 'ab' and return 2.
heads :: (Monad m, LL.ListLike s el, Eq el)
  => s
  -> Iteratee s m Int
heads st = icontP (step 0 st)
  where
  step !cnt str ck           | LL.null str = ContDone cnt ck
  step !cnt str (Chunk xs)   | LL.null xs  = continueP (step cnt str)
  step !cnt str NoData                     = continueP (step cnt str)
  step !cnt str s@(Chunk xs) =
    if LL.head str == LL.head xs
       then step (succ cnt) (LL.tail str) (Chunk $ LL.tail xs)
       else ContDone cnt s
  step cnt _ stream@EOF{} = ContDone cnt stream
{-# INLINE heads #-}


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return @Just c@ if successful, return @Nothing@ if the stream is
-- terminated by 'EOF'.
peek :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe el)
peek = icontP step
  where
    step s@(Chunk vec)
      | LL.null vec   = continueP step
      | otherwise     = ContDone (Just $ LL.head vec) s
    step NoData       = continueP step
    step stream@EOF{} = ContDone Nothing stream
{-# INLINE peek #-}

-- | Return a chunk of @t@ elements length while consuming @d@ elements
--   from the stream.  Useful for creating a 'rolling average' with
--  'convStream'.
roll
  :: (Monad m, LL.ListLike s el, LL.ListLike s' s)
  => Int  -- ^ length of chunk (t)
  -> Int  -- ^ amount to consume (d)
  -> Iteratee s m s'
roll t d | t > d  = icontP step
  where
    step (Chunk vec)
      | LL.length vec >= t =
          ContDone (LL.singleton $ LL.take t vec) $ Chunk $ LL.drop d vec
      | LL.null vec        = continueP step
      | otherwise          = continueP (step' vec)
    step NoData            = continueP step
    step stream@EOF{}      = ContDone LL.empty stream
    step' v1 (Chunk vec)   = step . Chunk $ v1 `mappend` vec
    step' v1 NoData        = continueP (step' v1)
    step' v1 stream@EOF{}  = ContDone (LL.singleton v1) stream
roll t d = LL.singleton `liftM` joinI (take t stream2stream) <** drop (d-t)
  -- d is >= t, so this version works
{-# INLINE roll #-}


-- |Drop n elements of the stream, if there are that many.
-- 
-- The analogue of @List.drop@
drop :: (Monad m, LL.ListLike s el) => Int -> Iteratee s m ()
drop n'
  | n' <= 0   = idone ()
  | otherwise = icontP (step n')
  where
    step n (Chunk str)
      | LL.length str < n = continueP . step $! n - LL.length str
      | otherwise         = ContDone () $ Chunk (LL.drop n str)
    step n NoData         = continueP (step n)
    step _ stream@EOF{}   = ContDone () stream
{-# INLINE drop #-}

-- |Skip all elements while the predicate is true.
-- 
-- The analogue of @List.dropWhile@
dropWhile :: (Monad m, LL.ListLike s el) => (el -> Bool) -> Iteratee s m ()
dropWhile p = icontP step
  where
    step (Chunk str)
      | LL.null left  = continueP step
      | otherwise     = ContDone () $ Chunk left
      where
        left = LL.dropWhile p str
    step NoData       = continueP step
    step stream@EOF{} = ContDone () stream
{-# INLINE dropWhile #-}


-- | Return the total length of the remaining part of the stream.
-- 
-- This forces evaluation of the entire stream.
-- 
-- The analogue of @List.length@
length :: (Monad m, Num a, LL.ListLike s el) => Iteratee s m a
length = icontP (step 0)
  where
    step i (Chunk xs)   = continueP . step $! i + fromIntegral (LL.length xs)
    step i NoData       = continueP (step i)
    step i stream@EOF{} = ContDone i stream
{-# INLINE length #-}

-- | Get the length of the current chunk ('Just 0', or @Nothing@ if 'EOF'.
-- 
-- This function consumes no input.
chunkLength :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe Int)
chunkLength = icontP step
 where
  step s@(Chunk xs) = ContDone (Just $ LL.length xs) s
  step NoData       = ContDone (Just 0) NoData
  step stream@EOF{} = ContDone Nothing stream
{-# INLINE chunkLength #-}

-- | Take @n@ elements from the current chunk, or the whole chunk if
-- @n@ is greater.
takeFromChunk ::
  (Monad m, LL.ListLike s el)
  => Int
  -> Iteratee s m s
takeFromChunk n | n <= 0 = idone LL.empty
takeFromChunk n = icontP step
 where
  step (Chunk xs)   = let (h,t) = LL.splitAt n xs in ContDone h $ Chunk t
  step NoData       = ContDone LL.empty NoData
  step stream@EOF{} = ContDone LL.empty stream
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
  :: (LL.ListLike s el, Monad m)
  => (el -> Bool)
  -> Enumeratee s s m a
breakE cpred = eneeCheckIfDonePass (icont . step) CM.>=>
                \i' -> dropWhile (not . cpred) >> return i'
 where
  go = eneeCheckIfDonePass (icont . step)
  step k (Chunk s) = case LL.break cpred s of
        (str', tail')
          | LL.null tail' ->
              doContEtee go k str'
                               -- if the inner iteratee completes before
                               -- the predicate is met, elements still
                               -- need to be dropped.
          | otherwise -> k (Chunk str') >>= \ret ->
                            contDoneM (wrapCont ret) (Chunk tail')
  step k NoData       =  continue (step k)
  step k stream@EOF{} =  contDoneM (icont k) stream
{-# INLINE breakE #-}

-- defining this local so we don't need Functor constraints on 'm'
(<**) :: Monad m => m a -> m b -> m a
l <** r = l >>= \a -> r >> return a

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements, even if the iteratee has accepted fewer.
-- 
-- The analogue of @List.take@
take ::
  (Monad m, LL.ListLike s el)
  => Int   -- ^ number of elements to consume
  -> Enumeratee s s m a
take = go
 where
  go n' iter
    | n' <= 0   = return iter
    | otherwise = runIter iter (onDone n') (onCont n') (onErr n')

  onDone n x = drop n >> idone (idone x)
  -- n > 0, or else 'go' would have taken a different branch
  onCont n k = icont (step n k)
  onErr n i  = ierr (go n i)

  step n k (Chunk str)
      | LL.null str        = continue (step n k)
      | LL.length str <= n = k (Chunk str) >>= \ret ->
                                contMoreM (go (n - LL.length str) (wrapCont ret))
      | otherwise          = k (Chunk s1) >>= \ret -> case ret of
          ContDone a _ -> contDoneM (idone a) (Chunk s2)
          ContMore i   -> contDoneM i (Chunk s2)
          ContErr  i e -> contDoneM (ierr i e) (Chunk s2)
      where (s1, s2) = LL.splitAt n str
  step n k NoData          = continue (step n k)
  step _n k stream         = contDoneM (icont k) stream
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
takeUpTo :: (Monad m, LL.ListLike s el) => Int -> Enumeratee s s m a
takeUpTo = go
  where
    go count iter
        | count <= 0 = idone iter
        | otherwise  = runIter iter onDone (onCont count) (onErr count)

    onDone x       = idone (idone x)
    onCont count k = icont (step count k)
    onErr count i' = ierr (go count i')

    step n k (Chunk str)
      | LL.null str       = continue (step n k)
      | LL.length str < n = k (Chunk str) >>= \ret -> case ret of
                              ContDone a str' -> contDoneM (idone a) str'
                              ContMore i -> contMoreM (go
                                                        (n - LL.length str)
                                                        i)
                              ContErr i e -> contErrM (go
                                                        (n - LL.length str)
                                                        i)
                                                      e
      | otherwise         = do
         -- check to see if the inner iteratee has completed, and if so,
         -- grab any remaining stream to put it in the outer iteratee.
         -- the outer iteratee is always complete at this stage, although
         -- the inner may not be.
         let (s1, s2) = LL.splitAt n str
         ret <- k (Chunk s1)
         case ret of
            ContDone a preStr -> case preStr of
                (Chunk pre)
                  | LL.null pre -> contDoneM (idone a) $ Chunk s2
                  | otherwise   -> contDoneM (idone a) $ Chunk $ pre `LL.append` s2
                NoData          -> contDoneM (idone a) $ Chunk s2

                -- this case shouldn't ever happen, except possibly
                -- with broken iteratees
                EOF{}           -> contDoneM (idone a) preStr

            ContMore i   -> contDoneM i (Chunk s2)
            ContErr  i e -> contDoneM (ierr i e) (Chunk s2)

    step n k NoData       = continue (step n k)
    step _ k stream@EOF{} = contDoneM (icont k) stream
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
 :: (LL.ListLike s el, Monad m)
 => (el -> Bool)
 -> Enumeratee s s m a
takeWhileE = breakE . (not .)
{-# INLINE takeWhileE #-}

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
     ,LooseMap s el el')
  => (el -> el')
  -> Enumeratee (s el) (s el') m a
mapStream elF = bimapChunks (lMap elF) unF
 where
  unF origXs newXs = let useL = LL.length newXs
                         fullL = LL.length origXs
                     in LL.drop (fullL - useL) origXs
{-# INLINE mapStream #-}

-- |Map the stream rigidly.
-- 
-- Like 'mapStream', but the element type cannot change.
-- This function is necessary for @ByteString@ and similar types
-- that cannot have 'LooseMap' instances, and may be more efficient.
rigidMapStream
  :: (Monad m, LL.ListLike s el)
  => (el -> el)
  -> Enumeratee s s m a
rigidMapStream elF = bimapChunks (LL.rigidMap elF) unF
 where
  unF origXs newXs = let useL = LL.length newXs
                         fullL = LL.length origXs
                     in LL.drop (fullL - useL) origXs
{-# INLINE rigidMapStream #-}


-- |Creates an 'enumeratee' with only elements from the stream that
-- satisfy the predicate function.  The outer stream is completely consumed.
-- 
-- The analogue of @List.filter@
filter
  :: (Monad m, LL.ListLike s el)
  => (el -> Bool)
  -> Enumeratee s s m a
filter p = mapChunks (LL.filter p)
{-# INLINE filter #-}

-- |Creates an 'Enumeratee' in which elements from the stream are
-- grouped into @sz@-sized blocks.  The final block may be smaller
-- than \sz\.
group
  :: (LL.ListLike s el, Monad m)
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
    | LL.null s               = continue (step sz pfxd icur)
    | LL.length s + sz < cksz = continue (step (sz+LL.length s) (pfxd . (s:)) icur)
    | otherwise               =
        let (full, rest) = gsplit . mconcat $ pfxd [s]
            pfxd'        = if LL.null rest then id else (rest:)
            onDone x  = contDoneM (idone x) $ Chunk rest
            onErr i e = contErrM (icont (step (LL.length rest) pfxd' i)) e
            onCont k  = do
                iRet <- k (Chunk full)
                case iRet of
                  ContDone x (Chunk str') -> contDoneM (return x)
                                                (Chunk . mconcat $ str' ++ [rest])
                  ContDone x NoData       -> contDoneM (return x) (Chunk rest)
                  ContDone x EOF{}        -> contDoneM (return x) (Chunk rest)
                  ContMore i'     -> continue (step (LL.length rest) pfxd' i')
                  ContErr  i' e   -> contErrM (icont $ step (LL.length rest) pfxd' i') e
        in  runIter icur onDone onCont onErr
  step sz pfxd icur NoData = continue (step sz pfxd icur)
  step _ pfxd icur mErr = case pfxd [] of
                         []   -> contDoneM icur mErr
                         rest -> do
                           i' <- enumPure1Chunk [mconcat rest] icur
                           contDoneM i' mErr

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
  :: forall s el m a. (LL.ListLike s el, Monad m)
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
    go icurr pfx NoData    = continue $ go icurr pfx
    go icurr pfx (Chunk s) = case gsplit pfx s of
      ([], partial)   -> continue $ go icurr partial
      (full, partial) ->
        -- if the inner iteratee is done, the outer iteratee needs to be
        -- notified to terminate.
        -- if the inner iteratee is in an error state, that error should
        -- be lifted to the outer iteratee
        let onCont k = do
              iret <- k (Chunk full) -- >>= \(inext, str') ->
              case iret of
                ContDone x rest  -> contDoneM (return x) (fmap mconcat $ rest)
                ContMore inext   -> continue $ go inext partial
                ContErr inext mx -> contErrM (icont $ go inext partial) mx
            onErr inext e = contErrM (icont $ go inext partial) e
            onDone a      = contDoneM (idone a) $ Chunk . mconcat $ snd partial []
        in runIter icurr onDone onCont onErr
    go icurr (_inpfx, pfxd) (EOF mex) = case pfxd [] of
      [] -> do
          i' <- enumChunk (EOF mex) icurr
          contDoneM i' (EOF mex)
      rest -> do
          i' <- enumPure1Chunk [mconcat rest] icurr >>= enumChunk (EOF mex)
          contDoneM i' (EOF mex)

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
      | otherwise = LL.cons x ys : llGroupBy eq zs
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
-- > ileaveLines :: (Monad m)
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
   ,Functor m
   ,Monad m)
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
  (LL.ListLike c1 el1, LL.ListLike c2 el2,Monad m)
  => (c1 -> c2 -> c3)  -- ^ merge function
  -> (c1 -> c3)
  -> (c2 -> c3)
  -> Enumeratee c2 c3 (Iteratee c1 m) a
mergeByChunks f f1 f2 = unfoldConvStream iter (0 :: Int)
 where
  iter 1 = ((1,) . f1) `liftM` lift getChunk
  iter 2 = ((2,) . f2) `liftM` getChunk
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
-- This is almost never the right thing, consider using @foldl'@ instead.
-- The analogue of @List.foldl@.
foldl
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el)
  => (a -> el -> a)
  -> a
  -> Iteratee s m a
foldl f i = icontP (step i)
  where
    step acc (Chunk xs) = continueP (step $ FLL.foldl f acc xs)
    step acc NoData     = continueP (step acc)
    step acc s@EOF{}    = ContDone acc s
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
    step !acc (Chunk xs) = continueP (step $ FLL.foldl' f acc xs)
    step !acc NoData     = continueP (step acc)
    step !acc s@EOF{}    = ContDone acc s
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
    step (Chunk xs)
      -- if the input is null, we need to toss it and wait for a full chunk
      | LL.null xs = continueP step
      | otherwise  = ContMore $ foldl f $ FLL.foldl1 f xs
    -- After the first chunk, just use regular foldl.
    step NoData     = continueP step
    step (EOF Nothing)  = ContErr (icontP step) $ toIterException exc
    step (EOF (Just e)) = ContErr (icontP step) $ wrapEnumExc e
    exc = EofException "Iteratee.foldl1"
{-# INLINE foldl1 #-}


-- | Strict variant of 'foldl1'.
foldl1'
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el)
  => (el -> el -> el)
  -> Iteratee s m el
foldl1' f = icontP step
  where
    step (Chunk xs)
      -- if the input is null, we need to toss it and wait for a full chunk
      | LL.null xs = continueP step
      | otherwise  = ContMore $ foldl' f $ FLL.foldl1 f xs
    -- After the first chunk, just use regular foldl'.
    step NoData     = continueP step
    step (EOF Nothing)  = ContErr (icontP step) $ toIterException exc
    step (EOF (Just e)) = ContErr (icontP step) $ wrapEnumExc e
    exc = EofException "Iteratee.foldl1'"
{-# INLINE foldl1' #-}


-- | Sum of a stream.
sum :: (Monad m, LL.ListLike s el, Num el) => Iteratee s m el
sum = icontP (step 0)
  where
    step acc (Chunk xs) = continueP (step $! acc + LL.sum xs)
    step acc NoData     = continueP (step acc)
    step acc str@EOF{}  = ContDone acc str
{-# INLINE sum #-}


-- | Product of a stream.
product :: (Monad m, LL.ListLike s el, Num el) => Iteratee s m el
product = icontP (step 1)
  where
    step acc (Chunk xs) = continueP (step $! acc * LL.product xs)
    step acc NoData     = continueP (step acc)
    step acc str@EOF{}  = ContDone acc str
{-# INLINE product #-}


-- ------------------------------------------------------------------------
-- Zips

-- |Enumerate two iteratees over a single stream simultaneously.
-- 
-- Compare to @List.zip@.
zip
  :: (Monad m, LL.ListLike s el)
  => Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a, b)
zip = go
 where
  go x0 y0 = runIter x0 (odx y0) (ocx y0) (oex y0)

  odx yIter a      = (a, ) `liftM` yIter
  ocx yIter k      = runIter yIter (ody k) (ocy k) (oey k)
  oex yIter i' e   = throwRec e (go i' yIter)

  ody x_k b        = (,b) `liftM` icont x_k
  ocy xK yK        = icont (step xK yK)
  oey xK i' e      = throwRec e (ocx i' xK) -- zip (icont xK) i')

  step xK yK NoData = contMoreM (icont $ step xK yK)
  step xK yK str    = do
    xRet <- xK str
    yRet <- yK str
    case (xRet,yRet) of
      -- special-case both done because we need to check for the shortest,
      -- and special-case both in 'ContMore' because it's likely to be the most
      -- common case and this will cut a few indirections.
      (ContDone x strX, ContDone y strY) -> contDoneM (x,y) (shorter strX strY)
      (ContMore x, ContMore y) -> contMoreM (go x y)
      (xRet', yRet') -> contMoreM (go (wrapCont xRet') (wrapCont yRet'))

  shorter c1@(Chunk xs) c2@(Chunk ys)
    | LL.length xs < LL.length ys = c1
    | otherwise                   = c2
  shorter e@(EOF _)  _         = e
  shorter _          e@(EOF _) = e
  shorter _          _         = NoData
  
{-# INLINE zip #-}

zip3
  :: (Monad m, LL.ListLike s el)
  => Iteratee s m a -> Iteratee s m b
  -> Iteratee s m c -> Iteratee s m (a, b, c)
zip3 a b c = zip a (zip b c) >>=
  \(r1, (r2, r3)) -> return (r1, r2, r3)
{-# INLINE zip3 #-}

zip4
  :: (Monad m, LL.ListLike s el)
  => Iteratee s m a -> Iteratee s m b
  -> Iteratee s m c -> Iteratee s m d
  -> Iteratee s m (a, b, c, d)
zip4 a b c d = zip a (zip3 b c d) >>=
  \(r1, (r2, r3, r4)) -> return (r1, r2, r3, r4)
{-# INLINE zip4 #-}

zip5
  :: (Monad m, LL.ListLike s el)
  => Iteratee s m a -> Iteratee s m b
  -> Iteratee s m c -> Iteratee s m d
  -> Iteratee s m e -> Iteratee s m (a, b, c, d, e)
zip5 a b c d e = zip a (zip4 b c d e) >>=
  \(r1, (r2, r3, r4, r5)) -> return (r1, r2, r3, r4, r5)
{-# INLINE zip5 #-}

-- | Enumerate over two iteratees in parallel as long as the first iteratee
-- is still consuming input.  The second iteratee will be terminated with EOF
-- when the first iteratee has completed.  An example use is to determine
-- how many elements an iteratee has consumed (although this is less efficient
-- than using @countConsumed@ directly)
-- 
-- > snd <$> enumWith (dropWhile (<5)) length
-- 
-- Compare to @zip@
enumWith
  :: forall m s el a b. (Monad m, LL.ListLike s el)
  => Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a, b)
enumWith = go
 where
  go x0 y0         = runIter x0 (odx y0) (ocx y0) (oex y0)
  odx yIter a      = (a,) `liftM` joinIM (enumEof yIter)
  ocx yIter k      = runIter yIter (ody k) (ocy k) (oey k)
  oex yIter i' e   = throwRec e (go i' yIter)

  ody x_k b        = (,b) `liftM` icont x_k
  ocy xK yK        = icont $ step xK yK
  oey xK i' e      = ierr (ocx i' xK) e

  step :: Cont s m a -> Cont s m b -> Cont s m (a,b)
  step xK yK NoData = contMoreM (icont $ step xK yK)
  step xK yK str = do
    xret <- xK str
    yret <- yK str
    case xret of
        -- TODO: use something better than 'run' here
        ContDone a str' -> run (wrapCont yret) >>= \y -> contDoneM (a,y) str'
        ContMore i      -> contMoreM (go i $ wrapCont yret)
        ContErr  i e    -> contErrM  (go i $ wrapCont yret) e
{-# INLINE enumWith #-}

-- |Enumerate a list of iteratees over a single stream simultaneously
-- and discard the results. This is a different behavior than Prelude's
-- sequence_ which runs iteratees in the list one after the other.
-- 
-- Compare to @Prelude.sequence_@.
sequence_
  :: forall el s m a. (Monad m, LL.ListLike s el)
  => [Iteratee s m a]
  -> Iteratee s m ()
sequence_ = check []
  where
    -- recursively checks each input iteratee to see if it's finished.
    -- all of the unfinished iteratees are run with a single chunk,
    -- then checked again.

    check [] [] = idone ()
    check ks [] = icont (step ks)
    check ks (i:iters) = runIter i (\_ -> check ks iters)
                                   (onCont ks iters)
                                   (onErr ks iters)
    onCont ks iters k  = check (k:ks) iters
    onErr ks iters i e = throwRec e (check ks (i:iters))

    step ks str = CM.foldM (accf str) ([], str,Nothing) (reverse ks) >>= \ret -> case ret of
        (iS, _, Just e)  -> contErrM (check [] (reverse iS)) e
        ([], str', _)    -> contDoneM () str'
        (iS, EOF Nothing, _)  -> contErrM (throwRec sExc (check [] (reverse iS)))
                                          (toIterException sExc)
        (iS, EOF (Just e), _) -> let e' = wrapEnumExc e
                                 in contErrM (throwRec e' (check [] (reverse iS))) e'
        (iS, _, _)            -> contMoreM (check [] (reverse iS))
    accf str (iS, !strs, !mErr) k = k str >>= \ret -> case ret of
        ContDone _ str' -> return (iS, shorter str' strs, mErr)
        ContMore i      -> return (i:iS, strs, mErr)
        ContErr  i e    -> return (i:iS, strs, Just e)
      
    -- return the shorter one of two streams; errors are propagated with the
    -- priority given to the "left"
    shorter c1@(Chunk xs) c2@(Chunk ys)
      | LL.length xs < LL.length ys = c1
      | otherwise                   = c2
    shorter (EOF e1 ) (EOF e2 ) = EOF (e1 `mplus` e2)
    shorter e@(EOF _) _         = e
    shorter _         e@(EOF _) = e
    shorter _         _         = NoData
    sExc = EofException "Iteratee.sequence_"
{-# INLINABLE sequence_ #-}

-- |Transform an iteratee into one that keeps track of how much data it
-- consumes.
countConsumed :: forall a s el m n.
                 (Monad m, LL.ListLike s el, Integral n)
              => Iteratee s m a
              -> Iteratee s m (a, n)
countConsumed = check 0
  where
    newLen :: n -> s -> Stream s -> n
    newLen n c (Chunk c') = n + fromIntegral (LL.length c - LL.length c')
    newLen n c _          = n + fromIntegral (LL.length c)
    check :: n -> Iteratee s m a -> Iteratee s m (a,n)
    check !n iter = runIter iter (onDone n)
                                 (onCont n)
                                 (onErr n)

    step !n k str@(Chunk c) = k str >>=
        return . mapContRet (check $ newLen n c mempty) (\a s -> (a,newLen n c s))

    step !n k NoData        = contMoreM (icont (step n k))
    step n k str@EOF{}      = k str >>=
        return . mapContRet (check n) (\a _s -> (a,n))

    onDone n a  = idone (a,n)
    onCont n k  = icont (step n k)
    onErr n i e = throwRec e (check n i)
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
                           onCont k     = doContIteratee k (Chunk s1) >>= enum' s2
                           onErr i' e   = return $ ierr i' e
                       in if LL.null s1
                            then enum' s2 iter'
                            else runIter iter' idoneM onCont onErr
{-# INLINE enumPureNChunk #-}

-- ------------------------------------------------------------------------
-- Monadic functions

-- | Map a monadic function over the elements of the stream and ignore the
-- result.
mapM_
  :: (Monad m, LL.ListLike s el)
  => (el -> m b)
  -> Iteratee s m ()
mapM_ f = icont step
  where
    step (Chunk xs) = LL.mapM_ f xs >> contMoreM (icont step)
    step NoData     = contMoreM (icont step)
    step s@(EOF _)  = contDoneM () s
{-# INLINE mapM_ #-}

-- |The analogue of @Control.Monad.foldM@
foldM
  :: (Monad m, LL.ListLike s b)
  => (a -> b -> m a)
  -> a
  -> Iteratee s m a
foldM f e = icont (step e)
  where
    step acc (Chunk xs) = CM.foldM f acc (LL.toList xs) >>= \acc' ->
                            nextStep $ step acc'
    step acc NoData     = nextStep $ step acc
    step acc s@EOF{}    = contDoneM acc s
{-# INLINE foldM #-}
