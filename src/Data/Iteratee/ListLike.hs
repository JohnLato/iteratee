{-# LANGUAGE FlexibleContexts, BangPatterns #-}

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
  ,heads
  ,peek
  ,length
  -- ** Nested iteratee combinators
  ,take
  ,takeUpTo
  ,mapStream
  ,rigidMapStream
  ,filter
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
  ,enumPair
  -- * Classes
  ,module Data.Iteratee.Iteratee
)
where

import Prelude hiding (null, head, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product)

import qualified Data.ListLike as LL
import qualified Data.ListLike.FoldableLL as FLL
import Data.Iteratee.Iteratee
import Data.Monoid
import Control.Monad.Trans.Class
import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC


-- Useful combinators for implementing iteratees and enumerators

-- | Check if a stream has received 'EOF'.
isFinished :: (Monad m, Nullable s) => Iteratee s m Bool
isFinished = liftI check
  where
  check c@(Chunk xs)
    | null xs     = liftI check
    | True        = idone False c
  check s@(EOF _) = idone True s
{-# INLINE isFinished #-}

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list.
-- This iteratee returns all data from the stream *strictly*.
stream2list :: (Monad m, Nullable s, LL.ListLike s el) => Iteratee s m [el]
stream2list = liftI (step [])
  where
    step acc (Chunk ls)
      | null ls  = liftI (step acc)
      | True     = liftI (step (acc ++ LL.toList ls))
    step acc str = idone acc str
{-# INLINE stream2list #-}

-- |Read a stream to the end and return all of its elements as a stream.
-- This iteratee returns all data from the stream *strictly*.
stream2stream :: (Monad m, Nullable s, Monoid s) => Iteratee s m s
stream2stream = icont (step mempty) Nothing
  where
    step acc (Chunk ls)
      | null ls   = icont (step acc) Nothing
      | True      = icont (step (acc `mappend` ls)) Nothing
    step acc str  = idone acc str
{-# INLINE stream2stream #-}


-- ------------------------------------------------------------------------
-- Parser combinators

-- |Takes an element predicate and returns the (possibly empty) prefix of
-- the stream.  None of the characters in the string satisfy the character
-- predicate.
-- If the stream is not terminated, the first character of the remaining stream
-- satisfies the predicate.
--
-- The analogue of @List.break@

break :: (Monad m, LL.ListLike s el) => (el -> Bool) -> Iteratee s m s
break cpred = icont (step mempty) Nothing
  where
    step bfr (Chunk str)
      | LL.null str       =  icont (step bfr) Nothing
      | True              =  case LL.break cpred str of
        (str', tail')
          | LL.null tail' -> icont (step (bfr `mappend` str)) Nothing
          | True          -> idone (bfr `mappend` str') (Chunk tail')
    step bfr stream       =  idone bfr stream
{-# INLINE break #-}


-- |Attempt to read the next element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated
--
-- The analogue of @List.head@
head :: (Monad m, LL.ListLike s el) => Iteratee s m el
head = liftI step
  where
  step (Chunk vec)
    | LL.null vec  = icont step Nothing
    | True         = idone (LL.head vec) (Chunk $ LL.tail vec)
  step stream      = icont step (Just (setEOF stream))
{-# INLINE head #-}


-- |Given a sequence of characters, attempt to match them against
-- the characters on the stream.  Return the count of how many
-- characters matched.  The matched characters are removed from the
-- stream.
-- For example, if the stream contains "abd", then (heads "abc")
-- will remove the characters "ab" and return 2.
heads :: (Monad m, Nullable s, LL.ListLike s el, Eq el) => s -> Iteratee s m Int
heads st | null st = return 0
heads st = loop 0 st
  where
  loop cnt xs
    | null xs = return cnt
    | True    = liftI (step cnt xs)
  step cnt str (Chunk xs) | null xs  = liftI (step cnt str)
  step cnt str stream     | null str = idone cnt stream
  step cnt str s@(Chunk xs) =
    if LL.head str == LL.head xs
       then step (succ cnt) (LL.tail str) (Chunk $ LL.tail xs)
       else idone cnt s
  step cnt _ stream         = idone cnt stream
{-# INLINE heads #-}


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return @Just c@ if successful, return @Nothing@ if the stream is
-- terminated by EOF.
peek :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe el)
peek = liftI step
  where
    step s@(Chunk vec)
      | LL.null vec = liftI step
      | True        = idone (Just $ LL.head vec) s
    step stream     = idone Nothing stream
{-# INLINE peek #-}


-- |Drop n elements of the stream, if there are that many.
--
-- The analogue of @List.drop@
drop :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Iteratee s m ()
drop 0  = return ()
drop n' = liftI (step n')
  where
    step n (Chunk str)
      | LL.length str <= n = liftI (step (n - LL.length str))
      | True               = idone () (Chunk (LL.drop n str))
    step _ stream          = idone () stream
{-# INLINE drop #-}

-- |Skip all elements while the predicate is true.
--
-- The analogue of @List.dropWhile@
dropWhile :: (Monad m, LL.ListLike s el) => (el -> Bool) -> Iteratee s m ()
dropWhile p = liftI step
  where
    step (Chunk str)
      | LL.null left = liftI step
      | True         = idone () (Chunk left)
      where
        left = LL.dropWhile p str
    step stream      = idone () stream
{-# INLINE dropWhile #-}


-- |Return the total length of the remaining part of the stream.
-- This forces evaluation of the entire stream.
--
-- The analogue of @List.length@
length :: (Monad m, Num a, LL.ListLike s el) => Iteratee s m a
length = liftI (step 0)
  where
    step !i (Chunk xs) = liftI (step $ i + LL.length xs)
    step !i stream     = idone (fromIntegral i) stream
{-# INLINE length #-}


-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements, even if the iteratee has accepted fewer.
--
-- The analogue of @List.take@
take :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Enumeratee s s m a
take n' iter
 | n' <= 0 = return iter
 | True    = Iteratee $ \od oc -> runIter iter (on_done od oc) (on_cont od oc)
  where
    on_done od oc x _ = runIter (drop n' >> return (return x)) od oc
    on_cont od oc k Nothing = if n' == 0 then od (liftI k) (Chunk mempty)
                                 else runIter (liftI (step n' k)) od oc
    on_cont od oc _ (Just e) = runIter (drop n' >> throwErr e) od oc
    step n k (Chunk str)
      | LL.null str        = liftI (step n k)
      | LL.length str <= n = take (n - LL.length str) $ k (Chunk str)
      | True               = idone (k (Chunk s1)) (Chunk s2)
      where (s1, s2) = LL.splitAt n str
    step _n k stream       = idone (k stream) stream
{-# SPECIALIZE take :: Monad m => Int -> Enumeratee [el] [el] m a #-}
{-# SPECIALIZE take :: Monad m => Int -> Enumeratee B.ByteString B.ByteString m a #-}
{-# SPECIALIZE take :: Monad m => Int -> Enumeratee BC.ByteString BC.ByteString m a #-}

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of `take' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early.
--
-- N.B. If the inner iteratee finishes early, remaining data within the current
-- chunk will be dropped.
takeUpTo :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Enumeratee s s m a
takeUpTo i iter
 | i <= 0    = return iter
 | otherwise = Iteratee $ \od oc ->
    runIter iter (onDone od oc) (onCont od oc)
  where
    onDone od oc x _        = runIter (return (return x)) od oc
    onCont od oc k Nothing  = if i == 0 then od (liftI k) (Chunk mempty)
                                 else runIter (liftI (step i k)) od oc
    onCont od oc _ (Just e) = runIter (throwErr e) od oc
    step n k (Chunk str)
      | LL.null str         = liftI (step n k)
      | LL.length str <= n  = takeUpTo (n - LL.length str) $ k (Chunk str)
      | True                = idone (k (Chunk s1)) (Chunk s2)
      where (s1, s2) = LL.splitAt n str
    step _ k stream         = idone (k stream) stream
{-# SPECIALIZE takeUpTo :: Monad m => Int -> Enumeratee [el] [el] m a #-}
{-# SPECIALIZE takeUpTo :: Monad m => Int -> Enumeratee B.ByteString B.ByteString m a #-}


-- |Map the stream: another iteratee transformer
-- Given the stream of elements of the type @el@ and the function @el->el'@,
-- build a nested stream of elements of the type @el'@ and apply the
-- given iteratee to it.
--
-- The analog of @List.map@
mapStream ::
 (Monad m,
  LL.ListLike (s el) el,
  LL.ListLike (s el') el',
  NullPoint (s el),
  LooseMap s el el') =>
 (el -> el')
 -> Enumeratee (s el) (s el') m a
mapStream f = eneeCheckIfDone (liftI . step)
  where
    step k (Chunk xs)
      | LL.null xs = liftI (step k)
      | True       = mapStream f $ k (Chunk $ lMap f xs)
    step k s       = idone (liftI k) s
{-# SPECIALIZE mapStream :: Monad m => (el -> el') -> Enumeratee [el] [el'] m a #-}

-- |Map the stream rigidly.
--
-- Like 'mapStream', but the element type cannot change.
-- This function is necessary for @ByteString@ and similar types
-- that cannot have 'LooseMap' instances, and may be more efficient.
rigidMapStream ::
 (Monad m, LL.ListLike s el, NullPoint s) =>
  (el -> el)
  -> Enumeratee s s m a
rigidMapStream f = eneeCheckIfDone (liftI . step)
  where
    step k (Chunk xs)
      | LL.null xs = liftI (step k)
      | True       = rigidMapStream f $ k (Chunk $ LL.rigidMap f xs)
    step k s       = idone (liftI k) s
{-# SPECIALIZE rigidMapStream :: Monad m => (el -> el) -> Enumeratee [el] [el] m a #-}
{-# SPECIALIZE rigidMapStream :: Monad m => (Word8 -> Word8) -> Enumeratee B.ByteString B.ByteString m a #-}


-- |Creates an 'enumeratee' with only elements from the stream that
-- satisfy the predicate function.  The outer stream is completely consumed.
--
-- The analogue of @List.filter@
filter ::
 (Monad m, Nullable s, LL.ListLike s el) =>
  (el -> Bool)
  -> Enumeratee s s m a
filter p = convStream f'
  where
    f' = icont step Nothing
    step (Chunk xs)
      | LL.null xs = f'
      | True       = idone (LL.filter p xs) mempty
    step _ = f'
{-# INLINE filter #-}

-- ------------------------------------------------------------------------
-- Folds

-- | Left-associative fold.
--
-- The analogue of @List.foldl@
foldl ::
 (Monad m, LL.ListLike s el, FLL.FoldableLL s el) =>
  (a -> el -> a)
  -> a
  -> Iteratee s m a
foldl f i = liftI (step i)
  where
    step acc (Chunk xs)
      | LL.null xs  = liftI (step acc)
      | True   = liftI (step $ FLL.foldl f acc xs)
    step acc stream = idone acc stream
{-# INLINE foldl #-}


-- | Left-associative fold that is strict in the accumulator.
-- This function should be used in preference to 'foldl' whenever possible.
--
-- The analogue of @List.foldl'@.
foldl' ::
 (Monad m, LL.ListLike s el, FLL.FoldableLL s el) =>
  (a -> el -> a)
  -> a
  -> Iteratee s m a
foldl' f i = liftI (step i)
  where
    step acc (Chunk xs)
      | LL.null xs = liftI (step acc)
      | True       = liftI (step $! FLL.foldl' f acc xs)
    step acc stream = idone acc stream
{-# INLINE foldl' #-}

-- | Variant of foldl with no base case.  Requires at least one element
--   in the stream.
--
-- The analogue of @List.foldl1@.
foldl1 ::
 (Monad m, LL.ListLike s el, FLL.FoldableLL s el) =>
  (el -> el -> el)
  -> Iteratee s m el
foldl1 f = liftI step
  where
    step (Chunk xs)
    -- After the first chunk, just use regular foldl.
      | LL.null xs = liftI step
      | True       = foldl f $ FLL.foldl1 f xs
    step stream    = icont step (Just (setEOF stream))
{-# INLINE foldl1 #-}


-- | Strict variant of 'foldl1'.
foldl1' ::
 (Monad m, LL.ListLike s el, FLL.FoldableLL s el) =>
  (el -> el -> el)
  -> Iteratee s m el
foldl1' f = liftI step
  where
    step (Chunk xs)
    -- After the first chunk, just use regular foldl'.
      | LL.null xs = liftI step
      | True       = foldl' f $ FLL.foldl1 f xs
    step stream    = icont step (Just (setEOF stream))
{-# INLINE foldl1' #-}


-- | Sum of a stream.
sum :: (Monad m, LL.ListLike s el, Num el) => Iteratee s m el
sum = liftI (step 0)
  where
    step acc (Chunk xs)
      | LL.null xs = liftI (step acc)
      | True       = liftI (step $! acc + LL.sum xs)
    step acc str   = idone acc str
{-# INLINE sum #-}


-- | Product of a stream.
product :: (Monad m, LL.ListLike s el, Num el) => Iteratee s m el
product = liftI (step 1)
  where
    step acc (Chunk xs)
      | LL.null xs = liftI (step acc)
      | True       = liftI (step $! acc * LL.product xs)
    step acc str   = idone acc str
{-# INLINE product #-}


-- ------------------------------------------------------------------------
-- Zips

-- |Enumerate two iteratees over a single stream simultaneously.
--
-- Compare to @zip@.
enumPair ::
 (Monad m, Nullable s, LL.ListLike s el) =>
  Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a,b)
enumPair i1 i2 = Iteratee $ \od oc -> runIter i1 (onDone od oc) (onCont od oc)
  where
    onDone od oc x s        = runIter i2 (oD12 od oc x s) (onCont' od oc x)
    oD12 od oc x1 s1 x2 s2  = runIter (idone (x1,x2) (longest s1 s2)) od oc
    onCont od oc k mErr     = runIter (icont (step k) mErr) od oc
      where
    onCont' od oc x1 k mErr = runIter (icont (step2 x1 k) mErr) od oc
    step k c@(Chunk str)
      | null str            = liftI (step k)
      | True                = lift (enumPure1Chunk str i2) >>= enumPair (k c)
    step k s@(EOF Nothing)  = lift (enumEof i2) >>= enumPair (k s)
    step k s@(EOF (Just e)) = lift (enumErr e i2) >>= enumPair (k s)
    step2 x1 k (Chunk str)
      | null str            = liftI (step2 x1 k)
    step2 x1 k str          = enumPair (return x1) (k str)
    longest c1@(Chunk xs) c2@(Chunk ys) = if LL.length xs > LL.length ys
                                          then c1 else c2
    longest e@(EOF _)  _         = e
    longest _          e@(EOF _) = e
{-# INLINE enumPair #-}


-- ------------------------------------------------------------------------
-- Enumerators

-- |The pure n-chunk enumerator
-- It passes a given stream of elements to the iteratee in @n@-sized chunks.
enumPureNChunk ::
 (Monad m, LL.ListLike s el) => s -> Int -> Enumerator s m a
enumPureNChunk str n iter
  | LL.null str = return iter
  | n > 0       = enum' str iter
  | True        = error $ "enumPureNChunk called with n==" ++ show n
  where
    enum' str' iter'
      | LL.null str' = return iter'
      | True         = let (s1, s2) = LL.splitAt n str'
                           on_cont k Nothing = enum' s2 . k $ Chunk s1
                           on_cont k e = return $ icont k e
                       in runIter iter' idoneM on_cont
{-# INLINE enumPureNChunk #-}
