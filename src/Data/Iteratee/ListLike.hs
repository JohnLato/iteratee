{-# LANGUAGE FlexibleContexts, BangPatterns #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.ListLike (
  -- * Iteratees
  -- ** Iteratee Utilities
  isFinished,
  stream2list,
  stream2stream,
  -- ** Basic Iteratees
  break,
  dropWhile,
  drop,
  head,
  heads,
  peek,
  length,
  -- ** Nested iteratee combinators
  take,
  takeR,
  mapStream,
  filter,
  -- ** Folds
  foldl,
  foldl',
  foldl1,
  -- ** Special Folds
  sum,
  product,
  -- * Enumerators
  -- ** Basic enumerators
  enumPureNChunk,
  -- ** Enumerator Combinators
  -- enumPair,
  -- * Classes
  module Data.Iteratee.Iteratee
)
where

import Prelude hiding (null, head, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product)
--import qualified Prelude as P

import qualified Data.ListLike as LL
import qualified Data.ListLike.FoldableLL as FLL
import Data.Iteratee.Iteratee
import Control.Monad
import Data.Monoid


-- Useful combinators for implementing iteratees and enumerators

-- | Check if a stream has finished, whether from EOF or Error.
isFinished :: (Monad m, Nullable s) => Iteratee s m Bool
isFinished = icont check Nothing
  where
  check c@(Chunk xs)
    | null xs     = isFinished
    | otherwise   = idone False c
  check s@(EOF _) = idone True s

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list
stream2list :: (Monad m, Nullable s, LL.ListLike s el) => Iteratee s m [el]
stream2list = liftM LL.toList stream2stream

-- |Read a stream to the end and return all of its elements as a stream
stream2stream :: (Monad m, Nullable s, Monoid s) => Iteratee s m s
stream2stream = icont (step mempty) Nothing
  where
    step acc (Chunk ls)
      | null ls   = icont (step acc) Nothing
      | otherwise = icont (step (acc `mappend` ls)) Nothing
    step acc str  = idone acc str


-- ------------------------------------------------------------------------
-- Parser combinators

-- |The analogue of List.break
-- It takes an element predicate and returns the (possibly empty) prefix of
-- the stream.  None of the characters in the string satisfy the character
-- predicate.
-- If the stream is not terminated, the first character on the stream
-- satisfies the predicate.

break :: (Monad m, LL.ListLike s el) => (el -> Bool) -> Iteratee s m s
break cpred = icont (step mempty) Nothing
  where
    step bfr (Chunk str)
      | LL.null str       =  icont (step bfr) Nothing
      | True              =  case LL.break cpred str of
        (str', tail')
          | LL.null tail' -> icont (step (bfr `mappend` str)) Nothing
          | otherwise     -> idone (bfr `mappend` str') (Chunk tail')
    step bfr stream       =  idone bfr stream


-- |Attempt to read the next element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated
head :: (Monad m, LL.ListLike s el) => Iteratee s m el
head = icont step Nothing
  where
  step (Chunk vec)
    | LL.null vec  = icont step Nothing
    | otherwise    = idone (LL.head vec) (Chunk $ LL.tail vec)
  step stream      = icont step (Just (setEOF stream))


-- |Given a sequence of characters, attempt to match them against
-- the characters on the stream.  Return the count of how many
-- characters matched.  The matched characters are removed from the
-- stream.
-- For example, if the stream contains "abd", then (heads "abc")
-- will remove the characters "ab" and return 2.
heads :: (Monad m, Nullable s, LL.ListLike s el, Eq el) => s -> Iteratee s m Int
heads st | LL.null st = return 0
heads st = loop 0 st
  where
  loop cnt xs | LL.null xs = return cnt
  loop cnt xs              = icont (step cnt xs) Nothing
  step cnt str (Chunk xs) | LL.null xs  = icont (step cnt str) Nothing
  step cnt str stream     | LL.null str = idone cnt stream
  step cnt str s@(Chunk xs) =
    if LL.head str == LL.head xs
       then step (succ cnt) (LL.tail str) (Chunk $ LL.tail xs)
       else idone cnt s
  step cnt _ stream         = idone cnt stream


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return (Just c) if successful, return Nothing if the stream is
-- terminated (by EOF or an error)
peek :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe el)
peek = icont step Nothing
  where
    step s@(Chunk vec)
      | LL.null vec = icont step Nothing
      | otherwise   = idone (Just $ LL.head vec) s
    step stream     = idone Nothing stream


-- |Skip n elements of the stream, if there are that many
-- This is the analogue of List.drop
drop :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Iteratee s m ()
drop 0 = return ()
drop n' = icont (step n') Nothing
  where
    step n (Chunk str)
      | LL.length str <= n = icont (step (n - LL.length str)) Nothing
      | otherwise          = idone () (Chunk (LL.drop n str))
    step _ stream          = idone () stream

-- |Skip all elements while the predicate is true.
-- This is the analogue of List.dropWhile
dropWhile
  :: (Monad m, LL.ListLike s el) =>
     (el -> Bool)
     -> Iteratee s m ()
dropWhile p = icont step Nothing
  where
    step (Chunk str)
      | LL.null left = icont step Nothing
      | otherwise    = idone () (Chunk left)
      where
        left = LL.dropWhile p str
    step stream      = idone () stream


-- |Return the total length of the stream
length :: (Monad m, Num a, LL.ListLike s el) => Iteratee s m a
length = length' 0
  where
    length' !n = icont (step n) Nothing
    step i (Chunk xs) = length' (i + LL.length xs)
    step i stream     = idone (fromIntegral i) stream


-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements (even if the iteratee has accepted fewer).
take :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Enumeratee s s m a
take 0 iter  = return iter
take n' iter = Iteratee $ \od oc -> runIter iter (on_done od oc) (on_cont od oc)
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

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of `take' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early.
takeR :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Enumeratee s s m a
takeR 0 iter = return iter
takeR i iter = Iteratee $ \od oc -> runIter iter (onDone od oc) (onCont od oc)
  where
    onDone od oc x _ = runIter (return (return x)) od oc
    onCont od oc k Nothing = if i == 0 then od (liftI k) (Chunk mempty)
                                 else runIter (liftI (step i k)) od oc
    onCont od oc _ (Just e) = runIter (throwErr e) od oc
    step n k (Chunk str)
      | LL.null str        = liftI (step n k)
      | LL.length str <= n = takeR (n - LL.length str) $ k (Chunk str)
      | True               = idone (k (Chunk s1)) (Chunk s2)
      where (s1, s2) = LL.splitAt n str
    step _n k stream       = idone (k stream) stream


-- |Map the stream: yet another iteratee transformer
-- Given the stream of elements of the type el and the function el->el',
-- build a nested stream of elements of the type el' and apply the
-- given iteratee to it.
-- Note the contravariance

mapStream
  :: (Monad m,
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


-- |Creates an enumerator with only elements from the stream that
-- satisfy the predicate function.
filter :: (Monad m, Nullable s, LL.ListLike s el) =>
  (el -> Bool) ->
  Enumeratee s s m a
filter p = convStream f'
  where
    f' = icont step Nothing
    step (Chunk xs)
      | LL.null xs = f'
      | otherwise  = idone (LL.filter p xs) mempty
    step _ = f'

-- ------------------------------------------------------------------------
-- Folds

-- | Left-associative fold.
foldl
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el) =>
     (a -> el -> a)
     -> a
     -> Iteratee s m a
foldl f i = icont (step i) Nothing
  where
    step acc (Chunk xs)
      | LL.null xs = icont (step acc) Nothing
      | otherwise  = icont (step $ FLL.foldl f acc xs) Nothing
    step acc stream    = idone acc stream

-- | Left-associative fold that is strict in the accumulator.
foldl'
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el) =>
     (a -> el -> a)
     -> a
     -> Iteratee s m a
foldl' f i = icont (step i) Nothing
  where
    step acc (Chunk xs)
      | LL.null xs  = icont (step acc) Nothing
      | otherwise   = icont (step $! FLL.foldl' f acc xs) Nothing
    step acc stream = idone acc stream

{-# INLINE foldl' #-}

-- | Variant of foldl with no base case.  Requires at least one element
--   in the stream.
foldl1
  :: (Monad m, LL.ListLike s el, FLL.FoldableLL s el) =>
     (el -> el -> el)
     -> Iteratee s m el
foldl1 f = icont step Nothing
  where
    step (Chunk xs)
    -- After the first chunk, just use regular foldl in order to account for
    -- the accumulator.
      | LL.null xs = icont step Nothing
      | otherwise  = foldl f $ FLL.foldl1 f xs
    step stream    = icont step (Just (setEOF stream))

-- | Sum of a stream.
sum :: (Monad m, LL.ListLike s el, Num el) => Iteratee s m el
sum = icont (step 0) Nothing
  where
    step acc (Chunk xs)
      | LL.null xs = icont (step acc) Nothing
      | otherwise  = icont (step $! acc + LL.sum xs) Nothing
    step acc str   = idone acc str

-- | Product of a stream
product :: (Monad m, LL.ListLike s el, Num el) => Iteratee s m el
product = icont (step 1) Nothing
  where
    step acc (Chunk xs)
      | LL.null xs = icont (step acc) Nothing
      | otherwise  = icont (step $! acc * LL.product xs) Nothing
    step acc str   = idone acc str

-- ------------------------------------------------------------------------
-- Zips

{-
-- |Enumerate two iteratees over a single stream simultaneously.
enumPair
  :: (Monad m, LL.ListLike s el) =>
     Iteratee s m a
     -> Iteratee s m b
     -> Iteratee s m (a,b)
enumPair i1 i2 = Iteratee check
 where
   longest c1@(Chunk xs) c2@(Chunk ys) = if LL.length xs > LL.length ys
                                         then c1 else c2
   longest e@(EOF _)  _          = e
   longest _          e@(EOF _)  = e
   check = do
     ia <- runIter i1
     ib <- runIter i2
     case (ia, ib) of
       (Done a astr, Done b bstr)  -> return . Done (a,b) $ longest astr bstr
       (Done _ _astr, Cont k mErr) -> return $ Cont (enumPair i1 . k ) mErr
       (Cont k mErr, Done _ _bstr) -> return $ Cont (flip enumPair i2 . k) mErr
       (Cont a aEr,  Cont b bEr)   -> return $ Cont (\s -> enumPair (a s) (b s))
                                                    (aEr `mappend` bEr)
-}

-- ------------------------------------------------------------------------
-- Enumerators

-- |The pure n-chunk enumerator
-- It passes a given chunk of elements to the iteratee in n chunks
-- This enumerator does no IO and is useful for testing of base parsing
-- and handling of chunk boundaries
enumPureNChunk
  :: (Monad m, LL.ListLike s el) =>
     s
     -> Int
     -> Enumerator s m a
enumPureNChunk str n iter
  | LL.null str = return iter
  | n > 0       = enum' str iter
  | otherwise   = error $ "enumPureNChunk called with n==" ++ show n
  where
    enum' str' iter'
      | LL.null str' = return iter'
      | True         = let (s1, s2) = LL.splitAt n str'
                           on_cont k Nothing = enum' s2 . k $ Chunk s1
                           on_cont k e = return $ icont k e
                       in runIter iter' idoneM on_cont
