{-# LANGUAGE FlexibleContexts #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Iteratee (
  -- * Types
  IterV (..),
  Iteratee (..),
  EnumeratorN,
  EnumeratorM,
  EnumeratorMM,
  -- * Iteratees
  -- ** Iteratee Utilities
  liftI,
  isFinished,
  getStatus,
  run,
  stream2list,
  stream2stream,
  checkIfDone,
  joinI,
  -- ** Error handling
  throwErr,
  checkErr,
  -- ** Basic Iteratees
  break,
  dropWhile,
  drop,
  identity,
  head,
  heads,
  peek,
  skipToEof,
  length,
  -- ** Nested iteratee combinators
  take,
  takeR,
  mapStream,
  rigidMapStream,
  convStream,
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
  enumEof,
  enumErr,
  enumPure1Chunk,
  enumPureNChunk,
  -- ** Enumerator Combinators
  (>.),
  enumPair,
  enumPar,
  -- * Misc.
  seek,
  FileOffset,
  -- * Classes
  module Data.Iteratee.Base
)
where

import Prelude hiding (head, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product)
import qualified Prelude as P

import qualified Data.ListLike as LL
import qualified Data.ListLike.FoldableLL as FLL
import Data.Iteratee.IO.Base
import Data.Iteratee.Base
import Control.Monad
import Control.Parallel
import Control.Applicative
import Data.Monoid
import Data.Maybe (fromMaybe)


-- |Iteratee -- a generic stream processor, what is being folded over
-- a stream
-- When Iteratee is in the 'done' state, it contains the computed
-- result and the remaining part of the stream.
-- In the 'cont' state, the iteratee has not finished the computation
-- and needs more input.
-- We assume that all iteratees are `good' -- given bounded input,
-- they do the bounded amount of computation and take the bounded amount
-- of resources. The monad m describes the sort of computations done
-- by the iteratee as it processes the stream. The monad m could be
-- the identity monad (for pure computations) or the IO monad
-- (to let the iteratee store the stream processing results as they
-- are computed).
-- We also assume that given a terminated stream, an iteratee
-- moves to the done state, so the results computed so far could be returned.

data IterV c el a =
  Done a (StreamG c)
  | Cont (Iteratee c el a) (Maybe ErrMsg)

instance (Show c, Show a) => Show (IterV c el a) where
  show (Done a str) = "IterV Done <<" ++ show a ++ ">> : <<" ++ show str ++ ">>"
  show (Cont _ mErr) = "IterV Cont :: " ++ show mErr

newtype Iteratee c el a = Iteratee{
  runIter :: StreamG c -> IterV c el a
  }


-- Useful combinators for implementing iteratees and enumerators

-- | Lift an IterV result into an 'Iteratee'
liftI :: (LL.ListLike s el) => IterV s el a -> Iteratee s el a
liftI (Cont k Nothing)     = k
liftI (Cont _k (Just err)) = throwErr err
liftI i@(Done _ (EOF _  )) = Iteratee (const i)
liftI (Done a (Chunk st )) = Iteratee (check st)
  where
  check str (Chunk str') = Done a (Chunk $ str `mappend` str')
  check _str e@(EOF _)   = Done a e

-- | Run an 'Iteratee' and get the result.  An 'EOF' is sent to the
-- iteratee as it is run.
run :: (LL.ListLike s el) => Iteratee s el a -> a
run iter = case runIter iter (EOF Nothing) of
  Done x _ -> x
  Cont _ e -> error $ "control message: " ++ show e

-- | Check if a stream has finished, whether from EOF or Error.
isFinished :: (LL.ListLike s el) =>
  Iteratee s el Bool
isFinished = Iteratee check
  where
  check (Chunk xs) | LL.null xs = Cont isFinished Nothing
  check s@(EOF _)               = Done True s
  check s                       = Done False s

-- | Get the stream status of an iteratee.
getStatus :: (LL.ListLike s el) =>
  Iteratee s el StreamStatus
getStatus = Iteratee check
  where
    check s@(EOF Nothing)  = Done EofNoError s
    check s@(EOF (Just e)) = Done (EofError e) s
    check s                = Done DataRemaining s

-- |If the iteratee ('IterV') has finished, return its value.  If it has not
-- finished then apply it to the given 'EnumeratorM'.
-- If in error, throw the error.
checkIfDone :: (LL.ListLike s el) =>
  (Iteratee s el a -> (Iteratee s el a)) ->
  IterV s el a ->
  (Iteratee s el a)
checkIfDone _ (Done x _)        = return $ x
checkIfDone k (Cont x Nothing)  = k x
checkIfDone _ (Cont _ (Just e)) = throwErr $ e

joinI :: (LL.ListLike s el, LL.ListLike s' el') =>
  Iteratee s el (Iteratee s' el' a) ->
  Iteratee s el a
joinI m = Iteratee (docase . runIter m)
  where
  docase (Done ma str) = (flip Done str) (run ma)
  docase (Cont k mErr) = Cont (joinI k) mErr


-- It turns out, Iteratee form a monad. We can use the familiar do
-- notation for composing Iteratees

instance Monad (Iteratee s el) where
  return x = Iteratee (Done x)
  (>>=)    = iterBind

iterBind :: Iteratee s el a ->
  (a -> Iteratee s el b) ->
  Iteratee s el b
iterBind m f = Iteratee (docase . runIter m)
  where
  docase (Done a str)  = runIter (f a) str
  docase (Cont k mErr) = Cont (k `iterBind` f) mErr

{-# INLINE iterBind #-}

instance Functor (Iteratee s el) where
  fmap f m = Iteratee (docase . runIter m)
    where
    -- docase :: IterV s el a -> (IterV s el a)
    docase (Done a stream) = Done (f a) stream
    docase (Cont k mErr)   = Cont (fmap f k) mErr

instance Applicative (Iteratee s el) where
  pure    = return
  m <*> a = m >>= flip fmap a

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list
stream2list :: (LL.ListLike s el) => Iteratee s el [el]
stream2list = Iteratee (step mempty)
  where
  -- step :: s el -> StreamG s -> (IterV s el [el])
  step acc (Chunk ls)
    | LL.null ls      = Cont (Iteratee (step acc)) Nothing
  step acc (Chunk ls) = Cont
                                 (Iteratee (step (acc `mappend` ls)))
                                 Nothing
  step acc str        = Done (LL.toList acc) str

-- |Read a stream to the end and return all of its elements as a stream
stream2stream :: (LL.ListLike s el) => Iteratee s el s
stream2stream = Iteratee (step mempty)
  where
  step acc (Chunk ls)
    | LL.null ls      = Cont (Iteratee (step acc)) Nothing
  step acc (Chunk ls) = Cont
                                 (Iteratee (step (acc `mappend` ls)))
                                 Nothing
  step acc str        = Done acc str


-- |Report and propagate an error.  Disregard the input first and then
-- propagate the error.
throwErr :: ErrMsg -> Iteratee s el a
throwErr e = Iteratee (\_ -> Cont (throwErr e) (Just e))

-- |Check if an iteratee produces an error.
-- Returns 'Right a' if it completes without errors, otherwise 'Left ErrMsg'
-- checkErr is useful for iteratees that may not terminate, such as 'head'
-- with an empty stream.  In particular, it enables them to be used with
-- 'convStream'.
checkErr :: (LL.ListLike s el) =>
  Iteratee s el a ->
  Iteratee s el (Either ErrMsg a)
checkErr iter = Iteratee (check . runIter iter)
  where
  check (Done a str) = Done (Right a) str
  check (Cont _ (Just err)) = Done (Left err) mempty
  check (Cont k Nothing) = Cont (checkErr k) Nothing



-- ------------------------------------------------------------------------
-- Parser combinators

-- |The analogue of List.break
-- It takes an element predicate and returns the (possibly empty) prefix of
-- the stream.  None of the characters in the string satisfy the character
-- predicate.
-- If the stream is not terminated, the first character on the stream
-- satisfies the predicate.

break :: (LL.ListLike s el) =>
  (el -> Bool) ->
  Iteratee s el s
break cpred = Iteratee (step mempty)
  where
  step before (Chunk str) | LL.null str = Cont (Iteratee (step before)) Nothing
  step before (Chunk str) =
    case LL.break cpred str of
      (_, tail') | LL.null tail' -> Cont
                              (Iteratee (step (before `mappend` str)))
                              Nothing
      (str', tail') -> Done (before `mappend` str') (Chunk tail')
  step before stream = Done before stream

-- |The identity iterator.  Doesn't do anything.
identity :: Iteratee s el ()
identity = return ()


-- |Attempt to read the next element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated
head :: (LL.ListLike s el) => Iteratee s el el
head = Iteratee step
  where
  step (Chunk vec)
    | LL.null vec  = Cont head Nothing
    | otherwise    = Done (LL.head vec) (Chunk $ LL.tail vec)
  step stream      = Cont head (Just (setEOF stream))


-- |Given a sequence of characters, attempt to match them against
-- the characters on the stream.  Return the count of how many
-- characters matched.  The matched characters are removed from the
-- stream.
-- For example, if the stream contains "abd", then (heads "abc")
-- will remove the characters "ab" and return 2.
heads :: (LL.ListLike s el, Eq el) =>
  s ->
  Iteratee s el Int
heads st | LL.null st = return 0
heads st = loop 0 st
  where
  loop cnt xs | LL.null xs = return cnt
  loop cnt xs              = Iteratee (step cnt xs)
  step cnt str (Chunk xs) | LL.null xs  = Cont (loop cnt str) Nothing
  step cnt str stream     | LL.null str = Done cnt stream
  step cnt str s@(Chunk xs) =
    if LL.head str == LL.head xs
       then step (succ cnt) (LL.tail str) (Chunk $ LL.tail xs)
       else Done cnt s
  step cnt _ stream         = Done cnt stream


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return (Just c) if successful, return Nothing if the stream is
-- terminated (by EOF or an error)
peek :: (LL.ListLike s el) => Iteratee s el (Maybe el)
peek = Iteratee step
  where
  step s@(Chunk vec)
    | LL.null vec = Cont peek Nothing
    | otherwise = Done (Just $ LL.head vec) s
  step stream   = Done Nothing stream


-- |Skip the rest of the stream
skipToEof :: Iteratee s el ()
skipToEof = Iteratee step
  where
  step (Chunk _) = Cont skipToEof Nothing
  step s         = Done () s


-- |Seek to a position in the stream
seek :: FileOffset -> Iteratee s el ()
seek n = Iteratee step
  where
  step (Chunk _) = Cont identity (Just (Seek n))
  step s         = Done () s



-- |Skip n elements of the stream, if there are that many
-- This is the analogue of List.drop
drop :: (LL.ListLike s el) => Int -> Iteratee s el ()
drop 0 = return ()
drop n = Iteratee step
  where
  step (Chunk str)
    | LL.length str <= n = Cont (drop (n - LL.length str)) Nothing
  step (Chunk str)       = Done () (Chunk (LL.drop n str))
  step stream            = Done () stream

-- |Skip all elements while the predicate is true.
-- This is the analogue of List.dropWhile
dropWhile :: (LL.ListLike s el) =>
  (el -> Bool) ->
  Iteratee s el ()
dropWhile p = Iteratee step
  where
  step (Chunk str) = let dropped = LL.dropWhile p str
                     in if LL.null dropped
                       then Cont (dropWhile p) Nothing
                       else Done () (Chunk dropped)
  step stream      = Done () stream


-- |Return the total length of the stream
length :: (Num a, LL.ListLike s el) => Iteratee s el a
length = length' 0
  where
  length' = Iteratee . step
  step i (Chunk xs) = Cont
                               (length' $! i + fromIntegral (LL.length xs))
                               Nothing
  step i stream     = Done i stream


-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- |The type of the converter from the stream with elements el_outer
-- to the stream with element el_inner.  The result is the iteratee
-- for the outer stream that uses an `Iteratee el_inner a'
-- to process the embedded, inner stream as it reads the outer stream.
type EnumeratorN s_outer el_outer s_inner el_inner a =
  Iteratee s_inner el_inner a ->
  Iteratee s_outer el_outer (Iteratee s_inner el_inner a)

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements (even if the iteratee has accepted fewer).
take :: (LL.ListLike s el) =>
  Int ->
  EnumeratorN s el s el a
take 0 iter = return iter
take n' iter = Iteratee (step n')
  where
  step n chk@(Chunk str)
    | LL.null str = Cont (take n iter) Nothing
    | LL.length str <= n = Cont inner Nothing
      where inner = (check (n - LL.length str)) (runIter iter chk)
  step n (Chunk str) = done (Chunk s1) (Chunk s2)
    where (s1, s2) = LL.splitAt n str
  step _n stream            = done stream stream
  check n (Done x _)        = drop n >> return (return x)
  check n (Cont x Nothing)  = take n x
  check n (Cont _ (Just e)) = drop n >> throwErr e
  done s1 s2 = (flip Done s2) (checkIfDone id $ runIter iter s1)


-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of `take' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early.
takeR :: (LL.ListLike s el) =>
  Int ->
  Iteratee s el a ->
  Iteratee s el (Iteratee s el a)
takeR 0 iter = return iter
takeR n iter = Iteratee (step n)
  where
  step n' s@(Chunk str)
    | LL.null str        = Cont (takeR n iter) Nothing
    | LL.length str <= n = check (n - LL.length str) $ runIter iter s
    | otherwise          = done (Chunk str1) (Chunk str2)
      where (str1, str2) = LL.splitAt n' str
  step _n str            = done str str
  check _n' (Done a str)   = Done (return a) str
  check n'  (Cont k mErr)  = Cont (takeR n' k) mErr
  done s1 s2 = (flip Done s2) (checkIfDone id $ runIter iter s1)


-- |Map the stream: yet another iteratee transformer
-- Given the stream of elements of the type el and the function el->el',
-- build a nested stream of elements of the type el' and apply the
-- given iteratee to it.
-- Note the contravariance

mapStream :: (LL.ListLike (s el) el,
    LL.ListLike (s el') el',
    LooseMap s el el') =>
 (el -> el')
  -> EnumeratorN (s el) el (s el') el' a
mapStream f i = go i
  where
    go iter = Iteratee (check . runIter iter . strMap (lMap f))
    check (Done a _)    = Done (return a) (Chunk LL.empty)
    check (Cont k mErr) = Cont (go k) mErr

-- |Map a stream without changing the element type.  For StreamChunks
-- with limited element types (e.g. bytestrings)
-- this can be much more efficient than regular mapStream
rigidMapStream :: (LL.ListLike s el) =>
  (el -> el)
  -> EnumeratorN s el s el a
rigidMapStream f i =  go i
  where
    go iter = Iteratee ((check . runIter iter) . strMap (LL.rigidMap f))
    check (Done a _)    = Done (return a) (Chunk LL.empty)
    check (Cont k mErr) = Cont (go k) mErr

-- |Convert one stream into another, not necessarily in `lockstep'
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- Iteratee s el (Maybe (s' el')).  The Maybe type is in case of
-- errors (or end of stream).
convStream :: Iteratee s el (Maybe s') ->
  EnumeratorN s el s' el' a
convStream fi iter = fi >>= check
  where
  check (Just xs) = docase (runIter iter (Chunk xs))
  check (Nothing) = return iter
  docase (Done a _)        = return . return $ a
  docase (Cont k Nothing)  = convStream fi k
  docase (Cont _ (Just e)) = return $ throwErr e

{-# INLINE convStream #-}

-- |Creates an enumerator with only elements from the stream that
-- satisfy the predicate function.
filter :: (LL.ListLike s el) =>
  (el -> Bool) ->
  EnumeratorN s el s el a
filter p = convStream f'
  where
  f' = Iteratee step
  step (Chunk xs) | LL.null xs = Cont f' Nothing
  step (Chunk xs) = Done (Just $ LL.filter p xs) mempty
  step stream     = Done Nothing stream

-- ------------------------------------------------------------------------
-- Folds

-- | Left-associative fold.
foldl :: (LL.ListLike s el, FLL.FoldableLL s el) =>
  (a -> el -> a) ->
  a ->
  Iteratee s el a
foldl f i = iter i
  where
  iter ac = Iteratee step
    where
      step (Chunk xs) | LL.null xs = Cont (iter ac) Nothing
      step (Chunk xs) = Cont (iter (FLL.foldl f ac xs)) Nothing
      step stream     = Done ac stream

-- | Left-associative fold that is strict in the accumulator.
foldl' :: (LL.ListLike s el, FLL.FoldableLL s el) =>
  (a -> el -> a) ->
  a ->
  Iteratee s el a
foldl' f i = Iteratee (step i)
  where
    step ac (Chunk xs) | LL.null xs = Cont (Iteratee (step ac))
                                               Nothing
    step ac (Chunk xs) = Cont (Iteratee (step $! FLL.foldl' f ac xs))
                                       Nothing
    step ac stream     = Done ac stream

{-# INLINE foldl' #-}

-- | Variant of foldl with no base case.  Requires at least one element
--   in the stream.
foldl1 :: (LL.ListLike s el, FLL.FoldableLL s el) =>
  (el -> el -> el) ->
  Iteratee s el el
foldl1 f = Iteratee step
  where
  step (Chunk xs) | LL.null xs = Cont (foldl1 f) Nothing
  -- After the first chunk, just use regular foldl in order to account for
  -- the accumulator.
  step (Chunk xs) = Cont (foldl f (FLL.foldl1 f xs)) Nothing
  step stream     = Cont (foldl1 f) (Just (setEOF stream))

-- | Sum of a stream.
sum :: (LL.ListLike s el, Num el) =>
  Iteratee s el el
sum = Iteratee (step 0)
  where
    step acc (Chunk xs)
      | LL.null xs = Cont (Iteratee (step acc)) Nothing
    step acc (Chunk xs) = Cont (Iteratee . step $! acc + LL.sum xs)
                                        Nothing
    step acc str = Done acc str

-- | Product of a stream
product :: (LL.ListLike s el, Num el) =>
  Iteratee s el el
product = Iteratee (step 1)
  where
    step acc (Chunk xs)
      | LL.null xs = Cont (Iteratee (step acc)) Nothing
    step acc (Chunk xs) = Cont (Iteratee . step $! acc *
                                          LL.product xs)
                                        Nothing
    step acc str = Done acc str

-- ------------------------------------------------------------------------
-- Zips

-- |Enumerate two iteratees over a single stream simultaneously.
enumPair :: (LL.ListLike s el) =>
  Iteratee s el a ->
  Iteratee s el b ->
  Iteratee s el (a,b)
enumPair i1 i2 = Iteratee step
  where
  longest c1@(Chunk xs) c2@(Chunk ys) = if LL.length xs > LL.length ys
                                        then c1 else c2
  longest e@(EOF _)  _          = e
  longest _          e@(EOF _)  = e
  step (Chunk xs) | LL.null xs = Cont (Iteratee step) Nothing
  step str = let ia = runIter i1 str
                 ib = runIter i2 str
             in case (ia, ib) of
               (Done a astr, Done b bstr)  -> Done (a,b) $ longest astr bstr
               (Done a _astr, Cont k mErr) -> Cont (enumPair (return a) k) mErr
               (Cont k mErr, Done b _bstr) -> Cont (enumPair k (return b)) mErr
               (Cont a aEr,  Cont b bEr)   -> Cont (enumPair a b)
                                                   (aEr `mappend` bEr)

enumPar :: (LL.ListLike s el) =>
  Iteratee s el a ->
  Iteratee s el b ->
  Iteratee s el (a,b)
enumPar i1 i2 = Iteratee step
  where
  longest c1@(Chunk xs) c2@(Chunk ys) = if LL.length xs > LL.length ys
                                        then c1 else c2
  longest e@(EOF _)  _          = e
  longest _          e@(EOF _)  = e
  step (Chunk xs) | LL.null xs = Cont (Iteratee step) Nothing
  step str = let ia = runIter i1 str
                 ib = runIter i2 str
                 tup = ia `par` ib `pseq` (ia, ib)
             in case tup of
               (Done a astr, Done b bstr)  -> Done (a,b) $ longest astr bstr
               (Done a _astr, Cont k mErr) -> Cont (enumPair (return a) k) mErr
               (Cont k mErr, Done b _bstr) -> Cont (enumPair k (return b)) mErr
               (Cont a aEr,  Cont b bEr)   -> Cont (enumPair a b)
                                                   (aEr `mappend` bEr)

-- ------------------------------------------------------------------------
-- Enumerators
-- |Each enumerator takes an iteratee and returns an iteratee
-- an Enumerator is an iteratee transformer.
-- The enumerator normally stops when the stream is terminated
-- or when the iteratee moves to the done state, whichever comes first.
-- When to stop is of course up to the enumerator...

type Enumerator s el a = Iteratee s el a -> Iteratee s el a

-- We have two choices of composition: compose iteratees or compose
-- enumerators. The latter is useful when one iteratee
-- reads from the concatenation of two data sources.

type EnumeratorM s el m a = Iteratee s el a -> m (Iteratee s el a)

-- |More general enumerator type: enumerator that maps
-- streams (not necessarily in lock-step).  This is
-- a flattened (`joinI-ed') EnumeratorN sfrom elfrom sto elto a
type EnumeratorMM sfrom elfrom sto elto m a =
  Iteratee sto elto a -> m (Iteratee sfrom elfrom a)

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee usually in the done state.
enumEof :: Enumerator s el a
enumEof iter = check $ runIter iter (EOF Nothing)
  where
  check (Done x _) = Iteratee $ Done x
  check (Cont _ e) = throwErr (fromMaybe (Err "Divergent Iteratee") e)

-- |Another primitive enumerator: report an error
enumErr :: (LL.ListLike s el) =>
  String ->
  Enumerator s el a
enumErr e iter = check $ runIter iter (EOF (Just (Err e)))
  where
  check (Done x _)  = Iteratee (Done x)
  check (Cont _ e') = throwErr (fromMaybe (Err "Divergent Iteratee") e')

-- |The composition of two enumerators: essentially the functional composition
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >. e2, e1 is executed first

(>.):: (LL.ListLike s el, Monad m) =>
  EnumeratorM s el m a -> EnumeratorM s el m a -> EnumeratorM s el m a
(>.) = (>=>)

-- |The pure 1-chunk enumerator
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enumPure1Chunk :: (LL.ListLike s el) =>
  s ->
  Enumerator s el a
enumPure1Chunk str iter = checkIfDone id $ runIter iter (Chunk str)


-- |The pure n-chunk enumerator
-- It passes a given chunk of elements to the iteratee in n chunks
-- This enumerator does no IO and is useful for testing of base parsing
-- and handling of chunk boundaries
enumPureNChunk :: (LL.ListLike s el) =>
  s ->
  Int ->
  Enumerator s el a
enumPureNChunk str _ iter | LL.null str = iter
enumPureNChunk str n iter | n > 0 = checkIfDone (enumPureNChunk s2 n) $
                                      runIter iter (Chunk s1)
  where
  (s1, s2) = LL.splitAt n str
enumPureNChunk _ n _ = error $ "enumPureNChunk called with n==" ++ show n

