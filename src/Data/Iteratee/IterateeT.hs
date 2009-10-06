{-# LANGUAGE FlexibleContexts #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.IterateeT (
  -- * Types
  IterTV (..),
  IterateeT (..),
  EnumeratorTN,
  EnumeratorTM,
  EnumeratorTMM,
  -- * Iteratees
  -- ** Iteratee Utilities
  joinI,
  liftI,
  isFinished,
  getStatus,
  run,
  joinIM,
  stream2list,
  stream2stream,
  checkIfDone,
  liftInner,
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
import Control.Applicative
import Control.Monad.Trans
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

data IterTV c el m a =
  Done a (StreamG c)
  | Cont (IterateeT c el m a) (Maybe ErrMsg)

instance (Show c, Show a) => Show (IterTV c el m a) where
  show (Done a str) = "IterTV Done <<" ++ show a ++ ">> : <<" ++ show str ++ ">>"
  show (Cont _ mErr) = "IterTV Cont :: " ++ show mErr

newtype IterateeT c el m a = IterateeT{
  runIter :: StreamG c -> m (IterTV c el m a)
  }


-- Useful combinators for implementing iteratees and enumerators

-- | Lift an IterTV result into an 'IterateeT'
liftI :: (Monad m, LL.ListLike s el) => IterTV s el m a -> IterateeT s el m a
liftI (Cont k Nothing)     = k
liftI (Cont _k (Just err)) = throwErr err
liftI i@(Done _ (EOF _  )) = IterateeT (const (return i))
liftI (Done a (Chunk st )) = IterateeT (check st)
  where
  check str (Chunk str') = return $ Done a (Chunk $ str `mappend` str')
  check _str e@(EOF _)   = return $ Done a e

-- | Run an 'IterateeT' and get the result.  An 'EOF' is sent to the
-- iteratee as it is run.
run :: (Monad m, LL.ListLike s el) => IterateeT s el m a -> m a
run iter = runIter iter (EOF Nothing) >>= \res ->
  case res of
    Done x _ -> return x
    Cont _ e -> error $ "control message: " ++ show e

-- | Check if a stream has finished, whether from EOF or Error.
isFinished :: (LL.ListLike s el, Monad m) =>
  IterateeT s el m Bool
isFinished = IterateeT check
  where
  check (Chunk xs) | LL.null xs = return $ Cont isFinished Nothing
  check s@(EOF _)               = return $ Done True s
  check s                       = return $ Done False s

-- | Get the stream status of an iteratee.
getStatus :: (LL.ListLike s el, Monad m) =>
  IterateeT s el m StreamStatus
getStatus = IterateeT check
  where
    check s@(EOF Nothing)  = return $ Done EofNoError s
    check s@(EOF (Just e)) = return $ Done (EofError e) s
    check s                = return $ Done DataRemaining s

-- |If the iteratee ('IterTV') has finished, return its value.  If it has not
-- finished then apply it to the given 'EnumeratorTM'.
-- If in error, throw the error.
checkIfDone :: (LL.ListLike s el, Monad m) =>
  (IterateeT s el m a -> m (IterateeT s el m a)) ->
  IterTV s el m a ->
  m (IterateeT s el m a)
checkIfDone _ (Done x _)        = return . return $ x
checkIfDone k (Cont x Nothing)  = k x
checkIfDone _ (Cont _ (Just e)) = return . throwErr $ e

-- |The following is a `variant' of join in the IterateeT s el m monad
-- When el' is the same as el, the type of joinI is indeed that of
-- true monadic join.  However, joinI is subtly different: since
-- generally el' is different from el, it makes no sense to
-- continue using the internal, IterateeT el' m a: we no longer
-- have elements of the type el' to feed to that iteratee.
-- We thus send EOF to the internal Iteratee and propagate its result.
-- This join function is useful when dealing with `derived iteratees'
-- for embedded/nested streams.  In particular, joinI is useful to
-- process the result of take, mapStream, or convStream below.
joinI :: (LL.ListLike s el, LL.ListLike s' el', Monad m) =>
  IterateeT s el m (IterateeT s' el' m a) ->
  IterateeT s el m a
joinI m = IterateeT (docase <=< runIter m)
  where
  docase (Done ma str) = liftM (flip Done str) (run ma)
  docase (Cont k mErr) = return $ Cont (joinI k) mErr

-- |Layer a monad transformer over the inner monad.
liftInner :: (Monad m, MonadTrans t, Monad (t m)) =>
  IterateeT s el m a ->
  IterateeT s el (t m) a
liftInner iter = IterateeT step
  where
  step str = do
    igv <- lift $ runIter iter str
    case igv of
      Done a res  -> return $ Done a res
      Cont k mErr -> return $ Cont (liftInner k) mErr

-- It turns out, IterateeT form a monad. We can use the familiar do
-- notation for composing Iteratees

instance (Monad m) => Monad (IterateeT s el m) where
  return x = IterateeT (return . Done x)
  (>>=)    = iterBind

iterBind :: (Monad m ) =>
  IterateeT s el m a ->
  (a -> IterateeT s el m b) ->
  IterateeT s el m b
iterBind m f = IterateeT (docase <=< runIter m)
  where
  docase (Done a str)  = runIter (f a) str
  docase (Cont k mErr) = return $ Cont (k `iterBind` f) mErr

{-# INLINE iterBind #-}

instance (Monad m, Functor m) =>
  Functor (IterateeT s el m) where
  fmap f m = IterateeT (docase <=< runIter m)
    where
    -- docase :: IterTV s el m a -> m (IterTV s el m a)
    docase (Done a stream) = return $ Done (f a) stream
    docase (Cont k mErr)   = return $ Cont (fmap f k) mErr

instance (Monad m, Functor m) => Applicative (IterateeT s el m) where
  pure    = return
  m <*> a = m >>= flip fmap a

instance MonadTrans (IterateeT s el) where
  lift m = IterateeT $ \str -> liftM (flip Done str) m

instance (MonadIO m) => MonadIO (IterateeT s el m) where
  liftIO = lift . liftIO

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list
stream2list :: (LL.ListLike s el, Monad m) => IterateeT s el m [el]
stream2list = IterateeT (step mempty)
  where
  -- step :: s el -> StreamG s -> m (IterTV s el m [el])
  step acc (Chunk ls)
    | LL.null ls      = return $ Cont (IterateeT (step acc)) Nothing
  step acc (Chunk ls) = return $ Cont
                                 (IterateeT (step (acc `mappend` ls)))
                                 Nothing
  step acc str        = return $ Done (LL.toList acc) str

-- |Read a stream to the end and return all of its elements as a stream
stream2stream :: (LL.ListLike s el, Monad m) => IterateeT s el m s
stream2stream = IterateeT (step mempty)
  where
  step acc (Chunk ls)
    | LL.null ls      = return $ Cont (IterateeT (step acc)) Nothing
  step acc (Chunk ls) = return $ Cont
                                 (IterateeT (step (acc `mappend` ls)))
                                 Nothing
  step acc str        = return $ Done acc str


-- |Report and propagate an error.  Disregard the input first and then
-- propagate the error.
throwErr :: (Monad m) => ErrMsg -> IterateeT s el m a
throwErr e = IterateeT (\_ -> return $ Cont (throwErr e) (Just e))

-- |Check if an iteratee produces an error.
-- Returns 'Right a' if it completes without errors, otherwise 'Left ErrMsg'
-- checkErr is useful for iteratees that may not terminate, such as 'head'
-- with an empty stream.  In particular, it enables them to be used with
-- 'convStream'.
checkErr :: (Monad m, LL.ListLike s el) =>
  IterateeT s el m a ->
  IterateeT s el m (Either ErrMsg a)
checkErr iter = IterateeT (check <=< runIter iter)
  where
  check (Done a str) = return $ Done (Right a) str
  check (Cont _ (Just err)) = return $ Done (Left err) mempty
  check (Cont k Nothing) = return $ Cont (checkErr k) Nothing



-- ------------------------------------------------------------------------
-- Parser combinators

-- |The analogue of List.break
-- It takes an element predicate and returns the (possibly empty) prefix of
-- the stream.  None of the characters in the string satisfy the character
-- predicate.
-- If the stream is not terminated, the first character on the stream
-- satisfies the predicate.

break :: (LL.ListLike s el, Monad m) =>
  (el -> Bool) ->
  IterateeT s el m s
break cpred = IterateeT (step mempty)
  where
  step before (Chunk str) | LL.null str = return $
    Cont (IterateeT (step before)) Nothing
  step before (Chunk str) =
    case LL.break cpred str of
      (_, tail') | LL.null tail' -> return $ Cont
                              (IterateeT (step (before `mappend` str)))
                              Nothing
      (str', tail') -> return $ Done (before `mappend` str') (Chunk tail')
  step before stream = return $ Done before stream

-- |The identity iterator.  Doesn't do anything.
identity :: (Monad m) => IterateeT s el m ()
identity = return ()


-- |Attempt to read the next element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated
head :: (LL.ListLike s el, Monad m) => IterateeT s el m el
head = IterateeT step
  where
  step (Chunk vec)
    | LL.null vec  = return $ Cont head Nothing
    | otherwise    = return $ Done (LL.head vec) (Chunk $ LL.tail vec)
  step stream      = return $ Cont head (Just (setEOF stream))


-- |Given a sequence of characters, attempt to match them against
-- the characters on the stream.  Return the count of how many
-- characters matched.  The matched characters are removed from the
-- stream.
-- For example, if the stream contains "abd", then (heads "abc")
-- will remove the characters "ab" and return 2.
heads :: (LL.ListLike s el, Monad m, Eq el) =>
  s ->
  IterateeT s el m Int
heads st | LL.null st = return 0
heads st = loop 0 st
  where
  loop cnt xs | LL.null xs = return cnt
  loop cnt xs              = IterateeT (step cnt xs)
  step cnt str (Chunk xs) | LL.null xs  = return $ Cont (loop cnt str) Nothing
  step cnt str stream     | LL.null str = return $ Done cnt stream
  step cnt str s@(Chunk xs) =
    if LL.head str == LL.head xs
       then step (succ cnt) (LL.tail str) (Chunk $ LL.tail xs)
       else return $ Done cnt s
  step cnt _ stream         = return $ Done cnt stream


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return (Just c) if successful, return Nothing if the stream is
-- terminated (by EOF or an error)
peek :: (LL.ListLike s el, Monad m) => IterateeT s el m (Maybe el)
peek = IterateeT step
  where
  step s@(Chunk vec)
    | LL.null vec = return $ Cont peek Nothing
    | otherwise = return $ Done (Just $ LL.head vec) s
  step stream   = return $ Done Nothing stream


-- |Skip the rest of the stream
skipToEof :: (Monad m) => IterateeT s el m ()
skipToEof = IterateeT step
  where
  step (Chunk _) = return $ Cont skipToEof Nothing
  step s         = return $ Done () s


-- |Seek to a position in the stream
seek :: (Monad m) => FileOffset -> IterateeT s el m ()
seek n = IterateeT step
  where
  step (Chunk _) = return $ Cont identity (Just (Seek n))
  step s         = return $ Done () s



-- |Skip n elements of the stream, if there are that many
-- This is the analogue of List.drop
drop :: (LL.ListLike s el, Monad m) => Int -> IterateeT s el m ()
drop 0 = return ()
drop n = IterateeT step
  where
  step (Chunk str)
    | LL.length str <= n = return $ Cont (drop (n - LL.length str)) Nothing
  step (Chunk str)       = return $ Done () (Chunk (LL.drop n str))
  step stream            = return $ Done () stream

-- |Skip all elements while the predicate is true.
-- This is the analogue of List.dropWhile
dropWhile :: (LL.ListLike s el, Monad m) =>
  (el -> Bool) ->
  IterateeT s el m ()
dropWhile p = IterateeT step
  where
  step (Chunk str) = let dropped = LL.dropWhile p str
                     in if LL.null dropped
                       then return $ Cont (dropWhile p) Nothing
                       else return $ Done () (Chunk dropped)
  step stream      = return $ Done () stream


-- |Return the total length of the stream
length :: (Num a, LL.ListLike s el, Monad m) => IterateeT s el m a
length = length' 0
  where
  length' = IterateeT . step
  step i (Chunk xs) = return $ Cont
                               (length' $! i + fromIntegral (LL.length xs))
                               Nothing
  step i stream     = return $ Done i stream


-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- |The type of the converter from the stream with elements el_outer
-- to the stream with element el_inner.  The result is the iteratee
-- for the outer stream that uses an `IterateeT el_inner m a'
-- to process the embedded, inner stream as it reads the outer stream.
type EnumeratorTN s_outer el_outer s_inner el_inner m a =
  IterateeT s_inner el_inner m a ->
  IterateeT s_outer el_outer m (IterateeT s_inner el_inner m a)

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements (even if the iteratee has accepted fewer).
take :: (LL.ListLike s el, Monad m) =>
  Int ->
  EnumeratorTN s el s el m a
take 0 iter = return iter
take n' iter = IterateeT (step n')
  where
  step n chk@(Chunk str)
    | LL.null str = return $ Cont (take n iter) Nothing
    | LL.length str < n = liftM (flip Cont Nothing) inner
      where inner = liftM (check (n - LL.length str)) (runIter iter chk)
  step n (Chunk str) = done (Chunk s1) (Chunk s2)
    where (s1, s2) = LL.splitAt n str
  step _n stream            = done stream stream
  check n (Done x _)        = drop n >> return (return x)
  check n (Cont x Nothing)  = take n x
  check n (Cont _ (Just e)) = drop n >> throwErr e
  done s1 s2 = liftM (flip Done s2) (runIter iter s1 >>= checkIfDone return)


-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of `take' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early.
takeR :: (LL.ListLike s el, Monad m) =>
  Int ->
  IterateeT s el m a ->
  IterateeT s el m (IterateeT s el m a)
takeR 0 iter = return iter
takeR n iter = IterateeT (step n)
  where
  step n' s@(Chunk str)
    | LL.null str        = return $ Cont (takeR n iter) Nothing
    | LL.length str <= n = runIter iter s >>= check (n - LL.length str)
    | otherwise          = done (Chunk str1) (Chunk str2)
      where (str1, str2) = LL.splitAt n' str
  step _n str            = done str str
  check _n' (Done a str)   = return $ Done (return a) str
  check n'  (Cont k mErr)  = return $ Cont (takeR n' k) mErr
  done s1 s2 = liftM (flip Done s2) (runIter iter s1 >>= checkIfDone return)


-- |Map the stream: yet another iteratee transformer
-- Given the stream of elements of the type el and the function el->el',
-- build a nested stream of elements of the type el' and apply the
-- given iteratee to it.
-- Note the contravariance

mapStream :: (LL.ListLike (s el) el,
    LL.ListLike (s el') el',
    LooseMap s el el',
    Monad m) =>
 (el -> el')
  -> EnumeratorTN (s el) el (s el') el' m a
mapStream f i = go i
  where
    go iter = IterateeT ((check <=< runIter iter) . strMap (lMap f))
    check (Done a _)    = return $ Done (return a) (Chunk LL.empty)
    check (Cont k mErr) = return $ Cont (go k) mErr

-- |Map a stream without changing the element type.  For StreamChunks
-- with limited element types (e.g. bytestrings)
-- this can be much more efficient than regular mapStream
rigidMapStream :: (LL.ListLike s el, Monad m) =>
  (el -> el)
  -> EnumeratorTN s el s el m a
rigidMapStream f i =  go i
  where
    go iter = IterateeT ((check <=< runIter iter) . strMap (LL.rigidMap f))
    check (Done a _)    = return $ Done (return a) (Chunk LL.empty)
    check (Cont k mErr) = return $ Cont (go k) mErr

-- |Convert one stream into another, not necessarily in `lockstep'
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- IterateeT s el m (Maybe (s' el')).  The Maybe type is in case of
-- errors (or end of stream).
convStream :: Monad m =>
  IterateeT s el m (Maybe s') ->
  EnumeratorTN s el s' el' m a
convStream fi iter = fi >>= check
  where
  check (Just xs) = lift (runIter iter (Chunk xs)) >>= docase
  check (Nothing) = return iter
  docase (Done a _)        = return . return $ a
  docase (Cont k Nothing)  = convStream fi k
  docase (Cont _ (Just e)) = return $ throwErr e

{-# INLINE convStream #-}

-- |Creates an enumerator with only elements from the stream that
-- satisfy the predicate function.
filter :: (LL.ListLike s el, Monad m) =>
  (el -> Bool) ->
  EnumeratorTN s el s el m a
filter p = convStream f'
  where
  f' = IterateeT step
  step (Chunk xs) | LL.null xs = return $ Cont f' Nothing
  step (Chunk xs) = return $ Done (Just $ LL.filter p xs) mempty
  step stream     = return $ Done Nothing stream

-- ------------------------------------------------------------------------
-- Folds

-- | Left-associative fold.
foldl :: (LL.ListLike s el, FLL.FoldableLL s el, Monad m) =>
  (a -> el -> a) ->
  a ->
  IterateeT s el m a
foldl f i = iter i
  where
  iter ac = IterateeT step
    where
      step (Chunk xs) | LL.null xs = return $ Cont (iter ac) Nothing
      step (Chunk xs) = return $ Cont (iter (FLL.foldl f ac xs)) Nothing
      step stream     = return $ Done ac stream

-- | Left-associative fold that is strict in the accumulator.
foldl' :: (LL.ListLike s el, FLL.FoldableLL s el, Monad m) =>
  (a -> el -> a) ->
  a ->
  IterateeT s el m a
foldl' f i = IterateeT (step i)
  where
    step ac (Chunk xs) | LL.null xs = return $ Cont (IterateeT (step ac))
                                               Nothing
    step ac (Chunk xs) = return $ Cont (IterateeT (step $! FLL.foldl' f ac xs))
                                       Nothing
    step ac stream     = return $ Done ac stream

{-# INLINE foldl' #-}

-- | Variant of foldl with no base case.  Requires at least one element
--   in the stream.
foldl1 :: (LL.ListLike s el, FLL.FoldableLL s el, Monad m) =>
  (el -> el -> el) ->
  IterateeT s el m el
foldl1 f = IterateeT step
  where
  step (Chunk xs) | LL.null xs = return $ Cont (foldl1 f) Nothing
  -- After the first chunk, just use regular foldl in order to account for
  -- the accumulator.
  step (Chunk xs) = return $ Cont (foldl f (FLL.foldl1 f xs)) Nothing
  step stream     = return $ Cont (foldl1 f) (Just (setEOF stream))

-- | Sum of a stream.
sum :: (LL.ListLike s el, Num el, Monad m) =>
  IterateeT s el m el
sum = IterateeT (step 0)
  where
    step acc (Chunk xs)
      | LL.null xs = return $ Cont (IterateeT (step acc)) Nothing
    step acc (Chunk xs) = return $ Cont (IterateeT . step $! acc + LL.sum xs)
                                        Nothing
    step acc str = return $ Done acc str

-- | Product of a stream
product :: (LL.ListLike s el, Num el, Monad m) =>
  IterateeT s el m el
product = IterateeT (step 1)
  where
    step acc (Chunk xs)
      | LL.null xs = return $ Cont (IterateeT (step acc)) Nothing
    step acc (Chunk xs) = return $ Cont (IterateeT . step $! acc *
                                          LL.product xs)
                                        Nothing
    step acc str = return $ Done acc str

-- ------------------------------------------------------------------------
-- Zips

-- |Enumerate two iteratees over a single stream simultaneously.
enumPair :: (LL.ListLike s el, Monad m) =>
  IterateeT s el m a ->
  IterateeT s el m b ->
  IterateeT s el m (a,b)
enumPair i1 i2 = IterateeT step
  where
  longest c1@(Chunk xs) c2@(Chunk ys) = if LL.length xs > LL.length ys
                                        then c1 else c2
  longest e@(EOF _)  _          = e
  longest _          e@(EOF _)  = e
  step (Chunk xs) | LL.null xs = return $ Cont (IterateeT step) Nothing
  step str = do
    ia <- runIter i1 str
    ib <- runIter i2 str
    case (ia, ib) of
      (Done a astr, Done b bstr)  -> return $ Done (a,b) $ longest astr bstr
      (Done a _astr, Cont k mErr) -> return $ Cont (enumPair (return a) k) mErr
      (Cont k mErr, Done b _bstr) -> return $ Cont (enumPair k (return b)) mErr
      (Cont a aEr,  Cont b bEr)   -> return $ Cont (enumPair a b)
                                                   (aEr `mappend` bEr)

-- ------------------------------------------------------------------------
-- Enumerators
-- |Each enumerator takes an iteratee and returns an iteratee
-- an Enumerator is an iteratee transformer.
-- The enumerator normally stops when the stream is terminated
-- or when the iteratee moves to the done state, whichever comes first.
-- When to stop is of course up to the enumerator...

-- We have two choices of composition: compose iteratees or compose
-- enumerators. The latter is useful when one iteratee
-- reads from the concatenation of two data sources.

type EnumeratorTM s el m a = IterateeT s el m a -> m (IterateeT s el m a)

-- |More general enumerator type: enumerator that maps
-- streams (not necessarily in lock-step).  This is
-- a flattened (`joinI-ed') EnumeratorTN sfrom elfrom sto elto m a
type EnumeratorTMM sfrom elfrom sto elto m a =
  IterateeT sto elto m a -> m (IterateeT sfrom elfrom m a)

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee usually in the done state.
enumEof :: Monad m =>
  EnumeratorTM s el m a
enumEof iter = runIter iter (EOF Nothing) >>= check
  where
  check (Done x _) = return $ IterateeT $ return . Done x
  check (Cont _ e) = return $ throwErr (fromMaybe (Err "Divergent Iteratee") e)

-- |Another primitive enumerator: report an error
enumErr :: (LL.ListLike s el, Monad m) =>
  String ->
  EnumeratorTM s el m a
enumErr e iter = runIter iter (EOF (Just (Err e))) >>= check
  where
  check (Done x _)  = return $ IterateeT (return . Done x)
  check (Cont _ e') = return $ throwErr
                      (fromMaybe (Err "Divergent Iteratee") e')

-- |The composition of two enumerators: essentially the functional composition
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >. e2, e1 is executed first

(>.):: (LL.ListLike s el, Monad m) =>
  EnumeratorTM s el m a -> EnumeratorTM s el m a -> EnumeratorTM s el m a
(>.) = (>=>)

-- |The pure 1-chunk enumerator
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enumPure1Chunk :: (LL.ListLike s el, Monad m) =>
  s ->
  EnumeratorTM s el m a
enumPure1Chunk str iter = runIter iter (Chunk str) >>= checkIfDone return


-- |The pure n-chunk enumerator
-- It passes a given chunk of elements to the iteratee in n chunks
-- This enumerator does no IO and is useful for testing of base parsing
-- and handling of chunk boundaries
enumPureNChunk :: (LL.ListLike s el, Monad m) =>
  s ->
  Int ->
  EnumeratorTM s el m a
enumPureNChunk str _ iter | LL.null str = return iter
enumPureNChunk str n iter | n > 0 = runIter iter (Chunk s1) >>=
                                    checkIfDone (enumPureNChunk s2 n)
  where
  (s1, s2) = LL.splitAt n str
enumPureNChunk _ n _ = error $ "enumPureNChunk called with n==" ++ show n

-- |A variant of join for Iteratees in a monad.
joinIM :: (Monad m) => m (IterateeT s el m a) -> IterateeT s el m a
joinIM m = IterateeT (\str -> m >>= flip runIter str)
