{-# LANGUAGE KindSignatures, FlexibleContexts, ScopedTypeVariables #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Iteratee (
  -- * Types
  -- ** Error handling
  throwErr,
  throwRecoverableErr,
  checkErr,
  -- ** Basic Iteratees
  identity,
  skipToEof,
  isStreamFinished,
  -- ** Nested iteratee combinators
  convStream,
  joinI,
  joinIM,
  -- * Enumerators
  Enumerator,
  Enumeratee,
  -- ** Basic enumerators
  enumEof,
  enumErr,
  enumPure1Chunk,
  enumFromCallback,
  enumFromCallbackCatch,
  -- ** Enumerator Combinators
  (>>>),
  eneeCheckIfDone,
  -- * Misc.
  seek,
  FileOffset,
  -- * Classes
  module Data.Iteratee.Base
)
where

import Prelude hiding (head, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product)

import Data.Iteratee.IO.Base
import Data.Iteratee.Base

import Control.Exception
import Control.Failure
import Data.Maybe
import Data.Monoid

-- exception helpers
excDivergent :: SomeException
excDivergent = toException DivergentException

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Report and propagate an unrecoverable error.
--  Disregard the input first and then propagate the error.
throwErr :: (Monad m) => SomeException -> Iteratee s m a
throwErr e = icont (const (throwErr e)) (Just e)

-- |Propagate a recoverable error.
throwRecoverableErr
  :: (Monad m) =>
     SomeException
     -> (StreamG s -> Iteratee s m a)
     -> Iteratee s m a
throwRecoverableErr e i = icont i (Just e)


-- |Check if an iteratee produces an error.
-- Returns 'Right a' if it completes without errors, otherwise
-- 'Left SomeException' checkErr is useful for iteratees that may not
-- terminate, such as 'head' with an empty stream.
-- In particular, it enables them to be used with 'convStream'.
checkErr
  :: (Monad m, NullPoint s) =>
     Iteratee s m a
     -> Iteratee s m (Either SomeException a)
checkErr iter = Iteratee $ \onDone onCont ->
  let od            = onDone . Right
      oc k Nothing  = onCont (checkErr . k) Nothing
      oc _ (Just e) = onDone (Left e) (Chunk empty)
  in runIter iter od oc

-- ------------------------------------------------------------------------
-- Parser combinators

-- |The identity iterator.  Doesn't do anything.
identity :: (Monad m, NullPoint s) => Iteratee s m ()
identity = idone () (Chunk empty)

-- |Get the stream status of an iteratee.
isStreamFinished :: Monad m => Iteratee s m (Maybe SomeException)
isStreamFinished = liftI check
  where
    check s@(EOF e) = idone (Just $ fromMaybe (toException EofException) e) s
    check s         = idone Nothing s
{-# INLINE isStreamFinished #-}

-- |Skip the rest of the stream
skipToEof :: (Monad m) => Iteratee s m ()
skipToEof = icont check Nothing
  where
    check (Chunk _) = skipToEof
    check s         = idone () s


-- |Seek to a position in the stream
seek :: (Monad m, NullPoint s) => FileOffset -> Iteratee s m ()
seek o = throwRecoverableErr (toException $ SeekException o) (const identity)


-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

type Enumeratee sFrom sTo (m :: * -> *) a =
  Iteratee sTo m a
  -> Iteratee sFrom m (Iteratee sTo m a)

-- The following pattern appears often in Enumeratee code
{-# INLINE eneeCheckIfDone #-}

eneeCheckIfDone
 :: (Monad m, NullPoint elo) =>
    ((StreamG eli -> Iteratee eli m a) -> Iteratee elo m (Iteratee eli m a))
    -> Enumeratee elo eli m a
eneeCheckIfDone f inner = Iteratee $ \od oc -> 
  let on_done x s = od (idone x s) (Chunk empty)
      on_cont k Nothing  = runIter (f k) od oc
      on_cont _ (Just e) = runIter (throwErr e) od oc
  in runIter inner on_done on_cont


-- |Convert one stream into another, not necessarily in `lockstep'
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- Iteratee s el s'.
convStream
  :: (Monad m, Monoid s, Nullable s) =>
     Iteratee s m s'
     -> Enumeratee s s' m a
convStream fi = eneeCheckIfDone check
  where
    check k = isStreamFinished >>= maybe (step k) (idone (liftI k) . EOF . Just)
    step k = fi >>= convStream fi . k . Chunk

joinI
  :: (Monad m, Nullable s) =>
     Iteratee s m (Iteratee s' m a)
     -> Iteratee s m a
joinI = (>>=
  \inner -> Iteratee $ \od oc ->
  let on_done  x _        = od x (Chunk empty)
      on_cont  k Nothing  = runIter (k (EOF Nothing)) on_done on_cont'
      on_cont  _ (Just e) = runIter (throwErr e) od oc
      on_cont' _ e        = runIter (throwErr (fromMaybe excDivergent e)) od oc
  in runIter inner on_done on_cont)

joinIM :: (Monad m) => m (Iteratee s m a) -> Iteratee s m a
joinIM mIter = Iteratee $ \od oc -> mIter >>= \iter -> runIter iter od oc


-- ------------------------------------------------------------------------
-- Enumerators
-- |Each enumerator takes an iteratee and returns an iteratee
-- an Enumerator is an iteratee transformer.
-- The enumerator normally stops when the stream is terminated
-- or when the iteratee moves to the done state, whichever comes first.
-- When to stop is of course up to the enumerator...

type Enumerator s m a = Iteratee s m a -> m (Iteratee s m a)

-- We have two choices of composition: compose iteratees or compose
-- enumerators. The latter is useful when one iteratee
-- reads from the concatenation of two data sources.

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee in the Done state.  It is an error
-- if the iteratee does not terminate on EOF.
enumEof :: (Monad m) => Enumerator s m a
enumEof iter = runIter iter onDone onCont
  where
    onDone  x _str    = return $ idone x (EOF Nothing)
    onCont  k Nothing = runIter (k (EOF Nothing)) onDone onCont'
    onCont  k e       = return $ icont k e
    onCont' _ Nothing = return $ throwErr excDivergent
    onCont' k e       = return $ icont k e

-- |Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error.
enumErr :: (Exception e, Monad m) => e -> Enumerator s m a
enumErr e iter = runIter iter onDone onCont
  where
    onDone  x _       = return $ idone x (EOF . Just $ toException e)
    onCont  k Nothing = runIter (k (EOF (Just (toException e)))) onDone onCont'
    onCont  k e'      = return $ icont k e'
    onCont' _ Nothing = return $ throwErr excDivergent
    onCont' k e'      = return $ icont k e'


-- |The composition of two enumerators: essentially the functional composition
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >>> e2, e1 is executed first

(>>>)
  :: (Monad m) =>
     Enumerator s m a -> Enumerator s m a -> Enumerator s m a
(e1 >>> e2) i =  e1 i >>= e2

-- |The pure 1-chunk enumerator
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enumPure1Chunk :: (Monad m) => s -> Enumerator s m a
enumPure1Chunk str iter = runIter iter idoneM onCont
  where
    onCont k Nothing = return $ k $ Chunk str
    onCont k e       = return $ icont k e

-- |Create an enumerator from a callback function
enumFromCallback ::
  (Monad m, NullPoint s) =>
  m (Either SomeException (Bool, s)) -> Enumerator s m a
enumFromCallback = flip enumFromCallbackCatch
  (\NothingException -> return Nothing)

-- |Create an enumerator from a callback function with an exception handler.
-- The exception handler is called if an iteratee reports an exception.
enumFromCallbackCatch ::
  (IException e, Monad m, NullPoint s) =>
  m (Either SomeException (Bool, s))
  -> (e -> m (Maybe EnumException))
  -> Enumerator s m a
enumFromCallbackCatch c handler = loop
  where
    loop iter = runIter iter idoneM on_cont
    on_cont k Nothing = c >>= either (return . k . EOF . Just) (uncurry check)
      where
        check b = if b then loop . k . Chunk else return . k . Chunk
    on_cont k j@(Just e) = case fromException e of
      Just e' -> handler e' >>= maybe (loop . k $ Chunk empty)
                                 (return . icont k . Just) . fmap toException
      Nothing -> return (icont k j)

