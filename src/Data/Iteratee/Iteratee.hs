{-# LANGUAGE KindSignatures, FlexibleContexts #-}

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
  -- ** Nested iteratee combinators
  convStream,
  joinI,
  -- * Enumerators
  Enumerator,
  Enumeratee,
  -- ** Basic enumerators
  enumEof,
  enumErr,
  enumPure1Chunk,
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
import qualified Prelude as P

import Data.Iteratee.IO.Base
import Data.Iteratee.Base
import Control.Monad
import Control.Exception
import Data.Monoid

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Report and propagate an unrecoverable error.
--  Disregard the input first and then propagate the error.
throwErr :: (Monad m) => SomeException -> Iteratee s m a
throwErr e = icont (const (throwErr e)) (Just $ Err e)

-- |Propagate a recoverable error.
throwRecoverableErr
  :: (Monad m, Nullable s) =>
     SomeException
     -> (StreamG s -> Iteratee s m a)
     -> Iteratee s m a
throwRecoverableErr e i = icont i (Just $ Err e)

-- |Generate a control message.
controlMsg
  :: (Monad m) =>
     CtrlMsg
     -> (StreamG s -> Iteratee s m a)
     -> Iteratee s m a
controlMsg c i = icont i (Just c)


-- |Check if an iteratee produces an error.
-- Returns 'Right a' if it completes without errors, otherwise 'Left CtrlMsg'
-- checkErr is useful for iteratees that may not terminate, such as 'head'
-- with an empty stream.  In particular, it enables them to be used with
-- 'convStream'.
checkErr
  :: (Monad m, Nullable s) =>
     Iteratee s m a
     -> Iteratee s m (Either CtrlMsg a)
checkErr iter = Iteratee $ \onDone onCont ->
  let od a str     = onDone (Right a) str
      oc k Nothing = onCont (checkErr . k) Nothing
      oc _ (Just e) = onDone (Left e) (Chunk empty)
  in runIter iter od oc

-- ------------------------------------------------------------------------
-- Parser combinators

-- |The identity iterator.  Doesn't do anything.
identity :: (Monad m, Nullable s) => Iteratee s m ()
identity = return ()

-- |Get the stream status of an iteratee.
isStreamFinished :: Monad m => Iteratee s m (Maybe SomeException)
isStreamFinished = liftI check
  where
    check s@(EOF e) = idone (Just $ maybe excEof id e) s
    check s         = idone Nothing s

-- |Skip the rest of the stream
skipToEof :: (Monad m) => Iteratee s m ()
skipToEof = icont check Nothing
  where
    check (Chunk _) = skipToEof
    check s         = idone () s


-- |Seek to a position in the stream
seek :: (Monad m, Nullable s) => FileOffset -> Iteratee s m ()
seek = icont (const identity) . Just . Seek


-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

type Enumeratee sFrom sTo (m :: * -> *) a =
  Iteratee sTo m a
  -> Iteratee sFrom m (Iteratee sTo m a)

-- The following pattern appears often in Enumeratee code
{-# INLINE eneeCheckIfDone #-}

eneeCheckIfDone
 :: (Monad m, Monoid elo) =>
    ((StreamG eli -> Iteratee eli m a) -> Iteratee elo m (Iteratee eli m a))
    -> Enumeratee elo eli m a
eneeCheckIfDone f inner = Iteratee $ \od oc -> 
  let on_done x s = od (idone x s) mempty
      on_cont k Nothing  = runIter (f k) od oc
      on_cont _ (Just (Err e)) = runIter (throwErr e) od oc
  in runIter inner on_done on_cont


-- |Convert one stream into another, not necessarily in `lockstep'
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- Iteratee s el s'.
convStream
  :: (Monad m, Nullable s, Monoid s, Nullable s') =>
     Iteratee s m s'
     -> Enumeratee s s' m a
convStream fi = eneeCheckIfDone check
  where
    check k = isStreamFinished >>= maybe (step k) (idone (liftI k) . EOF . Just)
    step  k = fi >>= \v -> convStream fi $ k (Chunk v)

joinI
  :: (Monad m, Nullable s) =>
     Iteratee s m (Iteratee s' m a)
     -> Iteratee s m a
joinI outer = outer >>=
  \inner -> Iteratee $ \od oc ->
  let on_done  x _        = od x (Chunk empty)
      on_cont  k Nothing  = runIter (k (EOF Nothing)) on_done on_cont'
      on_cont  _ (Just (Err e)) = runIter (throwErr e) od oc
      on_cont' _ e        = runIter (throwErr (maybe excDivergent fromErr e)) od oc
  in runIter inner on_done on_cont

fromErr :: CtrlMsg -> SomeException
fromErr (Err e) = e

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
    onDone  x str     = return $ idone x (EOF Nothing)
    onCont  k Nothing = runIter (k (EOF Nothing)) onDone onCont'
    onCont  k e       = return $ icont k e
    onCont' k Nothing = return $ throwErr excDivergent
    onCont' k e       = return $ icont k e

-- |Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error.
enumErr :: (Monad m, Nullable s) => SomeException -> Enumerator s m a
enumErr e iter = runIter iter onDone onCont
  where
    onDone  x _       = return $ idone x (EOF $ Just e)
    onCont  k Nothing = runIter (k (EOF (Just e))) onDone onCont'
    onCont  k e       = return $ icont k e
    onCont' k Nothing = return $ throwErr excDivergent
    onCont' k e       = return $ icont k e


-- |The composition of two enumerators: essentially the functional composition
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >. e2, e1 is executed first

(>>>)
  :: (Monad m) =>
     Enumerator s m a -> Enumerator s m a -> Enumerator s m a
e1 >>> e2 =  \i -> e1 i >>= e2

-- |The pure 1-chunk enumerator
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enumPure1Chunk :: (Monad m) => s -> Enumerator s m a
enumPure1Chunk str iter = runIter iter idoneM onCont
  where
    onCont k Nothing = return $ k $ Chunk str
    onCont k e       = return $ icont k e

enumStream :: (Monad m) => StreamG s -> Enumerator s m a
enumStream (EOF _)    = enumEof
enumStream (Chunk xs) = enumPure1Chunk xs

