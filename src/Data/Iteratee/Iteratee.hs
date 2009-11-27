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
  getStatus,
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
  (>.),
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
import Data.Monoid

-- ------------------------------------------------------------------------
-- useful functions
idone :: (Monad m) => a -> StreamG s -> Iteratee s m a
idone a = Iteratee . return . Done a

icont :: (Monad m) => (StreamG s -> Iteratee s m a) -> Maybe ErrMsg -> Iteratee s m a
icont k = Iteratee . return . Cont k


-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Report and propagate an unrecoverable error.
--  Disregard the input first and then propagate the error.
throwErr :: (Monad m) => ErrMsg -> Iteratee s m a
throwErr e = icont (const $ throwErr e) (Just e)

-- |Propagate a recoverable error.
-- Disregard input while in the error state.
throwRecoverableErr :: (Monad m, Nullable s) => ErrMsg -> Iteratee s m ()
throwRecoverableErr = icont (const identity) . Just


-- |Check if an iteratee produces an error.
-- Returns 'Right a' if it completes without errors, otherwise 'Left ErrMsg'
-- checkErr is useful for iteratees that may not terminate, such as 'head'
-- with an empty stream.  In particular, it enables them to be used with
-- 'convStream'.
checkErr
  :: (Monad m, Monoid s) =>
     Iteratee s m a
     -> Iteratee s m (Either ErrMsg a)
checkErr iter = iter >>== check
  where
  check (Done a str)        = idone (Right a) str
  check (Cont _ (Just err)) = idone (Left err) mempty
  check (Cont k Nothing)    = icont (checkErr . k) Nothing


-- ------------------------------------------------------------------------
-- Parser combinators

-- |The identity iterator.  Doesn't do anything.
identity :: (Monad m, Nullable s) => Iteratee s m ()
identity = return ()

-- |Get the stream status of an iteratee.
getStatus :: (Monad m) => Iteratee s m StreamStatus
getStatus = icont check Nothing
  where
    check s@(EOF Nothing)  = idone EofNoError s
    check s@(EOF (Just e)) = idone (EofError e) s
    check s                = idone DataRemaining s

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

-- |Convert one stream into another, not necessarily in `lockstep'
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- Iteratee s el (Maybe s').  The Maybe type is in case of
-- errors, or end of stream.
convStream
  :: (Monad m, Nullable s, Monoid s, Nullable s') =>
     Iteratee s m (Maybe s')
     -> Enumeratee s s' m a
convStream fi iter = fi >>= check
  where
    check (Just xs) = enumPure1Chunk xs iter >>== docase
    check (Nothing) = return iter
    docase (Done a _)          = idone (return a) mempty
    docase (Cont k Nothing)    = icont next Nothing
      where
        next = flip enumStream (convStream fi $ icont k Nothing)
    docase (Cont _ j@(Just e)) = icont (const $ throwErr e) j

{-# INLINE convStream #-}

joinI
  :: (Monad m, Nullable s) =>
     Iteratee s m (Iteratee s' m a)
     -> Iteratee s m a
joinI m = m >>= (\i -> enumEof i >>== check)
  where
    check (Done a _)        = return a
    check (Cont _ Nothing)  = throwErr $ Err "joinI: divergent iteratee"
    check (Cont _ (Just e)) = throwErr e

-- ------------------------------------------------------------------------
-- Enumerators
-- |Each enumerator takes an iteratee and returns an iteratee
-- an Enumerator is an iteratee transformer.
-- The enumerator normally stops when the stream is terminated
-- or when the iteratee moves to the done state, whichever comes first.
-- When to stop is of course up to the enumerator...

type Enumerator s (m :: * -> *) a = Iteratee s m a -> Iteratee s m a

-- We have two choices of composition: compose iteratees or compose
-- enumerators. The latter is useful when one iteratee
-- reads from the concatenation of two data sources.

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee in the Done state.  It is an error
-- if the iteratee does not terminate on EOF.
enumEof :: (Monad m) => Enumerator s m a
enumEof iter = iter >>== check False
  where
    check _ (Done x _)           = idone x (EOF Nothing)
    check _ (Cont k e@(Just _))  = icont k e
    check False (Cont k Nothing) = k (EOF Nothing) >>== check True
    check True  _ = throwErr . Err $ "Divergent Iteratee"

-- |Another primitive enumerator: report an error
enumErr
  :: (Monad m, Nullable s) =>
     String
     -> Enumerator s m a
enumErr e iter = iter >>== check False
  where
    check _ (Done x _)           = idone x (EOF . Just . Err $ e)
    check _ (Cont k j@(Just _))  = icont k j
    check False (Cont k Nothing) = k (EOF . Just . Err $ e) >>== check True
    check True _                 = throwErr . Err $ "Divergent Iteratee"


-- |The composition of two enumerators: essentially the functional composition
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >. e2, e1 is executed first

(>.)
  :: (Monad m) =>
     Enumerator s m a -> Enumerator s m a -> Enumerator s m a
(>.) = flip (.)

-- |The pure 1-chunk enumerator
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enumPure1Chunk :: (Monad m) => s -> Enumerator s m a
enumPure1Chunk str iter = iter >>== check
  where
    check (Cont k Nothing) = k (Chunk str)
    check (Cont k e)       = icont k e
    check (Done a _)       = idone a (Chunk str)

enumStream :: (Monad m) => StreamG s -> Enumerator s m a
enumStream (EOF _)    = enumEof
enumStream (Chunk xs) = enumPure1Chunk xs

