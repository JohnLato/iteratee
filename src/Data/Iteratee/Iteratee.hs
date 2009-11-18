{-# LANGUAGE KindSignatures, FlexibleContexts #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Iteratee (
  -- * Types
  -- ** Error handling
  throwErr,
  checkErr,
  -- ** Basic Iteratees
  identity,
  getStatus,
  skipToEof,
  -- ** Nested iteratee combinators
  convStream,
  -- * Enumerators
  Enumerator,
  EnumeratorM,
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
import Control.Parallel
import Control.Applicative
import Data.Monoid
import Data.Maybe (fromMaybe)

-- ------------------------------------------------------------------------
-- useful functions
idone :: (IterateeC i, Monad m) => a -> StreamG s -> i s m a
idone a = toIter . done a

icont
  :: (IterateeC i, Monad m) =>
     (StreamG s -> i s m a)
     -> Maybe ErrMsg -> i s m a
icont k = toIter . cont k


-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Report and propagate an unrecoverable error.
--  Disregard the input first and then propagate the error.
throwErr :: (IterateeC i, Monad m) => ErrMsg -> i s m a
throwErr e = icont (const $ throwErr e) (Just e)

-- |Propagate a recoverable error.
-- Disregard input while in the error state.
throwRecoverableErr :: (IterateeC i, Monad m, Nullable s) => ErrMsg -> i s m ()
throwRecoverableErr e = icont (const identity) (Just e)


-- |Check if an iteratee produces an error.
-- Returns 'Right a' if it completes without errors, otherwise 'Left ErrMsg'
-- checkErr is useful for iteratees that may not terminate, such as 'head'
-- with an empty stream.  In particular, it enables them to be used with
-- 'convStream'.
checkErr
  :: (IterateeC i, Monad m, Monoid s) =>
     i s m a
     -> i s m (Either ErrMsg a)
checkErr iter = iter >>== check
  where
  check (Done a str)        = done (Right a) str
  check (Cont _ (Just err)) = done (Left err) mempty
  check (Cont k Nothing)    = cont (checkErr . k) Nothing


-- ------------------------------------------------------------------------
-- Parser combinators

-- |The identity iterator.  Doesn't do anything.
identity :: (IterateeC i, Monad m, Nullable s) => i s m ()
identity = return ()

-- |Get the stream status of an iteratee.
getStatus :: (IterateeC i, Monad m) => i s m StreamStatus
getStatus = icont check Nothing
  where
    check s@(EOF Nothing)  = idone EofNoError s
    check s@(EOF (Just e)) = idone (EofError e) s
    check s                = idone DataRemaining s

-- |Skip the rest of the stream
skipToEof :: (IterateeC i, Monad m) => i s m ()
skipToEof = icont check Nothing
  where
    check (Chunk _) = skipToEof
    check s         = idone () s


-- |Seek to a position in the stream
seek :: (IterateeC i, Monad m, Nullable s) => FileOffset -> i s m ()
seek n = icont (const identity) (Just (Seek n))


-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

type Enumeratee i sFrom sTo (m :: * -> *) a =
  i sTo m a
  -> i sFrom m (i sTo m a)

-- |Convert one stream into another, not necessarily in `lockstep'
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- Iteratee s el (Maybe (s' el')).  The Maybe type is in case of
-- errors, or end of stream.
convStream
  :: (IterateeC i, Monad m, Nullable s, Nullable s') =>
     i s m (Maybe s')
     -> Enumeratee i s s' m a
convStream fi iter = fi >>= check
  where
    check (Just xs) = (enumPure1Chunk xs iter) >>== docase
    check (Nothing) = return iter
    docase (Done a s)          = done (return a) mempty
    docase (Cont k Nothing)    = cont next Nothing
      where
        next = flip enumStream (convStream fi $ icont k Nothing)
    docase (Cont k j@(Just e)) = cont (const $ throwErr e) j

{-# INLINE convStream #-}

-- ------------------------------------------------------------------------
-- Enumerators
-- |Each enumerator takes an iteratee and returns an iteratee
-- an Enumerator is an iteratee transformer.
-- The enumerator normally stops when the stream is terminated
-- or when the iteratee moves to the done state, whichever comes first.
-- When to stop is of course up to the enumerator...

type Enumerator i s (m :: * -> *) a = i s m a -> i s m a

-- We have two choices of composition: compose iteratees or compose
-- enumerators. The latter is useful when one iteratee
-- reads from the concatenation of two data sources.

type EnumeratorM i s m a = i s m a -> m (i s m a)

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee in the Done state.  It is an error
-- if the iteratee does not terminate on EOF.
enumEof :: (IterateeC i, Monad m) => Enumerator i s m a
enumEof iter = iter >>== check False
  where
    check _ (Done x s)           = done x (EOF Nothing)
    check _ (Cont k e@(Just _))  = cont k e
    check False (Cont k Nothing) = runIterC $ k (EOF Nothing) >>== check True
    check True  _ = runIterC . throwErr . Err $ "Divergent Iteratee"

-- |Another primitive enumerator: report an error
enumErr
  :: (IterateeC i, Monad m, Nullable s) =>
     String
     -> Enumerator i s m a
enumErr e iter = iter >>== check False
  where
    check _ (Done x _) = done x (EOF . Just . Err $ e)
    check _ (Cont k e@(Just _)) = cont k e
    check False (Cont k Nothing) =
      runIterC $ k (EOF . Just . Err $ e) >>== check True
    check True _                 =
      runIterC . throwErr . Err $ "Divergent Iteratee"


-- |The composition of two enumerators: essentially the functional composition
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >. e2, e1 is executed first

(>.)
  :: (IterateeC i, Monad m) =>
     EnumeratorM i s m a -> EnumeratorM i s m a -> EnumeratorM i s m a
(>.) = (<=<)

-- |The pure 1-chunk enumerator
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enumPure1Chunk :: (IterateeC i, Monad m) => s -> Enumerator i s m a
enumPure1Chunk str iter = iter >>== check
  where
    check (Cont k Nothing) = runIterC $ k (Chunk str)
    check (Cont k e)       = cont k e
    check (Done a _)       = done a (Chunk str)

enumStream :: (IterateeC i, Monad m) => StreamG s -> Enumerator i s m a
enumStream e@(EOF _) = enumEof
enumStream (Chunk xs) = enumPure1Chunk xs

