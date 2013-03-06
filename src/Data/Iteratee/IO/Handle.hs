{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- |Random and Binary IO with generic Iteratees.  These functions use Handles
-- for IO operations, and are provided for compatibility.  When available,
-- the File Descriptor based functions are preferred as these wastefully
-- allocate memory rather than running in constant space.

module Data.Iteratee.IO.Handle(
  -- * File enumerators
  enumHandle
  ,enumHandleCatch
  ,enumHandleRandom
  ,enumFile
  ,enumFileRandom
  -- * Iteratee drivers
  ,fileDriverHandle
  ,fileDriverRandomHandle
)

where

import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()

import Control.Monad
import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Exception.Lifted

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

makeHandleCallback :: forall st m s el.
  (MonadBaseControl IO m, ReadableChunk s el) =>
  Int
  -> Handle
  -> Callback st m s
makeHandleCallback bsize h st = do
  let fillFn p = fmap fromIntegral . hGetBuf h (castPtr p) . fromIntegral
  (s,numCopied) <- liftBase $ fillFromCallback (fromIntegral bsize) fillFn
  case numCopied of
    0   -> return ((Finished, st), s)
    n'  -> return ((HasMore, st), s)

-- |The (monadic) enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
-- Data is read into a buffer of the specified size.
enumHandle
 :: forall s el m a.
    (ReadableChunk s el, MonadBaseControl IO m)
  => Int -- ^Buffer size (number of elements per read)
  -> Handle
  -> Enumerator s m a
enumHandle bs h i =
  let bufsize = bs * sizeOf (undefined :: el)
  in enumFromCallback (makeHandleCallback bufsize h) () i

-- |An enumerator of a file handle that catches exceptions raised by
-- the Iteratee.
enumHandleCatch
 :: forall e s el m a.(IException e,
                    ReadableChunk s el,
                    MonadBaseControl IO m)
  => Int -- ^Buffer size (number of elements per read)
  -> Handle
  -> (e -> m (Maybe EnumException))
  -> Enumerator s m a
enumHandleCatch bs h handler i =
  let bufsize = bs * sizeOf (undefined :: el)
  in enumFromCallbackCatch (makeHandleCallback bufsize h) handler () i


-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests).
-- Data is read into a buffer of the specified size.
enumHandleRandom
 :: forall s el m a. (ReadableChunk s el
                     ,MonadBaseControl IO m)
  => Int -- ^ Buffer size (number of elements per read)
  -> Handle
  -> Enumerator s m a
enumHandleRandom bs h i = enumHandleCatch bs h handler i
  where
    handler (SeekException off) =
       liftM (either
              (Just . EnumException :: IOException -> Maybe EnumException)
              (const Nothing))
             . liftBase . try $ hSeek h AbsoluteSeek $ fromIntegral off

-- ----------------------------------------------
-- File Driver wrapper functions.

enumFile'
  :: (MonadBaseControl IO m, ReadableChunk s el)
  => (Int -> Handle -> Enumerator s m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFile' enumf bufsize filepath iter = bracket
  (liftBase $ openBinaryFile filepath ReadMode)
  (liftBase . hClose)
  (flip (enumf bufsize) iter)

enumFile
  :: (MonadBaseControl IO m, ReadableChunk s el)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFile = enumFile' enumHandle

enumFileRandom
  :: (MonadBaseControl IO m, ReadableChunk s el)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFileRandom = enumFile' enumHandleRandom

-- |Process a file using the given @Iteratee@.  This function wraps
-- @enumHandle@ as a convenience.
fileDriverHandle
  :: (MonadBaseControl IO m, ReadableChunk s el)
  => Int                      -- ^Buffer size (number of elements)
  -> Iteratee s m a
  -> FilePath
  -> m a
fileDriverHandle bufsize iter filepath =
  enumFile bufsize filepath iter >>= run

-- |A version of @fileDriverHandle@ that supports seeking.
fileDriverRandomHandle
  :: (MonadBaseControl IO m, ReadableChunk s el)
  => Int                      -- ^ Buffer size (number of elements)
  -> Iteratee s m a
  -> FilePath
  -> m a
fileDriverRandomHandle bufsize iter filepath =
  enumFileRandom bufsize filepath iter >>= run

