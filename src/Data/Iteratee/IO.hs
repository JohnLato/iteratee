{-# LANGUAGE CPP #-}

-- |Random and Binary IO with generic Iteratees.

module Data.Iteratee.IO(
  -- * Data
  defaultBufSize,
  -- * File enumerators
  -- ** Handle-based enumerators
  H.enumHandle,
  H.enumHandleRandom,
  enumFile,
  enumFileRandom,
#if defined(USE_POSIX)
  -- ** FileDescriptor based enumerators
  FD.enumFd,
  FD.enumFdRandom,
#endif
  -- * Iteratee drivers
  --   These are FileDescriptor-based on POSIX systems, otherwise they are
  --   Handle-based.  The Handle-based drivers are accessible on POSIX systems
  --   at Data.Iteratee.IO.Handle
  fileDriver,
  fileDriverVBuf,
  fileDriverRandom,
  fileDriverRandomVBuf,
)

where

import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()
import qualified Data.Iteratee.IO.Handle as H

#if defined(USE_POSIX)
import qualified Data.Iteratee.IO.Fd as FD
#endif

import Control.Monad.CatchIO

-- | The default buffer size.
defaultBufSize :: Int
defaultBufSize = 1024

-- If Posix is available, use the fileDriverRandomFd as fileDriverRandom.  Otherwise, use a handle-based variant.
#if defined(USE_POSIX)

enumFile
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Int
     -> FilePath
     -> Enumerator s m a
enumFile = FD.enumFile

enumFileRandom
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Int
     -> FilePath
     -> Enumerator s m a
enumFileRandom = FD.enumFileRandom

-- |Process a file using the given Iteratee.  This function wraps
-- enumFd as a convenience.
fileDriver
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriver = FD.fileDriverFd defaultBufSize

-- |A version of fileDriver with a user-specified buffer size (in elements).
fileDriverVBuf
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverVBuf = FD.fileDriverFd

-- |Process a file using the given Iteratee.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandom
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandom = FD.fileDriverRandomFd defaultBufSize

fileDriverRandomVBuf
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomVBuf = FD.fileDriverRandomFd

#else

-- -----------------------------------------------
-- Handle-based operations for compatibility.

-- |Process a file using the given Iteratee.  This function wraps
-- @enumHandle@ as a convenience.
fileDriver ::
 (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
  Iteratee s m a
  -> FilePath
  -> m a
fileDriver = H.fileDriverHandle defaultBufSize

-- |A version of fileDriver with a user-specified buffer size (in elements).
fileDriverVBuf ::
 (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
  Int
  -> Iteratee s m a
  -> FilePath
  -> m a
fileDriverVBuf = H.fileDriverHandle

-- |Process a file using the given Iteratee.  This function wraps
-- @enumRandomHandle@ as a convenience.
fileDriverRandom
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandom = H.fileDriverRandomHandle defaultBufSize

fileDriverRandomVBuf
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomVBuf = H.fileDriverRandomHandle

enumFile
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Int
     -> FilePath
     -> Enumerator s m a
enumFile = H.enumFile

enumFileRandom
  :: (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
     Int
     -> FilePath
     -> Enumerator s m a
enumFileRandom = H.enumFileRandom

#endif
