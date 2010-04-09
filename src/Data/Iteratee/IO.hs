{-# LANGUAGE CPP #-}

-- |Random and Binary IO with generic Iteratees.

module Data.Iteratee.IO(
  -- * File enumerators
  -- ** Handle-based enumerators
  enumHandle,
  enumHandleRandom,
#if defined(USE_POSIX)
  -- ** FileDescriptor based enumerators
  enumFd,
  enumFdRandom,
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
import Data.Iteratee.IO.Handle

#if defined(USE_POSIX)
import Data.Iteratee.IO.Fd
#endif

import Control.Monad.CatchIO

defaultBufSize :: Int
defaultBufSize = 4096

-- If Posix is available, use the fileDriverRandomFd as fileDriverRandom.  Otherwise, use a handle-based variant.
#if defined(USE_POSIX)

-- |Process a file using the given Iteratee.  This function wraps
-- enumFd as a convenience.
fileDriver
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriver = fileDriverFd defaultBufSize

-- |A version of fileDriver with a user-specified buffer size (in elements).
fileDriverVBuf
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverVBuf = fileDriverFd

-- |Process a file using the given Iteratee.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandom
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandom = fileDriverRandomFd defaultBufSize

fileDriverRandomVBuf
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomVBuf = fileDriverRandomFd

#else

-- -----------------------------------------------
-- Handle-based operations for compatibility.

-- |Process a file using the given Iteratee.  This function wraps
-- @enumHandle@ as a convenience.
fileDriver
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriver = fileDriverHandle defaultBufSize

fileDriverVBuf
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriver = fileDriverHandle

-- |Process a file using the given Iteratee.  This function wraps
-- @enumRandomHandle@ as a convenience.
fileDriverRandom
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandom = fileDriverRandomHandle defaultBufSize

fileDriverRandomVBuf
  :: (MonadCatchIO m, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomVBuf = fileDriverRandomHandle

#endif
