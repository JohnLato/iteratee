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
  --   Handle-based.
  fileDriver,
  fileDriverRandom,
)

where

import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Handle

#if defined(USE_POSIX)
import Data.Iteratee.IO.Fd
#endif

import Control.Monad.Trans

-- If Posix is available, use the fileDriverRandomFd as fileDriverRandom.  Otherwise, use a handle-based variant.
#if defined(USE_POSIX)

-- |Process a file using the given Iteratee.  This function wraps
-- enumFd as a convenience.
fileDriver
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriver = fileDriverFd

-- |Process a file using the given Iteratee.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandom
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandom = fileDriverRandomFd

#else

-- -----------------------------------------------
-- Handle-based operations for compatibility.

-- |Process a file using the given Iteratee.  This function wraps
-- enumHandle as a convenience.
fileDriver
  :: (MonadIO m, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriver = fileDriverHandle

-- |Process a file using the given Iteratee.  This function wraps
-- enumFdHandle as a convenience.
fileDriverRandom
  :: (MonadIO m, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandom = fileDriverRandomHandle

#endif
