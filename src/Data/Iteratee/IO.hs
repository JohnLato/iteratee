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
  fileDriver,
  fileDriverRandom,
)

where

import Data.Iteratee.Base.StreamChunk (ReadableChunk (..))
import Data.Iteratee.Base
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Handle

#if defined(USE_POSIX)
import Data.Iteratee.IO.Fd
#endif

-- If Posix is available, use the fileDriverRandomFd as fileDriverRandom.  Otherwise, use a handle-based variant.
#if defined(USE_POSIX)

-- |Process a file using the given IterateeGM.  This function wraps
-- enumFd as a convenience.
fileDriver :: ReadableChunk s el => IterateeGM s el IO a ->
              FilePath ->
              IO (Either (String, a) a)
fileDriver = fileDriverFd

-- |Process a file using the given IterateeGM.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandom :: ReadableChunk s el => IterateeGM s el IO a ->
                    FilePath ->
                    IO (Either (String, a) a)
fileDriverRandom = fileDriverRandomFd

#else

-- -----------------------------------------------
-- Handle-based operations for compatibility.

-- |Process a file using the given IterateeGM.  This function wraps
-- enumHandle as a convenience.
fileDriver :: ReadableChunk s el => IterateeGM s el IO a ->
              FilePath ->
              IO (Either (String, a) a)
fileDriver = fileDriverHandle

-- |Process a file using the given IterateeGM.  This function wraps
-- enumFdHandle as a convenience.
fileDriverRandom :: ReadableChunk s el => IterateeGM s el IO a ->
                    FilePath ->
                    IO (Either (String, a) a)
fileDriverRandom = fileDriverRandomHandle

#endif
