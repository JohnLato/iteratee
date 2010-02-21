{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees, using File Descriptors for IO.
-- when available, these are the preferred functions for performing IO as they
-- run in constant space and function properly with sockets, pipes, etc.

module Data.Iteratee.IO.Fd(
#if defined(USE_POSIX)
  -- * File enumerators
  -- ** FileDescriptor based enumerators for monadic iteratees
  enumFd
  ,enumFdRandom
  -- * Iteratee drivers
  ,fileDriverFd
  ,fileDriverRandomFd
#endif
)

where

#if defined(USE_POSIX)
import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base

import Data.Monoid
import Control.Exception
import Control.Monad
import Control.Monad.Trans

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.IO (SeekMode(..))

import System.Posix hiding (FileOffset)
import GHC.Conc

-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

makefdCallback ::
  ReadableChunk s el =>
  Ptr el
  -> ByteCount
  -> Fd
  -> IO (Either SomeException (Bool, s))
makefdCallback p bufsize fd = do
  GHC.Conc.threadWaitRead fd
  n <- myfdRead fd (castPtr p) bufsize
  case n of
    Left _   -> return $ Left undefined
    Right 0  -> return $ Right (False, mempty)
    Right n' -> liftM (\s -> Right (True, s)) $ readFromPtr p (fromIntegral n')

-- |The enumerator of a POSIX File Descriptor.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumFd
  :: forall s el m a.(Nullable s, ReadableChunk s el, MonadIO m) =>
     Fd
     -> Enumerator s m a
enumFd fd iter = do
  let bufsize = fromIntegral $ 4096 - mod 4096 (sizeOf (undefined :: el))
  p <- liftIO $ mallocBytes bufsize
  enumFromCallback (liftIO $ makefdCallback p (fromIntegral bufsize) fd) iter


-- |The enumerator of a POSIX File Descriptor: a variation of enumFd that
-- supports RandomIO (seek requests)
enumFdRandom
  :: forall s el m a.(Nullable s, ReadableChunk s el, MonadIO m) =>
     Fd
     -> Enumerator s m a
enumFdRandom fd iter = do
  let bufsize = fromIntegral $ 4096 - mod 4096 (sizeOf (undefined :: el))
  let handler (SeekException off) =
       liftM (either
              (const . Just $ enStrExc "Error seeking within file descriptor")
              (const Nothing))
             . liftIO . myfdSeek fd AbsoluteSeek $ fromIntegral off
  p <- liftIO $ mallocBytes bufsize
  enumFromCallbackCatch (liftIO $ makefdCallback p (fromIntegral bufsize) fd)
                        handler iter


-- |Process a file using the given Iteratee.  This function wraps
-- enumFd as a convenience.
fileDriverFd
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverFd iter filepath = do
  fd <- liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags
  result <- run =<< enumFd fd iter
  liftIO $ closeFd fd
  return result

-- |Process a file using the given Iteratee.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandomFd
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomFd iter filepath = do
  fd <- liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags
  result <- run =<< enumFdRandom fd iter
  liftIO $ closeFd fd
  return result

#endif
