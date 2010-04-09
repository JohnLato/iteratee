{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees, using File Descriptors for IO.
-- when available, these are the preferred functions for performing IO as they
-- run in constant space and function properly with sockets, pipes, etc.

module Data.Iteratee.IO.Fd(
#if defined(USE_POSIX)
  -- * File enumerators
  -- ** FileDescriptor based enumerators for monadic iteratees
  enumFd
  ,enumFdCatch
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

import Control.Exception
import Control.Monad
import Control.Monad.CatchIO as CIO
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
  (MonadIO m, NullPoint s, ReadableChunk s el) =>
  Ptr el
  -> ByteCount
  -> Fd
  -> m (Either SomeException (Bool, s))
makefdCallback p bufsize fd = do
  liftIO $ GHC.Conc.threadWaitRead fd
  n <- liftIO $ myfdRead fd (castPtr p) bufsize
  case n of
    Left _   -> return $ Left undefined
    Right 0  -> return $ Right (False, empty)
    Right n' -> liftM (\s -> Right (True, s)) $ readFromPtr p (fromIntegral n')

-- |The enumerator of a POSIX File Descriptor.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumFd
  :: forall s el m a.(NullPoint s, ReadableChunk s el, MonadIO m) =>
     Int
     -> Fd
     -> Enumerator s m a
enumFd bs fd iter = do
  let bufsize = bs * (sizeOf (undefined :: el))
  p <- liftIO $ mallocBytes bufsize
  enumFromCallback (makefdCallback p (fromIntegral bufsize) fd) iter

-- |A variant of enumFd that catches exceptions raised by the @Iteratee@.
enumFdCatch
 :: forall e s el m a.(IException e, NullPoint s, ReadableChunk s el, MonadIO m)
    => Int
    -> Fd
    -> (e -> m (Maybe EnumException))
    -> Enumerator s m a
enumFdCatch bs fd handler iter = do
  let bufsize = bs * (sizeOf (undefined :: el))
  p <- liftIO $ mallocBytes bufsize
  enumFromCallbackCatch (makefdCallback p (fromIntegral bufsize) fd)
    handler iter


-- |The enumerator of a POSIX File Descriptor: a variation of @enumFd@ that
-- supports RandomIO (seek requests).
enumFdRandom
 :: forall s el m a.(NullPoint s, ReadableChunk s el, MonadIO m) =>
    Int
    -> Fd
    -> Enumerator s m a
enumFdRandom bs fd iter = enumFdCatch bs fd handler iter
  where
    handler (SeekException off) =
      liftM (either
             (const . Just $ enStrExc "Error seeking within file descriptor")
             (const Nothing))
            . liftIO . myfdSeek fd AbsoluteSeek $ fromIntegral off

fileDriver
  :: (MonadCatchIO m, ReadableChunk s el) =>
     (Int -> Fd -> Enumerator s m a)
     -> Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriver enumf bufsize iter filepath = CIO.bracket
  (liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftIO . closeFd)
  (run <=< flip (enumf bufsize) iter)

-- |Process a file using the given @Iteratee@.
fileDriverFd
  :: (NullPoint s, MonadCatchIO m, ReadableChunk s el) =>
     Int -- ^Buffer size (number of elements)
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverFd = fileDriver enumFd

-- |A version of fileDriverFd that supports seeking.
fileDriverRandomFd
  :: (NullPoint s, MonadCatchIO m, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomFd = fileDriver enumFdRandom

#endif
