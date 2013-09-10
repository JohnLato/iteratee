{-# LANGUAGE CPP #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
  ,enumFile
  ,enumFileRandom
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

import Control.Concurrent (yield)
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.Base
import Control.Exception.Lifted

import Foreign.Ptr
import Foreign.Storable

import System.IO (SeekMode(..))

import "unix-bytestring" System.Posix.IO.ByteString (tryFdSeek)

-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

makefdCallback ::
  (MonadBase IO m, ReadableChunk s el)
  => ByteCount
  -> Fd
  -> Callback st m s
makefdCallback bufsize fd st = do
  let fillFn p = fmap fromIntegral . fdReadBuf fd (castPtr p) . fromIntegral
  (s,numCopied) <- liftBase $ fillFromCallback (fromIntegral bufsize) fillFn
  case numCopied of
    0   -> liftBase yield >> return (Finished st s)
    _n' -> return (HasMore st s)
{-# INLINABLE makefdCallback #-}

-- |The enumerator of a POSIX File Descriptor.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumFd
  :: forall s el m a.(ReadableChunk s el, MonadBase IO m) =>
     Int
     -> Fd
     -> Enumerator s m a
enumFd bs fd =
  let bufsize = bs * (sizeOf (undefined :: el))
  in enumFromCallback (makefdCallback (fromIntegral bufsize) fd) ()
{-# INLINABLE enumFd #-}

-- |A variant of enumFd that catches exceptions raised by the @Iteratee@.
enumFdCatch
 :: forall e s el m a.(IException e, ReadableChunk s el, MonadBase IO m)
    => Int
    -> Fd
    -> (e -> m (Maybe EnumException))
    -> Enumerator s m a
enumFdCatch bs fd handler iter =
  let bufsize = bs * (sizeOf (undefined :: el))
      handlers = [IHandler handler, IHandler eofHandler]
  in enumFromCallbackCatches (makefdCallback (fromIntegral bufsize) fd) handlers () iter
{-# INLINABLE enumFdCatch #-}


-- |The enumerator of a POSIX File Descriptor: a variation of @enumFd@ that
-- supports RandomIO (seek requests).
enumFdRandom
 :: forall s el m a.(ReadableChunk s el, MonadBase IO m) =>
    Int
    -> Fd
    -> Enumerator s m a
enumFdRandom bs fd iter = enumFdCatch bs fd handler iter
  where
    handler (SeekException off) =
      liftM (either
             (const . Just $ enStrExc "Error seeking within file descriptor")
             (const Nothing))
            . liftBase . tryFdSeek fd AbsoluteSeek $ fromIntegral off
{-# INLINABLE enumFdRandom #-}

fileDriver
  :: (MonadBaseControl IO m, ReadableChunk s el) =>
     (Int -> Fd -> Enumerator s m a)
     -> Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriver enumf bufsize iter filepath = bracket
  (liftBase $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftBase . closeFd)
  (run <=< flip (enumf bufsize) iter)
{-# INLINABLE fileDriver #-}

-- |Process a file using the given @Iteratee@.
fileDriverFd
  :: (MonadBaseControl IO m, ReadableChunk s el) =>
     Int -- ^Buffer size (number of elements)
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverFd = fileDriver enumFd
{-# INLINABLE fileDriverFd #-}

-- |A version of fileDriverFd that supports seeking.
fileDriverRandomFd
  :: (MonadBaseControl IO m, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomFd = fileDriver enumFdRandom
{-# INLINABLE fileDriverRandomFd #-}

enumFile' :: (MonadBaseControl IO m, ReadableChunk s el) =>
  (Int -> Fd -> Enumerator s m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFile' enumf bufsize filepath iter = bracket
  (liftBase $ openFd filepath ReadOnly Nothing defaultFileFlags)
  (liftBase . closeFd)
  (flip (enumf bufsize) iter)
{-# INLINABLE enumFile' #-}

enumFile ::
  (MonadBaseControl IO m, ReadableChunk s el)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFile = enumFile' enumFd
{-# INLINABLE enumFile #-}

enumFileRandom ::
  (MonadBaseControl IO m, ReadableChunk s el)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFileRandom = enumFile' enumFdRandom
{-# INLINABLE enumFileRandom #-}


#endif
