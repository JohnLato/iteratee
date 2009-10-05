{-# LANGUAGE CPP, ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees, using File Descriptors for IO.
-- when available, these are the preferred functions for performing IO as they
-- run in constant space and function properly with sockets, pipes, etc.

module Data.Iteratee.IO.Fd(
#if defined(USE_POSIX)
  -- * File enumerators
  -- ** FileDescriptor based enumerators
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
import Data.Iteratee.IterateeT
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base

import Control.Monad
import Control.Monad.Trans

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import System.IO (SeekMode(..))

import System.Posix hiding (FileOffset)
import GHC.Conc

-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a POSIX File Descriptor.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumFd :: forall s el m a.(ReadableChunk s el, MonadIO m) =>
  Fd ->
  EnumeratorGM s el m a
enumFd fd iter' =
  liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop iter'
  where
    buffer_size = fromIntegral $ 4096 - mod 4096 (sizeOf (undefined :: el))
    loop iter fp = do
      s <- liftIO . withForeignPtr fp $ \p -> do
        liftIO $ GHC.Conc.threadWaitRead fd
        n <- myfdRead fd (castPtr p) buffer_size
        case n of
          Left _errno -> return $ Left "IO error"
          Right 0 -> return $ Right Nothing
          Right n' -> liftM (Right . Just) $ readFromPtr p (fromIntegral n')
      checkres fp iter s
    checkres fp iter = either (flip enumErr iter)
                              (maybe (return iter)
                                     (check fp <=< runIter iter . Chunk))
    check _p (Done x _) = return . return $ x
    check p  (Cont i Nothing) = loop i p
    check _p (Cont _ (Just e)) = return $ throwErr e

-- |The enumerator of a POSIX File Descriptor: a variation of enumFd that
-- supports RandomIO (seek requests)
enumFdRandom :: forall s el m a.(ReadableChunk s el, MonadIO m) =>
  Fd ->
  EnumeratorGM s el m a
enumFdRandom fd iter' =
 liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop (0,0) iter'
 where
  -- this can be usefully varied.  Values between 512 and 4096 seem
  -- to provide the best performance for most cases.
  buffer_size = fromIntegral $ 4096 - mod 4096 (sizeOf (undefined :: el))
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'fp'
  loop :: (FileOffset,Int) ->
          IterateeT s el m a ->
          ForeignPtr el ->
          m (IterateeT s el m a)
    -- Thanks to John Lato for the strictness annotation
    -- Otherwise, the `off + fromIntegral len' below accumulates thunks
  loop (off,len) _iter _fp | off `seq` len `seq` False = undefined
  loop (off,len) iter fp = do
    s <- liftIO . withForeignPtr fp $ \p -> do
      liftIO $ GHC.Conc.threadWaitRead fd
      n <- myfdRead fd (castPtr p) buffer_size
      case n of
        Left _errno -> return $ Left "IO error"
        Right 0 -> return $ Right Nothing
        Right n' -> liftM
          (Right . Just . (,) (off + fromIntegral len, fromIntegral n'))
          (readFromPtr p (fromIntegral n'))
    checkres fp iter s
  seekTo pos@(off, len) off' iter fp
    | off <= off' && off' < off + fromIntegral len =   -- Seek within buffer
    do
    let local_off = fromIntegral $ off' - off
    s <- liftIO $ withForeignPtr fp $ \p ->
                    readFromPtr (p `plusPtr` local_off) (len - local_off)
    igv <- runIter iter (Chunk s)
    check pos fp igv
  seekTo _pos off iter fp = do                         -- Seek outside buffer
    off' <- liftIO $ myfdSeek fd AbsoluteSeek (fromIntegral off)
    case off' of
      Left _errno -> enumErr "IO error" iter
      Right off'' -> loop (off'',0) iter fp
  checkres fp iter = either
                       (flip enumErr iter)
                       (maybe (return iter) (uncurry $ runS fp iter))
  runS fp iter o s = runIter iter (Chunk s) >>= check o fp
  check _ _fp (Done x _)                 = return . return $ x
  check o fp  (Cont i Nothing)           = loop o i fp
  check o fp  (Cont i (Just (Seek off))) = seekTo o off i fp
  check _ _fp (Cont _ (Just e))          = return $ throwErr e

-- |Process a file using the given IterateeT.  This function wraps
-- enumFd as a convenience.
fileDriverFd :: (MonadIO m, ReadableChunk s el) =>
  IterateeT s el m a ->
  FilePath ->
  m a
fileDriverFd iter filepath = do
  fd <- liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags
  result <- enumFd fd iter >>= run
  liftIO $ closeFd fd
  return result

-- |Process a file using the given IterateeT.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandomFd :: (MonadIO m, ReadableChunk s el) =>
  IterateeT s el m a ->
  FilePath ->
  m a
fileDriverRandomFd iter filepath = do
  fd <- liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags
  result <- enumFdRandom fd iter >>= run
  liftIO $ closeFd fd
  return result

#endif
