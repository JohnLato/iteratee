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
  -- ** FileDescriptor based enumerators for pure iteratees
  ,enumFdPure
  ,enumFdRandomPure
  -- * Iteratee drivers
  ,fileDriverFd
  ,fileDriverRandomFd
  ,fileDriverPureFd
  ,fileDriverRandomPureFd
#endif
)

where

#if defined(USE_POSIX)
import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Iteratee
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
enumFdPure
  :: forall s el m a.(Nullable s, ReadableChunk s el, MonadIO m) =>
     Fd
     -> EnumeratorM IterateePure s m a
enumFdPure fd iter' =
  liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop iter'
  where
    buffer_size = fromIntegral $ 4096 - mod 4096 (sizeOf (undefined :: el))
    loop iter fp = do
      s <- liftIO . withForeignPtr fp $ \p -> do
        liftIO $ GHC.Conc.threadWaitRead fd
        n <- myfdRead fd (castPtr p) buffer_size
        case n of
          Left _errno -> return $ Left "IO error"
          Right 0     -> return $ Right Nothing
          Right n'    -> liftM (Right . Just) $ readFromPtr p (fromIntegral n')
      checkres fp iter s
    checkres fp iter =
      either (return . flip enumErr iter)
             (maybe (return iter)
                    (check fp (runIterPure iter) . Chunk))
    check _p (Done x _) s        = return . IterateePure $ Done x s
    check p  (Cont i Nothing)  s = loop (i s) p
    check _p (Cont _ (Just e)) _ = return $ throwErr e

enumFd
  :: forall s el m a.(Nullable s, ReadableChunk s el, MonadIO m) =>
     Fd
     -> Enumerator Iteratee s m a
enumFd fd iter' = Iteratee $
  liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop iter'
  where
    buffer_size = fromIntegral $ 4096 - mod 4096 (sizeOf (undefined :: el))
    loop iter fp = do
      s <- liftIO . withForeignPtr fp $ \p -> do
        liftIO $ GHC.Conc.threadWaitRead fd
        n <- myfdRead fd (castPtr p) buffer_size
        case n of
          Left _errno -> return $ Left "IO error"
          Right 0     -> return $ Right Nothing
          Right n'    -> liftM (Right . Just) $ readFromPtr p (fromIntegral n')
      checkres fp iter s
    checkres fp iter =
      either (runIter . flip enumErr iter)
             (maybe (runIter iter)
                    (\c -> runIter iter >>= check fp (Chunk c)))
    check _p s (Done x _)          = return $ Done x s
    check p  s (Cont i Nothing)    = loop (i s) p
    check _p _ c@(Cont _ (Just _)) = return c

-- |The enumerator of a POSIX File Descriptor: a variation of enumFdPure that
-- supports RandomIO (seek requests)
enumFdRandomPure
  :: forall s el m a.(Nullable s, ReadableChunk s el, MonadIO m) =>
     Fd
     -> EnumeratorM IterateePure s m a
enumFdRandomPure fd iter' =
 liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop (0,0) iter'
 where
  -- this can be usefully varied.  Values between 512 and 4096 seem
  -- to provide the best performance for most cases.
  buffer_size = fromIntegral $ 4096 - mod 4096 (sizeOf (undefined :: el))
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'fp'
{-
  loop :: (FileOffset,Int) ->
          IterateePure s m a ->
          ForeignPtr el ->
          m (IterateePure s m a)
-}
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
    check pos fp (runIterPure iter) (Chunk s)
  seekTo _pos off iter fp = do                         -- Seek outside buffer
    off' <- liftIO $ myfdSeek fd AbsoluteSeek (fromIntegral off)
    case off' of
      Left _errno -> return $ enumErr "IO error" iter
      Right off'' -> loop (off'',0) iter fp
  checkres fp iter = either
                       (return . flip enumErr iter)
                       (maybe (return iter) (uncurry $ runS fp iter))
  runS fp iter o s = check o fp (runIterPure iter) $ Chunk s
  check _ _fp (Done x _) s                 = return . IterateePure $ Done x s
  check o fp  (Cont i Nothing) s           = loop o (i s) fp
  check o fp  (Cont i (Just (Seek off))) s = seekTo o off (i s) fp
  check _ _fp (Cont _ (Just e)) _          = return $ throwErr e


-- |The enumerator of a POSIX File Descriptor: a variation of enumFd that
-- supports RandomIO (seek requests)
enumFdRandom
  :: forall s el m a.(Nullable s, ReadableChunk s el, MonadIO m) =>
     Fd
     -> Enumerator Iteratee s m a
enumFdRandom fd iter' = Iteratee $
 liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop (0,0) iter'
 where
  -- this can be usefully varied.  Values between 512 and 4096 seem
  -- to provide the best performance for most cases.
  buffer_size = fromIntegral $ 4096 - mod 4096 (sizeOf (undefined :: el))
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'fp'
{-
  loop :: (FileOffset,Int) ->
          IterateePure s m a ->
          ForeignPtr el ->
          m (IterateePure s m a)
-}
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
    runIter iter >>= check pos fp (Chunk s)
  seekTo _pos off iter fp = do                         -- Seek outside buffer
    off' <- liftIO $ myfdSeek fd AbsoluteSeek (fromIntegral off)
    case off' of
      Left _errno -> runIter $ enumErr "IO error" iter
      Right off'' -> loop (off'',0) iter fp
  checkres fp iter = either
                       (runIter . flip enumErr iter)
                       (maybe (runIter iter) (uncurry $ runS fp iter))
  runS fp iter o s = runIter iter >>= check o fp (Chunk s)
  check _ _fp s (Done x _)                 = return $ Done x s
  check o fp  s (Cont i Nothing)           = loop o (i s) fp
  check o fp  s (Cont i (Just (Seek off))) = seekTo o off (i s) fp
  check _ _fp _ c@(Cont _ (Just _))        = return c

-- |Process a file using the given Iteratee.  This function wraps
-- enumFd as a convenience.
fileDriverFd
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverFd iter filepath = do
  fd <- liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags
  result <- run $ enumFd fd iter
  liftIO $ closeFd fd
  return result

-- |Process a file using the given IterateePure.  This function wraps
-- enumFd as a convenience.
fileDriverPureFd
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     IterateePure s m a
     -> FilePath
     -> m a
fileDriverPureFd iter filepath = do
  fd <- liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags
  result <- liftM runPure $ enumFdPure fd iter
  liftIO $ closeFd fd
  return result

-- |Process a file using the given IterateePure.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandomPureFd
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     IterateePure s m a
     -> FilePath
     -> m a
fileDriverRandomPureFd iter filepath = do
  fd <- liftIO $ openFd filepath ReadOnly Nothing defaultFileFlags
  result <- liftM runPure $ enumFdRandomPure fd iter
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
  result <- run $ enumFdRandom fd iter
  liftIO $ closeFd fd
  return result

#endif
