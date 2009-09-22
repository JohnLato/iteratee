{-# LANGUAGE ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees.  These functions use Handles
-- for IO operations, and are provided for compatibility.  When available,
-- the File Descriptor based functions are preferred as these wastefully
-- allocate memory rather than running in constant space.

module Data.Iteratee.IO.Handle(
  -- * File enumerators
  enumHandle
  ,enumHandleRandom
  -- * Iteratee drivers
  ,fileDriverHandle
  ,fileDriverRandomHandle
)

where

import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Base
import Data.Iteratee.Binary()

import Data.Int
import Control.Exception.Extensible
import Control.Monad
import Control.Monad.Trans

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumHandle :: forall s el m a.(ReadableChunk s el, MonadIO m) =>
  Handle ->
  EnumeratorGM s el m a
enumHandle h i =
  liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop i
  where
    buffer_size = 4096 - mod 4096 (sizeOf (undefined :: el))
    loop iter fp = do
      s <- liftIO . withForeignPtr fp $ \p -> do
        n <- try $ hGetBuf h p buffer_size :: IO (Either SomeException Int)
        case n of
          Left _  -> return $ Left "IO error"
          Right 0 -> return $ Right Nothing
          Right n' -> liftM (Right . Just) $ readFromPtr p (fromIntegral n')
      checkres fp iter s
    checkres fp iter = either (flip enumErr iter)
                              (maybe (return iter)
                                     (check fp <=< runIter iter . Chunk))
    check _p (Done x _) = return . return $ x
    check p  (Cont i' Nothing) = loop i' p
    check _p (Cont _ (Just e)) = return $ throwErr e

-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests)
enumHandleRandom :: forall s el m a.(ReadableChunk s el, MonadIO m) =>
  Handle ->
  EnumeratorGM s el m a
enumHandleRandom h i =
 liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop (0,0) i
 where
  buffer_size = 4096 - mod 4096 (sizeOf (undefined :: el))
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'fp'
  loop :: (FileOffset,Int) ->
          IterateeG s el m a ->
          ForeignPtr el ->
          m (IterateeG s el m a)
  -- strictify `off', else the `off + fromIntegral len' accumulates thunks
  loop (off,len) _iter _p | off `seq` len `seq` False = undefined
  loop (off,len) iter fp = do
    s <- liftIO . withForeignPtr fp $ \p -> do
      n <- try $ hGetBuf h p buffer_size :: IO (Either SomeException Int)
      case n of
        Left _errno -> return $ Left "IO error"
        Right 0 -> return $ Right Nothing
        Right n' -> liftM
          (Right . Just . (,) (off + fromIntegral len, fromIntegral n'))
          (readFromPtr p (fromIntegral n'))
    checkres fp iter s
  seekTo pos@(off, len) off' iter fp
    | off <= off' && off' < off + fromIntegral len =    -- Seek within buffer
    do
    let local_off = fromIntegral $ off' - off
    s <- liftIO $ withForeignPtr fp $ \p ->
                    readFromPtr (p `plusPtr` local_off) (len - local_off)
    igv <- runIter iter (Chunk s)
    check pos fp igv
  seekTo _pos off iter fp = do                          -- Seek outside buffer
   off' <- liftIO (try $ hSeek h AbsoluteSeek
            (fromIntegral off) :: IO (Either SomeException ()))
   case off' of
    Left _errno -> enumErr "IO error" iter
    Right _     -> loop (off,0) iter fp
  checkres fp iter = either
                       (flip enumErr iter)
                       (maybe (return iter) (uncurry $ runS fp iter))
  runS fp iter o s = runIter iter (Chunk s) >>= check o fp
  check _ _ (Done x _)                   = return . return $ x
  check o fp (Cont i' Nothing)           = loop o i' fp
  check o fp (Cont i' (Just (Seek off))) = seekTo o off i' fp
  check _ _ (Cont _ (Just e))            = return $ throwErr e

-- ----------------------------------------------
-- File Driver wrapper functions.

-- |Process a file using the given IterateeGM.  This function wraps
-- enumHandle as a convenience.
fileDriverHandle :: (MonadIO m, ReadableChunk s el) =>
  IterateeG s el m a ->
  FilePath ->
  m a
fileDriverHandle iter filepath = do
  h <- liftIO $ openBinaryFile filepath ReadMode
  result <- enumHandle h iter >>= run
  liftIO $ hClose h
  return result

-- |Process a file using the given IterateeGM.  This function wraps
-- enumHandleRandom as a convenience.
fileDriverRandomHandle :: (MonadIO m, ReadableChunk s el) =>
                          IterateeG s el m a ->
                          FilePath ->
                          m a
fileDriverRandomHandle iter filepath = do
  h <- liftIO $ openBinaryFile filepath ReadMode
  result <- enumHandleRandom h iter >>= run
  liftIO $ hClose h
  return result
