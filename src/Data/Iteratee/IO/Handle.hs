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
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()

--import Data.Int
import Control.Exception.Extensible
import Control.Monad
import Control.Monad.Trans

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The (monadic) enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumHandle :: forall s el m a.(ReadableChunk s el, Nullable s, MonadIO m) =>
  Handle ->
  Enumerator s m a
enumHandle h i = Iteratee $
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
    checkres fp iter = either (runIter . flip enumErr iter)
                              (maybe (runIter iter)
                                (\c -> runIter iter >>= check fp (Chunk c)))
    check _p s   (Done x _)        = return $ Done x s
    check p  s   (Cont i' Nothing) = loop (i' s) p
    check _p _ c@(Cont _  _)       = return c

-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests)
enumHandleRandom
  :: forall s el m a.(ReadableChunk s el, Nullable s, MonadIO m) =>
     Handle
     -> Enumerator s m a
enumHandleRandom h i = Iteratee $
 liftIO (mallocForeignPtrBytes (fromIntegral buffer_size)) >>= loop (0,0) i
 where
  buffer_size = 4096 - mod 4096 (sizeOf (undefined :: el))
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'fp'
{-
  loop :: (FileOffset,Int) ->
          Iteratee s m a ->
          ForeignPtr el ->
          m (Iteratee s m a)
-}
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
    runIter iter >>= check pos fp (Chunk s)
  seekTo _pos off iter fp = do                          -- Seek outside buffer
   off' <- liftIO (try $ hSeek h AbsoluteSeek
            (fromIntegral off) :: IO (Either SomeException ()))
   case off' of
    Left _errno -> runIter $ enumErr "IO error" iter
    Right _     -> loop (off,0) iter fp
  checkres fp iter = either
                       (runIter . flip enumErr iter)
                       (maybe (runIter iter) (uncurry $ runS fp iter))
  runS fp iter o s = runIter iter >>= check o fp (Chunk s)
  check _ _ s (Done x _)                   = return $ Done x s
  check o fp s (Cont i' Nothing)           = loop o (i' s) fp
  check o fp s (Cont i' (Just (Seek off))) = seekTo o off (i' s) fp
  check _ _ _ c@(Cont _ _)                 = return c

-- ----------------------------------------------
-- File Driver wrapper functions.

-- |Process a file using the given Iteratee.  This function wraps
-- enumHandle as a convenience.
fileDriverHandle
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverHandle iter filepath = do
  h <- liftIO $ openBinaryFile filepath ReadMode
  result <- run $ enumHandle h iter
  liftIO $ hClose h
  return result

-- |Process a file using the given Iteratee.  This function wraps
-- enumHandleRandom as a convenience.
fileDriverRandomHandle
  :: (MonadIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomHandle iter filepath = do
  h <- liftIO $ openBinaryFile filepath ReadMode
  result <- run $ enumHandleRandom h iter
  liftIO $ hClose h
  return result
