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
import Data.Iteratee.Base.StreamChunk (ReadableChunk (..))
import Data.Iteratee.Base
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import System.IO (SeekMode(..))

import System.Posix hiding (FileOffset)

-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a POSIX File Descriptor.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumFd :: forall a s el.(ReadableChunk s el) => Fd -> EnumeratorGM s el IO a
enumFd fd iter' = allocaBytes (fromIntegral buffer_size) $ loop iter'
  where
    buffer_size = fromIntegral $ 2048 - (mod 2048 $ sizeOf (undefined :: el))
    loop iter p = do
      n <- myfdRead fd (castPtr p) buffer_size
      case n of
        Left _errno -> enumErr "IO error" iter
        Right 0 -> return iter
        Right n' -> do
          s <- readFromPtr p (fromIntegral n')
          igv <- runIter iter (Chunk s)
          check p igv
    check _p (Done x _) = return . return $ x
    check p  (Cont i Nothing) = loop i p
    check _p (Cont _ (Just e)) = return $ throwErr e

-- |The enumerator of a POSIX File Descriptor: a variation of enumFd that
-- supports RandomIO (seek requests)
enumFdRandom :: forall a s el.(ReadableChunk s el) => Fd -> EnumeratorGM s el IO a
enumFdRandom fd iter =
 allocaBytes (fromIntegral buffer_size) (loop (0,0) iter)
 where
  -- this can be usefully varied.  Values between 512 and 4096 seem
  -- to provide the best performance for most cases.
  buffer_size = fromIntegral $ 4096 - (mod 4096 $ sizeOf (undefined :: el))
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'p'
{-
  loop :: (ReadableChunk s el) =>
          (FileOffset,Int) ->
          IterateeG s el IO a -> 
	  Ptr el ->
          IO (IterateeG s el IO a)
-}
    -- Thanks to John Lato for the strictness annotation
    -- Otherwise, the `off + fromIntegral len' below accumulates thunks
  loop (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop (off,len) iter' p = do
   n <- myfdRead fd (castPtr p) buffer_size
   case n of
    Left _errno -> enumErr "IO error" iter'
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         igv <- runIter iter' (Chunk s)
         check (off + fromIntegral len, fromIntegral n') p igv
  seekTo pos@(off, len) off' iter' p
    | off <= off' && off' < off + fromIntegral len =   -- Seek within buffer
    do
    let local_off = fromIntegral $ off' - off
    s <- readFromPtr (p `plusPtr` local_off) (len - local_off)
    igv <- runIter iter' (Chunk s)
    check pos p igv
  seekTo _pos off iter' p = do                           -- Seek outside buffer
    off' <- myfdSeek fd AbsoluteSeek (fromIntegral off)
    case off' of
      Left _errno -> enumErr "IO error" iter'
      Right off'' -> loop (off'',0) iter' p
  check _ _p (Done x _)                 = return . return $ x
  check o p  (Cont i Nothing)           = loop o i p
  check o p  (Cont i (Just (Seek off))) = seekTo o off i p
  check _ _p (Cont _ (Just e))          = return $ throwErr e

-- |Process a file using the given IterateeGM.  This function wraps
-- enumFd as a convenience.
fileDriverFd :: ReadableChunk s el => IterateeG s el IO a ->
                FilePath ->
                IO a
fileDriverFd iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  result <- enumFd fd iter >>= run
  closeFd fd
  return result

-- |Process a file using the given IterateeGM.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandomFd :: ReadableChunk s el =>
                      IterateeG s el IO a ->
                      FilePath ->
                      IO a
fileDriverRandomFd iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  result <- enumFdRandom fd iter >>= run
  closeFd fd
  return result

#endif

