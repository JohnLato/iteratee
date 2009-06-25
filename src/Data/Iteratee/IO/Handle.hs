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

import Data.Iteratee.Base.StreamChunk (ReadableChunk (..))
import Data.Iteratee.Base
import Data.Iteratee.Binary()

import Data.Int
import Control.Exception.Extensible

import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumHandle :: forall a s el.(ReadableChunk s el) => Handle -> EnumeratorGM s el IO a
enumHandle h i = allocaBytes (fromIntegral buffer_size) $ loop i
  where
    buffer_size = 4096 - (mod 4096 $ sizeOf (undefined :: el))
    loop iter p = do
      n <- (try $ hGetBuf h p buffer_size) :: IO (Either SomeException Int)
      case n of
        Left _   -> enumErr "IO error" iter
        Right 0  -> return iter
        Right n' -> do
          s <- readFromPtr p (fromIntegral n')
          igv <- runIter iter (Chunk s)
          check p igv
    check _p (Done x _) = return . return $ x
    check p  (Cont i' Nothing) = loop i' p
    check _p (Cont _ (Just e)) = return $ throwErr e


-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests)
enumHandleRandom :: forall a s el.(ReadableChunk s el) => Handle -> EnumeratorGM s el IO a
enumHandleRandom h iter =
 allocaBytes (fromIntegral buffer_size) (loop (0,0) iter)
 where
  -- this can be usefully varied.  Values between 512 and 4096 seem
  -- to provide the best performance for most cases.
  buffer_size = 4096 - (mod 4096 $ sizeOf (undefined :: el))
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'p'
{-
  loop :: (ReadableChunk s el) =>
          (FileOffset,Int) ->
          IterateeG s el IO a ->
          Ptr el ->
          IO (IterateeG s el IO a)
-}
  -- Otherwise, the `off + fromIntegral len' below accumulates thunks
  loop (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop (off,len) iter' p = do
   n <- (try $ hGetBuf h p buffer_size) :: IO (Either SomeException Int)
   case n of
    Left _errno -> enumErr "IO error" iter
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         igv <- runIter iter' (Chunk s)
         check (off + fromIntegral len,fromIntegral n') p igv
  seekTo pos@(off, len) off' iter' p
    | off <= off' && off' < off + fromIntegral len =    -- Seek within buffer
    do
    let local_off = fromIntegral $ off' - off
    s <- readFromPtr (p `plusPtr` local_off) (len - local_off)
    igv <- runIter iter' (Chunk s)
    check pos p igv
  seekTo _pos off iter' p = do                          -- Seek outside buffer
   off' <- (try $ hSeek h AbsoluteSeek
            (fromIntegral off)) :: IO (Either SomeException ())
   case off' of
    Left _errno -> enumErr "IO error" iter'
    Right _     -> loop (off,0) iter' p
  check _ _ (Done x _)                 = return . return $ x
  check o p (Cont i Nothing)           = loop o i p
  check o p (Cont i (Just (Seek off))) = seekTo o off i p
  check _ _ (Cont _ (Just e))          = return $ throwErr e

-- ----------------------------------------------
-- File Driver wrapper functions.

-- |Process a file using the given IterateeGM.  This function wraps
-- enumHandle as a convenience.
fileDriverHandle :: ReadableChunk s el =>
                    IterateeG s el IO a ->
                    FilePath ->
                    IO a
fileDriverHandle iter filepath = do
  h <- openBinaryFile filepath ReadMode
  result <- enumHandle h iter >>= run
  hClose h
  return result

-- |Process a file using the given IterateeGM.  This function wraps
-- enumHandleRandom as a convenience.
fileDriverRandomHandle :: ReadableChunk s el =>
                          IterateeG s el IO a ->
                          FilePath ->
                          IO a
fileDriverRandomHandle iter filepath = do
  h <- openBinaryFile filepath ReadMode
  result <- enumHandleRandom h iter >>= run
  hClose h
  return result
