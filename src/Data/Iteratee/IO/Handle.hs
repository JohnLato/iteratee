-- |Random and Binary IO with generic Iteratees.  These functions use Handles
-- for IO operations, and are provided for compatibility.  When available,
-- the File Descriptor based functions are preferred as these wastefully
-- allocate memory rather than running in constant space.

module Data.Iteratee.IO.Handle(
  -- * File enumerators
  enumHandle,
  enumHandleRandom,
  -- * Iteratee drivers
  fileDriverHandle,
  fileDriverRandomHandle,
)

where

import Data.Iteratee.Base.StreamChunk (ReadableChunk (..))
import Data.Iteratee.Base
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base

import Data.Int
import Control.Exception

import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enumHandle :: ReadableChunk s el => Handle -> EnumeratorGM s el IO a
enumHandle h iter' = IM $ allocaBytes (fromIntegral buffer_size) $ loop iter'
  where
    buffer_size = 4096
    loop iter@Done{} _p     = return iter
    loop _iter@Seek{} _p    = error "enumH is not compatabile with seek IO"
    loop iter@(Cont step) p = do
      n <- try $ hGetBuf h p buffer_size
      case n of
        Left ex -> unIM $ step (Error $ show (ex :: SomeException))
        Right 0 -> return iter
        Right n' -> do
          s <- readFromPtr p (fromIntegral n')
          im <- unIM $ step (Chunk s)
	  loop im p

-- |The enumerator of a POSIX File Descriptor: a variation of enumFd that
-- supports RandomIO (seek requests)
enumHandleRandom :: ReadableChunk s el => Handle -> EnumeratorGM s el IO a
enumHandleRandom h iter =
 IM $ allocaBytes (fromIntegral buffer_size) (loop (0,0) iter)
 where
  -- this can be usefully varied.  Values between 512 and 4096 seem
  -- to provide the best performance for most cases.
  buffer_size = 4096
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'p'
  loop :: (ReadableChunk s el) =>
          (FileOffset,Int) ->
          IterateeG s el IO a -> 
	  Ptr el ->
          IO (IterateeG s el IO a)
  loop _pos iter'@Done{} _p = return iter'
  loop pos@(off,len) (Seek off' c) p | 
    off <= off' && off' < off + fromIntegral len =	-- Seek within buffer p
    do
    let local_off = fromIntegral $ off' - off
    s <- readFromPtr (p `plusPtr` local_off) (len - local_off)
    im <- unIM $ c (Chunk s)
    loop pos im p
  loop _pos iter'@(Seek off c) p = do -- Seek outside the buffer
   off' <- (try $ hSeek h AbsoluteSeek
            (fromIntegral off)) :: IO (Either SomeException ())
   case off' of
    Left _errno -> unIM $ enumErr "IO error" iter'
    Right _     -> loop (off,0) (Cont c) p
    -- Thanks to John Lato for the strictness annotation
    -- Otherwise, the `off + fromIntegral len' below accumulates thunks
  loop (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop (off,len) iter'@(Cont step) p = do
   n <- (try $ hGetBuf h p buffer_size) :: IO (Either SomeException Int)
   case n of
    Left _errno -> unIM $ step (Error "IO error")
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         im <- unIM $ step (Chunk s)
	 loop (off + fromIntegral len,fromIntegral n') im p

-- ----------------------------------------------
-- File Driver wrapper functions.

-- |Process a file using the given IterateeGM.  This function wraps
-- enumHandle as a convenience.
fileDriverHandle :: ReadableChunk s el =>
                    IterateeGM s el IO a ->
                    FilePath ->
                    IO (Either (String, a) a)
fileDriverHandle iter filepath = do
  h <- openBinaryFile filepath ReadMode
  result <- unIM $ (enumHandle h >. enumEof) ==<< iter
  hClose h
  print_res result
 where
  print_res (Done a (Error err)) = return $ Left (err, a)
  print_res (Done a _) = return $ Right a
  print_res (Cont _)   = return $ Left ("Iteratee unfinished", undefined)
  print_res (Seek _ _) = return $ Left ("Iteratee unfinished", undefined)

-- |Process a file using the given IterateeGM.  This function wraps
-- enumHandleRandom as a convenience.
fileDriverRandomHandle :: ReadableChunk s el =>
                          IterateeGM s el IO a ->
                          FilePath ->
                          IO (Either (String, a) a)
fileDriverRandomHandle iter filepath = do
  h <- openBinaryFile filepath ReadMode
  result <- unIM $ (enumHandleRandom h >. enumEof) ==<< iter
  hClose h
  print_res result
 where
  print_res (Done a (Error err)) = return $ Left (err, a)
  print_res (Done a _) = return $ Right a
  print_res (Cont _)   = return $ Left ("Iteratee unfinished", undefined)
  print_res (Seek _ _) = return $ Left ("Iteratee unfinished", undefined)

