-- |Random and Binary IO with generic Iteratees.

module Data.Iteratee.IO(
  -- * Iteratee drivers
  file_driver_rb,
  -- * File enumerators
  enum_fd,
  enum_fd_random
)

where

import Data.Iteratee.Base
import Data.Iteratee.Binary()
import Data.Iteratee.IO.Base
import Data.Int

import System.Posix hiding (FileOffset)
import Foreign.Ptr
import Foreign.Marshal.Alloc

import System.IO (SeekMode(..))


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- |The enumerator of a POSIX File Descriptor.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
enum_fd :: ReadableChunk s el => Fd -> EnumeratorGM s el IO a
enum_fd fd iter' = IM $ allocaBytes (fromIntegral buffer_size) $ loop iter'
  where
    buffer_size = 4096
    loop iter@IE_done{} _p = return iter
    loop _iter@IE_jmp{} _p = error "enum_fd is not compatabile with seek IO"
    loop (IE_cont step) p = do
      n <- myfdRead fd (castPtr p) buffer_size
      case n of
        Left _errno -> unIM $ step (Err "IO error")
        Right 0 -> return iter'
        Right n' -> do
          s <- readFromPtr p (fromIntegral n')
          im <- unIM $ step (Chunk s)
	  loop im p

-- |The enumerator of a POSIX File Descriptor: a variation of enum_fd that
-- supports RandomIO (seek requests)
enum_fd_random :: ReadableChunk s el => Fd -> EnumeratorGM s el IO a
enum_fd_random fd iter =
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
  loop _pos iter'@IE_done{} _p = return iter'
  loop pos@(off,len) (IE_jmp off' c) p | 
    off <= off' && off' < off + fromIntegral len =	-- Seek within buffer p
    do
    let local_off = fromIntegral $ off' - off
    s <- readFromPtr (p `plusPtr` local_off) (len - local_off)
    im <- unIM $ c (Chunk s)
    loop pos im p
  loop _pos iter'@(IE_jmp off c) p = do -- Seek outside the buffer
   off' <- myfdSeek fd AbsoluteSeek (fromIntegral off)
   case off' of
    Left _errno -> unIM $ enum_err "IO error" iter'
    Right off''  -> loop (off'',0) (IE_cont c) p
    -- Thanks to John Lato for the strictness annotation
    -- Otherwise, the `off + fromIntegral len' below accumulates thunks
  loop (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop (off,len) iter'@(IE_cont step) p = do
   n <- myfdRead fd (castPtr p) buffer_size
   case n of
    Left _errno -> unIM $ step (Err "IO error")
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         im <- unIM $ step (Chunk s)
	 loop (off + fromIntegral len,fromIntegral n') im p

-- |Process a file using the given IterateeGM.  This function wraps
-- enum_fd_random as a convenience.
file_driver_rb :: ReadableChunk s el => IterateeGM s el IO a ->
               FilePath ->
               IO (Either (String, a) a)
file_driver_rb iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  result <- unIM $ (enum_fd_random fd >. enum_eof) ==<< iter
  closeFd fd
  print_res result
 where
  print_res (IE_done a (Err err)) = return $ Left (err, a)
  print_res (IE_done a _) = return $ Right a
  print_res (IE_cont _) = return $ Left ("Iteratee unfinished", undefined)
  print_res (IE_jmp _ _) = return $ Left ("Iteratee unfinished", undefined)
                           
