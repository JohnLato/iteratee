-- |Random and Binary IO with generic Iteratees.

module Data.Iteratee.IO(
  -- * Iteratee drivers
  fileDriverRandom,
  -- * File enumerators
  enumFd,
  enumFdRandom
)

where

import Data.Iteratee.Base.StreamChunk (ReadableChunk (..))
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
enumFd :: ReadableChunk s el => Fd -> EnumeratorGM s el IO a
enumFd fd iter' = IM $ allocaBytes (fromIntegral buffer_size) $ loop iter'
  where
    buffer_size = 4096
    loop iter@Done{} _p = return iter
    loop _iter@Seek{} _p = error "enumFd is not compatabile with seek IO"
    loop (Cont step) p = do
      n <- myfdRead fd (castPtr p) buffer_size
      case n of
        Left _errno -> unIM $ step (Error "IO error")
        Right 0 -> return iter'
        Right n' -> do
          s <- readFromPtr p (fromIntegral n')
          im <- unIM $ step (Chunk s)
	  loop im p

-- |The enumerator of a POSIX File Descriptor: a variation of enumFd that
-- supports RandomIO (seek requests)
enumFdRandom :: ReadableChunk s el => Fd -> EnumeratorGM s el IO a
enumFdRandom fd iter =
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
   off' <- myfdSeek fd AbsoluteSeek (fromIntegral off)
   case off' of
    Left _errno -> unIM $ enumErr "IO error" iter'
    Right off''  -> loop (off'',0) (Cont c) p
    -- Thanks to John Lato for the strictness annotation
    -- Otherwise, the `off + fromIntegral len' below accumulates thunks
  loop (off,len) _iter' _p | off `seq` len `seq` False = undefined
  loop (off,len) iter'@(Cont step) p = do
   n <- myfdRead fd (castPtr p) buffer_size
   case n of
    Left _errno -> unIM $ step (Error "IO error")
    Right 0 -> return iter'
    Right n' -> do
         s <- readFromPtr p (fromIntegral n')
         im <- unIM $ step (Chunk s)
	 loop (off + fromIntegral len,fromIntegral n') im p

-- |Process a file using the given IterateeGM.  This function wraps
-- enumFdRandom as a convenience.
fileDriverRandom :: ReadableChunk s el => IterateeGM s el IO a ->
               FilePath ->
               IO (Either (String, a) a)
fileDriverRandom iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  result <- unIM $ (enumFdRandom fd >. enumEof) ==<< iter
  closeFd fd
  print_res result
 where
  print_res (Done a (Error err)) = return $ Left (err, a)
  print_res (Done a _) = return $ Right a
  print_res (Cont _)   = return $ Left ("Iteratee unfinished", undefined)
  print_res (Seek _ _) = return $ Left ("Iteratee unfinished", undefined)
                           
