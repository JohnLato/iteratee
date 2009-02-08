-- Haskell98!

-- Random and Binary IO with IterateeM

module Data.Iteratee.IO.RandomIO (
  bindm,
  iter_err,
  stakeR,
  Endian (..),
  endian_read2,
  endian_read3,
  endian_read4,
  enum_fd_random,
  test1r,
  test2r,
  test3r,
  test4r
)

where

import Data.Iteratee.IterateeM
import Foreign.Storable (Storable)
import Foreign.ForeignPtr (newForeignPtr_)
import qualified Data.StorableVector.Base as VB
import Text.Printf

import System.Posix
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Data.Word
import Data.Bits
import Data.Int
import qualified Data.StorableVector as Vec

import System.IO (SeekMode(..))
import Data.Iteratee.IO.LowLevelIO


-- ------------------------------------------------------------------------
-- Binary Random IO Iteratees

-- A useful combinator.
-- Perhaps a better idea would have been to define
-- Iteratee to have (Maybe a) in IE_done? In that case, we could
-- make IterateeGM to be the instance of MonadPlus
bindm :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
bindm m f = m >>= maybe (return Nothing) f


-- An iteratee that reports and propagates an error
-- We disregard the input first and then propagate error.
-- It is reminiscent of `abort'
iter_err :: Monad m => String -> IterateeGM el m ()
iter_err err = liftI $ IE_cont step
 where
 step _ = liftI $ IE_done () (Err err)


-- Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of `stake' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early. This variation is particularly useful for randomIO,
-- where we do not have to care to `drain the input stream'.
stakeR :: (Monad m, Storable el) => Int -> EnumeratorN el el m a
stakeR 0 iter = return iter
stakeR _n iter@IE_done{} = return iter
stakeR _n iter@IE_jmp{} = return iter
stakeR n (IE_cont k) = liftI $ IE_cont step
 where
 step chunk@(Chunk str)
   | Vec.null str        = liftI $ IE_cont step
   | Vec.length str <= n = stakeR (n - Vec.length str) ==<< k chunk
 step (Chunk str) = done (Chunk s1) (Chunk s2)
   where (s1,s2) = Vec.splitAt n str
 step stream = done stream stream
 done s1 s2 = k s1 >>== \r -> liftI $ IE_done r s2


-- Iteratees to read unsigned integers written in Big- or Little-endian ways

-- |Indicate endian-ness.
data Endian = MSB -- ^ Most Significant Byte is first (big-endian)
  | LSB           -- ^ Least Significan Byte is first (little-endian)
  deriving (Eq, Ord, Show, Enum)

endian_read2 :: Monad m => Endian -> IterateeGM Word8 m (Maybe Word16)
endian_read2 e =
  bindm snext $ \c1 ->
  bindm snext $ \c2 ->
  case e of
    MSB -> return $ return $ (fromIntegral c1 `shiftL` 8) .|. fromIntegral c2
    LSB -> return $ return $ (fromIntegral c2 `shiftL` 8) .|. fromIntegral c1

-- |read 3 bytes in an endian manner.  If the first bit is set (negative),
-- set the entire first byte so the Word32 can be properly set negative as
-- well.
endian_read3 :: Monad m => Endian -> IterateeGM Word8 m (Maybe Word32)
endian_read3 e = 
  bindm snext $ \c1 ->
  bindm snext $ \c2 ->
  bindm snext $ \c3 ->
  case e of
    MSB -> return $ return $ (((fromIntegral c1
                        `shiftL` 8) .|. fromIntegral c2)
                        `shiftL` 8) .|. fromIntegral c3
    LSB ->
     let m :: Int32
         m = shiftR (shiftL (fromIntegral c3) 24) 8 in
     return $ return $ (((fromIntegral c3
                        `shiftL` 8) .|. fromIntegral c2)
                        `shiftL` 8) .|. fromIntegral m

endian_read4 :: Monad m => Endian -> IterateeGM Word8 m (Maybe Word32)
endian_read4 e =
  bindm snext $ \c1 ->
  bindm snext $ \c2 ->
  bindm snext $ \c3 ->
  bindm snext $ \c4 ->
  case e of
    MSB -> return $ return $ 
	       (((((fromIntegral c1
		`shiftL` 8) .|. fromIntegral c2)
	        `shiftL` 8) .|. fromIntegral c3)
	        `shiftL` 8) .|. fromIntegral c4
    LSB -> return $ return $ 
	       (((((fromIntegral c4
		`shiftL` 8) .|. fromIntegral c3)
	        `shiftL` 8) .|. fromIntegral c2)
	        `shiftL` 8) .|. fromIntegral c1


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

-- The enumerator of a POSIX Fd: a variation of enum_fd that
-- supports RandomIO (seek requests)
enum_fd_random :: Fd -> EnumeratorGM Word8 IO a
enum_fd_random fd iter = {-# SCC "enum_fd_random" #-}
    IM $ allocaBytes (fromIntegral buffer_size) (loop (0,0) iter)
 where
  -- this can be usefully varied.  Values between 512 and 4096 seem
  -- to provide the best performance for most cases.
  buffer_size = 4096
  -- the first argument of loop is (off,len), describing which part
  -- of the file is currently in the buffer 'p'
  loop :: (FileOffset,Int) -> IterateeG Word8 IO a -> 
	  Ptr Word8 -> IO (IterateeG Word8 IO a)
  loop _pos iter'@IE_done{} _p = return iter'
  loop pos@(off,len) (IE_jmp off' c) p | 
    off <= off' && off' < off + fromIntegral len =	-- Seek within buffer p
    do
    let local_off = fromIntegral $ off' - off
    --str <- peekArray (len - local_off) (p `plusPtr` local_off)
    --im <- unIM $ c (Chunk $ Vec.pack str)
    fptr <- newForeignPtr_ (p `plusPtr` local_off)
    im <- unIM $ c (Chunk $ VB.fromForeignPtr (fptr) (len - local_off))
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
	 --str <- peekArray (fromIntegral n') p
	 --im  <- unIM $ step (Chunk $ Vec.pack str)
         fptr <- newForeignPtr_ p
         im <- unIM $ step (Chunk $ VB.fromForeignPtr fptr (fromIntegral n'))
	 loop (off + fromIntegral len,fromIntegral n') im p


-- ------------------------------------------------------------------------
-- Tests

test1 () = do
	   Just s1 <- snext
	   Just s2 <- snext
	   sseek 0
	   Just s3 <- snext
	   sseek 100
	   Just s4 <- snext
	   Just s5 <- snext
	   sseek 101
	   Just s6 <- snext
	   sseek 1
	   Just s7 <- snext
	   return [s1,s2,s3,s4,s5,s6,s7]

test2 () = do
	   sseek 100
	   sseek 0
	   sseek 100
	   Just s4 <- snext
	   Just s5 <- snext
	   sseek 101
	   Just s6 <- snext
	   sseek 1
	   Just s7 <- snext
	   sseek 0
	   Just s1 <- snext
	   Just s2 <- snext
	   sseek 0
	   Just s3 <- snext
	   return [s1,s2,s3,s4,s5,s6,s7]

test3 () = do
	   let show_x fmt = map (\x -> (printf fmt x)::String)
	   Just ns1 <- endian_read2 MSB
	   Just ns2 <- endian_read2 MSB
	   Just ns3 <- endian_read2 MSB
	   Just ns4 <- endian_read2 MSB
	   sseek 0
	   Just nl1 <- endian_read4 MSB
	   Just nl2 <- endian_read4 MSB
	   sseek 4
	   Just ns3' <- endian_read2 MSB
	   Just ns4' <- endian_read2 MSB
	   sseek 0
	   Just ns1' <- endian_read2 MSB
	   Just ns2' <- endian_read2 MSB
	   sseek 0
	   Just nl1' <- endian_read4 MSB
	   Just nl2' <- endian_read4 MSB
	   return [show_x "%04x" [ns1,ns2,ns3,ns4],
		   show_x "%08x" [nl1,nl2],
		   show_x "%04x" [ns1',ns2',ns3',ns4'],
		   show_x "%08x" [nl1',nl2']]
			    
test4 () = do
	   Just ns1 <- endian_read2 MSB
	   Just ns2 <- endian_read2 MSB
	   iter_err "Error"
	   ns3 <- endian_read2 MSB
	   return (ns1,ns2,ns3)

test_driver_random iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  result <- unIM $ (enum_fd_random fd >. enum_eof) ==<< iter
  closeFd fd
  print_res result
 where
  print_res (IE_done a EOF) = print a >> return a
  print_res (IE_done a (Err err)) = print a >> return a

test1r = test_driver_random (test1 ()) "test_full1.txt" >>=
	 return . (== [104,101,104,13,10,10,101])

test2r = test_driver_random (test2 ()) "test_full1.txt" >>=
	 return . (== [104,101,104,13,10,10,101])

test3r = test_driver_random (test3 ()) "test4.txt" >>=
	 return . (==
		   [["0001","0203","fffe","fdfc"],
		    ["00010203","fffefdfc"],
		    ["0100","0302","feff","fcfd"],
		    ["03020100","fcfdfeff"]])

test4r = test_driver_random (test4 ()) "test4.txt" >>=
	 return . (== (1,515,Nothing))


{-
About to read file
Read buffer, size 5
Finished reading file
(1,515,Nothing)
Stream error: Error
-}
