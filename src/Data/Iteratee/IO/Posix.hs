{-# LANGUAGE CPP, ForeignFunctionInterface #-}

-- Low-level IO operations
-- These operations are either missing from the GHC run-time library,
-- or implemented suboptimally or heavy-handedly

module Data.Iteratee.IO.Posix (
#if defined(USE_POSIX)
  FileOffset,
  myfdRead,
  myfdSeek,
  Errno(..),
  select'read'pending
#endif
)

where

#if defined(USE_POSIX)

import Foreign.C
import Foreign.Ptr
import System.Posix
import System.IO (SeekMode(..))
import Control.Monad
import Data.Bits                        -- for select
import Foreign.Marshal.Array            -- for select

-- |Alas, GHC provides no function to read from Fd to an allocated buffer.
-- The library function fdRead is not appropriate as it returns a string
-- already. I'd rather get data from a buffer.
-- Furthermore, fdRead (at least in GHC) allocates a new buffer each
-- time it is called. This is a waste. Yet another problem with fdRead
-- is in raising an exception on any IOError or even EOF. I'd rather
-- avoid exceptions altogether.

myfdRead :: Fd -> Ptr CChar -> ByteCount -> IO (Either Errno ByteCount)
myfdRead (Fd fd) ptr n = do
  n' <- cRead fd ptr n
  if n' == -1 then liftM Left getErrno
     else return . Right . fromIntegral $ n'

foreign import ccall unsafe "unistd.h read" cRead
  :: CInt -> Ptr CChar -> CSize -> IO CInt

-- |The following fseek procedure throws no exceptions.
myfdSeek:: Fd -> SeekMode -> FileOffset -> IO (Either Errno FileOffset)
myfdSeek (Fd fd) mode off = do
  n' <- cLSeek fd off (mode2Int mode)
  if n' == -1 then liftM Left getErrno
     else return . Right  $ n'
 where mode2Int :: SeekMode -> CInt     -- From GHC source
       mode2Int AbsoluteSeek = 0
       mode2Int RelativeSeek = 1
       mode2Int SeekFromEnd  = 2

foreign import ccall unsafe "unistd.h lseek" cLSeek
  :: CInt -> FileOffset -> CInt -> IO FileOffset


-- Darn! GHC doesn't provide the real select over several descriptors!
-- We have to implement it ourselves

type FDSET = CUInt
type TIMEVAL = CLong -- Two longs
foreign import ccall "unistd.h select" c_select
  :: CInt -> Ptr FDSET -> Ptr FDSET -> Ptr FDSET -> Ptr TIMEVAL -> IO CInt

-- Convert a file descriptor to an FDSet (for use with select)
-- essentially encode a file descriptor in a big-endian notation
fd2fds :: CInt -> [FDSET]
fd2fds fd = replicate nb 0 ++ [setBit 0 off]
  where
    (nb,off) = quotRem (fromIntegral fd) (bitSize (undefined::FDSET))

fds2mfd :: [FDSET] -> [CInt]
fds2mfd fds = [fromIntegral (j+i*bitsize) |
               (afds,i) <- zip fds [0..], j <- [0..bitsize],
               testBit afds j]
  where bitsize = bitSize (undefined::FDSET)

unFd :: Fd -> CInt
unFd (Fd x) = x

-- |poll if file descriptors have something to read
-- Return the list of read-pending descriptors
select'read'pending :: [Fd] -> IO (Either Errno [Fd])
select'read'pending mfd =
    withArray ([0,1]::[TIMEVAL]) $ \_timeout ->
      withArray fds $ \readfs -> do
          rc <- c_select (fdmax+1) readfs nullPtr nullPtr nullPtr
          if rc == -1
            then liftM Left getErrno
            -- because the wait was indefinite, rc must be positive!
            else liftM (Right . map Fd . fds2mfd) (peekArray (length fds) readfs)
  where
    fds :: [FDSET]
    fds  = foldr ormax [] (map (fd2fds . unFd) mfd)
    fdmax = maximum $ map fromIntegral mfd
    ormax [] x = x
    ormax x [] = x
    ormax (a:ar) (b:br) = (a .|. b) : ormax ar br

#endif
