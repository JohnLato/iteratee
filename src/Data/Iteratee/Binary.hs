{-# LANGUAGE FlexibleContexts #-}

-- |Monadic Iteratees:
-- incremental input parsers, processors, and transformers
--
-- Iteratees for parsing binary data.

module Data.Iteratee.Binary (
  -- * Types
  Endian (..)
  -- * Endian multi-byte iteratees
  ,endianRead2
  ,endianRead3
  ,endianRead3i
  ,endianRead4
  ,endianRead8
  -- ** bytestring specializations
  ,readWord16be_bs
  ,readWord16le_bs
  ,readWord32be_bs
  ,readWord32le_bs
  ,readWord64be_bs
  ,readWord64le_bs
)
where

import Data.Iteratee.Base
import qualified Data.Iteratee.ListLike as I
import qualified Data.ListLike as LL
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import Data.Word
import Data.Bits
import Data.Int

import Control.Exception

-- ------------------------------------------------------------------------
-- Binary Random IO Iteratees

-- Iteratees to read unsigned integers written in Big- or Little-endian ways

-- |Indicate endian-ness.
data Endian = MSB -- ^ Most Significant Byte is first (big-endian)
  | LSB           -- ^ Least Significan Byte is first (little-endian)
  deriving (Eq, Ord, Show, Enum)

endianRead2
  :: (Nullable s, LL.ListLike s Word8, Monad m) =>
     Endian
     -> Iteratee s m Word16
endianRead2 e = do
  c1 <- I.head
  c2 <- I.head
  case e of
    MSB -> return $ word16 c1 c2
    LSB -> return $ word16 c2 c1
{-# INLINE endianRead2 #-}

endianRead3
  :: (Nullable s, LL.ListLike s Word8, Monad m) =>
     Endian
     -> Iteratee s m Word32
endianRead3 e = do
  c1 <- I.head
  c2 <- I.head
  c3 <- I.head
  case e of
    MSB -> return $ word32 0 c1 c2 c3
    LSB -> return $ word32 0 c3 c2 c1
{-# INLINE endianRead3 #-}

-- |Read 3 bytes in an endian manner.  If the first bit is set (negative),
-- set the entire first byte so the Int32 will be negative as
-- well.
endianRead3i
  :: (Nullable s, LL.ListLike s Word8, Monad m) =>
     Endian
     -> Iteratee s m Int32
endianRead3i e = do
  c1 <- I.head
  c2 <- I.head
  c3 <- I.head
  case e of
    MSB -> return $ (((fromIntegral c1
                        `shiftL` 8) .|. fromIntegral c2)
                        `shiftL` 8) .|. fromIntegral c3
    LSB ->
     let m :: Int32
         m = shiftR (shiftL (fromIntegral c3) 24) 8
     in return $ (((fromIntegral c3
                        `shiftL` 8) .|. fromIntegral c2)
                        `shiftL` 8) .|. fromIntegral m
{-# INLINE endianRead3i #-}

endianRead4
  :: (Nullable s, LL.ListLike s Word8, Monad m) =>
     Endian
     -> Iteratee s m Word32
endianRead4 MSB = do
  c1 <- I.head
  c2 <- I.head
  c3 <- I.head
  c4 <- I.head
  return $ word32 c1 c2 c3 c4
endianRead4 LSB = do
  c1 <- I.head
  c2 <- I.head
  c3 <- I.head
  c4 <- I.head
  return $ word32 c4 c3 c2 c1
{-# INLINE [1] endianRead4 #-}

endianRead8
  :: (Nullable s, LL.ListLike s Word8, Monad m) =>
     Endian
     -> Iteratee s m Word64
endianRead8 MSB = do
  cs <- I.joinI $ I.take 8 I.stream2list
  case cs of
    [c1,c2,c3,c4,c5,c6,c7,c8] -> return $ word64 c1 c2 c3 c4 c5 c6 c7 c8
    _ -> I.throwErr (toException EofException)
endianRead8 LSB = do
  cs <- I.joinI $ I.take 8 I.stream2list
  case cs of
    [c8,c7,c6,c5,c4,c3,c2,c1] -> return $ word64 c1 c2 c3 c4 c5 c6 c7 c8
    _ -> I.throwErr (toException EofException)
{-# INLINE [1] endianRead8 #-}

{-# RULES "iteratee: binary bytestring spec." endianRead8 = endianRead8BS #-}

endianRead4BS :: Monad m => Endian -> Iteratee B.ByteString m Word32
endianRead4BS MSB = readWord32be_bs
endianRead4BS LSB = readWord32le_bs
{-# INLINE endianRead4BS  #-}

endianRead8BS :: Monad m => Endian -> Iteratee B.ByteString m Word64
endianRead8BS MSB = readWord64be_bs
endianRead8BS LSB = readWord64le_bs
{-# INLINE endianRead8BS  #-}

-- the 16- and 32-bit variants are only included for completeness; the
-- polymorphic code is as fast as any specialization I've yet found
-- in these cases.  (JWL, 2012-01-09)
readWord16be_bs :: Monad m => Iteratee B.ByteString m Word16
readWord16be_bs = endianRead2 MSB
{-# INLINE readWord16be_bs  #-}

readWord16le_bs :: Monad m => Iteratee B.ByteString m Word16
readWord16le_bs = endianRead2 LSB
{-# INLINE readWord16le_bs  #-}

readWord32be_bs :: Monad m => Iteratee B.ByteString m Word32
readWord32be_bs = endianRead4 MSB
{-# INLINE readWord32be_bs  #-}

readWord32le_bs :: Monad m => Iteratee B.ByteString m Word32
readWord32le_bs = endianRead4 LSB
{-# INLINE readWord32le_bs  #-}

readWord64be_bs :: Monad m => Iteratee B.ByteString m Word64
readWord64be_bs = do
  cs <- I.joinI $ I.take 8 I.stream2stream
  if B.length cs == 8
    then return $ word64 (B.unsafeIndex cs 0)
                         (B.unsafeIndex cs 1)
                         (B.unsafeIndex cs 2)
                         (B.unsafeIndex cs 3)
                         (B.unsafeIndex cs 4)
                         (B.unsafeIndex cs 5)
                         (B.unsafeIndex cs 6)
                         (B.unsafeIndex cs 7)
    else I.throwErr (toException EofException)
{-# INLINE readWord64be_bs  #-}

readWord64le_bs :: Monad m => Iteratee B.ByteString m Word64
readWord64le_bs = do
  cs <- I.joinI $ I.take 8 I.stream2stream
  if B.length cs == 8
    then return $ word64 (B.unsafeIndex cs 7)
                         (B.unsafeIndex cs 6)
                         (B.unsafeIndex cs 5)
                         (B.unsafeIndex cs 4)
                         (B.unsafeIndex cs 3)
                         (B.unsafeIndex cs 2)
                         (B.unsafeIndex cs 1)
                         (B.unsafeIndex cs 0)
    else I.throwErr (toException EofException)
{-# INLINE readWord64le_bs  #-}

word16 :: Word8 -> Word8 -> Word16
word16 c1 c2 = (fromIntegral c1 `shiftL`  8) .|.  fromIntegral c2
{-# INLINE word16 #-}

word32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word32 c1 c2 c3 c4 =
  (fromIntegral c1 `shiftL` 24) .|.
  (fromIntegral c2 `shiftL` 16) .|.
  (fromIntegral c3 `shiftL`  8) .|.
   fromIntegral c4
{-# INLINE word32 #-}

word64
  :: Word8 -> Word8 -> Word8 -> Word8 
  -> Word8 -> Word8 -> Word8 -> Word8 
  -> Word64
word64 c1 c2 c3 c4 c5 c6 c7 c8 =
  (fromIntegral c1 `shiftL` 56) .|.
  (fromIntegral c2 `shiftL` 48) .|.
  (fromIntegral c3 `shiftL` 40) .|.
  (fromIntegral c4 `shiftL` 32) .|.
  (fromIntegral c5 `shiftL` 24) .|.
  (fromIntegral c6 `shiftL` 16) .|.
  (fromIntegral c7 `shiftL`  8) .|.
   fromIntegral c8
{-# INLINE word64 #-}
