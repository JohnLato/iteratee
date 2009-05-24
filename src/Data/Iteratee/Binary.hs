{-# LANGUAGE FlexibleContexts #-}

-- |Iteratees for parsing binary data.
module Data.Iteratee.Binary (
  -- * Types
  Endian (..),
  -- * Endian multi-byte iteratees
  endianRead2,
  endianRead3,
  endianRead4
)
where

import Data.Iteratee.Base.StreamChunk (StreamChunk)
import qualified Data.Iteratee.Base as It
import Data.Word
import Data.Bits
import Data.Int


-- ------------------------------------------------------------------------
-- Binary Random IO Iteratees

-- Iteratees to read unsigned integers written in Big- or Little-endian ways

-- |Indicate endian-ness.
data Endian = MSB -- ^ Most Significant Byte is first (big-endian)
  | LSB           -- ^ Least Significan Byte is first (little-endian)
  deriving (Eq, Ord, Show, Enum)

endianRead2 :: (StreamChunk s Word8, Monad m) => Endian ->
                It.IterateeG s Word8 m Word16
endianRead2 e = do
  c1 <- It.head
  c2 <- It.head
  case e of
    MSB -> return $ (fromIntegral c1 `shiftL` 8) .|. fromIntegral c2
    LSB -> return $ (fromIntegral c2 `shiftL` 8) .|. fromIntegral c1

-- |read 3 bytes in an endian manner.  If the first bit is set (negative),
-- set the entire first byte so the Word32 can be properly set negative as
-- well.
endianRead3 :: (StreamChunk s Word8, Monad m) => Endian ->
                It.IterateeG s Word8 m Word32
endianRead3 e = do
  c1 <- It.head
  c2 <- It.head
  c3 <- It.head
  case e of
    MSB -> return $ (((fromIntegral c1
                        `shiftL` 8) .|. fromIntegral c2)
                        `shiftL` 8) .|. fromIntegral c3
    LSB ->
     let m :: Int32
         m = shiftR (shiftL (fromIntegral c3) 24) 8 in
     return $ (((fromIntegral c3
                        `shiftL` 8) .|. fromIntegral c2)
                        `shiftL` 8) .|. fromIntegral m

endianRead4 :: (StreamChunk s Word8, Monad m) => Endian ->
                It.IterateeG s Word8 m Word32
endianRead4 e = do
  c1 <- It.head
  c2 <- It.head
  c3 <- It.head
  c4 <- It.head
  case e of
    MSB -> return $ 
	       (((((fromIntegral c1
		`shiftL` 8) .|. fromIntegral c2)
	        `shiftL` 8) .|. fromIntegral c3)
	        `shiftL` 8) .|. fromIntegral c4
    LSB -> return $ 
	       (((((fromIntegral c4
		`shiftL` 8) .|. fromIntegral c3)
	        `shiftL` 8) .|. fromIntegral c2)
	        `shiftL` 8) .|. fromIntegral c1


