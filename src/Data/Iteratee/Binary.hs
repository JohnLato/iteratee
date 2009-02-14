{-# LANGUAGE FlexibleContexts #-}

module Data.Iteratee.Binary (
  Endian (..),
  endian_read2,
  endian_read3,
  endian_read4
)
where

import Data.Iteratee.Base
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

endian_read2 :: (StreamChunk s Word8, Monad m) => Endian -> IterateeGM s Word8 m (Maybe Word16)
endian_read2 e =
  bindm snext $ \c1 ->
  bindm snext $ \c2 ->
  case e of
    MSB -> return $ return $ (fromIntegral c1 `shiftL` 8) .|. fromIntegral c2
    LSB -> return $ return $ (fromIntegral c2 `shiftL` 8) .|. fromIntegral c1

-- |read 3 bytes in an endian manner.  If the first bit is set (negative),
-- set the entire first byte so the Word32 can be properly set negative as
-- well.
endian_read3 :: (StreamChunk s Word8, Monad m) => Endian -> IterateeGM s Word8 m (Maybe Word32)
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

endian_read4 :: (StreamChunk s Word8, Monad m) => Endian -> IterateeGM s Word8 m (Maybe Word32)
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


