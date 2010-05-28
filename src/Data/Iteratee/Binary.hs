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
)
where

import Data.Iteratee.Base
import qualified Data.Iteratee.ListLike as I
import qualified Data.ListLike as LL
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

endianRead2
  :: (Nullable s, LL.ListLike s Word8, Monad m) =>
     Endian
     -> Iteratee s m Word16
endianRead2 e = do
  c1 <- I.head
  c2 <- I.head
  case e of
    MSB -> return $ (fromIntegral c1 `shiftL` 8) .|. fromIntegral c2
    LSB -> return $ (fromIntegral c2 `shiftL` 8) .|. fromIntegral c1

endianRead3
  :: (Nullable s, LL.ListLike s Word8, Monad m) =>
     Endian
     -> Iteratee s m Word32
endianRead3 e = do
  c1 <- I.head
  c2 <- I.head
  c3 <- I.head
  case e of
    MSB -> return $ (((fromIntegral c1
                        `shiftL` 8) .|. fromIntegral c2)
                        `shiftL` 8) .|. fromIntegral c3
    LSB -> return $ (((fromIntegral c3
                        `shiftL` 8) .|. fromIntegral c2)
                        `shiftL` 8) .|. fromIntegral c1

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

endianRead4
  :: (Nullable s, LL.ListLike s Word8, Monad m) =>
     Endian
     -> Iteratee s m Word32
endianRead4 e = do
  c1 <- I.head
  c2 <- I.head
  c3 <- I.head
  c4 <- I.head
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
