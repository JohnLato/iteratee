{-# LANGUAGE FlexibleContexts, BangPatterns #-}

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

-- | Indicate endian-ness.
data Endian = MSB -- ^ Most Significant Byte is first (big-endian)
  | LSB           -- ^ Least Significan Byte is first (little-endian)
  deriving (Eq, Ord, Show, Enum)

endianRead2
  :: (Nullable s, LL.ListLike s Word8, Monad m)
  => Endian
  -> Iteratee s m Word16
endianRead2 e = endianReadN e 2 word16'
{-# INLINE endianRead2 #-}

endianRead3
  :: (Nullable s, LL.ListLike s Word8, Monad m)
  => Endian
  -> Iteratee s m Word32
endianRead3 e = endianReadN e 3 (word32' . (0:))
{-# INLINE endianRead3 #-}

-- |Read 3 bytes in an endian manner.  If the first bit is set (negative),
-- set the entire first byte so the Int32 will be negative as
-- well.
endianRead3i
  :: (Nullable s, LL.ListLike s Word8, Monad m)
  => Endian
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
  :: (Nullable s, LL.ListLike s Word8, Monad m)
  => Endian
  -> Iteratee s m Word32
endianRead4 e = endianReadN e 4 word32'
{-# INLINE endianRead4 #-}

endianRead8
  :: (Nullable s, LL.ListLike s Word8, Monad m)
  => Endian
  -> Iteratee s m Word64
endianRead8 e = endianReadN e 8 word64'
{-# INLINE endianRead8 #-}

-- This function does all the parsing work, depending upon provided arguments
endianReadN ::
 (Nullable s, LL.ListLike s Word8, Monad m)
  => Endian
  -> Int
  -> ([Word8] -> b)
  -> Iteratee s m b
endianReadN MSB n0 cnct = icontP (step n0 [])
 where
  step !n acc (Chunk c)
    | LL.null c        = (icontP (step n acc), NoData)
    | LL.length c >= n = let (this,next) = LL.splitAt n c
                             !result     = cnct $ acc ++ LL.toList this
                         in (idone result, Chunk next)
    | otherwise        = (icontP (step (n - LL.length c) (acc ++ LL.toList c))
                          , NoData)
  step !n acc NoData   = (icontP (step n acc), NoData)
  step !n acc (EOF Nothing  )  = (icontP (step n acc)
                                 , EOF . Just $ toException EofException)
  step !n acc s@(EOF (Just _)) = (icontP (step n acc), s)
endianReadN LSB n0 cnct = icontP (step n0 [])
 where
  step !n acc (Chunk c)
    | LL.null c        = (icontP (step n acc), Chunk c)
    | LL.length c >= n = let (this,next) = LL.splitAt n c
                             !result = cnct $ reverse (LL.toList this) ++ acc
                         in (idone result, Chunk next)
    | otherwise        = (icontP (step (n - LL.length c)
                                       (reverse (LL.toList c) ++ acc))
                          , NoData)
  step !n acc (EOF Nothing) = (icontP (step n acc)
                               , EOF . Just $ toException EofException)
  step !n acc str           = (icontP (step n acc), str)
{-# INLINE endianReadN #-}

-- As of now, the polymorphic code is as fast as the best specializations
-- I have found, so these just call out.  They may be improved in the
-- future, or possibly deprecated.
-- JWL, 2012-01-16

word16' :: [Word8] -> Word16
word16' [c1,c2] = word16 c1 c2
word16' _ = error "iteratee: internal error in word16'"

word16 :: Word8 -> Word8 -> Word16
word16 c1 c2 = (fromIntegral c1 `shiftL`  8) .|.  fromIntegral c2
{-# INLINE word16 #-}

word32' :: [Word8] -> Word32
word32' [c1,c2,c3,c4] = word32 c1 c2 c3 c4
word32' _ = error "iteratee: internal error in word32'"

word32 :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word32 c1 c2 c3 c4 =
  (fromIntegral c1 `shiftL` 24) .|.
  (fromIntegral c2 `shiftL` 16) .|.
  (fromIntegral c3 `shiftL`  8) .|.
   fromIntegral c4
{-# INLINE word32 #-}

word64' :: [Word8] -> Word64
word64' [c1,c2,c3,c4,c5,c6,c7,c8] = word64 c1 c2 c3 c4 c5 c6 c7 c8
word64' _ = error "iteratee: internal error in word64'"
{-# INLINE word64' #-}

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
