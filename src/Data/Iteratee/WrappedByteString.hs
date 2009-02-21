{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Iteratee.WrappedByteString (
  WrappedByteString (..)
)

where

import Data.Iteratee.Base
import qualified Data.ByteString as BW
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BBase
import Data.Word
import Foreign.ForeignPtr

-- |Wrap a Data.ByteString ByteString
newtype WrappedByteString a = WrapBS { unWrap :: BBase.ByteString }

instance ReadableChunk WrappedByteString Word8 where
  readFromPtr p l = do
    fptr <- newForeignPtr_ p
    return $ WrapBS $ BBase.fromForeignPtr fptr 0 l

instance StreamChunk WrappedByteString Word8 where
  cLength        = BW.length . unWrap
  cEmpty         = WrapBS BW.empty
  cNull          = BW.null . unWrap
  cCons a        = WrapBS . BW.cons a . unWrap
  cHead          = BW.head . unWrap
  cTail          = WrapBS . BW.tail . unWrap
  cFindIndex p   = BW.findIndex p . unWrap
  cSplitAt i s   = let (a1, a2) = BW.splitAt i $ unWrap s
                   in (WrapBS a1, WrapBS a2)
  cDropWhile p   = WrapBS . BW.dropWhile p . unWrap
  cAppend a1 a2  = WrapBS (BW.append (unWrap a1) (unWrap a2))
  fromList       = WrapBS . BW.pack
  toList         = BW.unpack . unWrap
  cMap           = bwmap

bwmap :: (StreamChunk s' el') =>
         (Word8 -> el') ->
         WrappedByteString Word8 ->
         s' el'
bwmap f xs = step xs
  where
  step bs
    | cNull bs = cEmpty
    | True     = f (cHead bs) `cCons` step (cTail bs)

-- a specialized version to use in the RULE
bwmap' :: (Word8 -> Word8) ->
          WrappedByteString Word8 ->
          WrappedByteString Word8
bwmap' f = WrapBS . BW.map f . unWrap

{-# RULES "bwmap/map" forall s (f :: Word8 -> Word8). bwmap f s = bwmap' f s #-}


instance StreamChunk WrappedByteString Char where
  cLength        = BC.length . unWrap
  cEmpty         = WrapBS BC.empty
  cNull          = BC.null . unWrap
  cCons a        = WrapBS . BC.cons a . unWrap
  cHead          = BC.head . unWrap
  cTail          = WrapBS . BC.tail . unWrap
  cFindIndex p   = BC.findIndex p . unWrap
  cSplitAt i s   = let (a1, a2) = BC.splitAt i $ unWrap s
                   in (WrapBS a1, WrapBS a2)
  cDropWhile p   = WrapBS . BC.dropWhile p . unWrap
  cAppend a1 a2  = WrapBS (BC.append (unWrap a1) (unWrap a2))
  fromList       = WrapBS . BC.pack
  toList         = BC.unpack . unWrap
  cMap           = bcmap

bcmap :: (StreamChunk s' el') =>
         (Char -> el') ->
         WrappedByteString Char ->
         s' el'
bcmap f xs = step xs
  where
  step bs
    | cNull bs = cEmpty
    | True     = f (cHead bs) `cCons` step (cTail bs)

-- a specialized version to use in the RULE
bcmap' :: (Char -> Char) ->
          WrappedByteString Char ->
          WrappedByteString Char
bcmap' f = WrapBS . BC.map f . unWrap

{-# RULES "bcmap/map" forall s (f :: Char -> Char). bcmap f s = bcmap' f s #-}
