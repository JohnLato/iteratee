{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Iteratee.WrappedByteString (
  WrappedByteString (..)
)

where

import qualified Data.Iteratee.Base.StreamChunk as SC
import qualified Data.ByteString as BW
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BBase
import Data.Word
import Foreign.ForeignPtr

-- |Wrap a Data.ByteString ByteString
newtype WrappedByteString a = WrapBS { unWrap :: BBase.ByteString }

instance SC.ReadableChunk WrappedByteString Word8 where
  readFromPtr p l = do
    fptr <- newForeignPtr_ p
    return . WrapBS $ BBase.fromForeignPtr fptr 0 l

instance SC.ReadableChunk WrappedByteString Char where
  readFromPtr p l = do
    fptr <- newForeignPtr_ p
    return . WrapBS $ BBase.fromForeignPtr (castForeignPtr fptr) 0 l

instance SC.StreamChunk WrappedByteString Word8 where
  length        = BW.length . unWrap
  empty         = WrapBS BW.empty
  null          = BW.null . unWrap
  cons a        = WrapBS . BW.cons a . unWrap
  head          = BW.head . unWrap
  tail          = WrapBS . BW.tail . unWrap
  findIndex p   = BW.findIndex p . unWrap
  splitAt i s   = let (a1, a2) = BW.splitAt i $ unWrap s
                  in (WrapBS a1, WrapBS a2)
  dropWhile p   = WrapBS . BW.dropWhile p . unWrap
  append a1 a2  = WrapBS (BW.append (unWrap a1) (unWrap a2))
  fromList      = WrapBS . BW.pack
  toList        = BW.unpack . unWrap
  cMap          = bwmap

bwmap :: (SC.StreamChunk s' el') =>
         (Word8 -> el') ->
         WrappedByteString Word8 ->
         s' el'
bwmap f xs = step xs
  where
  step bs
    | SC.null bs = SC.empty
    | True     = f (SC.head bs) `SC.cons` step (SC.tail bs)

-- a specialized version to use in the RULE
bwmap' :: (Word8 -> Word8) ->
          WrappedByteString Word8 ->
          WrappedByteString Word8
bwmap' f = WrapBS . BW.map f . unWrap

{-# RULES "bwmap/map" forall s (f :: Word8 -> Word8). bwmap f s = bwmap' f s #-}


instance SC.StreamChunk WrappedByteString Char where
  length        = BC.length . unWrap
  empty         = WrapBS BC.empty
  null          = BC.null . unWrap
  cons a        = WrapBS . BC.cons a . unWrap
  head          = BC.head . unWrap
  tail          = WrapBS . BC.tail . unWrap
  findIndex p   = BC.findIndex p . unWrap
  splitAt i s   = let (a1, a2) = BC.splitAt i $ unWrap s
                  in (WrapBS a1, WrapBS a2)
  dropWhile p   = WrapBS . BC.dropWhile p . unWrap
  append a1 a2  = WrapBS (BC.append (unWrap a1) (unWrap a2))
  fromList      = WrapBS . BC.pack
  toList        = BC.unpack . unWrap
  cMap          = bcmap

bcmap :: (SC.StreamChunk s' el') =>
         (Char -> el') ->
         WrappedByteString Char ->
         s' el'
bcmap f xs = step xs
  where
  step bs
    | SC.null bs = SC.empty
    | True     = f (SC.head bs) `SC.cons` step (SC.tail bs)

-- a specialized version to use in the RULE
bcmap' :: (Char -> Char) ->
          WrappedByteString Char ->
          WrappedByteString Char
bcmap' f = WrapBS . BC.map f . unWrap

{-# RULES "bcmap/map" forall s (f :: Char -> Char). bcmap f s = bcmap' f s #-}
