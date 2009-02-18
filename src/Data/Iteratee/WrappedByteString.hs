{-# LANGUAGE MultiParamTypeClasses #-}

module Data.Iteratee.WrappedByteString (
  WrappedByteString (..)
)

where

import Data.Iteratee.Base
import qualified Data.ByteString as BW
import Data.Word

-- |Wrap a Data.ByteString ByteString
newtype WrappedByteString a = WrapBS { unWrap :: BW.ByteString }

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
