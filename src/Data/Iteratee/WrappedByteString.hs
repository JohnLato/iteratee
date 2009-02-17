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
  cCons a s      = WrapBS $ BW.cons a $ unWrap s
  cHead          = BW.head . unWrap
  cTail s        = WrapBS (BW.tail $ unWrap s)
  cFindIndex p s = BW.findIndex p $ unWrap s
  cSplitAt i s   = let (a1, a2) = BW.splitAt i $ unWrap s
                   in (WrapBS a1, WrapBS a2)
  cDropWhile p s = WrapBS (BW.dropWhile p $ unWrap s)
  cAppend a1 a2  = WrapBS (BW.append (unWrap a1) (unWrap a2))
  fromList xs    = WrapBS (BW.pack xs)
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
bwmap' f s = WrapBS (BW.map f $ unWrap s)

{-# RULES "bwmap/map" forall s (f :: Word8 -> Word8). bwmap f s = bwmap' f s #-}
