{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Data.Iteratee.Instances.WrappedByteString (
  WrappedByteString (..)
)

where

import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Base
import qualified Data.ByteString as BW
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Internal as BBase
import qualified Data.ListLike as LL
import Data.Word
import Data.Monoid
import Foreign.Ptr
import Control.Monad

-- |Wrap a Data.ByteString ByteString
newtype WrappedByteString a = WrapBS { unWrap :: BBase.ByteString }

instance Monoid (WrappedByteString Word8) where
  mempty = WrapBS BW.empty
  mappend a1 a2 = WrapBS (BW.append (unWrap a1) (unWrap a2))

instance LL.FoldableLL (WrappedByteString Word8) Word8 where
  foldl f z = BW.foldl f z . unWrap
  foldr f z = BW.foldr f z . unWrap

-- Thanks to Echo Nolan for indicating that the bytestring must copy
-- data to a new ptr to preserve referential transparency.
instance ReadableChunk (WrappedByteString Word8) Word8 where
  readFromPtr buf l = let csl = (castPtr buf, l) in
                      liftM WrapBS $ BW.packCStringLen csl

instance ReadableChunk (WrappedByteString Char) Char where
  readFromPtr buf l = let csl = (castPtr buf, l) in
                      liftM WrapBS $ BC.packCStringLen csl

instance LL.ListLike (WrappedByteString Word8) Word8 where
  length        = BW.length . unWrap
  null          = BW.null . unWrap
  singleton     = WrapBS . BW.singleton
  cons a        = WrapBS . BW.cons a . unWrap
  head          = BW.head . unWrap
  tail          = WrapBS . BW.tail . unWrap
  findIndex p   = BW.findIndex p . unWrap
  splitAt i s   = let (a1, a2) = BW.splitAt i $ unWrap s
                  in (WrapBS a1, WrapBS a2)
  dropWhile p   = WrapBS . BW.dropWhile p . unWrap
  fromList      = WrapBS . BW.pack
  toList        = BW.unpack . unWrap
  rigidMap f    = WrapBS . BW.map f . unWrap

-- Now the Char instance

instance Monoid (WrappedByteString Char) where
    mempty = WrapBS BW.empty
    mappend a1 a2 = WrapBS (BW.append (unWrap a1) (unWrap a2))

instance LL.FoldableLL (WrappedByteString Char) Char where
  foldl f z = BC.foldl f z . unWrap
  foldr f z = BC.foldr f z . unWrap

instance LL.ListLike (WrappedByteString Char) Char where
  length        = BC.length . unWrap
  null          = BC.null . unWrap
  singleton     = WrapBS . BC.singleton
  cons a        = WrapBS . BC.cons a . unWrap
  head          = BC.head . unWrap
  tail          = WrapBS . BC.tail . unWrap
  findIndex p   = BC.findIndex p . unWrap
  splitAt i s   = let (a1, a2) = BC.splitAt i $ unWrap s
                  in (WrapBS a1, WrapBS a2)
  dropWhile p   = WrapBS . BC.dropWhile p . unWrap
  fromList      = WrapBS . BC.pack
  toList        = BC.unpack . unWrap
  rigidMap f    = WrapBS . BC.map f . unWrap

instance LL.StringLike (WrappedByteString Char) where
  toString = BC.unpack . unWrap
  fromString = WrapBS . BC.pack
  lines      = LL.fromList . map WrapBS . BC.lines . unWrap
  words      = LL.fromList . map WrapBS . BC.words . unWrap

instance LooseMap WrappedByteString Word8 Word8 where
  lMap f = WrapBS . BW.map f . unWrap

instance LooseMap WrappedByteString Char Char where
  lMap f = WrapBS . BC.map f . unWrap

instance LooseMap WrappedByteString Char Word8 where
  lMap f = WrapBS . BW.map (f . BBase.w2c) . unWrap

instance LooseMap WrappedByteString Word8 Char where
  lMap f = WrapBS . BW.map (BBase.c2w . f) . unWrap

instance Nullable (WrappedByteString Word8) where
  null = BW.null . unWrap
  empty = WrapBS BW.empty

instance Nullable (WrappedByteString Char) where
  null = BC.null . unWrap
  empty = WrapBS BC.empty

