{-# LANGUAGE MultiParamTypeClasses
            ,FlexibleInstances
            ,FunctionalDependencies
            ,TupleSections #-}

-- |Monadic Iteratees:
-- incremental input parsers, processors and transformers
--
-- Support for IO enumerators

module Data.Iteratee.Base.ReadableChunk (
  ReadableChunk (..)
)
where

import Prelude hiding (head, tail, dropWhile, length, splitAt )

import Control.Applicative
import Control.Arrow (first)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import Data.Monoid
import Data.Word
import Foreign.C
import Foreign.ForeignPtr  (withForeignPtr, mallocForeignPtrBytes)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc

-- |Class of streams which can be filled from a 'Ptr'.  Typically these
-- are streams which can be read from a file, @Handle@, or similar resource.
--
-- instances must implement 'readFromPtr' and optionally 'fillFromCallback'.
-- if available, 'fillFromCallback' results in less copying.
class (Monoid s, Storable el) => ReadableChunk s el | s -> el where
  readFromPtr :: Ptr el
              -> Int -- ^ The pointer must not be used after @readFromPtr@ completes.
              -> IO s -- ^ The Int parameter is the length of the data in *bytes*.
  fillFromCallback :: Int -> (Ptr el -> Int -> IO Int) -> IO (s,Int)
  fillFromCallback sz cb = allocaBytes sz $ \p -> do
          nRead <- cb p sz
          (,nRead) <$> readFromPtr p nRead
  empty :: s
  empty = mempty

{-# DEPRECATED readFromPtr "use fillFromCallback" #-}

instance ReadableChunk [Char] Char where
  readFromPtr buf l = peekCAStringLen (castPtr buf, l)

instance ReadableChunk [Word8] Word8 where
  readFromPtr buf l = peekArray l buf
instance ReadableChunk [Word16] Word16 where
  readFromPtr buf l = peekArray l buf
instance ReadableChunk [Word32] Word32 where
  readFromPtr buf l = peekArray l buf
instance ReadableChunk [Word] Word where
  readFromPtr buf l = peekArray l buf

instance ReadableChunk B.ByteString Word8 where
  readFromPtr buf l = B.packCStringLen (castPtr buf, l)
  fillFromCallback sz cb = do
      fp <- mallocForeignPtrBytes sz
      numFill <- withForeignPtr fp $ \p -> cb p sz
      return $ (B.PS fp 0 numFill, numFill)

instance ReadableChunk L.ByteString Word8 where
  readFromPtr buf l = return . L.fromChunks . (:[]) =<< readFromPtr buf l
  fillFromCallback sz cb = first (L.fromChunks . (:[])) <$> fillFromCallback sz cb
