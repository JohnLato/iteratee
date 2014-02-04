{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

-- |Monadic Iteratees:
-- incremental input parsers, processors and transformers
--
-- Support for IO enumerators

module Data.Iteratee.Base.ReadableChunk (
  ReadableChunk (..)
)
where

import Prelude hiding (head, tail, dropWhile, length, splitAt )

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Word
import Control.Monad.IO.Class
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- |Class of streams which can be filled from a 'Ptr'.  Typically these
-- are streams which can be read from a file, @Handle@, or similar resource.
--
--
class (Storable el) => ReadableChunk s el | s -> el where
  readFromPtr ::
    MonadIO m =>
      Ptr el
      -> Int -- ^ The pointer must not be used after @readFromPtr@ completes.
      -> m s -- ^ The Int parameter is the length of the data in *bytes*.

instance ReadableChunk [Char] Char where
  readFromPtr buf l = liftIO $ peekCAStringLen (castPtr buf, l)

instance ReadableChunk [Word8] Word8 where
  readFromPtr buf l = liftIO $ peekArray l buf
instance ReadableChunk [Word16] Word16 where
  readFromPtr buf l = liftIO $ peekArray l buf
instance ReadableChunk [Word32] Word32 where
  readFromPtr buf l = liftIO $ peekArray l buf
instance ReadableChunk [Word] Word where
  readFromPtr buf l = liftIO $ peekArray l buf

instance ReadableChunk B.ByteString Word8 where
  readFromPtr buf l = liftIO $ B.packCStringLen (castPtr buf, l)

instance ReadableChunk L.ByteString Word8 where
  readFromPtr buf l = liftIO $
    return . L.fromChunks . (:[]) =<< readFromPtr buf l
