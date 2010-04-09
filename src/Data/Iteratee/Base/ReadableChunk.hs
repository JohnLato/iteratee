{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base.ReadableChunk (
  -- * Classes
  ReadableChunk (..)
)
where

import Prelude hiding (head, tail, dropWhile, length, splitAt )

import qualified Data.ByteString as B
import Data.Word
import Control.Monad.Trans
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array

-- |Class of streams which can be filled from a 'Ptr'.  Typically these
-- are streams which can be read from a file.
-- The Int parameter is the length of the data in bytes.
-- N.B. The pointer must not be returned or used after readFromPtr completes.
class (Storable el) => ReadableChunk c el | c -> el where
  readFromPtr :: MonadIO m => Ptr el -> Int -> m c

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
