{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base.ReadableChunk (
  -- * Classes
  ReadableChunk (..)
)
where

import Prelude hiding (head, tail, dropWhile, length, splitAt )
import qualified Prelude as P

import qualified Data.ListLike as LL
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO

-- |Class of streams which can be filled from a 'Ptr'.  Typically these
-- are streams which can be read from a file.
-- The Int parameter is the length of the data in bytes.
-- N.B. The pointer must not be returned or used after readFromPtr completes.
class (LL.ListLike c el, Storable el) => ReadableChunk c el where
  readFromPtr :: Ptr (el) -> Int -> IO c

instance (Storable el) => ReadableChunk [el] el where
  readFromPtr = flip peekArray
