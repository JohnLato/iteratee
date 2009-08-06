{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base.StreamChunk (
  -- * Types
  StreamChunk (..),
  ReadableChunk (..)
)
where

import Prelude hiding (head, tail, dropWhile, length, splitAt )
import qualified Prelude as P

import qualified Data.List as L
import qualified Data.ListLike as LL
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO

-- |Class of types that can be used to hold chunks of data within Iteratee
-- streams.
class LL.ListLike (c el) el => StreamChunk c el where
  -- |Length of currently available data.
  length :: c el -> Int
  length = LL.length

  -- |Test if the current stream is null.
  null :: c el -> Bool
  null = LL.null

  -- |Prepend an element to the front of the data.
  cons :: el -> c el -> c el
  cons = LL.cons

  -- |Return the first element of the stream.
  head :: c el -> el
  head = LL.head

  -- |Return the tail of the stream.
  tail :: c el -> c el
  tail = LL.tail

  -- |First index matching the predicate.
  findIndex :: (el -> Bool) -> c el -> Maybe Int
  findIndex = LL.findIndex

  -- |Split the data at the specified index.
  splitAt :: Int -> c el -> (c el, c el)
  splitAt = LL.splitAt

  -- |Drop data matching the predicate.
  dropWhile :: (el -> Bool) -> c el -> c el
  dropWhile = LL.dropWhile

  -- |Create a stream from a list.
  fromList :: [el] -> c el
  fromList = LL.fromList

  -- |Create a list from the stream.
  toList :: c el -> [el]
  toList = LL.toList

  -- |Map a computation over the stream.
  cMap :: (StreamChunk c el') => (el -> el') -> c el -> c el'
  cMap f = LL.foldr (LL.cons . f) LL.empty

instance StreamChunk [] el where
  cMap       = map

-- |Class of streams which can be filled from a 'Ptr'.  Typically these
-- are streams which can be read from a file.
-- The Int parameter is the length of the data in bytes.
-- N.B. The pointer must not be returned or used after readFromPtr completes.
class (StreamChunk s el, Storable el) => ReadableChunk s el where
  readFromPtr :: Ptr (el) -> Int -> IO (s el)

instance (Storable el) => ReadableChunk [] el where
  readFromPtr = flip peekArray
