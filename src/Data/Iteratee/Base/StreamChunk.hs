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
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO

-- |Class of types that can be used to hold chunks of data within Iteratee
-- streams.
class StreamChunk c el where
  -- |Length of currently available data.
  length :: c el -> Int
  -- |Create an empty chunk of data.
  empty :: c el
  -- |Test if the current stream is null.
  null :: c el -> Bool
  -- |Prepend an element to the front of the data.
  cons :: el -> c el -> c el
  -- |Return the first element of the stream.
  head :: c el -> el
  -- |Return the tail of the stream.
  tail :: c el -> c el
  -- |First index matching the predicate.
  findIndex :: (el -> Bool) -> c el -> Maybe Int
  -- |Split the data at the specified index.
  splitAt :: Int -> c el -> (c el, c el)
  -- |Drop data matching the predicate.
  dropWhile :: (el -> Bool) -> c el -> c el
  -- |Append to chunks of data into one.
  append :: c el -> c el -> c el
  -- |Create a stream from a list.
  fromList :: [el] -> c el
  -- |Create a list from the stream.
  toList :: c el -> [el]
  -- |Map a computation over the stream.
  cMap :: (StreamChunk c' el') => (el -> el') -> c el -> c' el'

instance StreamChunk [] el where
  length    = P.length
  empty     = []
  null []   = True
  null _    = False
  cons      = (:)
  head      = P.head
  tail      = P.tail
  findIndex = L.findIndex
  splitAt   = P.splitAt
  dropWhile = P.dropWhile
  append    = (++)
  fromList   = id
  toList     = id
  cMap       = listmap

listmap :: (StreamChunk s' el') => (el -> el') -> [el] -> s' el'
listmap f = foldr (cons . f) empty

{-# RULES "listmap/map" listmap = map #-}

-- |Class of streams which can be filled from a 'Ptr'.  Typically these
-- are streams which can be read from a file.
class StreamChunk s el => ReadableChunk s el where
  readFromPtr :: Ptr (el) -> Int -> IO (s el)

instance (Storable el) => ReadableChunk [] el where
  readFromPtr = flip peekArray

