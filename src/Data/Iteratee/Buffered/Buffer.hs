module Data.Iteratee.Buffered.Buffer (
  Buffer
  ,makeBuffer
  ,nullBuffer
  ,emptyBuffer
  ,length
  ,pop
  ,lookAtHead
  ,drop
  ,take
)

where

import Prelude hiding (length, take, drop)

import Data.Iteratee.Base

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Utils
import Foreign.Marshal.Array
import System.IO.Unsafe

import Control.Monad

-- |A mutable buffer to hold storable elements.  This data type supports
-- memory recycling.
data Buffer s el = Buffer Int (Ptr Int) (Ptr el)

instance (Storable el) => NullPoint (Buffer s el) where
  empty = emptyBuffer

-- |Create a buffer from a length and data array.
makeBuffer :: Storable el => Int -> Ptr el -> IO (Buffer s el)
makeBuffer len buf = do
  off <- new 0
  return $ Buffer len off buf

-- |Empty buffer.
emptyBuffer :: Storable el => Buffer s el
emptyBuffer = Buffer 0 nullPtr nullPtr

-- |Check if the buffer has data.
nullBuffer :: Buffer s el -> IO Bool
nullBuffer (Buffer 0 _  _) = return True
nullBuffer (Buffer l po _) = liftM (>= l) $ peek po

-- |Buffer length.
length :: Buffer s el -> IO Int
length (Buffer l po _) = liftM (l -) $ peek po

-- |Retrieve the front element from the buffer and advance the internal pointer.
-- It is an error to call this on an empty buffer.
pop :: Storable el => Buffer s el -> IO el
pop (Buffer 0 _  _ ) = error "Can't pop head off of empty buffer"
pop (Buffer l po pb) = do
  off <- peek po
  if off >= l then error "Can't pop head from empty buffer"
              else poke po (off+1) >> peek (pb `advancePtr` off)

-- |Retrieve the first element, if it exists.
-- This function does not advance the buffer pointer.
lookAtHead :: Storable el => Buffer s el -> IO (Maybe el)
lookAtHead (Buffer 0 _  _ ) = return Nothing
lookAtHead (Buffer l po pb) = do
  off <- peek po
  if off >= l then return Nothing
              else liftM Just $ peek (pb `advancePtr` off)

-- |Drop n elements from the front of the buffer.
-- if the buffer has fewer elements, all are dropped.
drop :: Buffer s el -> Int -> IO ()
drop (Buffer l po pb) n_drop = do
  off <- peek po
  poke po (max l (off+n_drop))

-- |Create a new buffer from the first n elements, sharing data.
-- This function advances the pointer of the original buffer.
take :: Storable el => Buffer s el -> Int -> IO (Buffer s el)
take (Buffer 0 _  _ ) _      = return emptyBuffer
take (Buffer l po pb) n_take = do
  off <- peek po
  let off' = max l (off+n_take)
  po' <- new off
  poke po off'
  return $ Buffer l po' pb
