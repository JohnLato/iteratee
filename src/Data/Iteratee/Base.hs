{-# LANGUAGE FlexibleContexts #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base (
  -- * Types
  ErrMsg (..),
  StreamG (..),
  StreamStatus (..),
  strMap,
  -- * Error handling
  setEOF,
  -- * Classes
  module Data.Iteratee.Base.LooseMap
)
where

import Data.Iteratee.Base.LooseMap
import Data.Iteratee.IO.Base
import Data.Monoid


-- |A stream is a (continuing) sequence of elements bundled in Chunks.
-- The first two variants indicate termination of the stream.
-- Chunk a gives the currently available part of the stream.
-- The stream is not terminated yet.
-- The case (null Chunk) signifies a stream with no currently available
-- data but which is still continuing. A stream processor should,
-- informally speaking, ``suspend itself'' and wait for more data
-- to arrive.

data StreamG c =
  EOF (Maybe ErrMsg)
  | Chunk c

instance Eq c => Eq (StreamG c) where
  EOF mErr1 == EOF mErr2 = mErr1 == mErr2
  Chunk xs == Chunk ys   = xs == ys
  _ == _ = False

instance Show c => Show (StreamG c) where
  show (EOF mErr) = "StreamG: EOF " ++ show mErr
  show (Chunk xs) = "StreamG: Chunk " ++ show xs

instance Monoid c => Monoid (StreamG c) where
  mempty = Chunk mempty
  mappend (EOF mErr) _ = EOF mErr
  mappend _ (EOF mErr) = EOF mErr
  mappend (Chunk s1) (Chunk s2) = Chunk (s1 `mappend` s2)

-- |Map a function over a stream.
strMap :: (c -> c') -> StreamG c -> StreamG c'
strMap f (Chunk xs) = Chunk $ f xs
strMap _ (EOF mErr) = EOF mErr

-- |Describe the status of a stream of data.
data StreamStatus =
  DataRemaining
  | EofNoError
  | EofError ErrMsg
  deriving (Show, Eq)

data ErrMsg = Err String
              | Seek FileOffset
              deriving (Show, Eq)

instance Monoid ErrMsg where
  mempty = Err ""
  mappend (Err s1) (Err s2)  = Err (s1 ++ s2)
  mappend e@(Err _) _        = e
  mappend _        e@(Err _) = e
  mappend (Seek _) (Seek b)  = Seek b


-- |Produce the EOF error message.  If the stream was terminated because
-- of an error, keep the original error message.
setEOF :: StreamG c -> ErrMsg
setEOF (EOF (Just e)) = e
setEOF _              = Err "EOF"

