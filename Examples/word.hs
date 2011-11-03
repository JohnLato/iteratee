{-# LANGUAGE BangPatterns #-}

-- A simple wc-like program using Data.Iteratee.
-- Demonstrates a few different ways of composing iteratees.
module Main where

import Prelude as P
import Data.Iteratee
import Data.Iteratee.Char as C
import qualified Data.Iteratee as I
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Data.Char
import Data.ListLike as LL
import System.Environment


-- | An iteratee to calculate the number of characters in a stream.
--   Very basic, assumes ASCII, not particularly efficient.
numChars :: (Monad m, ListLike s el) => I.Iteratee s m Int
numChars = I.length

-- | An iteratee to calculate the number of words in a stream of Word8's.
-- this operates on a Word8 stream in order to use ByteStrings.
--
-- This function converts the stream of Word8s into a stream of words,
-- then counts the words with Data.Iteratee.length
-- This is the equivalent of "length . BC.words".
numWords :: Monad m => I.Iteratee BC.ByteString m Int
numWords = I.joinI $ enumWordsBS I.length

-- | Count the number of lines, in the same manner as numWords.
numLines :: Monad m => I.Iteratee BC.ByteString m Int
numLines = I.joinI $ enumLinesBS I.length

-- | A much more efficient numLines using the foldl' iteratee.
-- Rather than converting a stream, this simply counts newline characters.
numLines2 :: Monad m => I.Iteratee BC.ByteString m Int
numLines2 = I.foldl' step 0
 where
  step !acc el = if el == (fromIntegral $ ord '\n') then acc + 1 else acc

-- | Combine multiple iteratees into a single unit using "enumPair".
-- The iteratees combined with enumPair are run in parallel.
-- Any number of iteratees can be joined with multiple enumPair's.
twoIter :: Monad m => I.Iteratee BC.ByteString m (Int, Int)
twoIter = numLines2 `I.zip` numChars

main = do
  f:_ <- getArgs
  words <- fileDriverVBuf 65536 twoIter f
  print words
