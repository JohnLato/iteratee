-- A simple wc-like program using Data.Iteratee
module Main where

import Prelude as P
import Data.Iteratee
import Data.Iteratee.Char as C
import qualified Data.Iteratee.Iteratee as I
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Data.ListLike as LL
import System


-- An iteratee to calculate the number of characters in a stream.  Very basic.
numChars :: (ListLike s el) => I.Iteratee s el Int
numChars = I.length

-- An iteratee to calculate the number of words in a stream of Word8's.
-- this operates on a Word8 stream because that's the only way to use
-- ByteString's, however each word in the stream produced by enumWordsBS
-- is a (strict) bytestring that can be used with Data.ByteString.Char8.
numWords :: I.Iteratee BC.ByteString Word8 Int
numWords = I.joinI $ enumWordsBS I.length

-- Count the number of lines, similar to numWords
numLines :: I.Iteratee BC.ByteString Word8 Int
numLines = I.joinI $ enumLinesBS I.length

allIter = numLines `I.enumPar` numWords `I.enumPar` numChars

allIter2 = numLines `I.enumPar` numWords `I.enumPar` numWords `I.enumPar` numChars `I.enumPar` numWords

main = do
  f:_ <- getArgs
  words <- fileDriver (liftIteratee allIter2) f
  print words
