-- A simple wc-like program using Data.Iteratee
module Main where

import Data.Iteratee
import Data.Iteratee.Char as C
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Data.ListLike
import System


-- An iteratee to calculate the number of characters in a stream.  Very basic.
numChars :: (ListLike s el, Monad m) => IterateeG s el m Int
numChars = C.length

-- An iteratee to calculate the number of words in a stream of Word8's.
-- this operates on a Word8 stream because that's the only way to use
-- ByteString's, however each word in the stream produced by enumWordsBS
-- is a (strict) bytestring that can be used with Data.ByteString.Char8.
numWords :: (Monad m, Functor m) => IterateeG BC.ByteString Word8 m Int
numWords = joinI $ enumWordsBS C.length

-- This function uses enumWords, which operates on lists of Chars.
-- It's a bit slower than using ByteStrings.
numWordsL :: (Monad m, Functor m) => IterateeG String Char m Int
numWordsL = joinI $ enumWords C.length

main = do
  f:_ <- getArgs
  words <- fileDriver (numWords `enumPair` numChars) f
  print words
