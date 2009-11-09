-- A simple wc-like program using Data.Iteratee
module Main where

import Prelude as P
import Data.Iteratee
import Data.Iteratee.Char as C
import qualified Data.ByteString.Char8 as BC
import Data.Word
import Data.ListLike as LL
import System


-- An iteratee to calculate the number of characters in a stream.  Very basic.
numChars :: (ListLike s el, Monad m) => IterateeT s el m Int
numChars = C.length

-- An iteratee to calculate the number of words in a stream of Word8's.
-- this operates on a Word8 stream because that's the only way to use
-- ByteString's, however each word in the stream produced by enumWordsBS
-- is a (strict) bytestring that can be used with Data.ByteString.Char8.
numWords :: (Monad m, Functor m) => IterateeT BC.ByteString Word8 m Int
numWords = joinI $ enumWordsTBS C.length

-- This function uses enumWords, which operates on lists of Chars.
-- It's a bit slower than using ByteStrings.
numWordsL :: (Monad m, Functor m) => IterateeT String Char m Int
numWordsL = joinI $ enumWords C.length

-- Count the number of lines, similar to numWords
numLines :: (Monad m, Functor m) => IterateeT BC.ByteString Word8 m Int
numLines = joinI $ enumLinesTBS C.length

main = do
  f:_ <- getArgs
  words <- fileDriver (numLines `enumPair` numWords `enumPair` numChars) f
  print words
