module Main where

import Data.Iteratee
import Data.Iteratee.Char as C
import qualified Data.ByteString.Char8 as BC
import Data.Word
import System

numWords :: (Monad m, Functor m) => IterateeG String Char m Int
numWords = {-# SCC "numWords" #-} joinI $ enumWords2 C.length

numChars :: (Monad m) => IterateeG String Char m Int
numChars = C.length

main = do
  f:_ <- getArgs
  --words <- fileDriver (enumPair numWords numChars) f
  words <- fileDriver numWords f
  print words
