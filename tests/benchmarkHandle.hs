{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (null, length)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Criterion.Main
import Data.Word
import Data.Iteratee
import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.IO.Fd (fileDriverFd)
import Data.Iteratee.IO.Handle (fileDriverHandle)

bufSize = 65536
file = "/usr/share/dict/words"

length' :: Monad m => Iteratee ByteString m Int
length' = length

testFdString :: IO ()
testFdString = fileDriverFd bufSize len file >> return ()
  where
  len :: Monad m => Iteratee String m Int
  len = length

testFdByte :: IO ()
testFdByte = fileDriverFd bufSize len file >> return ()
  where
  len :: Monad m => Iteratee ByteString m Int
  len = length

testHdString :: IO ()
testHdString = fileDriverHandle bufSize len file >> return ()
  where
  len :: Monad m => Iteratee String m Int
  len = length

testHdByte :: IO ()
testHdByte = fileDriverHandle bufSize len file >> return ()
  where
  len :: Monad m => Iteratee ByteString m Int
  len = length

main = defaultMain
  [ bench "Fd with String" testFdString
  , bench "Hd with String" testHdString
  , bench "Fd with ByteString" testFdByte
  , bench "Hd with ByteString" testHdByte
  ]
