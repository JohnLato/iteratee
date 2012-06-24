{-# LANGUAGE BangPatterns #-}

module BenchIO where

import Prelude hiding (null, length)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Criterion.Main
import Data.Monoid
import Data.Word
import Data.Iteratee
import Data.Iteratee.Parallel
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

testFdMapReduce :: Int -> IO ()
testFdMapReduce n = fileDriverFd bufSize sum file >> return ()
 where
  sum :: Iteratee ByteString IO Word8
  sum = getSum `fmap` mapReduce n (Sum . B.foldl' (+) 0)

testFdFold :: IO ()
testFdFold = fileDriverFd bufSize sum file >> return ()
 where
  sum :: Iteratee ByteString IO Word8
  sum = foldl' (+) 0

main = defaultMain (stringIO ++ ioBenches)

stringIO =
  [ bgroup "String"
    [bench "Fd" testFdString
    ,bench "Hd with String" testHdString
    ]
  ]

ioBenches =
  [bgroup "ByteString" [
     bench "Fd" testFdByte
    ,bench "Hd" testHdByte
   ]

  ,bgroup "folds" [
     bench "Fd/fold" testFdFold
    ,bench "Fd/mapReduce 2" $ testFdMapReduce 2
    ,bench "Fd/mapReduce 4" $ testFdMapReduce 4
    ,bench "Fd/mapReduce 8" $ testFdMapReduce 8
   ]
  ]
