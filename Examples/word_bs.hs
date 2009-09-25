-- A simple wc-like program using the obvious bytestring implementation

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import System
import System.IO


main = do
  f:_ <- getArgs
  h <- openFile f ReadMode
  bw <- BL.hGetContents h
  print . length . BL.words $ bw
