-- Read a wave file and return some information about it.

{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Iteratee
import Data.Iteratee.Codecs.Wave
import qualified Data.IntMap as IM
import Data.List (foldl')
import Data.Word (Word8)
import Control.Monad.Trans
import System

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: wave_reader FileName"
    fname:xs -> do
      putStrLn $ "Reading file: " ++ fname
      fileDriverRandom (waveReader >>= test) fname
      return ()

-- Use the collection of [WAVEDE] returned from wave_reader to
-- do further processing.  The IntMap has an entry for each type of chunk
-- in the wave file.  Read the first format chunk and disply the 
-- format information, then use the dict_process_data function
-- to enumerate over the maxIter iteratee to find the maximum value
-- (peak amplitude) in the file.
test :: Maybe (IM.IntMap [WAVEDE]) -> IterateeG [] Word8 IO ()
test Nothing = lift $ putStrLn "No dictionary"
test (Just dict) = do
  fmtm <- dictReadFirstFormat dict
  lift . putStrLn $ show fmtm
  maxm <- dictProcessData 0 dict maxIter
  lift . putStrLn $ show maxm
  return ()

-- an iteratee that calculates the maximum value found so far.
-- this could be written with head as well, however it is more
-- efficient to operate on an entire chunk at once.
maxIter :: IterateeG [] Double IO Double
maxIter = m 0
  where
  m n = IterateeG (step n)
  step acc (Chunk []) = return $ Cont (m acc) Nothing
  step acc (Chunk xs) = return $ Cont (m $! foldl' (max . abs) acc xs) Nothing
  step acc str = return $ Done acc str

