{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Iteratee
import Data.Iteratee.Codecs.Wave
import qualified Data.StorableVector as Vec
import qualified Data.IntMap as IM
import Data.Word (Word8)
import Control.Monad.Trans
import System

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: wave_reader FileName"
    filename:xs -> do
      putStrLn $ "Reading file: " ++ filename
      file_driver_rb (wave_reader >>= test) filename
      return ()

type V = Vec.Vector

test :: Maybe (IM.IntMap [WAVEDE]) -> IterateeGM V Word8 IO ()
test Nothing = lift $ putStrLn "No dictionary"
test (Just dict) = do
  fmtm <- dict_read_first_format dict
  lift . putStrLn $ show fmtm
  maxm <- dict_process_data 0 dict max_iter
  lift . putStrLn $ show maxm
  return ()

max_iter :: IterateeGM V Double IO Double
max_iter = m' 0
  where
  m' acc = liftI $ IE_cont (step acc)
  step acc (Chunk xs)
    | Vec.null xs  = m' acc
    | otherwise = m' $! Vec.foldl' (max . abs) acc xs
  step acc str = liftI $ IE_done acc str
