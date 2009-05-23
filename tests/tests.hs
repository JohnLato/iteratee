{-# OPTIONS_GHC -O #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.Iteratee hiding (head, break)
import qualified Data.Iteratee as Iter
import Control.Monad.Identity

import Text.Printf (printf)
import System.Environment (getArgs)

-- ---------------------------------------------
-- List <-> Stream

runner1 = runIdentity . Iter.run . runIdentity
runner2 = runIdentity . Iter.run

prop_list xs = runner1 (enumPure1Chunk xs stream2list) == xs
  where types = xs :: [Int]

prop_clist xs n = n > 0 ==> runner1 (enumPureNChunk xs n stream2list) == xs
  where types = xs :: [Int]

prop_breakAll xs = runner1 (enumPure1Chunk xs (Iter.break (const True))) == []
  where types = xs :: [Int]

prop_breakNone xs = runner1 (enumPure1Chunk xs (Iter.break (const False))) == xs
  where types = xs :: [Int]

prop_break xs = runner1 (enumPure1Chunk xs (Iter.break (<= 10))) == fst (break (<= 10) xs)
  where types = xs :: [Int]

prop_head [] = True
prop_head xs = runner1 (enumPure1Chunk xs Iter.head) == head xs
  where types = xs :: [Int]

prop_peek xs = runner1 (enumPure1Chunk xs peek) == sHead xs
  where
  types = xs :: [Int]
  sHead [] = Nothing
  sHead (x:_) = Just x

-- ---------------------------------------------
tests = [
  testGroup "Elementary" [
    testProperty "list" prop_list,
    testProperty "chunkList" prop_clist],
  testGroup "Simple Iteratees" [
    testProperty "breakAll" prop_breakAll,
    testProperty "breakNone" prop_breakNone,
    testProperty "break" prop_break,
    testProperty "head" prop_head,
    testProperty "peek" prop_peek
    ]
  ]

------------------------------------------------------------------------
-- The entry point

main = defaultMain tests

