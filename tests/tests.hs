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

instance Show (a -> b) where
  show _ = "<<function>>"

-- ---------------------------------------------
-- List <-> Stream

runner1 = runIdentity . Iter.run . runIdentity

prop_list xs = runner1 (enumPure1Chunk xs stream2list) == xs
  where types = xs :: [Int]

prop_clist xs n = n > 0 ==> runner1 (enumPureNChunk xs n stream2list) == xs
  where types = xs :: [Int]

prop_break f xs = runner1 (enumPure1Chunk xs (Iter.break f)) == fst (break f xs)
  where types = xs :: [Int]

prop_break2 f xs = runner1 (enumPure1Chunk xs (Iter.break f >> stream2list)) == snd (break f xs)
  where types = xs :: [Int]

prop_head xs = length xs > 0 ==> runner1 (enumPure1Chunk xs Iter.head) == head xs
  where types = xs :: [Int]

prop_head2 xs = length xs > 0 ==> runner1 (enumPure1Chunk xs (Iter.head >> stream2list)) == tail xs
  where types = xs :: [Int]

prop_peek xs = runner1 (enumPure1Chunk xs peek) == sHead xs
  where
  types = xs :: [Int]
  sHead [] = Nothing
  sHead (x:_) = Just x

prop_peek2 xs = runner1 (enumPure1Chunk xs (peek >> stream2list)) == xs
  where types = xs :: [Int]

-- ---------------------------------------------
-- Simple enumerator tests


-- ---------------------------------------------
tests = [
  testGroup "Elementary" [
    testProperty "list" prop_list,
    testProperty "chunkList" prop_clist],
  testGroup "Simple Iteratees" [
    testProperty "break" prop_break,
    testProperty "break2" prop_break2,
    testProperty "head" prop_head,
    testProperty "head2" prop_head2,
    testProperty "peek" prop_peek,
    testProperty "peek2" prop_peek2
    ]
  ]

------------------------------------------------------------------------
-- The entry point

main = defaultMain tests

