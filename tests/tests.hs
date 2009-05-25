{-# OPTIONS_GHC -O #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import QCUtils

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

type I = IterateeG [] Int Identity [Int]

-- guard against null lists for head
prop_enumChunks n xs i = n > 0 && length xs > 0 ==>
  runner1 (enumPure1Chunk xs i) == runner1 (enumPureNChunk xs n i)
  where types = (n :: Int, xs :: [Int], i :: I)

-- guard against null lists for head
prop_app1 xs ys i = (length xs > 0 || length ys > 0) ==>
                    runner1 (enumPure1Chunk ys (joinIM $ enumPure1Chunk xs i))
                    == runner1 (enumPure1Chunk (xs ++ ys) i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

prop_app2 xs ys = runner1 ((enumPure1Chunk xs >. enumPure1Chunk ys) stream2list)
                  == runner1 (enumPure1Chunk (xs ++ ys) stream2list)
  where types = (xs :: [Int], ys :: [Int])

prop_app3 xs ys i = (length xs > 0 || length ys > 0) ==>
                    runner1 ((enumPure1Chunk xs >. enumPure1Chunk ys) i)
                    == runner1 (enumPure1Chunk (xs ++ ys) i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

prop_eof xs ys i = length xs > 0 ==>
                runner1 (enumPure1Chunk ys $ runIdentity $ (enumPure1Chunk xs >. enumEof) i)
                == runner1 (enumPure1Chunk xs i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

-- ---------------------------------------------
tests = [
  testGroup "Elementary" [
    testProperty "list" prop_list
    ,testProperty "chunkList" prop_clist],
  testGroup "Simple Iteratees" [
    testProperty "break" prop_break
    ,testProperty "break2" prop_break2
    ,testProperty "head" prop_head
    ,testProperty "head2" prop_head2
    ,testProperty "peek" prop_peek
    ,testProperty "peek2" prop_peek2
    ]
  ,testGroup "Simple Enumerators/Combinators" [
    testProperty "enumPureNChunk" prop_enumChunks
    ,testProperty "enum append 1" prop_app1
    ,testProperty "enum sequencing" prop_app2
    ,testProperty "enum sequencing 2" prop_app3
    ,testProperty "enumEof" prop_eof
    ]
  ]

------------------------------------------------------------------------
-- The entry point

main = defaultMain tests

