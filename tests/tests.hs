{-# OPTIONS_GHC -O #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import QCUtils

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

import Data.Iteratee hiding (head, break)
import qualified Data.Iteratee as Iter
import Control.Monad.Identity
import Data.Monoid

import Text.Printf (printf)
import System.Environment (getArgs)

instance Show (a -> b) where
  show _ = "<<function>>"

-- ---------------------------------------------
-- StreamG instances

type ST = StreamG [] Int

prop_mempty = mempty == (Chunk [] :: StreamG [] Int)

prop_mappend str1 str2 | isChunk str1 && isChunk str2 =
  str1 `mappend` str2 == Chunk (chunkData str1 ++ chunkData str2)
prop_mappend str1 str2 = isEOF $ str1 `mappend` str2
  where types = (str1 :: ST, str2 :: ST)

prop_functor str@(EOF _) f = isEOF $ fmap f str
prop_functor str@(Chunk xs) f = fmap f str == Chunk (fmap f xs)
  where types = (str :: ST, f :: Int -> Integer)

prop_mappend2 str = str `mappend` mempty == mempty `mappend` str
  where types = (str :: ST)


isChunk (Chunk _) = True
isChunk (EOF _)   = False

chunkData (Chunk xs) = xs

isEOF (EOF _)   = True
isEOF (Chunk _) = False


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

prop_skip xs = runner1 (enumPure1Chunk xs (skipToEof >> stream2list)) == []
  where types = xs :: [Int]

-- ---------------------------------------------
-- Simple enumerator tests

type I = IterateeG [] Int Identity [Int]

prop_enumChunks n xs i = n > 0  ==>
  runner1 (enumPure1Chunk xs i) == runner1 (enumPureNChunk xs n i)
  where types = (n :: Int, xs :: [Int], i :: I)

prop_app1 xs ys i = runner1 (enumPure1Chunk ys (joinIM $ enumPure1Chunk xs i))
                    == runner1 (enumPure1Chunk (xs ++ ys) i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

prop_app2 xs ys = runner1 ((enumPure1Chunk xs >. enumPure1Chunk ys) stream2list)
                  == runner1 (enumPure1Chunk (xs ++ ys) stream2list)
  where types = (xs :: [Int], ys :: [Int])

prop_app3 xs ys i = runner1 ((enumPure1Chunk xs >. enumPure1Chunk ys) i)
                    == runner1 (enumPure1Chunk (xs ++ ys) i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

prop_eof xs ys i = runner1 (enumPure1Chunk ys $ runIdentity $
                           (enumPure1Chunk xs >. enumEof) i)
                 == runner1 (enumPure1Chunk xs i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

prop_isFinished = runner1 (enumEof (isFinished :: IterateeG [] Int Identity (Maybe String))) == Just "EOF"

prop_isFinished2 = runner1 (enumErr "Error" (isFinished :: IterateeG [] Int Identity (Maybe String))) == Just "Error"

-- ---------------------------------------------
-- Nested Iteratees

-- take, mapStream, convStream, and takeR

runner2 = runIdentity . run . runner1

prop_mapStream xs i = runner2 (enumPure1Chunk xs $ mapStream id i)
                      == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])

prop_mapStream2 xs n i = n > 0 ==>
                         runner2 (enumPureNChunk xs n $ mapStream id i)
                         == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])

prop_mapjoin xs i =
  runIdentity (run (joinI . runIdentity $ enumPure1Chunk xs $ mapStream id i))
  == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])

prop_take xs n = n >= 0 ==>
                 runner2 (enumPure1Chunk xs $ Iter.take n stream2list)
                 == runner1 (enumPure1Chunk (Prelude.take n xs) stream2list)
  where types = (xs :: [Int])

prop_take2 xs n = n > 0 ==>
                  runner2 (enumPure1Chunk xs $ Iter.take n peek)
                  == runner1 (enumPure1Chunk (Prelude.take n xs) peek)
  where types = (xs :: [Int])

-- ---------------------------------------------
tests = [
  testGroup "Elementary" [
    testProperty "list" prop_list
    ,testProperty "chunkList" prop_clist]
  ,testGroup "StreamG tests" [
    testProperty "mempty" prop_mempty
    ,testProperty "mappend" prop_mappend
    ,testProperty "mappend associates" prop_mappend
    ,testProperty "functor" prop_functor
  ]
  ,testGroup "Simple Iteratees" [
    testProperty "break" prop_break
    ,testProperty "break2" prop_break2
    ,testProperty "head" prop_head
    ,testProperty "head2" prop_head2
    ,testProperty "peek" prop_peek
    ,testProperty "peek2" prop_peek2
    ,testProperty "skipToEof" prop_skip
    ]
  ,testGroup "Simple Enumerators/Combinators" [
    testProperty "enumPureNChunk" prop_enumChunks
    ,testProperty "enum append 1" prop_app1
    ,testProperty "enum sequencing" prop_app2
    ,testProperty "enum sequencing 2" prop_app3
    ,testProperty "enumEof" prop_eof
    ,testProperty "isFinished" prop_isFinished
    ,testProperty "isFinished error" prop_isFinished2
    ]
  ,testGroup "Nested iteratees" [
    testProperty "mapStream identity" prop_mapStream
    ,testProperty "mapStream identity 2" prop_mapStream2
    ,testProperty "mapStream identity joinI" prop_mapjoin
    ,testProperty "take" prop_take
    ,testProperty "take (finished iteratee)" prop_take
    ]
  ]

------------------------------------------------------------------------
-- The entry point

main = defaultMain tests

