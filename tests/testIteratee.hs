{-# OPTIONS_GHC -O #-}
{-# LANGUAGE NoMonomorphismRestriction, ViewPatterns, TupleSections #-}

import Prelude as P

import QCUtils

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit
import Test.QuickCheck

import           Data.Iteratee hiding (head, break)
import qualified Data.Iteratee.Char as IC
import qualified Data.Iteratee as Iter
import           Data.Functor.Identity
import qualified Data.List as List (groupBy, unfoldr)
import           Data.Monoid
import qualified Data.ListLike as LL

import           Control.Monad as CM
import           Control.Monad.Writer
import           Control.Exception (SomeException)

instance Show (a -> b) where
  show _ = "<<function>>"

-- ---------------------------------------------
-- Stream instances

type ST = Stream [Int]

prop_eq str = str == str
  where types = str :: ST

prop_mempty = mempty == (Chunk [] :: Stream [Int])

prop_mappend str1 str2 | isChunk str1 && isChunk str2 =
  str1 `mappend` str2 == Chunk (chunkData str1 ++ chunkData str2)
prop_mappend str1 str2 = isEOF $ str1 `mappend` str2
  where types = (str1 :: ST, str2 :: ST)

prop_mappend2 str = str `mappend` mempty == mempty `mappend` str
  where types = str :: ST

isChunk (Chunk _) = True
isChunk (EOF _)   = False

chunkData (Chunk xs) = xs

isEOF (EOF _)   = True
isEOF (Chunk _) = False

-- ---------------------------------------------
-- Iteratee instances

runner1 = runIdentity . Iter.run . runIdentity
enumSpecial xs n = enumPure1Chunk LL.empty >=> enumPureNChunk xs n

prop_iterFmap xs f a = runner1 (enumPure1Chunk xs (fmap f $ return a))
                     == runner1 (enumPure1Chunk xs (return $ f a))
  where types = (xs :: [Int], f :: Int -> Int, a :: Int)

prop_iterFmap2 xs f i = runner1 (enumPure1Chunk xs (fmap f i))
                      == f (runner1 (enumPure1Chunk xs i))
  where types = (xs :: [Int], i :: I, f :: [Int] -> [Int])

prop_iterMonad1 xs a f = runner1 (enumSpecial xs 1 (return a >>= f))
                       == runner1 (enumPure1Chunk xs (f a))
  where types = (xs :: [Int], a :: Int, f :: Int -> I)

prop_iterMonad2 m xs = runner1 (enumSpecial xs 1 (m >>= return))
                     == runner1 (enumPure1Chunk xs m)
  where types = (xs :: [Int], m :: I)

prop_iterMonad3 m f g xs = runner1 (enumSpecial xs 1 ((m >>= f) >>= g))
                         == runner1 (enumPure1Chunk xs (m >>= (\x -> f x >>= g)))
  where types = (xs :: [Int], m :: I, f :: [Int] -> I, g :: [Int] -> I)

-- ---------------------------------------------
-- List <-> Stream

prop_list xs = runner1 (enumPure1Chunk xs stream2list) == xs
  where types = xs :: [Int]

prop_clist xs n = n > 0 ==> runner1 (enumSpecial xs n stream2list) == xs
  where types = xs :: [Int]

prop_break f xs = runner1 (enumPure1Chunk xs (Iter.break f)) == fst (break f xs)
  where types = xs :: [Int]

prop_break2 f xs = runner1 (enumPure1Chunk xs (Iter.break f >> stream2list)) == snd (break f xs)
  where types = xs :: [Int]

prop_breakE f xs = runner1 (enumPure1Chunk xs (joinI $ Iter.breakE f stream2stream)) == fst (break f xs)
  where types = xs :: [Int]

prop_breakE2 f xs = runner1 (enumPure1Chunk xs (joinI (Iter.breakE f stream2stream) >> stream2list)) == snd (break f xs)
  where types = xs :: [Int]


prop_head xs = P.length xs > 0 ==> runner1 (enumPure1Chunk xs Iter.head) == head xs
  where types = xs :: [Int]

prop_head2 xs = P.length xs > 0 ==> runner1 (enumPure1Chunk xs (Iter.head >> stream2list)) == tail xs
  where types = xs :: [Int]

prop_tryhead xs = case xs of
  [] -> runner1 (enumPure1Chunk xs tryHead) == Nothing
  _  -> runner1 (enumPure1Chunk xs tryHead) == Just (P.head xs)
  where types = xs :: [Int]

prop_heads xs n = n > 0 ==>
 runner1 (enumSpecial xs n $ heads xs) == P.length xs
  where types = xs :: [Int]

prop_heads2 xs = runner1 (enumPure1Chunk xs $ heads [] >>= \c ->
                          stream2list >>= \s -> return (c,s))
                 == (0, xs)
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

prop_last1 xs = P.length xs > 0 ==>
 runner1 (enumPure1Chunk xs (Iter.last)) == P.last xs
  where types = xs :: [Int]

prop_last2 xs = P.length xs > 0 ==>
 runner1 (enumPure1Chunk xs (Iter.last >> Iter.peek)) == Nothing
  where types = xs :: [Int]

prop_drop xs n k = (n > 0 && k >= 0) ==>
 runner1 (enumSpecial xs n (Iter.drop k >> stream2list)) == P.drop k xs
  where types = xs :: [Int]

prop_dropWhile f xs =
 runner1 (enumPure1Chunk xs (Iter.dropWhile f >> stream2list))
 == P.dropWhile f xs
  where types = (xs :: [Int], f :: Int -> Bool)

prop_length xs = runner1 (enumPure1Chunk xs Iter.length) == P.length xs
  where types = xs :: [Int]

-- length 0 is an odd case.  enumPureNChunk skips null inputs, returning
-- the original iteratee, which is then given to `enumEof` by `run`.
-- This is different from enumPure1Chunk, which will provide a null chunk
-- to the iteratee.
-- 
-- not certain ATM which should be correct...
prop_chunkLength xs n = n > 0 ==>
  runner1 (enumPureNChunk xs n (liftM2 (,) chunkLength stream2list))
  == case P.length xs of
       0              -> (Nothing, xs)
       xl | xl >= n   -> (Just n, xs)
          | otherwise -> (Just (P.length xs), xs)
 where types = xs :: [Int]

prop_chunkLength2 xs =
  runner1 ((enumEof >=> enumPure1Chunk xs) chunkLength) == Nothing
 where types = xs :: [Int]

prop_takeFromChunk xs n k = n > 0 ==>
  runner1 (enumPureNChunk xs n (liftM2 (,) (takeFromChunk k) stream2list))
  == if k > n then splitAt n xs else splitAt k xs
 where types = xs :: [Int]

-- ---------------------------------------------
-- Simple enumerator tests

type I = Iteratee [Int] Identity [Int]

prop_enumChunks n xs i = n > 0  ==>
  runner1 (enumPure1Chunk xs i) == runner1 (enumSpecial xs n i)
  where types = (n :: Int, xs :: [Int], i :: I)

prop_app1 xs ys i = runner1 (enumPure1Chunk ys (joinIM $ enumPure1Chunk xs i))
                    == runner1 (enumPure1Chunk (xs ++ ys) i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

prop_app2 xs ys = runner1 ((enumPure1Chunk xs >>> enumPure1Chunk ys) stream2list)
                  == runner1 (enumPure1Chunk (xs ++ ys) stream2list)
  where types = (xs :: [Int], ys :: [Int])

prop_app3 xs ys i = runner1 ((enumPure1Chunk xs >>> enumPure1Chunk ys) i)
                    == runner1 (enumPure1Chunk (xs ++ ys) i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

prop_eof xs ys i = runner1 (enumPure1Chunk ys $ runIdentity $
                           (enumPure1Chunk xs >>> enumEof) i)
                 == runner1 (enumPure1Chunk xs i)
  where types = (xs :: [Int], ys :: [Int], i :: I)

prop_isFinished = runner1 (enumEof (isFinished :: Iteratee [Int] Identity Bool)) == True

prop_isFinished2 = runner1 (enumErr (iterStrExc "Error") (isFinished :: Iteratee [Int] Identity Bool)) == True

prop_null xs i = runner1 (enumPure1Chunk xs =<< enumPure1Chunk [] i)
                 == runner1 (enumPure1Chunk xs i)
  where types = (xs :: [Int], i :: I)

prop_nullH xs = P.length xs > 0 ==>
                runner1 (enumPure1Chunk xs =<< enumPure1Chunk [] Iter.head)
                == runner1 (enumPure1Chunk xs Iter.head)
  where types = xs :: [Int]

prop_enumList xs i =
  not (P.null xs) ==>
  runner1 (enumList (replicate 100 xs) i)
  == runner1 (enumPure1Chunk (concat $ replicate 100 xs) i)
 where types = (xs :: [Int], i :: I)

prop_enumCheckIfDone xs i =
   runner1 (enumPure1Chunk xs (lift (enumCheckIfDone i) >>= snd))
   == runner1 (enumPure1Chunk xs i)
 where types = (xs :: [Int], i :: I)

-- ---------------------------------------------
-- Enumerator Combinators

prop_enumWith xs f n = n > 0 ==> runner1 (enumSpecial xs n $ fmap fst $ enumWith (Iter.dropWhile f) (stream2list))
   == runner1 (enumSpecial xs n $ Iter.dropWhile f)
 where types = (xs :: [Int])

prop_enumWith2 xs f n = n > 0 ==> runner1 (enumSpecial xs n $ enumWith (Iter.dropWhile f) (stream2list) >> stream2list)
   == runner1 (enumSpecial xs n $ Iter.dropWhile f >> stream2list)
 where types = (xs :: [Int])

prop_enumWith3 xs i n =
   n > 0
   ==> runner1 (enumSpecial xs n $ enumWith i stream2list >> stream2list)
   ==  runner1 (enumSpecial xs n (i >> stream2list))
 where types = (xs :: [Int], i :: I)

prop_countConsumed (Positive (min (2^10) -> n)) (Positive (min (2^20) -> a)) (Positive k) =
            runner1 (enumPureNChunk [1..] n iter) == (a, a)
  where
    iter = countConsumed . joinI $ (takeUpTo (a + k) ><> Iter.take a) Iter.last

-- ---------------------------------------------
-- Nested Iteratees

-- take, mapStream, convStream, and takeR

runner2 = runIdentity . run . runner1

prop_mapStream xs i = runner2 (enumPure1Chunk xs $ mapStream id i)
                      == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])

prop_mapStream2 xs n i = n > 0 ==>
                         runner2 (enumSpecial xs n $ mapStream id i)
                         == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])

prop_mapjoin xs i =
  runIdentity (run (joinI . runIdentity $ enumPure1Chunk xs $ mapStream id i))
  == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])

prop_rigidMapStream xs n f = n > 0 ==>
 runner2 (enumSpecial xs n $ rigidMapStream f stream2list) == map f xs
  where types = (xs :: [Int])

prop_foldl xs n f x0 = n > 0 ==>
 runner1 (enumSpecial xs n (Iter.foldl f x0)) == P.foldl f x0 xs
  where types = (xs :: [Int], x0 :: Int)

prop_foldl' xs n f x0 = n > 0 ==>
 runner1 (enumSpecial xs n (Iter.foldl' f x0)) == LL.foldl' f x0 xs
  where types = (xs :: [Int], x0 :: Int)

prop_foldl1 xs n f = (n > 0 && not (null xs)) ==>
 runner1 (enumSpecial xs n (Iter.foldl1 f)) == P.foldl1 f xs
  where types = (xs :: [Int])

prop_foldl1' xs n f = (n > 0 && not (null xs)) ==>
 runner1 (enumSpecial xs n (Iter.foldl1' f)) == P.foldl1 f xs
  where types = (xs :: [Int])

prop_sum xs n = n > 0 ==> runner1 (enumSpecial xs n Iter.sum) == P.sum xs
  where types = (xs :: [Int])

prop_product xs n = n > 0 ==>
 runner1 (enumSpecial xs n Iter.product) == P.product xs
  where types = (xs :: [Int])

convId :: (LL.ListLike s el, Monad m) => Iteratee s m s
convId = liftI (\str -> case str of
  s@(Chunk xs) | LL.null xs -> convId
  s@(Chunk xs) -> idone xs (Chunk mempty)
  s@(EOF e)    -> idone mempty (EOF e)
  )

prop_convId xs = runner1 (enumPure1Chunk xs convId) == xs
  where types = xs :: [Int]

prop_convstream xs i = P.length xs > 0 ==>
                       runner2 (enumPure1Chunk xs $ convStream convId i)
                       == runner1 (enumPure1Chunk xs i)
  where types = (xs :: [Int], i :: I)

prop_convstream2 xs = P.length xs > 0 ==>
                      runner2 (enumPure1Chunk xs $ convStream convId Iter.head)
                      == runner1 (enumPure1Chunk xs Iter.head)
  where types = xs :: [Int]

prop_convstream3 xs = P.length xs > 0 ==>
                      runner2 (enumPure1Chunk xs $ convStream convId stream2list)
                      == runner1 (enumPure1Chunk xs stream2list)
  where types = xs :: [Int]

prop_take xs n = n >= 0 ==>
                 runner2 (enumPure1Chunk xs $ Iter.take n stream2list)
                 == runner1 (enumPure1Chunk (P.take n xs) stream2list)
  where types = xs :: [Int]

prop_take2 xs n = n > 0 ==>
                  runner2 (enumPure1Chunk xs $ Iter.take n peek)
                  == runner1 (enumPure1Chunk (P.take n xs) peek)
  where types = xs :: [Int]

prop_takeUpTo xs n = n >= 0 ==>
                  runner2 (enumPure1Chunk xs $ Iter.take n stream2list)
                  == runner2 (enumPure1Chunk xs $ takeUpTo n stream2list)
  where types = xs :: [Int]

prop_takeUpTo2 xs n = n >= 0 ==>
 runner2 (enumPure1Chunk xs (takeUpTo n identity)) == ()
  where types = xs :: [Int]

-- check for final stream state
prop_takeUpTo3 xs n d t = n > 0 ==>
 runner1 (enumPureNChunk xs n (joinI (takeUpTo t (Iter.drop d)) >> stream2list))
 == P.drop (min t d) xs
  where types = xs :: [Int]

prop_takeWhile xs n f = n > 0 ==>
  runner1 (enumSpecial xs n (liftM2 (,) (Iter.takeWhile f) stream2list))
  == (P.takeWhile f xs, P.dropWhile f xs)
 where types = xs :: [Int]

prop_filter xs n f = n > 0 ==>
 runner2 (enumSpecial xs n (Iter.filter f stream2list)) == P.filter f xs
  where types = xs :: [Int]

prop_group xs n = n > 0 ==>
                  runner2 (enumPure1Chunk xs $ Iter.group n stream2list)
                  == runner1 (enumPure1Chunk groups stream2list)
  where types = xs :: [Int]
        groups :: [[Int]]
        groups = List.unfoldr groupOne xs
          where groupOne [] = Nothing
                groupOne elts@(_:_) = Just . splitAt n $ elts
                           
prop_groupBy xs = forAll (choose (2,5)) $ \m ->
  let pred z1 z2 = (z1 `mod` m == z2 `mod` m)
  in runner2 (enumPure1Chunk xs $ Iter.groupBy pred stream2list)
       == runner1 (enumPure1Chunk (List.groupBy pred xs) stream2list)
  where types = xs :: [Int]

prop_mapChunksM xs n = n > 0 ==>
 runWriter ((enumSpecial xs n (joinI $ Iter.mapChunksM f stream2list)) >>= run)
 == (xs, Sum (P.length xs))
  where f ck = tell (Sum $ P.length ck) >> return ck
        types = xs :: [Int]
{-
prop_mapjoin xs i =
  runIdentity (run (joinI . runIdentity $ enumPure1Chunk xs $ mapStream id i))
  == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])
-}

prop_mapChunksM_ xs n = n > 0 ==>
 snd (runWriter ((enumSpecial xs n (Iter.mapChunksM_ f)) >>= run))
 == Sum (P.length xs)
  where f ck = tell (Sum $ P.length ck)
        types = xs :: [Int]

prop_mapM_ xs n = n > 0 ==>
 runWriter ((enumSpecial xs n (Iter.mapM_ f)) >>= run)
 == runWriter (CM.mapM_ f xs)
  where f = const $ tell (Sum 1)
        types = xs :: [Int]

prop_foldChunksM xs x0 n = n > 0 ==>
 runWriter ((enumSpecial xs n (Iter.foldChunksM f x0)) >>= run)
 == runWriter (f x0 xs)
  where f acc ck = CM.foldM f' acc ck
        f' acc el = tell (Sum 1) >> return (acc+el)
        types = xs :: [Int]

prop_foldM xs x0 n = n > 0 ==>
 runWriter ((enumSpecial xs n (Iter.foldM f x0)) >>= run)
 == runWriter (CM.foldM f x0 xs)
  where f acc el = tell (Sum 1) >> return (acc - el)
        types = xs :: [Int]
-- ---------------------------------------------
-- Zips

prop_zip xs i1 i2 n = n > 0 ==>
  runner1 (enumPureNChunk xs n $ liftM2 (,) (Iter.zip i1 i2) stream2list)
  == let (r1, t1) = runner1 $ enumPure1Chunk xs $ liftM2 (,) i1 stream2list
         (r2, t2) = runner1 $ enumPure1Chunk xs $ liftM2 (,) i2 stream2list
         shorter = if P.length t1 > P.length t2 then t2 else t1
     in ((r1,r2), shorter)
 where types = (i1 :: I, i2 :: I, xs :: [Int])

-- ---------------------------------------------
-- Sequences

test_sequence_ =
  assertEqual "sequence_: no duplicate runs" ((),[4,5])
              (runWriter (Iter.enumList [[4],[5::Int]] (Iter.sequence_ [iter])
                          >>= run))
 where
  iter = do
    x <- Iter.head
    lift $ tell [x]
    y <- Iter.head
    lift $ tell [y]

-- ---------------------------------------------
-- Data.Iteratee.PTerm

mk_prop_pt_id etee p_etee i xs n = n > 0 ==>
  runner1 (enumSpecial xs n $ joinI (p_etee i))
  == runner1 (enumSpecial xs n $ joinI (etee i))
 where types = (etee, p_etee, i, xs) :: (Etee, Etee, Itee, [Int])

instance Eq SomeException where
  l == r = show l == show r

type Etee = Enumeratee [Int] [Int] Identity [Int]
type Itee = Iteratee [Int] Identity [Int]

prop_mapChunksPT f i = mk_prop_pt_id (mapChunks f) (mapChunksPT f)
 where types = (i :: Itee)

prop_mapChunksMPT f i =
  mk_prop_pt_id (mapChunksM (return . f)) (mapChunksMPT (return . f))
 where types = (i :: Itee)

-- would like to test with arbitrary iteratees, but we need to guarantee
-- that they will return a value from the stream, which isn't always true
-- for the arbitrary instance.
-- could use a newtype to make it work...
prop_convStreamPT = mk_prop_pt_id (convStream getChunk) (convStreamPT getChunk)

prop_unfoldConvStreamPT f =
  mk_prop_pt_id (unfoldConvStream f' (0 :: Int)) (unfoldConvStreamPT f' 0)
 where f' x = fmap (f x,) getChunk

prop_breakEPT i = mk_prop_pt_id (breakE i) (breakEPT i)
prop_takePT i = mk_prop_pt_id (Iter.take i) (takePT i)
prop_takeUpToPT i = mk_prop_pt_id (Iter.takeUpTo i) (takeUpToPT i)
prop_takeWhileEPT i = mk_prop_pt_id (Iter.takeWhileE i) (takeWhileEPT i)

prop_mapStreamPT i = mk_prop_pt_id (Iter.mapStream i) (mapStreamPT i)
prop_rigidMapStreamPT i =
  mk_prop_pt_id (Iter.rigidMapStream i) (rigidMapStreamPT i)
prop_filterPT i = mk_prop_pt_id (Iter.filter i) (filterPT i)


-- ---------------------------------------------
-- Data.Iteratee.Char

{-
-- this isn't true, since lines "\r" returns ["\r"], and IC.line should
-- return Right "".  Not sure what a real test would be...
prop_line xs = P.length xs > 0 ==>
               fromEither (runner1 (enumPure1Chunk xs $ IC.line))
               == head (lines xs)
  where
  types = xs :: [Char]
  fromEither (Left l)  = l
  fromEither (Right l) = l
-}

-- ---------------------------------------------
tests = [
  testGroup "Elementary" [
    testProperty "list" prop_list
    ,testProperty "chunkList" prop_clist]
  ,testGroup "Stream tests" [
    testProperty "mempty" prop_mempty
    ,testProperty "mappend" prop_mappend
    ,testProperty "mappend associates" prop_mappend2
    ,testProperty "eq" prop_eq
  ]
  ,testGroup "Simple Iteratees" [
    testProperty "break" prop_break
    ,testProperty "break remainder" prop_break2
    ,testProperty "head" prop_head
    ,testProperty "head remainder" prop_head2
    ,testProperty "tryhead" prop_tryhead
    ,testProperty "heads" prop_heads
    ,testProperty "null heads" prop_heads2
    ,testProperty "peek" prop_peek
    ,testProperty "peek2" prop_peek2
    ,testProperty "last" prop_last1
    ,testProperty "last ends properly" prop_last2
    ,testProperty "length" prop_length
    ,testProperty "chunkLength" prop_chunkLength
    ,testProperty "chunkLength of EoF" prop_chunkLength2
    ,testProperty "takeFromChunk" prop_takeFromChunk
    ,testProperty "drop" prop_drop
    ,testProperty "dropWhile" prop_dropWhile
    ,testProperty "skipToEof" prop_skip
    ,testProperty "iteratee Functor 1" prop_iterFmap
    ,testProperty "iteratee Functor 2" prop_iterFmap2
    ,testProperty "iteratee Monad LI" prop_iterMonad1
    ,testProperty "iteratee Monad RI" prop_iterMonad2
    ,testProperty "iteratee Monad Assc" prop_iterMonad3
    ]
  ,testGroup "Simple Enumerators/Combinators" [
    testProperty "enumPureNChunk" prop_enumChunks
    ,testProperty "enum append 1" prop_app1
    ,testProperty "enum sequencing" prop_app2
    ,testProperty "enum sequencing 2" prop_app3
    ,testProperty "enumEof" prop_eof
    ,testProperty "isFinished" prop_isFinished
    ,testProperty "isFinished error" prop_isFinished2
    ,testProperty "null data idempotence" prop_null
    ,testProperty "null data head idempotence" prop_nullH
    ,testProperty "enumList" prop_enumList
    ,testProperty "enumCheckIfDone" prop_enumCheckIfDone
    ]
  ,testGroup "Nested iteratees" [
    testProperty "mapStream identity" prop_mapStream
    ,testProperty "mapStream identity 2" prop_mapStream2
    ,testProperty "mapStream identity joinI" prop_mapjoin
    ,testProperty "rigidMapStream" prop_rigidMapStream
    ,testProperty "breakE" prop_breakE
    ,testProperty "breakE remainder" prop_breakE2
    ,testProperty "take" prop_take
    ,testProperty "take (finished iteratee)" prop_take2
    ,testProperty "takeUpTo" prop_takeUpTo
    ,testProperty "takeUpTo (finished iteratee)" prop_takeUpTo2
    ,testProperty "takeUpTo (remaining stream)" prop_takeUpTo3
    ,testProperty "takeWhile" prop_takeWhile
    ,testProperty "filter" prop_filter
    ,testProperty "group" prop_group
    ,testProperty "groupBy" prop_groupBy
    ,testProperty "convStream EOF" prop_convstream2
    ,testProperty "convStream identity" prop_convstream
    ,testProperty "convStream identity 2" prop_convstream3
    ]
  ,testGroup "Enumerator Combinators" [
    testProperty "enumWith" prop_enumWith
    ,testProperty "enumWith remaining" prop_enumWith2
    ,testProperty "enumWith remaining 2" prop_enumWith3
    ,testProperty "countConsumed" prop_countConsumed
    ]
  ,testGroup "Folds" [
    testProperty "foldl" prop_foldl
   ,testProperty "foldl'" prop_foldl'
   ,testProperty "foldl1" prop_foldl1
   ,testProperty "foldl1'" prop_foldl1'
   ,testProperty "sum" prop_sum
   ,testProperty "product" prop_product
   ]
  ,testGroup "Zips" [
    testProperty "zip" prop_zip
   ,testCase     "sequence_" test_sequence_
   ]
  ,testGroup "Data.Iteratee.Char" [
    --testProperty "line" prop_line
    ]
  ,testGroup "PT variants" [
     testProperty "mapChunksPT" prop_mapChunksPT
    ,testProperty "mapChunksMPT" prop_mapChunksMPT
    ,testProperty "convStreamPT" prop_convStreamPT
    ,testProperty "unfoldConvStreamPT" prop_unfoldConvStreamPT
    ,testProperty "breakEPT" prop_breakEPT
    ,testProperty "takePT" prop_takePT
    ,testProperty "takeUpToPT" prop_takeUpToPT
    ,testProperty "takeWhileEPT" prop_takeWhileEPT
    ,testProperty "mapStreamPT" prop_mapStreamPT
    ,testProperty "rigidMapStreamPT" prop_rigidMapStreamPT
    ,testProperty "filterPT" prop_filterPT
    ]
  ,testGroup "Monadic functions" [
    testProperty "mapM_" prop_mapM_
   ,testProperty "foldM" prop_foldM
   ,testProperty "mapChunksM" prop_mapChunksM
   ,testProperty "mapChunksM_" prop_mapChunksM_
   ,testProperty "foldChunksM" prop_foldChunksM
   ]
  ]

------------------------------------------------------------------------
-- The entry point

main = defaultMain tests
