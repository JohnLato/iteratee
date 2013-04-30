{-# OPTIONS_GHC -O #-}

{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Prelude as P

import QCUtils
import Seek

import Test.Framework (defaultMain, testGroup, buildTest)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.Providers.HUnit (testCase)

import Test.HUnit
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QC

import           Data.Iteratee hiding (head, break)
import           Data.Iteratee.Parallel
import qualified Data.Iteratee.Char as IC
import qualified Data.Iteratee as Iter
import           Data.Functor.Identity
import qualified Data.List as List (groupBy, unfoldr)
import           Data.Monoid
import qualified Data.ListLike as LL

import           Control.Applicative
import           Control.Monad as CM
import           Control.Monad.Writer
import           Control.Exception (SomeException)

import           System.IO.Unsafe

instance Show (a -> b) where
  show _ = "<<function>>"

-- ---------------------------------------------
-- Stream instances

type ST = Stream [Int]

prop_eq str = str == str
  where types = str :: ST

prop_mappend str1 str2
  | isEOF str1 || isEOF str2       = isEOF $ str1 `mappend` str2
  | isNoData str1 && isNoData str2 = isNoData $ str1 `mappend` str2
  | otherwise =
       str1 `mappend` str2 == Chunk (chunkData str1 ++ chunkData str2)
  where types = (str1 :: ST, str2 :: ST)

prop_mappend2 str = str `mappend` mempty == mempty `mappend` str
  where types = str :: ST

chunkData (Chunk xs) = xs
chunkData NoData     = mempty

isChunk (Chunk _) = True
isChunk _         = False

isNoData NoData   = True
isNoData _        = False

isEOF (EOF _)   = True
isEOF _         = False

-- ---------------------------------------------
-- for testing only
-- real enumerators shouldn't ever feed NoData to an iteratee.
-- They can receive it via (>>=) though, so we test it this way

enumNoData :: Monad m => Enumerator s m a
enumNoData iter = runIter iter idoneM onC ierrM
  where
    onC k = wrapCont `liftM` k NoData

-- ---------------------------------------------
-- Iteratee instances

runner1 = runIdentity . Iter.run . runIdentity

runnerIO mIter = mIter >>= Iter.run

enumSpecial xs n
  | LL.null xs = enumPure1Chunk LL.empty >=> enumPureNChunk xs n
  | otherwise  = enumPure1Chunk LL.empty >=> enumPure1Chunk (LL.take 1 xs)
                       >=> enumNoData
                       >=> enumPureNChunk (LL.tail xs) n

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

prop_clist xs (Positive n) = runner1 (enumSpecial xs n stream2list) == xs
  where types = xs :: [Int]

prop_break f xs = runner1 (enumPure1Chunk xs (Iter.break f)) == fst (break f xs)
  where types = xs :: [Int]

prop_break2 f xs = runner1 (enumPure1Chunk xs (Iter.break f >> stream2list)) == snd (break f xs)
  where types = xs :: [Int]

prop_breakE f xs = runner1 (enumPure1Chunk xs (joinI $ Iter.breakE f stream2stream)) == fst (break f xs)
  where types = xs :: [Int]

prop_breakE2 f xs = runner1 (enumPure1Chunk xs (joinI (Iter.breakE f stream2stream) >> stream2list)) == snd (break f xs)
  where types = xs :: [Int]

prop_breakE3 f xs = runner1 (enumPure1Chunk xs (joinI (Iter.breakE f (return ())) >> stream2list)) == snd (break f xs)
  where types = xs :: [Int]


prop_head (NonEmpty xs) = runner1 (enumPure1Chunk xs Iter.head) == head xs
  where types = xs :: [Int]

prop_head2 (NonEmpty xs) = runner1 (enumPure1Chunk xs (Iter.head >> stream2list)) == tail xs
  where types = xs :: [Int]

-- resumption after an exception
prop_head3 (NonEmpty xs) =
 runner1 ((enumEof >=> enumPure1Chunk xs) Iter.head) == P.head xs
  where types = xs :: [Int]

prop_tryhead xs = case xs of
  [] -> runner1 (enumPure1Chunk xs tryHead) == Nothing
  _  -> runner1 ((enumNoData >=> enumPure1Chunk xs) ((,) <$> tryHead <*> stream2list))
        == (Just $ P.head xs, P.drop 1 xs)
  where types = xs :: [Int]

prop_heads xs (Positive n) =
 runner1 (enumSpecial xs n $ heads xs) == P.length xs
  where types = xs :: [Int]

prop_heads2 xs = runner1 (enumPure1Chunk xs $ heads [] >>= \c ->
                          stream2list >>= \s -> return (c,s))
                 == (0, xs)
  where types = xs :: [Int]

prop_peek xs = runner1 ((enumNoData >=> enumPure1Chunk xs) peek) == sHead xs
  where
  types = xs :: [Int]
  sHead [] = Nothing
  sHead (x:_) = Just x

prop_peek2 xs = runner1 (enumPure1Chunk xs (peek >> stream2list)) == xs
  where types = xs :: [Int]

prop_skip xs = runner1 (enumPure1Chunk xs (skipToEof >> stream2list)) == []
  where types = xs :: [Int]

prop_last1 (NonEmpty xs) =
 runner1 (enumSpecial xs 2 (Iter.last)) == P.last xs
  where types = xs :: [Int]

prop_last2 (NonEmpty xs) =
 runner1 (enumSpecial xs 2 (Iter.last >> Iter.peek)) == Nothing
  where types = xs :: [Int]

-- resumption after an exception
prop_last3 (NonEmpty xs) =
 runner1 ((enumEof >=> enumPure1Chunk xs) Iter.last) == P.last xs
  where types = xs :: [Int]

test_lastExc = testGot (runIdentity $ tryRun =<< enumNoData (Iter.last :: Iteratee [Int] Identity Int))
  where
    testGot (Left (fromException . toException -> Just (EofException "Iteratee.last"))) = return ()
    testGot _ = assertFailure "Iter.last exception: didn't produce the correct exception"

prop_roll xs (Positive t) (Positive d) (Positive n) = if t > d
    then runner1 (enumNoData >=> enumSpecial xs n $ (,) <$> Iter.roll t d <*> stream2list)
          == if | null xs    -> ([],[])
                | t > len -> ([xs],[])
                | otherwise  -> ([P.take t xs],P.drop d xs)
    else runner1 (enumSpecial xs n $ (,) <$> roll t d <*> stream2list)
         == ([P.take t xs], P.drop d xs)
  where
    len = P.length xs
    types = xs :: [Int]

-- quickcheck tends to miss the very simple case where everything can be
-- calculated within a single chunk.
test_roll = runner1 (enumPure1Chunk [1..10::Int] $ (,) <$> roll 4 2 <*> stream2list)
            @?= ([[1..4]],[3..10])

prop_drop xs (Positive n) (NonNegative k) =
 runner1 (enumSpecial xs n (Iter.drop k >> stream2list)) == P.drop k xs
  where types = xs :: [Int]

prop_dropWhile f xs =
 runner1 (enumPure1Chunk xs (Iter.dropWhile f >> stream2list))
 == P.dropWhile f xs
  where types = (xs :: [Int], f :: Int -> Bool)

prop_length xs = runner1 ((enumNoData >=> enumPure1Chunk xs) Iter.length) == P.length xs
  where types = xs :: [Int]

-- length 0 is an odd case.  enumPureNChunk skips null inputs, returning
-- the original iteratee, which is then given to `enumEof` by `run`.
--
-- so instead we use enumNoData to test null inputs.  Eof isn't tested here,
-- so we need to catch it in the following property.
prop_chunkLength xs (Positive n) =
  runner1 (enum (liftM2 (,) chunkLength stream2list))
  == case P.length xs of
       xl | xl >= n   -> (Just n, xs)
          | otherwise -> (Just (P.length xs), xs)
 where
   types = xs :: [Int]
   enum = if null xs then enumNoData else enumPureNChunk xs n

prop_chunkLength2 xs =
  runner1 ((enumEof >=> enumPure1Chunk xs) chunkLength) == Nothing
 where types = xs :: [Int]

prop_takeFromChunk xs b (Positive n) k =
  runner1 (enum (liftM2 (,) (takeFromChunk k) stream2list))
  == if k > n then splitAt n xs else splitAt k xs
 where
   types = xs :: [Int]
   -- b is boolean flag to determine if we check enumNoData or enumEof.
   -- either way the result should be the same (empty), but we want to check
   -- both paths
   enum = if null xs && b then enumNoData else enumPureNChunk xs n

-- ---------------------------------------------
-- Simple enumerator tests

type I = Iteratee [Int] Identity [Int]

prop_enumChunks (Positive n) xs i =
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

prop_isFinished xs = runner1 ((enumNoData >=> enumPure1Chunk xs >=> enumEof) (isFinished :: Iteratee [Int] Identity Bool))
                     == null xs
  where types = xs :: [Int]

prop_isFinished2 = runner1 (enumErr (enumStrExc "Error") (isFinished :: Iteratee [Int] Identity Bool)) == True

prop_null xs i = runner1 (enumPure1Chunk xs =<< enumPure1Chunk [] i)
                 == runner1 (enumPure1Chunk xs i)
  where types = (xs :: [Int], i :: I)

prop_nullH (NonEmpty xs) =
                runner1 ((enumNoData >=> enumPure1Chunk [] >=> enumPure1Chunk xs) Iter.head)
                == runner1 (enumPure1Chunk xs Iter.head)
  where types = xs :: [Int]

test_headExc = testGot (runIdentity $ tryRun =<< enumNoData (Iter.head :: Iteratee [Int] Identity Int))
  where
    testGot (Left (fromException . toException -> Just (EofException "Iteratee.head"))) = return ()
    testGot _ = assertFailure "Iter.head exception: didn't produce the correct exception"

prop_enumList (NonEmpty xs) i =
  runner1 (enumList (replicate 100 xs) i)
  == runner1 (enumPure1Chunk (concat $ replicate 100 xs) i)
 where types = (xs :: [Int], i :: I)

prop_enumCheckIfDone xs i =
   runner1 (enumPure1Chunk xs (lift (enumCheckIfDone i) >>= snd))
   == runner1 (enumPure1Chunk xs i)
 where types = (xs :: [Int], i :: I)

-- ---------------------------------------------
-- Enumerator Combinators

prop_enumWith xs f (Positive n) = runner1 (enumSpecial xs n $ fmap fst $ enumWith (Iter.dropWhile f) (stream2list))
   == runner1 (enumSpecial xs n $ Iter.dropWhile f)
 where types = (xs :: [Int])

prop_enumWith2 xs f (Positive n) = runner1 (enumSpecial xs n $ enumWith (Iter.dropWhile f) (stream2list) >> stream2list)
   == runner1 (enumSpecial xs n $ Iter.dropWhile f >> stream2list)
 where types = (xs :: [Int])

prop_enumWith3 xs i (Positive n) =
   runner1 (enumSpecial xs n $ enumWith i stream2list >> stream2list)
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

prop_mapStream2 xs (Positive n) i =
    runner2 (enumSpecial xs n $ mapStream id i)
    == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])

prop_mapStreamPushback xs = runner1 (enumPureNChunk xs 4 $ joinI (mapStream id (Iter.drop 2)) >> stream2list)
                            == runner1 (enumPureNChunk xs 4 $ Iter.drop 2 >> stream2list)
  where types = xs :: [Int]


prop_mapjoin xs i =
  runIdentity (run (joinI . runIdentity $ enumPure1Chunk xs $ mapStream id i))
  == runner1 (enumPure1Chunk xs i)
  where types = (i :: I, xs :: [Int])

prop_rigidMapStream xs (Positive n) f =
 runner2 (enumSpecial xs n $ rigidMapStream f stream2list) == map f xs
  where types = (xs :: [Int])

prop_foldl xs (Positive n) f x0 =
 runner1 (enumSpecial xs n (Iter.foldl f x0)) == P.foldl f x0 xs
  where types = (xs :: [Int], x0 :: Int)

prop_foldl' xs (Positive n) f x0 =
 runner1 (enumSpecial xs n (Iter.foldl' f x0)) == LL.foldl' f x0 xs
  where types = (xs :: [Int], x0 :: Int)

prop_foldl1 (NonEmpty xs) (Positive n) f =
 runner1 (enumSpecial xs n (Iter.foldl1 f)) == P.foldl1 f xs
  where types = (xs :: [Int])

prop_foldl1' f (NonEmpty xs) (Positive n) =
 runner1 (enumSpecial xs n (Iter.foldl1' f)) == P.foldl1 f xs
  where types = (xs :: [Int])

prop_sum xs (Positive n) = runner1 (enumSpecial xs n Iter.sum) == P.sum xs
  where types = (xs :: [Int])

prop_product xs (Positive n) =
 runner1 (enumSpecial xs n Iter.product) == P.product xs
  where types = (xs :: [Int])

convId :: (LL.ListLike s el, Monad m) => Iteratee s m s
convId = icontP (\str -> case str of
  s@(Chunk xs) -> ContDone xs NoData
  NoData       -> ContDone mempty NoData
  s@(EOF e)    -> ContDone mempty $ EOF e
  )

prop_convId xs = runner1 (enumPure1Chunk xs convId) == xs
  where types = xs :: [Int]

prop_convstream i (NonEmpty xs) =
                       runner2 (enumPure1Chunk xs $ convStream convId i)
                       == runner1 (enumPure1Chunk xs i)
  where types = (xs :: [Int], i :: I)

prop_convstream2 (NonEmpty xs) =
                      runner2 (enumPure1Chunk xs $ convStream convId Iter.head)
                      == runner1 (enumPure1Chunk xs Iter.head)
  where types = xs :: [Int]

prop_convstream3 (NonEmpty xs) =
                      runner2 (enumPure1Chunk xs $ convStream convId stream2list)
                      == runner1 (enumPure1Chunk xs stream2list)
  where types = xs :: [Int]

prop_take xs i (NonNegative n) =
    runner2 (enumPure1Chunk xs $ Iter.take n i)
    == runner1 (enumPure1Chunk (P.take n xs) i)
  where types = i :: Iteratee [Int] Identity [Int]

prop_take2 xs (Positive n) =
                  runner2 (enumPure1Chunk xs $ Iter.take n peek)
                  == runner1 (enumPure1Chunk (P.take n xs) peek)
  where types = xs :: [Int]

prop_takeResume xs' h (Positive n) =
                 runner2 (enumPure1Chunk xs $ do
                    i' <- lift (enumEof Iter.head)
                    Iter.take n i')
                 == runner1 (enumPure1Chunk (P.take n xs) Iter.head)
  where xs = h:xs' :: [Int]

-- if the inner iteratee throws an exception, we should pass it up the chain.
prop_take3 (NonEmpty xs) (Positive n) =
    testGot (runIdentity $ tryRun =<< (enumPure1Chunk xs $ Iter.take n (Iter.drop 1 >> Iter.seek 20) >> stream2list))
  where
    testGot (Left (fromException . toException -> Just (SeekException 20))) = True
    testGot _ = False
    types = xs :: [Int]

prop_takeUpTo xs (NonNegative n) =
                  runner2 (enumPure1Chunk xs $ Iter.take n stream2list)
                  == runner2 (enumPure1Chunk xs $ takeUpTo n stream2list)
  where types = xs :: [Int]

prop_takeUpTo2 xs (NonNegative n) =
 runner2 (enumPure1Chunk xs (takeUpTo n identity)) == ()
  where types = xs :: [Int]

-- check for final stream state
prop_takeUpTo3 xs d t (Positive n) =
 runner1 (enumPureNChunk xs n (joinI (takeUpTo t (Iter.drop d)) >> stream2list))
 == P.drop (min t d) xs
  where types = xs :: [Int]

prop_takeWhile xs f (Positive n) =
  runner1 (enumSpecial xs n (liftM2 (,) (Iter.takeWhile f) stream2list))
  == (P.takeWhile f xs, P.dropWhile f xs)
 where types = xs :: [Int]

prop_filter xs f (Positive n) =
 runner2 (enumSpecial xs n (Iter.filter f stream2list)) == P.filter f xs
  where types = xs :: [Int]

prop_group xs (Positive n) =
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

prop_mapChunksM xs (Positive n) =
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

prop_mapChunksM_ xs (Positive n) =
 snd (runWriter ((enumSpecial xs n (Iter.mapChunksM_ f)) >>= run))
 == Sum (P.length xs)
  where f ck = tell (Sum $ P.length ck)
        types = xs :: [Int]

prop_mapM_ xs (Positive n) =
 runWriter ((enumSpecial xs n (Iter.mapM_ f)) >>= run)
 == runWriter (CM.mapM_ f xs)
  where f = const $ tell (Sum 1)
        types = xs :: [Int]

prop_foldChunksM xs x0 (Positive n) =
 runWriter ((enumSpecial xs n (Iter.foldChunksM f x0)) >>= run)
 == runWriter (f x0 xs)
  where f acc ck = CM.foldM f' acc ck
        f' acc el = tell (Sum 1) >> return (acc+el)
        types = xs :: [Int]

prop_foldM xs x0 (Positive n) =
 runWriter ((enumSpecial xs n (Iter.foldM f x0)) >>= run)
 == runWriter (CM.foldM f x0 xs)
  where f acc el = tell (Sum 1) >> return (acc - el)
        types = xs :: [Int]
-- ---------------------------------------------
-- Zips

prop_zip xs i1 i2 (Positive n) =
  runner1 (enumSpecial xs n $ liftM2 (,) (Iter.zip i1 i2) stream2list)
  == let (r1, t1) = runner1 $ enumPure1Chunk xs $ liftM2 (,) i1 stream2list
         (r2, t2) = runner1 $ enumPure1Chunk xs $ liftM2 (,) i2 stream2list
         shorter = if P.length t1 > P.length t2 then t2 else t1
     in ((r1,r2), shorter)
 where types = (i1 :: I, i2 :: I, xs :: [Int])

prop_zip_assoc xs (Positive n) i1 i2 i3 =
    runner1 (enumSpecial xs n (Iter.zip i1 i2 `Iter.zip` i3))
    == rearrange (runner1 (enumSpecial xs n (i1 `Iter.zip` Iter.zip i2 i3)))
  where
    types = (i1 :: I, i2 :: I, i3 :: I, xs :: [Int])
    rearrange (a,(b,c)) = ((a,b),c)

prop_zip3 xs (Positive n) i1 i2 i3 =
    runner1 (enumSpecial xs n (Iter.zip3 i1 i2 i3))
    == rearrange (runner1 (enumSpecial xs n (i1 `Iter.zip` Iter.zip i2 i3)))
  where
    types = (i1 :: I, i2 :: I, i3 :: I, xs :: [Int])
    rearrange (a,(b,c)) = (a,b,c)

prop_zip4 xs (Positive n) i1 i2 i3 i4 =
    runner1 (enumSpecial xs n (Iter.zip4 i1 i2 i3 i4))
    == rearrange (runner1 (enumSpecial xs n (i1 `Iter.zip` (i2 `Iter.zip` (i3 `Iter.zip` i4)))))
  where
    types = (i1 :: I, i2 :: I, i3 :: I, i4 :: I, xs :: [Int])
    rearrange (a,(b,(c,d))) = (a,b,c,d)

prop_zip5 xs (Positive n) i1 i2 i3 i4 i5 =
    runner1 (enumSpecial xs n (Iter.zip5 i1 i2 i3 i4 i5))
    == rearrange (runner1 (enumSpecial xs n (i1 `Iter.zip` (i2 `Iter.zip` (i3 `Iter.zip` (i4 `Iter.zip` i5))))))
  where
    types = (i1 :: I, i2 :: I, i3 :: I, i4 :: I, i5 :: I, xs :: [Int])
    rearrange (a,(b,(c,(d,e)))) = (a,b,c,d,e)



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
-- Data.Iteratee.Parallel

prop_parI :: [Int] -> Positive Int -> Iteratee [Int] IO [Int] -> Property
prop_parI xs (Positive n) i = QC.monadicIO $ do
  v1 <- QC.run $ runnerIO (enumSpecial xs n i)
  v2 <- QC.run $ runnerIO (enumSpecial xs n (parI i))
  QC.assert (v1==v2)

-- ---------------------------------------------
-- Data.Iteratee.PTerm

mk_prop_pt_id etee p_etee i xs(Positive n) =
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

-- check that the tail is dropped properly
prop_breakEPT3 f xs = runner1 (enumPure1Chunk xs (joinI (breakEPT f (return ())) >> stream2list)) == snd (break f xs)
  where types = xs :: [Int]

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
     testProperty "mappend" prop_mappend
    ,testProperty "mappend associates" prop_mappend2
    ,testProperty "eq" prop_eq
  ]
  ,testGroup "Simple Iteratees"
    [ testProperty "break" prop_break
    , testProperty "break remainder" prop_break2
    , testProperty "head" prop_head
    , testProperty "head remainder" prop_head2
    , testProperty "head resumption" prop_head3
    , testCase     "head exception" test_headExc
    , testProperty "tryhead" prop_tryhead
    , testProperty "heads" prop_heads
    , testProperty "null heads" prop_heads2
    , testProperty "peek" prop_peek
    , testProperty "peek2" prop_peek2
    , testProperty "last" prop_last1
    , testProperty "last ends properly" prop_last2
    , testProperty "last resumption" prop_last3
    , testCase     "last exceptions" test_lastExc
    , testProperty "length" prop_length
    , testProperty "chunkLength" prop_chunkLength
    , testProperty "chunkLength of EoF" prop_chunkLength2
    , testProperty "takeFromChunk" prop_takeFromChunk
    , testProperty "drop" prop_drop
    , testProperty "dropWhile" prop_dropWhile
    , testProperty "skipToEof" prop_skip
    , testProperty "iteratee Functor 1" prop_iterFmap
    , testProperty "iteratee Functor 2" prop_iterFmap2
    , testProperty "iteratee Monad LI" prop_iterMonad1
    , testProperty "iteratee Monad RI" prop_iterMonad2
    , testProperty "iteratee Monad Assc" prop_iterMonad3
    , testProperty "roll" prop_roll
    , testCase     "roll simple" test_roll
    ]
  ,testGroup "Simple Enumerators/Combinators"
    [ testProperty "enumPureNChunk" prop_enumChunks
    , testProperty "enum append 1" prop_app1
    , testProperty "enum sequencing" prop_app2
    , testProperty "enum sequencing 2" prop_app3
    , testProperty "enumEof" prop_eof
    , testProperty "isFinished" prop_isFinished
    , testProperty "isFinished error" prop_isFinished2
    , testProperty "null data idempotence" prop_null
    , testProperty "null data head idempotence" prop_nullH
    , testProperty "enumList" prop_enumList
    , testProperty "enumCheckIfDone" prop_enumCheckIfDone
    ]
  ,testGroup "Nested iteratees" [
    testGroup "mapStream"
      [ testProperty "mapStream identity" prop_mapStream
      , testProperty "mapStream identity 2" prop_mapStream2
      , testProperty "mapStream identity joinI" prop_mapjoin
      , testProperty "mapStream leftovers" prop_mapStreamPushback
      , testProperty "rigidMapStream" prop_rigidMapStream
      ]
    , testGroup "breakE"
      [ testProperty "breakE" prop_breakE
      , testProperty "breakE remainder" prop_breakE2
      , testProperty "breakE remainder after completion" prop_breakE3
      ]
    , testGroup "take"
      [ testProperty "take" prop_take
      , testProperty "take (finished iteratee)" prop_take2
      , testProperty "take (exception output)" prop_takeResume
      , testProperty "take (exception handling)" prop_take3
      , testProperty "takeUpTo" prop_takeUpTo
      , testProperty "takeUpTo (finished iteratee)" prop_takeUpTo2
      , testProperty "takeUpTo (remaining stream)" prop_takeUpTo3
      , testProperty "takeWhile" prop_takeWhile
      ]
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
  ,testGroup "Zips"
    [ testProperty "zip" prop_zip
    , testProperty "zip associates" prop_zip_assoc
    , testProperty "zip3" prop_zip3
    , testProperty "zip4" prop_zip4
    , testProperty "zip5" prop_zip5
    , testCase     "sequence_" test_sequence_
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
    ,testProperty "breakEPT remainder after completion" prop_breakEPT3
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
  ,testGroup "Parallel" [
    testProperty "parI" prop_parI
   ]
  ,testGroup "Seeking" [
    testCase "seek" testSeek1
   ,testCase "nested seek" testSeek2
   ,testCase "seek (handle)" testSeek1h
   ,testCase "nested seek (handle)" testSeek2h
   ]
  ]

------------------------------------------------------------------------
-- The entry point

main = defaultMain tests
