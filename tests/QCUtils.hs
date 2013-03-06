{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module QCUtils where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Iteratee
import Data.Iteratee.Iteratee
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Data.Functor.Identity

import Control.Applicative
import Control.Exception
import System.IO.Unsafe

-- Show instance
instance (Show a, LL.ListLike s el) => Show (Iteratee s Identity a) where
  show = (++) "<<Iteratee>> " . show . runIdentity . run

instance (Show a, LL.ListLike s el) => Show (Iteratee s IO a) where
  show = (++) "<<Iteratee>> " . show . unsafePerformIO . run


-- Arbitrary instances

instance Arbitrary c => Arbitrary (Stream c) where
  arbitrary = do
    err <- arbitrary
    xs <- arbitrary
    elements [EOF err, Chunk xs, NoData]

tE :: EException e => e -> EnumException
tE = toEnumException

instance Arbitrary EnumException where
  arbitrary = do
    i <- arbitrary
    elements [tE DivergentException, tE $ EnumUnhandledIterException i]

tI :: IException e => e -> IterException
tI = toIterException

instance Arbitrary IterException where
  arbitrary = do
    str <- arbitrary
    off <- fromInteger <$> (arbitrary :: Gen Integer)
    elements [ tI (SeekException off)
             , tI (EofException "someEofException")
             , iterStrExc str]


instance (Num a, Ord a, Arbitrary a, Monad m) => Arbitrary (Iteratee [a] m [a]) where
  arbitrary = do
    n <- suchThat arbitrary (>0)
    ns <- arbitrary
    elements [
              I.drop n >> stream2list
              ,stream2list
              ,I.drop n >> return ns
              ,I.break (< 5)
              ,I.heads ns >> stream2list
              ,I.peek >> stream2list
              ,I.peek >> return ns
              ,I.identity >> return []
              ,I.identity >> return ns
              ]
