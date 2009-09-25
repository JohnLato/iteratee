{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}

module QCUtils where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Iteratee
import qualified Data.Iteratee as I
import qualified Data.ListLike as LL
import Control.Monad.Identity

-- Show instance
instance (Show a, LL.ListLike s el) => Show (IterateeT s el Identity a) where
  show = (++) "<<Iteratee>> " . show . runIdentity . run

-- Arbitrary instances

instance Arbitrary ErrMsg where
  arbitrary = do
    err <- arbitrary
    n <- arbitrary :: Gen Int
    elements [Err err, Seek (fromIntegral n)]

instance Arbitrary c => Arbitrary (StreamG c) where
  arbitrary = do
    err <- arbitrary
    xs <- arbitrary
    elements [EOF err, Chunk xs]

instance (Num a, Ord a, Arbitrary a, Monad m) => Arbitrary (IterateeT [a] a m [a]) where
  arbitrary = do
    n <- suchThat arbitrary (>0)
    ns <- arbitrary
    elements [
              I.drop n >> stream2list
              ,I.break (< 5)
              ,I.heads ns >> stream2list
              ,I.peek >> stream2list
              ]
