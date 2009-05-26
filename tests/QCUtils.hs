{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module QCUtils where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Data.Iteratee
import qualified Data.Iteratee as I
import Data.Iteratee.Base.StreamChunk (StreamChunk)
import Control.Monad.Identity

-- Show instance
instance (Show a, StreamChunk s el) => Show (IterateeG s el Identity a) where
  show = (++) "<<Iteratee>> " . show . runIdentity . run

-- Arbitrary instances

instance Arbitrary (c el) => Arbitrary (StreamG c el) where
  arbitrary = do
    err <- arbitrary
    xs <- arbitrary
    elements [EOF err, Chunk xs]

instance (Num a, Ord a, Arbitrary a, Monad m) => Arbitrary (IterateeG [] a m [a]) where
  arbitrary = do
    n <- suchThat arbitrary (>0)
    ns <- arbitrary
    elements [
              I.drop n >> stream2list
              ,I.break (< 5)
              ,I.heads ns >> stream2list
              ,I.peek >> stream2list
              ]

