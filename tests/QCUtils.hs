{-# LANGUAGE FlexibleInstances #-}

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
instance (Num a, Ord a, Monad m) => Arbitrary (IterateeG [] a m [a]) where
  arbitrary = elements [
              I.head >> stream2list
              ,I.drop 2 >> stream2list
              ,I.break (< 5)
              ,I.heads [1,3,5] >> stream2list
              ,I.peek >> stream2list
              ]

