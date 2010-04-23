-- |Nullable
-- Test if a type (container) is null.

module Data.Nullable (
  -- * Types
  Nullable (..)
)
where

import Data.NullPoint
import qualified Data.ByteString as B


-- ----------------------------------------------
-- |Nullable container class
class NullPoint c => Nullable c where
  null :: c -> Bool

instance Nullable [a] where
  null [] = True
  null _  = False

instance Nullable B.ByteString where
  null = B.null
