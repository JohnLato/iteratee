-- |Nullable:
-- test if a type (container) is null.

module Data.Nullable (
  -- * Classes
  Nullable (..)
)
where

import Data.NullPoint
import qualified Data.ByteString as B


-- ----------------------------------------------
-- |Nullable container class
class NullPoint c => Nullable c where
  nullC :: c -> Bool

instance Nullable [a] where
  nullC [] = True
  nullC _  = False

instance Nullable B.ByteString where
  nullC = B.null
