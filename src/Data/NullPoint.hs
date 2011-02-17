-- |NullPoint:
-- Pointed types (usually containers) that can be empty.
-- Corresponds to Data.Monoid.mempty

module Data.NullPoint (
  -- * Classes
  NullPoint (..)
)
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

-- ----------------------------------------------
-- |NullPoint class.  Containers that have a null representation, corresponding
-- to Data.Monoid.mempty.
class NullPoint c where
  empty :: c

instance NullPoint [a] where
  empty   = []

instance NullPoint B.ByteString where
  empty = B.empty

instance NullPoint L.ByteString where
  empty = L.empty
