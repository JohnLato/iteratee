-- |NullPoint
-- Pointed types (usually containers) that can be empty.

module Data.NullPoint (
  -- * Types
  NullPoint (..)
)
where

import qualified Data.ByteString as B

-- ----------------------------------------------
-- |NullPoint class.  Containers that have a null representation.
class NullPoint c where
  empty :: c

instance NullPoint [a] where
  empty   = []

instance NullPoint B.ByteString where
  empty = B.empty

