{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Data.Iteratee.Base.LooseMap (
  LooseMap (..)
)

where

-- |Enable map functions for containers that require class contexts on the
-- element types.  For lists, this is identical to plain `map`.
class LooseMap c el el' where
  lMap :: (el -> el') -> c el -> c el'

instance LooseMap [] el el' where
  lMap = map
