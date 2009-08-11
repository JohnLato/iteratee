{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Iteratee.Base.LooseMap (
  LooseMap (..)
)

where

-- |Enable map functions for containers that require class contexts on the
-- element types.  There's really no reason to ever use this with
-- types that are fully polymorphic, such as Lists.
class LooseMap c el el' where
  looseMap :: (el -> el') -> c el -> c el'
