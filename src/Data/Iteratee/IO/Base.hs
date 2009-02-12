module Data.Iteratee.IO.Base (
  FileOffset
)
where

import qualified System.Posix as Posix

type FileOffset = Posix.FileOffset

