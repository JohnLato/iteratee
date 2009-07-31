{-# LANGUAGE CPP #-}

module Data.Iteratee.IO.Base (
#if defined(USE_WINDOWS)
  module Data.Iteratee.IO.Windows,
#endif
#if defined(USE_POSIX)
  module Data.Iteratee.IO.Posix,
#else
  FileOffset
#endif
)
where

#if defined(USE_WINDOWS)
import Data.Iteratee.IO.Windows
#endif

-- Provide the FileOffset type, which is available in Posix modules
-- and maybe Windows
#if defined(USE_POSIX)
import Data.Iteratee.IO.Posix
#else
type FileOffset = Integer
#endif
