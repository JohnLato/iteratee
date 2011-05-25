{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

-- | Utilities for Char-based iteratee processing.

module Data.Iteratee.Char (
  -- * Word and Line processors
  printLines
  ,printLinesUnterminated
  ,enumLines
  ,enumLinesBS
  ,enumWords
  ,enumWordsBS
)

where

import           Data.Iteratee.Iteratee
import qualified Data.Iteratee.ListLike as I
import           Data.Iteratee.ListLike (heads)
import           Data.Char
import           Data.Monoid
import qualified Data.ListLike as LL
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BC


-- |Print lines as they are received. This is the first `impure' iteratee
-- with non-trivial actions during chunk processing
--
--  Only lines ending with a newline are printed,
--  data terminated with EOF is not printed.
printLines :: Iteratee String IO ()
printLines = lines'
 where
  lines' = I.break (`elem` "\r\n") >>= \l -> terminators >>= check l
  check _  0 = return ()
  check "" _ = return ()
  check l  _ = liftIO (putStrLn l) >> lines'

-- |Print lines as they are received.
--
--  All lines are printed, including a line with a terminating EOF.
--  If the final line is terminated by EOF without a newline,
--  no newline is printed.
--  this function should be used in preference to printLines when possible,
--  as it is more efficient with long lines.
printLinesUnterminated :: forall s el.
                       (Eq el, Nullable s, LL.StringLike s, LL.ListLike s el)
                       => Iteratee s IO ()
printLinesUnterminated = lines'
 where
  lines' = do
    joinI $ I.breakE (`LL.elem` t1) (I.mapChunksM_ (putStr . LL.toString))
    terminators >>= check
  check 0 = return ()
  check _ = liftIO (putStrLn "") >> lines'
  t1 :: s
  t1 = LL.fromString "\r\n"

terminators :: (Eq el, Nullable s, LL.StringLike s, LL.ListLike s el)
            => Iteratee s IO Int
terminators = do
  l <- heads (LL.fromString "\r\n")
  if l == 0 then heads (LL.fromString "\n") else return l


-- |Convert the stream of characters to the stream of lines, and
-- apply the given iteratee to enumerate the latter.
-- The stream of lines is normally terminated by the empty line.
-- When the stream of characters is terminated, the stream of lines
-- is also terminated.
-- This is the first proper iteratee-enumerator: it is the iteratee of the
-- character stream and the enumerator of the line stream.

enumLines
  :: (LL.ListLike s el, LL.StringLike s, Nullable s, Monad m) =>
     Enumeratee s [s] m a
enumLines = convStream getter
  where
    getter = icont step Nothing
    lChar = (== '\n') . last . LL.toString
    step (Chunk xs)
      | LL.null xs = getter
      | lChar xs   = idone (LL.lines xs) mempty
      | otherwise  = icont (step' xs) Nothing
    step _str      = getter
    step' xs (Chunk ys)
      | LL.null ys = icont (step' xs) Nothing
      | lChar ys   = idone (LL.lines . mappend xs $ ys) mempty
      | otherwise  = let w' = LL.lines $ mappend xs ys
                         ws = init w'
                         ck = last w'
                     in idone ws (Chunk ck)
    step' xs str   = idone (LL.lines xs) str

-- |Convert the stream of characters to the stream of words, and
-- apply the given iteratee to enumerate the latter.
-- Words are delimited by white space.
-- This is the analogue of List.words
enumWords :: (LL.ListLike s Char, Nullable s, Monad m) => Enumeratee s [s] m a
enumWords = convStream $ I.dropWhile isSpace >> liftM (:[]) (I.break isSpace)
{-# INLINE enumWords #-}

-- Like enumWords, but operates on ByteStrings.
-- This is provided as a higher-performance alternative to enumWords, and
-- is equivalent to treating the stream as a Data.ByteString.Char8.ByteString.
enumWordsBS
  :: (Monad m) => Enumeratee BC.ByteString [BC.ByteString] m a 
enumWordsBS iter = convStream getter iter
  where
    getter = liftI step
    lChar = isSpace . BC.last
    step (Chunk xs)
      | BC.null xs = getter
      | lChar xs   = idone (BC.words xs) (Chunk BC.empty)
      | otherwise  = icont (step' xs) Nothing
    step str       = idone mempty str
    step' xs (Chunk ys)
      | BC.null ys = icont (step' xs) Nothing
      | lChar ys   = idone (BC.words . BC.append xs $ ys) mempty
      | otherwise  = let w' = BC.words . BC.append xs $ ys
                         ws = init w'
                         ck = last w'
                     in idone ws (Chunk ck)
    step' xs str   = idone (BC.words xs) str

{-# INLINE enumWordsBS #-}

-- Like enumLines, but operates on ByteStrings.
-- This is provided as a higher-performance alternative to enumLines, and
-- is equivalent to treating the stream as a Data.ByteString.Char8.ByteString.
enumLinesBS :: (Monad m) => Enumeratee BC.ByteString [BC.ByteString] m a
enumLinesBS = convStream getter
  where
    getter = icont step Nothing
    lChar = (== '\n') . BC.last
    step (Chunk xs)
      | BC.null xs = getter
      | lChar xs   = idone (BC.lines xs) (Chunk BC.empty)
      | otherwise  = icont (step' xs) Nothing
    step str       = idone mempty str
    step' xs (Chunk ys)
      | BC.null ys = icont (step' xs) Nothing
      | lChar ys   = idone (BC.lines . BC.append xs $ ys) mempty
      | otherwise  = let w' = BC.lines $ BC.append xs ys
                         ws = init w'
                         ck = last w'
                     in idone ws (Chunk ck)
    step' xs str   = idone (BC.lines xs) str

