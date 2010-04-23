{-# LANGUAGE FlexibleContexts #-}

-- |Utilties for Char-based iteratee processing.

-- The running example, parts 1 and 2
-- Part 1 is reading the headers, the sequence of lines terminated by an
-- empty line. Each line is terminated by CR, LF, or CRLF.
-- We should return the headers in order. In the case of error,
-- we should return the headers read so far and the description of the error.
-- Part 2 is reading the headers and reading all the lines from the
-- HTTP-chunk-encoded content that follows the headers. Part 2 thus
-- verifies layering of streams, and processing of one stream
-- embedded (chunk encoded) into another stream.

module Data.Iteratee.Char (
  -- * Types
  EofBehavior (..)
  -- ** Type synonyms
  ,Stream
  ,Iteratee
  ,Line
  -- * Word and Line processors
  -- ,line
  ,printLines
  -- ,readLines
  ,enumLines
  ,enumLinesBS
  ,enumWords
  ,enumWordsBS
)

where

import Data.Iteratee.Iteratee
import qualified Data.Iteratee.ListLike as Iter
import Data.Iteratee.ListLike (heads)
import Data.Char
import Data.Monoid
import qualified Data.ListLike as LL
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as BC

-- |A particular instance of StreamG: the stream of characters.
-- This stream is used by many input parsers.
type Stream = StreamG String

-- Useful combinators for implementing iteratees and enumerators

type Line = String      -- The line of text, terminators are not included

-- |Determine the behavior of line/word operators on EOF.
data EofBehavior = ErrOnEof | EolOnEof

-- |Read the line of text from the stream
-- The line can be terminated by CR, LF or CRLF.
-- Return (Right Line) if successful. Return (Left Line) if
-- a stream error were encountered before the terminator is seen.
-- EOF can be treated as either an implicit EOL or an error depending
-- on the setting of EofBehavior.
-- The returned line is the string read so far.

{-
-- The code is the same as that of pure Iteratee, only the signature
-- has changed.
-- Compare the code below with GHCBufferIO.line_lazy
line
  :: (Monad m) =>
     EofBehavior
     -> Iteratee String m (Either Line Line)
line ErrOnEof = Iter.break (\c -> c == '\r' || c == '\n') >>= \l ->
       terminators >>= check l
  where
    check l 0 = return . Left $ l
    check l _ = return . Right $ l
    terminators = heads "\r\n" >>= \l -> if l == 0 then heads "\n" else return l
line EolOnEof = Iter.break (\c -> c == '\r' || c == '\n') >>= \l ->
       terminators >> getStatus >>= check l
  where
    check l DataRemaining = return . Right $ l
    check l EofNoError    = return . Right $ l
    check l (EofError _)  = return . Left $ l
    terminators = heads "\r\n" >>= \l -> if l == 0 then heads "\n" else return l
-}

-- Line iteratees: processors of a stream whose elements are made of Lines

-- Collect all read lines and return them as a list
-- see stream2list

-- |Print lines as they are received. This is the first `impure' iteratee
-- with non-trivial actions during chunk processing
printLines :: Iteratee String IO ()
printLines = lines'
  where
  lines' = Iter.break (`elem` "\r\n") >>= \l -> terminators >>= check l
  check _  0 = return ()
  check "" _ = return ()
  check l  _ = liftIO (putStrLn l) >> lines'
  terminators = heads "\r\n" >>= \l -> if l == 0 then heads "\n" else return l


-- |Read a sequence of lines from the stream up to the empty line.
-- The line can be terminated by CR, LF, or CRLF -- or by EOF or stream error.
-- Return the read lines, in order, not including the terminating empty line
-- Upon stream error, return the complete, terminated lines accumulated
-- so far.
-- EOF is treated as either a stream error or an implicit EOL depending
-- upon the EofBehavior

{-
readLines
  :: (Monad m) =>
     EofBehavior
     -> Iteratee String m (Either [Line] [Line])
readLines eb = lines' []
  where
    lines' acc = line eb >>= check acc
    check acc (Left _)   = return . Left . reverse $ acc
    check acc (Right "") = return . Right . reverse $ acc
    check acc (Right l)  = lines' (l:acc)
-}

-- |Convert the stream of characters to the stream of lines, and
-- apply the given iteratee to enumerate the latter.
-- The stream of lines is normally terminated by the empty line.
-- When the stream of characters is terminated, the stream of lines
-- is also terminated, abnormally.
-- This is the first proper iteratee-enumerator: it is the iteratee of the
-- character stream and the enumerator of the line stream.

enumLines
  :: (LL.ListLike s el, LL.StringLike s, Nullable s, Monad m) =>
     Iteratee [s] m a
     -> Iteratee s m (Iteratee [s] m a)
enumLines = convStream getter
  where
    getter = icont step Nothing
    lChar = (== '\n') . last . LL.toString
    step (Chunk xs)
      | LL.null xs = getter
      | lChar xs   = idone (LL.lines xs) mempty
      | True       = icont (step' xs) Nothing
    step _str      = getter
    step' xs (Chunk ys)
      | LL.null ys = icont (step' xs) Nothing
      | lChar ys   = idone (LL.lines . mappend xs $ ys) mempty
      | True       = let w' = LL.lines $ mappend xs ys
                         ws = init w'
                         ck = last w'
                     in idone ws (Chunk ck)
    step' xs str   = idone (LL.lines xs) str

-- |Convert the stream of characters to the stream of words, and
-- apply the given iteratee to enumerate the latter.
-- Words are delimited by white space.
-- This is the analogue of List.words
-- One should keep in mind that enumWords is a more general, monadic
-- function.

enumWords
  :: (LL.ListLike s el, LL.StringLike s, Nullable s, Monad m) =>
     Iteratee [s] m a
     -> Iteratee s m (Iteratee [s] m a)
enumWords = convStream getter
  where
    getter = icont step Nothing
    lChar = isSpace . last . LL.toString
    step (Chunk xs)
      | LL.null xs = getter
      | lChar xs   = idone (LL.words xs) mempty
      | True       = icont (step' xs) Nothing
    step _str      = getter
    step' xs (Chunk ys)
      | LL.null ys = icont (step' xs) Nothing
      | lChar ys   = idone (LL.words . mappend xs $ ys) mempty
      | True       = let w' = LL.words $ mappend xs ys
                         ws = init w'
                         ck = last w'
                     in idone ws (Chunk ck)
    step' xs str   = idone (LL.words xs) str

{-# INLINE enumWords #-}

-- Like enumWords, but operates on ByteStrings.
-- This is provided as a higher-performance alternative to enumWords.
enumWordsBS
  :: (Monad m) =>
     Iteratee [BC.ByteString] m a
     -> Iteratee BC.ByteString m (Iteratee [BC.ByteString] m a)
enumWordsBS iter = convStream getter iter
  where
    getter = icont step Nothing
    lChar = isSpace . BC.last
    step (Chunk xs)
      | BC.null xs = getter
      | lChar xs   = idone (BC.words xs) (Chunk BC.empty)
      | True       = icont (step' xs) Nothing
    step _str      = getter
    step' xs (Chunk ys)
      | BC.null ys = icont (step' xs) Nothing
      | lChar ys   = idone (BC.words . BC.append xs $ ys) mempty
      | True       = let w' = BC.words . BC.append xs $ ys
                         ws = init w'
                         ck = last w'
                     in idone ws (Chunk ck)
    step' xs str   = idone (BC.words xs) str

{-# INLINE enumWordsBS #-}

enumLinesBS
  :: (Monad m) =>
     Iteratee [BC.ByteString] m a
     -> Iteratee BC.ByteString m (Iteratee [BC.ByteString] m a)
enumLinesBS = convStream getter
  where
    getter = icont step Nothing
    lChar = (== '\n') . BC.last
    step (Chunk xs)
      | BC.null xs = getter
      | lChar xs   = idone (BC.lines xs) (Chunk BC.empty)
      | True       = icont (step' xs) Nothing
    step _str      = icont step Nothing
    step' xs (Chunk ys)
      | BC.null ys = icont (step' xs) Nothing
      | lChar ys   = idone (BC.lines . BC.append xs $ ys) mempty
      | True       = let w' = BC.lines $ BC.append xs ys
                         ws = init w'
                         ck = last w'
                     in idone ws (Chunk ck)
    step' xs str   = idone (BC.lines xs) str

