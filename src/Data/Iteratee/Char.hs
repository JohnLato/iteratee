-- Haskell98!

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
  -- * Type synonyms
  Stream,
  Iteratee,
  EnumeratorM,
  Line,
  -- * Word and Line processors
  line,
  printLines,
  readLines,
  enumLines,
  enumWords,

  module Data.Iteratee.Base
)

where

import qualified Data.Iteratee.Base as Iter
import Data.Iteratee.Base hiding (break)
import Data.Char
import Control.Monad.Trans

-- |A particular instance of StreamG: the stream of characters.
-- This stream is used by many input parsers.
type Stream = StreamG String

type Iteratee = IterateeG String Char

-- Useful combinators for implementing iteratees and enumerators

type Line = String      -- The line of text, terminators are not included

-- |Read the line of text from the stream
-- The line can be terminated by CR, LF or CRLF.
-- Return (Right Line) if successful. Return (Left Line) if EOF or
-- a stream error were encountered before the terminator is seen.
-- The returned line is the string read so far.

-- The code is the same as that of pure Iteratee, only the signature
-- has changed.
-- Compare the code below with GHCBufferIO.line_lazy
line :: Monad m => IterateeG String Char m (Either Line Line)
line = Iter.break (\c -> c == '\r' || c == '\n') >>= \l ->
       terminators >>= check l
  where
  check l 0 = return . Left $ l
  check l _ = return . Right $ l
  terminators = heads "\r\n" >>= \l -> if l == 0 then heads "\n" else return l

-- Line iteratees: processors of a stream whose elements are made of Lines

-- Collect all read lines and return them as a list
-- see stream2list

-- |Print lines as they are received. This is the first `impure' iteratee
-- with non-trivial actions during chunk processing
printLines :: IterateeG String Char IO ()
printLines = lines'
  where
  lines' = Iter.break (\c -> c == '\r' || c == '\n') >>= \l ->
               terminators >>= check l
  check _  0 = return ()
  check "" _ = return ()
  check l  _ = liftIO (putStrLn l) >> lines'
  terminators = heads "\r\n" >>= \l -> if l == 0 then heads "\n" else return l


-- |Read a sequence of lines from the stream up to the empty lin
-- The line can be terminated by CR, LF, or CRLF -- or by EOF or stream error.
-- Return the read lines, in order, not including the terminating empty line
-- Upon EOF or stream error, return the complete, terminated lines accumulated
-- so far.

readLines :: (Monad m) => IterateeG String Char m (Either [Line] [Line])
readLines = lines' []
  where
  lines' acc = Iter.break (\c -> c == '\r' || c == '\n') >>= \l ->
               terminators >>= check acc l
  check acc _  0 = return . Left . reverse $ acc -- no terminator found
  check acc "" _ = return . Right . reverse $ acc
  check acc l  _ = lines' (l:acc)
  terminators = heads "\r\n" >>= \l -> if l == 0 then heads "\n" else return l


-- |Convert the stream of characters to the stream of lines, and
-- apply the given iteratee to enumerate the latter.
-- The stream of lines is normally terminated by the empty line.
-- When the stream of characters is terminated, the stream of lines
-- is also terminated, abnormally.
-- This is the first proper iteratee-enumerator: it is the iteratee of the
-- character stream and the enumerator of the line stream.

enumLines :: (Functor m, Monad m) =>
  IterateeG [Line] Line m a ->
  IterateeG String Char m (IterateeG [Line] Line m a)
enumLines iter = line >>= check iter
  where
  --check :: Either Line Line -> IterateeG String Char m (IterateeG [Line] Line m a)
  check iter' (Left l)  = runLine iter' l
  check iter' (Right l) = runLine iter' l
  runLine i' l = return . joinIM . fmap Iter.liftI $ runIter i' (Chunk [l])


-- |Convert the stream of characters to the stream of words, and
-- apply the given iteratee to enumerate the latter.
-- Words are delimited by white space.
-- This is the analogue of List.words
-- One should keep in mind that enumWords is a more general, monadic
-- function.

enumWords :: (Functor m, Monad m) =>
  IterateeG [String] String m a ->
  IterateeG String Char m (IterateeG [String] String m a)
enumWords iter = Iter.break isSpace >>= check iter
  where
  --check :: String -> IterateeG String Char m (IterateeG [String] String m a)
  check iter' "" = return iter'
  check iter' w  = return . joinIM . fmap Iter.liftI $ runIter iter' (Chunk [w])


-- ------------------------------------------------------------------------
-- Enumerators

type EnumeratorM m a = EnumeratorGM String Char m a
