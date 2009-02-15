-- Haskell98!

-- Utilties for Char-based iteratee processing

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
  Stream,
  Iteratee,
  IterateeM,
  EnumeratorM,
  Line,
  line,
  print_lines,
  enum_lines,
  enum_words,
  module Data.Iteratee.Base
)

where

import Data.Iteratee.Base
import Data.Char
import Control.Monad.Trans

-- |A particular instance of StreamG: the stream of characters.
-- This stream is used by many input parsers.
type Stream = StreamG [] Char

type Iteratee  m a = IterateeG  [] Char m a
type IterateeM m a = IterateeGM [] Char m a

-- Useful combinators for implementing iteratees and enumerators

type Line = String	-- The line of text, terminators are not included

-- |Read the line of text from the stream
-- The line can be terminated by CR, LF or CRLF.
-- Return (Right Line) if successful. Return (Left Line) if EOF or
-- a stream error were encountered before the terminator is seen.
-- The returned line is the string read so far.

-- The code is the same as that of pure Iteratee, only the signature
-- has changed.
-- Compare the code below with GHCBufferIO.line_lazy
line :: Monad m => IterateeM m (Either Line Line)
line = sbreak (\c -> c == '\r' || c == '\n') >>= check_next
 where
 check_next (line',Just '\r') = speek >>= \c ->
	case c of
	  Just '\n' -> snext >> return (Right line')
	  Just _    -> return (Right line')
	  Nothing   -> return (Left line')
 check_next (line',Just _)  = return (Right line')
 check_next (line',Nothing) = return (Left line')


-- Line iteratees: processors of a stream whose elements are made of Lines

-- Collect all read lines and return them as a list
-- see stream2list

-- |Print lines as they are received. This is the first `impure' iteratee
-- with non-trivial actions during chunk processing
print_lines :: IterateeGM [] Line IO ()
print_lines = liftI $ IE_cont step
 where
 step (Chunk []) = print_lines
 step (Chunk ls) = lift (mapM_ pr_line ls) >> print_lines
 step EOF        = lift (putStrLn ">> natural end") >> liftI (IE_done () EOF)
 step stream     = lift (putStrLn ">> unnatural end") >>
		   liftI (IE_done () stream)
 pr_line line' = putStrLn $ ">> read line: " ++ line'


-- |Convert the stream of characters to the stream of lines, and
-- apply the given iteratee to enumerate the latter.
-- The stream of lines is normally terminated by the empty line.
-- When the stream of characters is terminated, the stream of lines
-- is also terminated, abnormally.
-- This is the first proper iteratee-enumerator: it is the iteratee of the
-- character stream and the enumerator of the line stream.

enum_lines :: Monad m =>
	      IterateeG [] Line m a ->
              IterateeGM [] Char m (IterateeG [] Line m a)
enum_lines iter@IE_done{} = return iter
enum_lines IE_jmp{}       = error "Seeking is not supported by enum_lines"
enum_lines (IE_cont k) = line >>= check_line k
 where
 check_line k' (Right "") =
   enum_lines ==<< k' EOF      -- empty line, normal term
 check_line k' (Right l)  = enum_lines ==<< k' (Chunk [l])
 check_line k' _          = enum_lines ==<< k' (Err "EOF") -- abnormal termin

-- |Convert the stream of characters to the stream of words, and
-- apply the given iteratee to enumerate the latter.
-- Words are delimited by white space.
-- This is the analogue of List.words
-- One should keep in mind that enum_words is a more general, monadic
-- function.

enum_words :: Monad m =>
	      IterateeG [] String m a ->
              IterateeGM [] Char m (IterateeG [] String m a)
enum_words iter@IE_done{} = return iter
enum_words IE_jmp{}       = error "Seeking not supported by enum_words"
enum_words (IE_cont k) = sdropWhile isSpace >> sbreak isSpace >>= check_word k
 where
 check_word k' ("",_)  = enum_words ==<< k' EOF
 check_word k' (str,_) = enum_words ==<< k' (Chunk [str])


-- ------------------------------------------------------------------------
-- Enumerators

type EnumeratorM m a = EnumeratorGM [] Char m a

