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
  StreamG (..),
  Stream,
  IterateeG (..),
  IterateeGM (..),
  Iteratee,
  IterateeM,
  liftI,
  (>>==),
  (==<<),
  joinI,
  stream2list,
  iter_report_err,
  sbreak,
  sdropWhile,
  snext,
  speek,
  skip_till_eof,
  sdrop,
  sseek,
  EnumeratorN,
  stake,
  map_stream,
  conv_stream,
  Line,
  line,
  print_lines,
  enum_lines,
  enum_words,
  EnumeratorGM,
  EnumeratorM,
  EnumeratorGMM,
  enum_eof,
  enum_err,
  (>.),
  enum_pure_1chunk,
  enum_pure_nchunk
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
-- It is instructive to compare the code below with the code of
-- List.words, which is:
-- words                   :: String -> [String]
-- words s                 =  case dropWhile isSpace s of
--                                 "" -> []
--                                 s' -> w : words s''
--                                       where (w, s'') =
--                                              break isSpace s'
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

-- HTTP chunk decoding
-- Each chunk has the following format:
--
-- 	  <chunk-size> CRLF <chunk-data> CRLF
--
-- where <chunk-size> is the hexadecimal number; <chunk-data> is a
-- sequence of <chunk-size> bytes.
-- The last chunk (so-called EOF chunk) has the format
-- 0 CRLF CRLF (where 0 is an ASCII zero, a character with the decimal code 48).
-- For more detail, see "Chunked Transfer Coding", Sec 3.6.1 of
-- the HTTP/1.1 standard:
-- http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.6.1

-- The following enum_chunk_decoded has the signature of the enumerator
-- of the nested (encapsulated and chunk-encoded) stream. It receives
-- an iteratee for the embedded stream and returns the iteratee for
-- the base, embedding stream. Thus what is an enumerator and what
-- is an iteratee may be a matter of perspective.

-- We have a decision to make: Suppose an iteratee has finished (either because
-- it obtained all needed data or encountered an error that makes further
-- processing meaningless). While skipping the rest of the stream/the trailer,
-- we encountered a framing error (e.g., missing CRLF after chunk data).
-- What do we do? We chose to disregard the latter problem.
-- Rationale: when the iteratee has finished, we are in the process
-- of skipping up to the EOF (draining the source).
-- Disregarding the errors seems OK then.
-- Also, the iteratee may have found an error and decided to abort further
-- processing. Flushing the remainder of the input is reasonable then.
-- One can make a different choice...

{-
enum_chunk_decoded :: Monad m => Iteratee m a -> IterateeM m a
enum_chunk_decoded = docase
 where
 docase iter@IE_done{} =
    liftI iter >>= (\r -> (enum_chunk_decoded ==<< skip_till_eof) >> return r)
 docase iter@(IE_cont k) = line >>= check_size
  where
  check_size (Right "0") = line >> k EOF
  check_size (Right str) =
     maybe (k . Err $ "Bad chunk size: " ++ str) (read_chunk iter)
         $ read_hex 0 str
  check_size _ = k (Err "Error reading chunk size")

 read_chunk iter size =
     do
     r  <- stake size iter
     c1 <- snext
     c2 <- snext
     case (c1,c2) of
       (Just '\r',Just '\n') -> docase r
       _ -> (enum_chunk_decoded ==<< skip_till_eof) >>
	    enum_err "Bad chunk trailer" r

 read_hex acc "" = Just acc
 read_hex acc (d:rest) | isHexDigit d = read_hex (16*acc + digitToInt d) rest
 read_hex _acc _ = Nothing


-- ------------------------------------------------------------------------
-- Tests

-- Pure tests, requiring no IO

test_str1 :: String
test_str1 =
    "header1: v1\rheader2: v2\r\nheader3: v3\nheader4: v4\n" ++
    "header5: v5\r\nheader6: v6\r\nheader7: v7\r\n\nrest\n"

testp1 :: Bool
testp1 =
    let IE_done (IE_done lines' EOF) (Chunk rest)
	    = runIdentity . unIM $ enum_pure_1chunk test_str1 ==<<
	                             (enum_lines ==<< stream2list)
    in
    lines' == ["header1: v1","header2: v2","header3: v3","header4: v4",
	      "header5: v5","header6: v6","header7: v7"]
    && rest == "rest\n"

testp2 :: Bool
testp2 =
    let IE_done (IE_done lines' EOF) (Chunk rest)
	    = runIdentity . unIM $ enum_pure_nchunk test_str1 5 ==<<
	                             (enum_lines ==<< stream2list)
    in
    lines' == ["header1: v1","header2: v2","header3: v3","header4: v4",
	      "header5: v5","header6: v6","header7: v7"]
    && rest == "r"


testw1 :: Bool
testw1 =
    let test_str = "header1: v1\rheader2: v2\r\nheader3:\t v3"
	expected = ["header1:","v1","header2:","v2","header3:","v3"] in
    let run_test test_str' =
         let IE_done (IE_done words' EOF) EOF
	       = runIdentity . unIM $ (enum_pure_nchunk test_str' 5 >. enum_eof)
	                                ==<< (enum_words ==<< stream2list)
         in words'
    in
    and [run_test test_str == expected,
	 run_test (test_str ++ " ") == expected]

-- Run the complete test, reading the headers and the body

-- This simple iteratee is used to process a variety of streams:
-- embedded, interleaved, etc.
line_printer :: IterateeGM [] Char IO (IterateeG [] Line IO ())
line_printer = enum_lines ==<< print_lines

-- Two sample processors

-- Read the headers, print the headers, read the lines of the chunk-encoded
-- body and print each line as it has been read
read_headers_print_body :: IterateeGM Char IO (IterateeG Line IO ())
read_headers_print_body = do
     headers' <- enum_lines ==<< stream2list
     case headers' of
	IE_done headers EOF -> lift $ do
	   putStrLn "Complete headers"
	   print headers
	IE_done headers (Err err) -> lift $ do
	   putStrLn $ "Incomplete headers due to " ++ err
	   print headers
        _ -> lift $ putStrLn "Pattern not matched"

     lift $ putStrLn "\nLines of the body follow"
     enum_chunk_decoded ==<< line_printer

-- Read the headers and print the header right after it has been read
-- Read the lines of the chunk-encoded body and print each line as
-- it has been read
print_headers_print_body :: IterateeGM Char IO (IterateeG Line IO ())
print_headers_print_body = do
     lift $ putStrLn "\nLines of the headers follow"
     line_printer
     lift $ putStrLn "\nLines of the body follow"
     enum_chunk_decoded ==<< line_printer

-}
