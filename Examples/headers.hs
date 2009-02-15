import Data.Iteratee
import Data.Iteratee.Char
import qualified Data.Iteratee.IO as IIO
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Char

import System.Posix


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
read_headers_print_body :: IterateeGM [] Char IO (IterateeG [] Line IO ())
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
print_headers_print_body :: IterateeGM [] Char IO (IterateeG [] Line IO ())
print_headers_print_body = do
     lift $ putStrLn "\nLines of the headers follow"
     line_printer
     lift $ putStrLn "\nLines of the body follow"
     enum_chunk_decoded ==<< line_printer

-- This iteratee uses a map_stream to convert the stream from
-- Word8 elements to Char elements.
test_driver_full iter filepath = do
  fd <- openFd filepath ReadOnly Nothing defaultFileFlags
  putStrLn "About to read headers"
  unIM $ (IIO.enum_fd fd >. enum_eof) ==<< (map_stream (chr . fromIntegral) ==<< iter)
  closeFd fd
  putStrLn "Finished reading"

test31 = test_driver_full read_headers_print_body "test_full1.txt"
test32 = test_driver_full read_headers_print_body "test_full2.txt"
test33 = test_driver_full read_headers_print_body "test_full3.txt"

test34 = test_driver_full print_headers_print_body "test_full3.txt"

