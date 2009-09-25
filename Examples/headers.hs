import Data.Iteratee
import qualified Data.Iteratee as Iter
import Data.Iteratee.Char
import qualified Data.Iteratee.IO as IIO
import Control.Monad.Trans
import Control.Monad.Identity
import Data.Char
import Data.Word


-- HTTP chunk decoding
-- Each chunk has the following format:
--
--        <chunk-size> CRLF <chunk-data> CRLF
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
-- Upon further consideration, I reversed the earlier decision:
-- if we detected a framing error, we can't trust the rest of the stream
-- We can't skip till the EOF chunk as we aren't even sure we can
-- recognize the EOF chunk any more.
-- So, we just report the _recoverable_ error upstream:
-- the recovery will be to report the accumlated nested iteratee.

enum_chunk_decoded :: Monad m => Iteratee m a -> m (Iteratee m a)
enum_chunk_decoded iter = return read_size
  where
  read_size = Iter.break (== '\r') >>= checkCRLF iter . check_size
  checkCRLF iter' m = do
    n <- heads "\r\n"
    if n == 2 then m else frame_err "Bad Chunk: no CRLF" iter'
  check_size "0" = checkCRLF iter (joinIM $ enumEof iter)
  check_size str@(_:_) =
    maybe (frame_err ("Bad chunk size: " ++ str) iter) read_chunk $
      read_hex 0 str
  check_size _ = frame_err "Error reading chunk size" iter

  read_chunk size = Iter.take size iter >>= \r -> checkCRLF r (joinIM $ enum_chunk_decoded r)
  read_hex acc "" = Just acc
  read_hex acc (d:rest) | isHexDigit d = read_hex (16*acc + digitToInt d) rest
  read_hex acc _ = Nothing

  frame_err e iter = IterateeT (\_ ->
                     return $ Cont (joinIM $ enumErr e iter)
                     (Just $ Err "Frame error"))

-- ------------------------------------------------------------------------
-- Tests

-- Pure tests, requiring no IO

read_lines_rest :: Iteratee Identity (Either [Line] [Line], String)
read_lines_rest = do
  ls <- readLines ErrOnEof
  rest <- Iter.break (const False)
  return (ls, rest)

test_str1 :: String
test_str1 =
    "header1: v1\rheader2: v2\r\nheader3: v3\nheader4: v4\n" ++
    "header5: v5\r\nheader6: v6\r\nheader7: v7\r\n\nrest\n"

testp1 :: Bool
testp1 =
    let (Right lines, rest) = runIdentity . run . joinIM $
         enumPure1Chunk test_str1 read_lines_rest
    in
    lines == ["header1: v1","header2: v2","header3: v3","header4: v4",
             "header5: v5","header6: v6","header7: v7"]
    && rest == "rest\n"

testp2 :: Bool
testp2 =
    let (Right lines, rest) = runIdentity . run . joinIM $
                              enumPureNChunk test_str1 5 read_lines_rest
    in
    lines == ["header1: v1","header2: v2","header3: v3","header4: v4",
             "header5: v5","header6: v6","header7: v7"]
    && rest == "rest\n"


-- Run the complete test, reading the headers and the body

test_driver_full filepath = do
  putStrLn "About to read headers"
  result <- fileDriver read_headers_body filepath
  putStrLn "Finished reading"
  case result of
    (Right headers, Right body, _) ->
      do
      putStrLn "Complete headers"
      print headers
      putStrLn "\nComplete body"
      print body
    (Left headers, _, status) ->
      do
      putStrLn $ "Problem " ++ show status
      putStrLn "Incomplete headers"
      print headers
    (Right headers, Left body, status) ->
      do
      putStrLn "Complete headers"
      print headers
      putStrLn $ "Problem " ++ show status
      putStrLn "Incomplete body"
      print body
 where
  read_headers_body = do
    headers <- readLines ErrOnEof
    body <- joinIM . enum_chunk_decoded $ readLines ErrOnEof
    status <- getStatus
    return (headers, body, status)

test31 = do
  putStrLn "Expected result is:"
  putStrLn "About to read headers"
  putStrLn "Finished reading"
  putStrLn "Complete headers"
  putStrLn "[\"header1: v1\",\"header2: v2\",\"header3: v3\",\"header4: v4\"]"
  putStrLn "Problem EofNoError"
  putStrLn "Incomplete body"
  putStrLn "[\"body line 1\",\"body line    2\",\"body line       3\",\"body line          4\"]"
  putStrLn ""
  putStrLn "Actual result is:"
  test_driver_full "test_full1.txt"

test32 = do
  putStrLn "Expected result is:"
  putStrLn "About to read headers"
  putStrLn "*** Exception: control message: Just (Err \"Frame error\")"

  putStrLn ""
  putStrLn "Actual result is:"
  test_driver_full "test_full2.txt"

test33 = do
  putStrLn "Expected result is:"
  putStrLn "About to read headers"
  putStrLn "Finished reading"
  putStrLn "Complete headers"
  putStrLn "[\"header1: v1\",\"header2: v2\",\"header3: v3\",\"header4: v4\"]"
  putStrLn "Problem EofNoError"
  putStrLn "Incomplete body"
  putStrLn "[\"body line 1\",\"body line    2\",\"body line       3\",\"body line          4\",\"body line             5\"]"

  putStrLn ""
  putStrLn "Actual result is:"
  test_driver_full "test_full3.txt"
