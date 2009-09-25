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
  -- * Types
  EofBehavior (..),
  -- ** Type synonyms
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
  enumWords2,
  enumWords3,

  module Data.Iteratee.Base
)

where

import qualified Data.Iteratee.Base as Iter
import Data.Iteratee.Base hiding (break)
import Data.Char
import Data.Word
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BC

-- |A particular instance of StreamG: the stream of characters.
-- This stream is used by many input parsers.
type Stream = StreamG String

type Iteratee = IterateeG String Char

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

-- The code is the same as that of pure Iteratee, only the signature
-- has changed.
-- Compare the code below with GHCBufferIO.line_lazy
line :: Monad m => EofBehavior -> IterateeG String Char m (Either Line Line)
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


-- |Read a sequence of lines from the stream up to the empty line.
-- The line can be terminated by CR, LF, or CRLF -- or by EOF or stream error.
-- Return the read lines, in order, not including the terminating empty line
-- Upon stream error, return the complete, terminated lines accumulated
-- so far.
-- EOF is treated as either a stream error or an implicit EOL depending
-- upon the EofBehavior

readLines :: (Monad m) =>
  EofBehavior ->
  IterateeG String Char m (Either [Line] [Line])
readLines eb = lines' []
  where
    lines' acc = line eb >>= check acc
    check acc (Left _)   = return . Left . reverse $ acc
    check acc (Right "") = return . Right . reverse $ acc
    check acc (Right l)  = lines' (l:acc)

-- |Convert the stream of characters to the stream of lines, and
-- apply the given iteratee to enumerate the latter.
-- The stream of lines is normally terminated by the empty line.
-- When the stream of characters is terminated, the stream of lines
-- is also terminated, abnormally.
-- This is the first proper iteratee-enumerator: it is the iteratee of the
-- character stream and the enumerator of the line stream.

enumLines :: (Functor m, Monad m) =>
  EofBehavior ->
  IterateeG [Line] Line m a ->
  IterateeG String Char m (IterateeG [Line] Line m a)
enumLines eb iter = line eb >>= check iter
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
enumWords iter = {-# SCC "enumWords" #-} Iter.dropWhile isSpace >> Iter.break isSpace >>= check
  where
  --check :: String -> IterateeG String Char m (IterateeG [String] String m a)
  check "" = {-# SCC "enumWords/checkEnd" #-} return iter
  check w = {-# SCC "enumWords/check" #-} joinIM $ runIter iter (Chunk [w]) >>= check'
    where
      check' (Cont k Nothing)    = return . enumWords $ k
      check' (Done a _)          = return . return . return $ a
      check' (Cont _ (Just err)) = return . throwErr $ err

-- this still gives an incorrect answer, and it's only slightly faster than
-- enumWords...
-- incorrect answer is probably due to words that end on buffer boundaries,
-- which are currently elided into the next word.  Oops.
enumWords2 :: (Functor m, Monad m) =>
  IterateeG [String] String m a ->
  IterateeG String Char m (IterateeG [String] String m a)
enumWords2 iter = convStream getter iter
  where
    getter = {-# SCC "enumWord2/getter" #-} IterateeG step
    step (Chunk []) = return $ Cont getter Nothing
    step (Chunk xs) = return $ Cont (IterateeG (step' xs)) Nothing
    step str        = return $ Done Nothing str
    step' xs (Chunk []) = return $ Cont (IterateeG (step' xs)) Nothing
    step' xs (Chunk ys) = let ws = init $ words (xs ++ ys)
                              ck = last $ words ys
                          in return $ Done (Just ws) (Chunk ck)
    step' xs str    = return $ Done (Just $ words xs) str


-- this is even slower... probably causes a lot of copying...
-- trying doing pretty much the same thing, only changing check to have less
-- nesting; i.e. put it inside an IterateeG block.
-- alternatively, change the type of enumWords to operate in the monad
enumWords3 :: (Functor m, Monad m) =>
  IterateeG [BC.ByteString] BC.ByteString m a ->
  IterateeG BC.ByteString Word8 m (IterateeG [BC.ByteString] BC.ByteString m a)
enumWords3 iter = {-# SCC "enumWords3" #-} Iter.dropWhile iSpace >> Iter.break iSpace >>= check
  where
  iSpace = isSpace . chr . fromIntegral
  check w | BC.null w = return iter
  check w = {-# SCC "ew3/check" #-} joinIM $ runIter iter (Chunk [w]) >>= check'
    where
      check' (Done a _)          = return . return . return $ a
      check' (Cont k Nothing)    = return . enumWords3 $ k
      check' (Cont _ (Just err)) = return . throwErr $ err


-- ------------------------------------------------------------------------
-- Enumerators

type EnumeratorM m a = EnumeratorGM String Char m a
