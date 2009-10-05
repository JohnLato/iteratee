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
  ,EnumeratorM
  ,Line
  -- * Word and Line processors
  ,line
  ,printLines
  ,readLines
  ,enumLines
  ,enumLinesBS
  ,enumWords
  ,enumWordsBS

  ,module Data.Iteratee.IterateeT
)

where

import qualified Data.Iteratee.IterateeT as Iter
import Data.Iteratee.IterateeT hiding (break)
import Data.Char
import Data.Word
import Data.Monoid
import qualified Data.ListLike as LL
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as BC

-- |A particular instance of StreamG: the stream of characters.
-- This stream is used by many input parsers.
type Stream = StreamG String

type Iteratee = IterateeT String Char

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
line :: Monad m => EofBehavior -> IterateeT String Char m (Either Line Line)
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
printLines :: IterateeT String Char IO ()
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
  IterateeT String Char m (Either [Line] [Line])
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

enumLines :: (LL.ListLike s el, LL.StringLike s, Functor m, Monad m) =>
  IterateeT [s] s m a ->
  IterateeT s el m (IterateeT [s] s m a)
enumLines = convStream getter
  where
    getter = IterateeT step
    lChar = (== '\n') . last . LL.toString
    step (Chunk xs)
      | LL.null xs = return $ Cont getter Nothing
      | lChar xs   = return $ Done (Just $ LL.lines xs) (Chunk mempty)
      | True       = return $ Cont (IterateeT (step' xs)) Nothing
    step str       = return $ Done Nothing str
    step' xs (Chunk ys)
      | LL.null ys = return $ Cont (IterateeT (step' xs)) Nothing
      | lChar ys   = return $ Done (Just . LL.lines . mappend xs $ ys)
                                   (Chunk mempty)
      | True       = let w' = LL.lines $ mappend xs ys
                         ws = init w'
                         ck = last w'
                     in return $ Done (Just ws) (Chunk ck)
    step' xs str   = return $ Done (Just $ LL.lines xs) str

-- |Convert the stream of characters to the stream of words, and
-- apply the given iteratee to enumerate the latter.
-- Words are delimited by white space.
-- This is the analogue of List.words
-- One should keep in mind that enumWords is a more general, monadic
-- function.

enumWords :: (LL.ListLike s el, LL.StringLike s, Functor m, Monad m) =>
  IterateeT [s] s m a ->
  IterateeT s el m (IterateeT [s] s m a)
enumWords = convStream getter
  where
    getter = IterateeT step
    lChar = isSpace . last . LL.toString
    step (Chunk xs) | LL.null xs = return $ Cont getter Nothing
    step (Chunk xs)
      | LL.null xs = return $ Cont getter Nothing
      | lChar xs   = return $ Done (Just $ LL.words xs) (Chunk mempty)
      | True       = return $ Cont (IterateeT (step' xs)) Nothing
    step str       = return $ Done Nothing str
    step' xs (Chunk ys)
      | LL.null ys = return $ Cont (IterateeT (step' xs)) Nothing
      | lChar ys   = return $ Done (Just . LL.words . mappend xs $ ys)
                                   (Chunk mempty)
      | True       = let w' = LL.words $ mappend xs ys
                         ws = init w'
                         ck = last w'
                     in return $ Done (Just ws) (Chunk ck)
    step' xs str   = return $ Done (Just $ LL.words xs) str

{-# INLINE enumWords #-}

-- Like enumWords, but operates on ByteStrings.
-- This is provided as a higher-performance alternative to enumWords.
enumWordsBS :: (Functor m, Monad m) =>
  IterateeT [BC.ByteString] BC.ByteString m a ->
  IterateeT BC.ByteString Word8 m (IterateeT [BC.ByteString] BC.ByteString m a)
enumWordsBS iter = convStream getter iter
  where
    getter = IterateeT step
    lChar = isSpace . BC.last
    step (Chunk xs)
      | BC.null xs = return $ Cont getter Nothing
      | lChar xs   = return $ Done (Just $ BC.words xs) (Chunk BC.empty)
      | True       = return $ Cont (IterateeT (step' xs)) Nothing
    step str       = return $ Done Nothing str
    step' xs (Chunk ys)
      | BC.null ys = return $ Cont (IterateeT (step' xs)) Nothing
      | lChar ys   = return $ Done (Just . BC.words . BC.append xs $ ys)
                                   (Chunk BC.empty)
      | True       = let w' = BC.words . BC.append xs $ ys
                         ws = init w'
                         ck = last w'
                     in return $ Done (Just ws) (Chunk ck)
    step' xs str   = return $ Done (Just $ BC.words xs) str

{-# INLINE enumWordsBS #-}

enumLinesBS :: (Functor m, Monad m) =>
  IterateeT [BC.ByteString] BC.ByteString m a ->
  IterateeT BC.ByteString Word8 m (IterateeT [BC.ByteString] BC.ByteString  m a)
enumLinesBS = convStream getter
  where
    getter = IterateeT step
    lChar = (== '\n') . BC.last
    step (Chunk xs)
      | BC.null xs = return $ Cont getter Nothing
      | lChar xs   = return $ Done (Just $ BC.lines xs) (Chunk BC.empty)
      | True       = return $ Cont (IterateeT (step' xs)) Nothing
    step str       = return $ Done Nothing str
    step' xs (Chunk ys)
      | BC.null ys = return $ Cont (IterateeT (step' xs)) Nothing
      | lChar ys   = return $ Done (Just . BC.lines . BC.append xs $ ys)
                                   (Chunk BC.empty)
      | True       = let w' = BC.lines $ BC.append xs ys
                         ws = init w'
                         ck = last w'
                     in return $ Done (Just ws) (Chunk ck)
    step' xs str   = return $ Done (Just $ BC.lines xs) str


-- ------------------------------------------------------------------------
-- Enumerators

type EnumeratorM m a = EnumeratorGM String Char m a
