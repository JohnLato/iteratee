{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Seek (testSeek1, testSeek2) where

import Data.Iteratee as I
import Data.Data
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 as B
import Test.HUnit

import Control.Monad.Trans
import Control.Exception (bracket)
import System.IO
import System.Directory

data RewindExc = RewindExc deriving (Eq, Show, Data, Typeable)

instance Exception RewindExc where
    toException = iterExceptionToException
    fromException = iterExceptionFromException

instance IException RewindExc

rewindEtee :: Show a => Enumeratee s s IO a
rewindEtee = go
  where
    go = eneeCheckIfDoneHandle handler (icont . step)
    handler recOuter e = case fromIterException e of
        Just RewindExc -> I.seek 0 >> recOuter >>= go
        Nothing -> error $ "unhandled exception: " ++ show e
    step k NoData     = continue $ step k
    step k (Chunk cs) = doContEtee go k cs
    step k (EOF mErr) = do
        res <- k (EOF mErr)
        case res of
          ContDone a _ -> contDoneM (return a) (EOF mErr)
          ContMore i   -> contDoneM i (EOF mErr)
          ContErr i e  -> contErrM (throwRec e (go i)) e


iSeek1 :: Iteratee ByteString IO (ByteString,ByteString)
iSeek1 = do
  s1 <- joinI $ takeUpTo 24 $ stream2stream
  I.seek 0
  s2 <- joinI $ takeUpTo 12 $ stream2stream
  return (s1,s2)

iSeek2 :: Iteratee ByteString IO (ByteString,ByteString)
iSeek2 = do
  s1 <- joinI $ takeUpTo 24 $ stream2stream
  throwRec RewindExc (return ())
  s2 <- joinI $ takeUpTo 12 $ stream2stream
  return (s1,s2)

testSeek1 :: Assertion
testSeek1 = withTestData $ \fp -> do
    result <- fileDriverRandomVBuf 2 iSeek1 fp
    assertEqual "standard seek" result testData

testSeek2 :: Assertion
testSeek2 = withTestData $ \fp -> do
    result <- fileDriverRandomVBuf 3 (joinI $ rewindEtee iSeek2) fp
    assertEqual "seek through enumeratees" result testData

testData1 = pack "Text in a flat file\nread"
testData2 = B.take 12 testData1
testData = (testData1,testData2)

withTestData :: (FilePath -> Assertion) -> Assertion
withTestData = bracket opener removeFile
  where
    opener = do
        tdir <- getTemporaryDirectory
        (fpath,handle) <- openTempFile tdir "iteratee-test"
        B.hPut handle testFile
        hClose handle
        return fpath


testFile = pack "Text in a flat file\n\
                \read from the beginning\n\
                \shouldn't one check again?"
