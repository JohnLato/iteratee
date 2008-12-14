{-# LANGUAGE RankNTypes #-}
module Data.Iteratee.Codecs.Wave (
  WAVEDE (..),
  WAVEDE_ENUM (..),
  WAVE_TYPE (..),
  WAVE_CHUNK (..),
  AudioFormat (..),
  wave_reader,
  read_riff
)
where

import Data.Iteratee.IterateeM
import Data.Iteratee.IO.RandomIO
import Control.Monad.Trans
import Data.Char (chr)
import Data.Int
import Data.Word
import Data.Ratio
import qualified Data.IntMap as IM
import Data.Maybe (catMaybes)

-- =====================================================
-- WAVE libary code

-- |A WAVE directory is a finite map associating a WAVE chunk with
-- a rcord WAVEDE
type WAVEDict = IM.IntMap WAVEDE

data WAVEDE = WAVEDE{
  wavede_count :: Int, -- ^number of items
  wavede_enum :: WAVEDE_ENUM -- ^enumerator to get values
  }

data WAVEDE_ENUM = WEN_CHAR (forall a. EnumeratorGMM Word8 Char RBIO a)
                 | WEN_BYTE (forall a. EnumeratorGMM Word8 Word8 RBIO a)
                 | WEN_INT  (forall a. EnumeratorGMM Word8 Integer RBIO a)
                 | WEN_DUB  (forall a. EnumeratorGMM Word8 Double RBIO a)

-- |Standard WAVE data types
data WAVE_TYPE = WAVE_NONE -- ^None
  | WAVE_BYTE              -- ^8-bit signed integer
  | WAVE_SSHORT            -- ^16-bit signed integer
  | WAVE_S24               -- ^24-bit signed integer
  | WAVE_SLONG             -- ^32-bit signed integer
  deriving (Eq, Enum, Ord, Bounded, Show)

-- |Standard WAVE Chunks
data WAVE_CHUNK = WAVE_FMT -- ^Format
  | WAVE_DATA              -- ^Data
  | WAVE_OTHER             -- ^Other
  deriving (Eq, Enum, Ord, Bounded, Show)

-- -----------------
-- throw this in here for now...
data AudioFormat = AudioFormat {
  numberOfChannels :: NumChannels, -- ^Number of channels in the audio data
  sampleRate :: SampleRate, -- ^Sample rate of the audio
  bitDepth :: BitDepth -- ^Bit depth of the audio data
  } deriving (Show, Eq)

type NumChannels = Integer
type SampleRate = Integer
type BitDepth = Integer

-- convenience function to read a 4-byte ASCII string
string_read4 :: Monad m => IterateeGM Word8 m (Maybe String)
string_read4 = do
  s1 <- snext
  s2 <- snext
  s3 <- snext
  s4 <- snext
  let str = map (chr . fromIntegral) $ catMaybes [s1, s2, s3, s4]
  return $ s4 >> return str

-- -----------------

-- |The library function to read the WAVE dictionary
wave_reader :: IterateeGM Word8 RBIO (Maybe WAVEDict)
wave_reader = do
  read_riff

-- |Read the RIFF header of a file, returning the total size (bytes)
-- if it's a valid WAVE file, otherwise Nothing.
read_riff :: IterateeGM Word8 RBIO (Maybe Int32)
read_riff = do
  lift $ rb_msb_first_set False
  s <- string_read4
  n <- endian_read4
  s2 <- string_read4
  case (s == Just "RIFF", s2 == Just "WAVE") of
    (True, True) -> return $ fmap fromIntegral n
    _ -> iter_err ("Bad RIFF header: " ++ show (s, s2)) >> return Nothing
