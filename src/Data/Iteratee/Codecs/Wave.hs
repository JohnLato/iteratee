{-# LANGUAGE RankNTypes #-}
module Data.Iteratee.Codecs.Wave (
  WAVEDE (..),
  WAVEDE_ENUM (..),
  WAVE_TYPE (..),
  WAVE_CHUNK (..),
  AudioFormat (..),
  wave_reader,
  read_riff,
  wave_chunk,
  chunk_to_string
)
where

import Data.Iteratee.IterateeM
import Data.Iteratee.IO.RandomIO
import Control.Monad.Trans
import Data.Char (chr)
import Data.Int
import Data.Word
import qualified Data.IntMap as IM

-- =====================================================
-- WAVE libary code

-- |A WAVE directory is a list associating WAVE chunks with
-- a record WAVEDE
type WAVEDict = IM.IntMap [WAVEDE]

data WAVEDE = WAVEDE{
  wavede_count :: Int, -- ^length of chunk
  wavede_type :: WAVE_CHUNK, -- ^type of chunk
  wavede_enum :: WAVEDE_ENUM -- ^enumerator to get values of chunk
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
  | WAVE_OTHER String      -- ^Other
  deriving (Eq, Ord, Show)
instance Enum WAVE_CHUNK where
  fromEnum WAVE_FMT = 1
  fromEnum WAVE_DATA = 2
  fromEnum (WAVE_OTHER _) = 3
  toEnum 1 = WAVE_FMT
  toEnum 2 = WAVE_DATA
  toEnum 3 = WAVE_OTHER ""
  toEnum _ = error "Invalid enumeration value"

-- -----------------
-- wave chunk reading/writing functions

-- |Convert a string to WAVE_CHUNK type
wave_chunk :: String -> Maybe WAVE_CHUNK
wave_chunk str
  | str == "fmt " = Just WAVE_FMT
  | str == "data" = Just WAVE_DATA
  | length str == 4 = Just $ WAVE_OTHER str
  | otherwise = Nothing

-- |Convert a WAVE_CHUNK to the representative string
chunk_to_string :: WAVE_CHUNK -> String
chunk_to_string WAVE_FMT = "fmt "
chunk_to_string WAVE_DATA = "data"
chunk_to_string (WAVE_OTHER str) = str

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
  bindm snext $ \s1 ->
   bindm snext $ \s2 ->
   bindm snext $ \s3 ->
   bindm snext $ \s4 ->
   return . Just $ map (chr . fromIntegral) [s1, s2, s3, s4]

-- -----------------

-- |The library function to read the WAVE dictionary
wave_reader :: IterateeGM Word8 RBIO (Maybe WAVEDict)
wave_reader = do
  lift $ rb_msb_first_set False
  read_riff
  bindm endian_read4 $ \tot_size -> do
    read_riff_wave
    chunks_m <- find_chunks $ fromIntegral tot_size
    load_dict $ join_m chunks_m

-- |Read the RIFF header of a file.
read_riff :: IterateeGM Word8 RBIO ()
read_riff = do
  s <- string_read4
  case s == Just "RIFF" of
    True -> return ()
    False -> iter_err $ "Bad RIFF header: " ++ show s

-- | Read the WAVE part of the RIFF header.
read_riff_wave :: IterateeGM Word8 RBIO ()
read_riff_wave = do
  s <- string_read4
  case s == Just "WAVE" of
    True -> return ()
    False -> iter_err $ "Bad RIFF/WAVE header: " ++ show s

-- | An internal function to find all the chunks.  It assumes that the
-- stream is positioned to read the first chunk.
find_chunks :: Int -> IterateeGM Word8 RBIO (Maybe [(Int, WAVE_CHUNK, Int)])
find_chunks n = find_chunks' 12 []
  where
  find_chunks' offset acc = do
    bindm string_read4 $ \typ -> do
      count <- endian_read4
      case (wave_chunk typ, count) of
        (Nothing, _) -> (iter_err $ "Bad subchunk descriptor: " ++ show typ)
          >> return Nothing
        (_, Nothing) -> (iter_err $ "Bad subchunk length") >> return Nothing
        (Just chk, Just count') -> let newpos = offset + 8 + count' in
          case newpos >= (fromIntegral n) of
            True -> return . Just $ reverse acc --end iteration here
            False -> do
              sseek $ fromIntegral newpos
              find_chunks' newpos $
               (fromIntegral newpos, chk, fromIntegral count') : acc

load_dict :: [(Int, WAVE_CHUNK, Int)] ->
               IterateeGM Word8 RBIO (Maybe WAVEDict)
load_dict = foldl read_entry (return (Just IM.empty))
  where
  read_entry dictM (offset, typ, count) = do
    bindm dictM $ \dict -> do
      enum_m <- read_value dict offset typ count
      case (enum_m, IM.lookup (fromEnum typ) dict) of
        (Just enum, Nothing) -> --insert new entry
          return . Just $ IM.insert (fromEnum typ)
                                    [WAVEDE (fromIntegral count) typ enum] dict
        (Just enum, Just _vals) -> --existing entry
          return . Just $ IM.update
            (\ls -> Just $ ls ++ [WAVEDE (fromIntegral count) typ enum])
            (fromEnum typ) dict
        (Nothing, _) -> return (Just dict)

read_value :: WAVEDict ->
              Int -> -- ^Offset
              WAVE_CHUNK -> -- ^Chunk type
              Int -> -- ^Count
              IterateeGM Word8 RBIO (Maybe WAVEDE_ENUM)
read_value _dict offset _ 0 = do
  iter_err $ "Zero count in the entry of chunk at: " ++ show offset
  return Nothing

-- TODO - endian_reader and conv_dub need to be defined
read_value dict offset WAVE_DATA count = do
  fmt_m <- dict_read_last_format dict
  case fmt_m of
    Just fmt -> do
      return . Just . WEN_DUB $ \iter_dub -> do
        sseek (8 + fromIntegral offset)
        let iter = conv_stream
                     (bindm (endian_reader fmt)
                       (return . Just . (:[]) . conv_dub fmt))
                       iter_dub
        joinI $ joinI $ stakeR count ==<< iter
    Nothing -> do
      iter_err $ "No valid format for data chunk at: " ++ show offset
      return Nothing
  where
  --endian_reader (AudioFormat nc sr bd) = endian_read4
  --conv_dub (AudioFormat nc sr bd) = fromIntegral

-- return the WaveFormat iteratee
read_value _dict offset WAVE_FMT count =
  return . Just . WEN_BYTE $ \iter -> do
    sseek (8 + fromIntegral offset)
    joinI $ stakeR count iter

-- for WAVE_OTHER, return Word8s and maybe the user can parse them
read_value _dict offset (WAVE_OTHER _str) count =
  return . Just . WEN_BYTE $ \iter -> do
    sseek (8 + fromIntegral offset)
    joinI $ stakeR count iter

-- |An Iteratee to read a wave format chunk
sWaveFormat :: IterateeGM Word8 RBIO (Maybe AudioFormat)
sWaveFormat = do
  bindm endian_read2 $ \f' -> --data format, 1==PCM
   bindm endian_read2 $ \nc ->
   bindm endian_read4 $ \sr -> do
     sdrop 6
     bindm endian_read2 $ \bd ->
       case f' == 1 of
         True -> return . Just $ AudioFormat (fromIntegral nc)
                                             (fromIntegral sr)
                                             (fromIntegral bd)
         False -> return Nothing

-- ---------------------
-- functions to assist with reading from the dictionary

dict_read_format :: Int -> -- ^Index in the format chunk list to read
                    WAVEDict -> -- ^Dictionary
                    IterateeGM Word8 RBIO (Maybe AudioFormat)
dict_read_format ix dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = (!!) xs ix in do
    e <- enum ==<< sWaveFormat
    return e
  _ -> return Nothing

dict_read_last_format :: WAVEDict -> IterateeGM Word8 RBIO (Maybe AudioFormat)
dict_read_last_format dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = head $ reverse xs in
    enum ==<< sWaveFormat
  _ -> return Nothing

-- ---------------------
-- convenience functions

join_m :: Maybe [a] -> [a]
join_m Nothing = []
join_m (Just a) = a
