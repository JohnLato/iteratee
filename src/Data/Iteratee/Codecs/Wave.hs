{-# LANGUAGE RankNTypes, FlexibleContexts #-}

{-

This module is not meant primarily for instructive and pedagogical purposes.
As such, it is not fully featured, and sacrifices performance and generality
for clarity of code.

-}

module Data.Iteratee.Codecs.Wave (
  WAVEDE (..),
  WAVEDE_ENUM (..),
  WAVE_CHUNK (..),
  AudioFormat (..),
  wave_reader,
  read_riff,
  wave_chunk,
  chunk_to_string,
  dict_read_format,
  dict_read_first_format,
  dict_read_last_format,
  dict_read_first_data,
  dict_read_last_data,
  dict_read_data,
  dict_process_data
)
where

import Data.Iteratee.Base
import qualified Data.Iteratee.Base as Iter
import Data.Iteratee.Binary
import Data.Char (chr, ord)
import Data.Int
import Data.Word
import Data.Bits (shiftL)
import Data.Maybe
import qualified Data.IntMap as IM

-- =====================================================
-- WAVE libary code

-- useful type synonyms

type L = []

-- |A WAVE directory is a list associating WAVE chunks with
-- a record WAVEDE
type WAVEDict = IM.IntMap [WAVEDE]

data WAVEDE = WAVEDE{
  wavede_count :: Int, -- ^length of chunk
  wavede_type :: WAVE_CHUNK, -- ^type of chunk
  wavede_enum :: WAVEDE_ENUM -- ^enumerator to get values of chunk
  }

data WAVEDE_ENUM =
  WEN_BYTE  (forall a. EnumeratorGMM L Word8 L Word8 IO a)
  | WEN_DUB (forall a. EnumeratorGMM L Word8 L Double IO a)

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
data AudioFormat = AudioFormat {
  numberOfChannels :: NumChannels, -- ^Number of channels in the audio data
  sampleRate :: SampleRate, -- ^Sample rate of the audio
  bitDepth :: BitDepth -- ^Bit depth of the audio data
  } deriving (Show, Eq)

type NumChannels = Integer
type SampleRate = Integer
type BitDepth = Integer

-- convenience function to read a 4-byte ASCII string
string_read4 :: Monad m => IterateeG L Word8 m String
string_read4 = do
  s1 <- Iter.head
  s2 <- Iter.head
  s3 <- Iter.head
  s4 <- Iter.head
  return $ map (chr . fromIntegral) [s1, s2, s3, s4]

-- -----------------

-- |The library function to read the WAVE dictionary
wave_reader :: IterateeG L Word8 IO (Maybe WAVEDict)
wave_reader = do
  read_riff
  tot_size <- endianRead4 LSB
  read_riff_wave
  chunks_m <- find_chunks $ fromIntegral tot_size
  load_dict $ join_m chunks_m

-- |Read the RIFF header of a file.
read_riff :: IterateeG L Word8 IO ()
read_riff = do
  cnt <- heads $ fmap (fromIntegral . ord) "RIFF"
  if cnt == 4 then return () else throwErr $ Err "Bad RIFF header"

-- | Read the WAVE part of the RIFF header.
read_riff_wave :: IterateeG L Word8 IO ()
read_riff_wave = do
  cnt <- heads $ fmap (fromIntegral . ord) "WAVE"
  if cnt == 4 then return () else throwErr $ Err "Bad RIFF/WAVE header"

-- | An internal function to find all the chunks.  It assumes that the
-- stream is positioned to read the first chunk.
find_chunks :: Int -> IterateeG L Word8 IO (Maybe [(Int, WAVE_CHUNK, Int)])
find_chunks n = find_chunks' 12 []
  where
  find_chunks' offset acc = do
    typ <- string_read4
    count <- endianRead4 LSB
    case wave_chunk typ of
      Nothing -> (throwErr . Err $ "Bad subchunk descriptor: " ++ show typ)
        >> return Nothing
      Just chk -> let newpos = offset + 8 + count in
        case newpos >= fromIntegral n of
          True -> return . Just $ reverse $
              (fromIntegral offset, chk, fromIntegral count) : acc
          False -> do
            Iter.seek $ fromIntegral newpos
            find_chunks' newpos $
             (fromIntegral offset, chk, fromIntegral count) : acc

load_dict :: [(Int, WAVE_CHUNK, Int)] ->
               IterateeG L Word8 IO (Maybe WAVEDict)
load_dict = foldl read_entry (return (Just IM.empty))
  where
  read_entry dictM (offset, typ, count) = do
    dict' <- dictM
    maybe (return Nothing) (\dict -> do
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
    ) dict'

read_value :: WAVEDict ->
              Int -> -- Offset
              WAVE_CHUNK -> -- Chunk type
              Int -> -- Count
              IterateeG L Word8 IO (Maybe WAVEDE_ENUM)
read_value _dict offset _ 0 = do
  throwErr . Err $ "Zero count in the entry of chunk at: " ++ show offset
  return Nothing

read_value dict offset WAVE_DATA count = do
  fmt_m <- dict_read_last_format dict
  case fmt_m of
    Just fmt ->
      return . Just . WEN_DUB $ \iter_dub -> return $ do
        Iter.seek (8 + fromIntegral offset)
        let iter = Iter.convStream (conv_func fmt) iter_dub
        Iter.joinI $ Iter.joinI $ Iter.takeR count iter
    Nothing -> do
      throwErr . Err $ "No valid format for data chunk at: " ++ show offset
      return Nothing

-- return the WaveFormat iteratee
read_value _dict offset WAVE_FMT count =
  return . Just . WEN_BYTE $ \iter -> return $ do
    Iter.seek (8 + fromIntegral offset)
    Iter.joinI $ Iter.takeR count iter

-- for WAVE_OTHER, return Word8s and maybe the user can parse them
read_value _dict offset (WAVE_OTHER _str) count =
  return . Just . WEN_BYTE $ \iter -> return $ do
    Iter.seek (8 + fromIntegral offset)
    Iter.joinI $ Iter.takeR count iter

unroller :: (Integral a, Monad m) =>
            IterateeG L Word8 m a ->
            IterateeG L Word8 m (StreamG L a)
unroller iter = do
  w1 <- iter
  w2 <- iter
  w3 <- iter
  w4 <- iter
  return $ Chunk [w1, w2, w3, w4]

-- |Convert Word8s to Doubles
conv_func :: AudioFormat -> IterateeG L Word8 IO (StreamG L Double)
conv_func (AudioFormat _nc _sr 8) = (fmap . fmap)
  (normalize 8 . (fromIntegral :: Word8 -> Int8)) (unroller Iter.head)
conv_func (AudioFormat _nc _sr 16) = (fmap . fmap)
  (normalize 16 . (fromIntegral :: Word16 -> Int16))
    (unroller (endianRead2 LSB))
conv_func (AudioFormat _nc _sr 24) = (fmap . fmap)
  (normalize 24 . (fromIntegral :: Word32 -> Int32))
    (unroller (endianRead3 LSB))
conv_func (AudioFormat _nc _sr 32) = (fmap . fmap)
  (normalize 32 . (fromIntegral :: Word32 -> Int32))
    (unroller (endianRead4 LSB))
conv_func _ = return $ EOF (Just $ Err "Invalid wave bit depth")

-- |An Iteratee to read a wave format chunk
sWaveFormat :: IterateeG L Word8 IO (Maybe AudioFormat)
sWaveFormat = do
  f' <- endianRead2 LSB --data format, 1==PCM
  nc <- endianRead2 LSB
  sr <- endianRead4 LSB
  Iter.drop 6
  bd <- endianRead2 LSB
  case f' == 1 of
    True -> return . Just $ AudioFormat (fromIntegral nc)
                                        (fromIntegral sr)
                                        (fromIntegral bd)
    False -> return Nothing

-- ---------------------
-- functions to assist with reading from the dictionary

-- |Read the first format chunk in the WAVE dictionary.
dict_read_first_format :: WAVEDict -> IterateeG L Word8 IO (Maybe AudioFormat)
dict_read_first_format dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_FMT (WEN_BYTE enum)) : _xs) -> joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the last fromat chunk from the WAVE dictionary.  This is useful
-- when parsing all chunks in the dictionary.
dict_read_last_format :: WAVEDict -> IterateeG L Word8 IO (Maybe AudioFormat)
dict_read_last_format dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = last xs in
    joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the specified format chunk from the WAVE dictionary
dict_read_format :: Int -> --Index in the format chunk list to read
                    WAVEDict -> --Dictionary
                    IterateeG L Word8 IO (Maybe AudioFormat)
dict_read_format ix dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = (!!) xs ix in
    joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the first data chunk in the WAVE dictionary.
dict_read_first_data :: WAVEDict -> IterateeG L Word8 IO (Maybe [Double])
dict_read_first_data dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_DATA (WEN_DUB enum)) : _xs) -> do
       e <- joinIM $ enum Iter.stream2list
       return $ Just e
  _ -> return Nothing

-- |Read the last data chunk in the WAVE dictionary.
dict_read_last_data :: WAVEDict -> IterateeG L Word8 IO (Maybe [Double])
dict_read_last_data dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = last xs in do
    e <- joinIM $ enum Iter.stream2list
    return $ Just e
  _ -> return Nothing

-- |Read the specified data chunk from the WAVE dictionary.
dict_read_data :: Int -> --Index in the data chunk list to read
                  WAVEDict -> --Dictionary
                  IterateeG L Word8 IO (Maybe [Double])
dict_read_data ix dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    e <- joinIM $ enum Iter.stream2list
    return $ Just e
  _ -> return Nothing

-- |Read the specified data chunk from the dictionary, applying the
-- data to the specified IterateeG.
dict_process_data :: Int -> -- Index in the data chunk list to read
                     WAVEDict -> -- Dictionary
                     IterateeG L Double IO a ->
                     IterateeG L Word8 IO (Maybe a)
dict_process_data ix dict iter = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    e <- joinIM $ enum iter
    return $ Just e
  _ -> return Nothing

-- ---------------------
-- convenience functions

-- |Convert (Maybe []) to [].  Nothing maps to an empty list.
join_m :: Maybe [a] -> [a]
join_m Nothing = []
join_m (Just a) = a

-- |Normalize a given value for the provided bit depth.
normalize :: Integral a => BitDepth -> a -> Double
normalize 8 a = (fromIntegral a - 128) / 128
normalize bd a = case (a > 0) of
  True ->  fromIntegral a / divPos
  False -> fromIntegral a / divNeg
  where
    divPos = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Int) - 1
    divNeg = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Int)

