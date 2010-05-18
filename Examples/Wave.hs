{-# LANGUAGE RankNTypes, FlexibleContexts #-}

{-

This module is not meant primarily for instructive and pedagogical purposes.
As such, it is not fully featured, and sacrifices performance and generality
for clarity of code.

-}

module Data.Iteratee.Codecs.Wave {-# DEPRECATED "This will be moved to a separate package in the future" #-} (
  WAVEDE (..),
  WAVEDE_ENUM (..),
  WAVE_CHUNK (..),
  AudioFormat (..),
  waveReader,
  readRiff,
  waveChunk,
  chunkToString,
  dictReadFormat,
  dictReadFirstFormat,
  dictReadLastFormat,
  dictReadFirstData,
  dictReadLastData,
  dictReadData,
  dictProcessData
)
where

import Prelude as P
import Control.Monad (join)
import Control.Monad.Trans (lift)
import Data.Iteratee
import qualified Data.Iteratee as Iter
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

-- |A WAVE directory is a list associating WAVE chunks with
-- a record WAVEDE
type WAVEDict = IM.IntMap [WAVEDE]

data WAVEDE = WAVEDE{
  wavede_count :: Int, -- ^length of chunk
  wavede_type :: WAVE_CHUNK, -- ^type of chunk
  wavede_enum :: WAVEDE_ENUM -- ^enumerator to get values of chunk
  }

type EnumeratorM sFrom sTo m a = Iteratee sTo m a -> m (Iteratee sFrom m a)

joinL :: (Monad m, Nullable s) => m (Iteratee s m a) -> Iteratee s m a
joinL = join . lift

data WAVEDE_ENUM =
  WEN_BYTE  (forall a. EnumeratorM [Word8] [Word8] IO a)
  | WEN_DUB (forall a. EnumeratorM [Word8] [Double] IO a)

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
waveChunk :: String -> Maybe WAVE_CHUNK
waveChunk str
  | str == "fmt " = Just WAVE_FMT
  | str == "data" = Just WAVE_DATA
  | P.length str == 4 = Just $ WAVE_OTHER str
  | otherwise = Nothing

-- |Convert a WAVE_CHUNK to the representative string
chunkToString :: WAVE_CHUNK -> String
chunkToString WAVE_FMT = "fmt "
chunkToString WAVE_DATA = "data"
chunkToString (WAVE_OTHER str) = str

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
stringRead4 :: Monad m => Iteratee [Word8] m String
stringRead4 = do
  s1 <- Iter.head
  s2 <- Iter.head
  s3 <- Iter.head
  s4 <- Iter.head
  return $ map (chr . fromIntegral) [s1, s2, s3, s4]

-- -----------------

-- |The library function to read the WAVE dictionary
waveReader :: Iteratee [Word8] IO (Maybe WAVEDict)
waveReader = do
  readRiff
  tot_size <- endianRead4 LSB
  readRiffWave
  chunks_m <- findChunks $ fromIntegral tot_size
  loadDict $ joinM chunks_m

-- |Read the RIFF header of a file.
readRiff :: Iteratee [Word8] IO ()
readRiff = do
  cnt <- heads $ fmap (fromIntegral . ord) "RIFF"
  if cnt == 4 then return () else throwErr $ iterStrExc "Bad RIFF header"

-- | Read the WAVE part of the RIFF header.
readRiffWave :: Iteratee [Word8] IO ()
readRiffWave = do
  cnt <- heads $ fmap (fromIntegral . ord) "WAVE"
  if cnt == 4 then return () else throwErr $ iterStrExc "Bad RIFF/WAVE header"

-- | An internal function to find all the chunks.  It assumes that the
-- stream is positioned to read the first chunk.
findChunks :: Int -> Iteratee [Word8] IO (Maybe [(Int, WAVE_CHUNK, Int)])
findChunks n = findChunks' 12 []
  where
  findChunks' offset acc = do
    typ <- stringRead4
    count <- endianRead4 LSB
    case waveChunk typ of
      Nothing -> (throwErr . iterStrExc $ "Bad subchunk descriptor: " ++ show typ)
        >> return Nothing
      Just chk -> let newpos = offset + 8 + count in
        case newpos >= fromIntegral n of
          True -> return . Just $ reverse $
              (fromIntegral offset, chk, fromIntegral count) : acc
          False -> do
            Iter.seek $ fromIntegral newpos
            findChunks' newpos $
             (fromIntegral offset, chk, fromIntegral count) : acc

loadDict :: [(Int, WAVE_CHUNK, Int)] ->
               Iteratee [Word8] IO (Maybe WAVEDict)
loadDict = P.foldl read_entry (return (Just IM.empty))
  where
  read_entry dictM (offset, typ, count) = dictM >>=
    maybe (return Nothing) (\dict -> do
    enum_m <- readValue dict offset typ count
    case (enum_m, IM.lookup (fromEnum typ) dict) of
      (Just enum, Nothing) -> --insert new entry
        return . Just $ IM.insert (fromEnum typ)
                                  [WAVEDE (fromIntegral count) typ enum] dict
      (Just enum, Just _vals) -> --existing entry
        return . Just $ IM.update
          (\ls -> Just $ ls ++ [WAVEDE (fromIntegral count) typ enum])
          (fromEnum typ) dict
      (Nothing, _) -> return (Just dict)
    )

readValue :: WAVEDict ->
              Int -> -- Offset
              WAVE_CHUNK -> -- Chunk type
              Int -> -- Count
              Iteratee [Word8] IO (Maybe WAVEDE_ENUM)
readValue _dict offset _ 0 = do
  throwErr . iterStrExc $ "Zero count in the entry of chunk at: " ++ show offset
  return Nothing

readValue dict offset WAVE_DATA count = do
  fmt_m <- dictReadLastFormat dict
  case fmt_m of
    Just fmt ->
      return . Just . WEN_DUB $ \iter_dub -> return $ do
        Iter.seek (8 + fromIntegral offset)
        let iter = Iter.convStream (convFunc fmt) iter_dub
        joinI . joinI . Iter.take count $ iter
    Nothing -> do
      throwErr . iterStrExc $ "No valid format for data chunk at: " ++ show offset
      return Nothing

-- return the WaveFormat iteratee
readValue _dict offset WAVE_FMT count =
  return . Just . WEN_BYTE $ \iter -> return $ do
    Iter.seek (8 + fromIntegral offset)
    Iter.joinI $ Iter.take count iter

-- for WAVE_OTHER, return Word8s and maybe the user can parse them
readValue _dict offset (WAVE_OTHER _str) count =
  return . Just . WEN_BYTE $ \iter -> return $ do
    Iter.seek (8 + fromIntegral offset)
    Iter.joinI $ Iter.take count iter


-- |Convert Word8s to Doubles
convFunc :: AudioFormat -> Iteratee [Word8] IO [Double]
convFunc (AudioFormat _nc _sr 8) = fmap
  ((:[]) . normalize 8 . (fromIntegral :: Word8 -> Int8))
    Iter.head
convFunc (AudioFormat _nc _sr 16) = fmap
  ((:[]) . normalize 16 . (fromIntegral :: Word16 -> Int16))
    (endianRead2 LSB)
convFunc (AudioFormat _nc _sr 24) = fmap
  ((:[]) . normalize 24 . (fromIntegral :: Word32 -> Int32))
    (endianRead3 LSB)
convFunc (AudioFormat _nc _sr 32) = fmap
  ((:[]) . normalize 32 . (fromIntegral :: Word32 -> Int32))
    (endianRead4 LSB)
convFunc _ = error "unrecognized audio format in convFunc"

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

-- |An Iteratee to read a wave format chunk
sWaveFormat :: Iteratee [Word8] IO (Maybe AudioFormat)
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
dictReadFirstFormat :: WAVEDict -> Iteratee [Word8] IO (Maybe AudioFormat)
dictReadFirstFormat dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_FMT (WEN_BYTE enum)) : _xs) -> joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the last fromat chunk from the WAVE dictionary.  This is useful
-- when parsing all chunks in the dictionary.
dictReadLastFormat :: WAVEDict -> Iteratee [Word8] IO (Maybe AudioFormat)
dictReadLastFormat dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = last xs in
    joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the specified format chunk from the WAVE dictionary
dictReadFormat :: Int -> --Index in the format chunk list to read
                    WAVEDict -> --Dictionary
                    Iteratee [Word8] IO (Maybe AudioFormat)
dictReadFormat ix dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = (!!) xs ix in
    joinIM $ enum sWaveFormat
  _ -> return Nothing

-- |Read the first data chunk in the WAVE dictionary.
dictReadFirstData :: WAVEDict -> Iteratee [Word8] IO (Maybe [Double])
dictReadFirstData dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_DATA (WEN_DUB enum)) : _xs) -> do
       e <- joinIM $ enum Iter.stream2list
       return $ Just e
  _ -> return Nothing

-- |Read the last data chunk in the WAVE dictionary.
dictReadLastData :: WAVEDict -> Iteratee [Word8] IO (Maybe [Double])
dictReadLastData dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = last xs in do
    e <- joinIM $ enum Iter.stream2list
    return $ Just e
  _ -> return Nothing

-- |Read the specified data chunk from the WAVE dictionary.
dictReadData :: Int -> --Index in the data chunk list to read
                  WAVEDict -> --Dictionary
                  Iteratee [Word8] IO (Maybe [Double])
dictReadData ix dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    e <- joinIM $ enum Iter.stream2list
    return $ Just e
  _ -> return Nothing

-- |Read the specified data chunk from the dictionary, applying the
-- data to the specified Iteratee.
dictProcessData :: Int -> -- Index in the data chunk list to read
                     WAVEDict -> -- Dictionary
                     Iteratee [Double] IO a ->
                     Iteratee [Word8] IO (Maybe a)
dictProcessData ix dict iter = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    e <- joinIM $ enum iter
    return $ Just e
  _ -> return Nothing

-- ---------------------
-- convenience functions

-- |Convert (Maybe []) to [].  Nothing maps to an empty list.
joinM :: Maybe [a] -> [a]
joinM Nothing = []
joinM (Just a) = a

-- |Normalize a given value for the provided bit depth.
normalize :: Integral a => BitDepth -> a -> Double
normalize 8 a = (fromIntegral a - 128) / 128
normalize bd a = case (a > 0) of
  True ->  fromIntegral a / divPos
  False -> fromIntegral a / divNeg
  where
    divPos = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Int) - 1
    divNeg = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Int)
