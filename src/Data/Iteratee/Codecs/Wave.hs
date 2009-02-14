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

import System.IO.Unsafe (unsafePerformIO)
import qualified Foreign.Ptr as FP
import qualified Foreign.ForeignPtr as FFP
import Data.Iteratee.Base
import Data.Iteratee.IO.RandomIO
import qualified Data.StorableVector as Vec
import qualified Data.StorableVector.Base as VB
import Foreign.Storable
import qualified Foreign.Marshal.Utils as FMU
import Control.Monad.Trans
import Data.Char (chr)
import Data.Int
import Data.Word
import Data.Bits (shiftL)
import qualified Data.IntMap as IM

-- =====================================================
-- WAVE libary code

-- useful type synonyms

type V    = Vec.Vector

-- determine host endian-ness
be :: Bool
be = (==1) $ unsafePerformIO $ FMU.with (1 :: Word16) (\p -> peekByteOff p 1 :: IO Word8)

-- |A WAVE directory is a list associating WAVE chunks with
-- a record WAVEDE
type WAVEDict = IM.IntMap [WAVEDE]

data WAVEDE = WAVEDE{
  wavede_count :: Int, -- ^length of chunk
  wavede_type :: WAVE_CHUNK, -- ^type of chunk
  wavede_enum :: WAVEDE_ENUM -- ^enumerator to get values of chunk
  }

data WAVEDE_ENUM =
  WEN_BYTE  (forall a. EnumeratorGMM V Word8 V Word8 IO a)
  | WEN_DUB (forall a. EnumeratorGMM V Word8 V Double IO a)

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
string_read4 :: Monad m => IterateeGM V Word8 m (Maybe String)
string_read4 = do
  bindm snext $ \s1 ->
   bindm snext $ \s2 ->
   bindm snext $ \s3 ->
   bindm snext $ \s4 ->
   return . Just $ map (chr . fromIntegral) [s1, s2, s3, s4]

-- -----------------

-- |The library function to read the WAVE dictionary
wave_reader :: IterateeGM V Word8 IO (Maybe WAVEDict)
wave_reader = do
  read_riff
  bindm (endian_read4 LSB) $ \tot_size -> do
    read_riff_wave
    chunks_m <- find_chunks $ fromIntegral tot_size
    load_dict $ join_m chunks_m

-- |Read the RIFF header of a file.
read_riff :: IterateeGM V Word8 IO ()
read_riff = do
  s <- string_read4
  case s == Just "RIFF" of
    True -> return ()
    False -> iter_err $ "Bad RIFF header: " ++ show s

-- | Read the WAVE part of the RIFF header.
read_riff_wave :: IterateeGM V Word8 IO ()
read_riff_wave = do
  s <- string_read4
  case s == Just "WAVE" of
    True -> return ()
    False -> iter_err $ "Bad RIFF/WAVE header: " ++ show s

-- | An internal function to find all the chunks.  It assumes that the
-- stream is positioned to read the first chunk.
find_chunks :: Int -> IterateeGM V Word8 IO (Maybe [(Int, WAVE_CHUNK, Int)])
find_chunks n = find_chunks' 12 []
  where
  find_chunks' offset acc = do
    bindm string_read4 $ \typ -> do
      count <- endian_read4 LSB
      case (wave_chunk typ, count) of
        (Nothing, _) -> (iter_err $ "Bad subchunk descriptor: " ++ show typ)
          >> return Nothing
        (_, Nothing) -> (iter_err $ "Bad subchunk length") >> return Nothing
        (Just chk, Just count') -> let newpos = offset + 8 + count' in
          case newpos >= fromIntegral n of
            True -> return . Just $ reverse $
                (fromIntegral offset, chk, fromIntegral count') : acc
            False -> do
              sseek $ fromIntegral newpos
              find_chunks' newpos $
               (fromIntegral offset, chk, fromIntegral count') : acc

load_dict :: [(Int, WAVE_CHUNK, Int)] ->
               IterateeGM V Word8 IO (Maybe WAVEDict)
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
              Int -> -- Offset
              WAVE_CHUNK -> -- Chunk type
              Int -> -- Count
              IterateeGM V Word8 IO (Maybe WAVEDE_ENUM)
read_value _dict offset _ 0 = do
  iter_err $ "Zero count in the entry of chunk at: " ++ show offset
  return Nothing

read_value dict offset WAVE_DATA count = do
  fmt_m <- dict_read_last_format dict
  case fmt_m of
    Just fmt -> do
      return . Just . WEN_DUB $ \iter_dub -> do
        sseek (8 + fromIntegral offset)
        let iter = conv_stream (conv_func fmt) iter_dub
        joinI $ joinI $ stakeR count ==<< iter
    Nothing -> do
      iter_err $ "No valid format for data chunk at: " ++ show offset
      return Nothing

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

unroll_8 :: (Monad m) => IterateeGM V Word8 m (Maybe (V Word8))
unroll_8 = liftI $ IE_cont step
  where
  step (Chunk vec)
    | Vec.null vec = unroll_8
    | True         = liftI $ IE_done (Just vec) (Chunk $ Vec.empty)
  step stream      = liftI $ IE_done Nothing stream

-- TODO :: Use a rewrite rule to use unroll_8 when the return of
-- unroll_n is Word8
-- first parameter is sizeOf a
unroll_n :: Storable a => Int -> IterateeGM V Word8 IO (Maybe (V a))
unroll_n wSize = liftI $ IE_cont step
  where
  step (Chunk vec)
    | Vec.null vec                    = unroll_n wSize
    | Vec.length vec < wSize          = liftI $ IE_cont $ step' vec
    | Vec.length vec `rem` wSize == 0 = lift (convert_vec vec) >>= \v ->
                                        liftI $ IE_done v (Chunk $ Vec.empty)
    | True    = let newLen = (Vec.length vec `div` wSize) * wSize
                    (h, t) = Vec.splitAt newLen vec
                in
                lift (convert_vec h) >>= \v -> liftI $ IE_done v (Chunk t)
  step stream = liftI $ IE_done Nothing stream
  step' i (Chunk vec)
    | Vec.null vec                          = liftI $ IE_cont $ step' i
    | Vec.length vec + Vec.length i < wSize = liftI $ IE_cont $ step'
                                              (Vec.append i vec)
    | True        = let vec' = Vec.append i vec
                        newLen = (Vec.length vec' `div` wSize) * wSize
                        (h, t) = Vec.splitAt newLen vec'
                    in
                    lift (convert_vec $ Vec.append i h) >>= \v ->
                      liftI $ IE_done v (Chunk t)
  step' _i stream = liftI $ IE_done Nothing stream
  convert_vec vec = let (fp, off, len) = VB.toForeignPtr vec
                        f = FP.plusPtr (FFP.unsafeForeignPtrToPtr fp) off
                    in
                    do
                    newFp <- FFP.newForeignPtr_ f
                    let newV = VB.fromForeignPtr (FFP.castForeignPtr newFp)
                               (len `div` wSize)
                    v' <- host_to_le newV
                    return $ Just v'

host_to_le :: Storable a => V a -> IO (V a)
host_to_le vec = case be of
  True -> let
            (fp, off, len) = VB.toForeignPtr vec
            wSize = sizeOf $ Vec.head vec
          in
          loop wSize fp len off
  False -> return vec
  where
    loop _wSize _fp 0 _off = return vec
    loop wSize fp len off  = do
      FFP.withForeignPtr fp (\p -> swap_bytes wSize (p `FP.plusPtr` off))
      loop wSize fp (len - 1) (off + 1)

swap_bytes :: Int -> FP.Ptr a -> IO ()
swap_bytes wSize p = case wSize of
                          1 -> return ()
                          2 -> do
                               w1 <- (peekByteOff p 0) :: IO Word8
                               w2 <- (peekByteOff p 1) :: IO Word8
                               pokeByteOff p 0 w2
                               pokeByteOff p 1 w1
                          3 -> do
                               w1 <- (peekByteOff p 0) :: IO Word8
                               w3 <- (peekByteOff p 2) :: IO Word8
                               pokeByteOff p 0 w3
                               pokeByteOff p 1 w1
                          4 -> do
                               w1 <- (peekByteOff p 0) :: IO Word8
                               w2 <- (peekByteOff p 1) :: IO Word8
                               w3 <- (peekByteOff p 2) :: IO Word8
                               w4 <- (peekByteOff p 3) :: IO Word8
                               pokeByteOff p 0 w4
                               pokeByteOff p 1 w3
                               pokeByteOff p 2 w2
                               pokeByteOff p 3 w1
                          x -> do
                               let ns = [0..(x-1)]
                               ws <- sequence
                                     [(peekByteOff p n) :: IO Word8 | n <- ns]
                               sequence [ pokeByteOff p n w | n <- ns, w <- reverse ws]
                               return ()

-- |Convert Word8s to Doubles
conv_func :: AudioFormat -> IterateeGM V Word8 IO (Maybe (V Double))
conv_func (AudioFormat _nc _sr 8) = (fmap . fmap . Vec.map)
  (normalize 8 . (fromIntegral :: Word8 -> Int8)) unroll_8
conv_func (AudioFormat _nc _sr 16) = (fmap . fmap . Vec.map)
  (normalize 16 . (fromIntegral :: Word16 -> Int16))
  (unroll_n $ sizeOf (undefined :: Word16))
conv_func (AudioFormat _nc _sr 24) = (fmap . fmap . Vec.map)
  (normalize 24 . (fromIntegral :: Word32 -> Int32))
  (unroll_n 3)
conv_func (AudioFormat _nc _sr 32) = (fmap . fmap . Vec.map)
  (normalize 32 . (fromIntegral :: Word32 -> Int32))
  (unroll_n $ sizeOf (undefined :: Word32))
conv_func _ = iter_err "Invalid wave bit depth" >> return Nothing

-- |An Iteratee to read a wave format chunk
sWaveFormat :: IterateeGM V Word8 IO (Maybe AudioFormat)
sWaveFormat = do
  bindm (endian_read2 LSB) $ \f' -> --data format, 1==PCM
   bindm (endian_read2 LSB) $ \nc ->
   bindm (endian_read4 LSB) $ \sr -> do
     sdrop 6
     bindm (endian_read2 LSB) $ \bd ->
       case f' == 1 of
         True -> return . Just $ AudioFormat (fromIntegral nc)
                                             (fromIntegral sr)
                                             (fromIntegral bd)
         False -> return Nothing

-- ---------------------
-- functions to assist with reading from the dictionary

-- |Read the first format chunk in the WAVE dictionary.
dict_read_first_format :: WAVEDict -> IterateeGM V Word8 IO (Maybe AudioFormat)
dict_read_first_format dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_FMT (WEN_BYTE enum)) : _xs) -> enum ==<< sWaveFormat
  _ -> return Nothing

-- |Read the last fromat chunk from the WAVE dictionary.  This is useful
-- when parsing all chunks in the dictionary.
dict_read_last_format :: WAVEDict -> IterateeGM V Word8 IO (Maybe AudioFormat)
dict_read_last_format dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = last xs in
    enum ==<< sWaveFormat
  _ -> return Nothing

-- |Read the specified format chunk from the WAVE dictionary
dict_read_format :: Int -> --Index in the format chunk list to read
                    WAVEDict -> --Dictionary
                    IterateeGM V Word8 IO (Maybe AudioFormat)
dict_read_format ix dict = case IM.lookup (fromEnum WAVE_FMT) dict of
  Just xs -> let (WAVEDE _ WAVE_FMT (WEN_BYTE enum)) = (!!) xs ix in do
    e <- enum ==<< sWaveFormat
    return e
  _ -> return Nothing

-- |Read the first data chunk in the WAVE dictionary.
dict_read_first_data :: WAVEDict -> IterateeGM V Word8 IO (Maybe [Double])
dict_read_first_data dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just ((WAVEDE _ WAVE_DATA (WEN_DUB enum)) : _xs) -> do
       e <- enum ==<< stream2list
       return $ Just e
  _ -> return Nothing

-- |Read the last data chunk in the WAVE dictionary.
dict_read_last_data :: WAVEDict -> IterateeGM V Word8 IO (Maybe [Double])
dict_read_last_data dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just [] -> return Nothing
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = last xs in do
    e <- enum ==<< stream2list
    return $ Just e
  _ -> return Nothing

-- |Read the specified data chunk from the WAVE dictionary.
dict_read_data :: Int -> --Index in the data chunk list to read
                  WAVEDict -> --Dictionary
                  IterateeGM V Word8 IO (Maybe [Double])
dict_read_data ix dict = case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    e <- enum ==<< stream2list
    return $ Just e
  _ -> return Nothing

-- |Read the specified data chunk from the dictionary, applying the
-- data to the specified IterateeGM.
dict_process_data :: Int -> -- Index in the data chunk list to read
                     WAVEDict -> -- Dictionary
                     IterateeGM V Double IO a ->
                     IterateeGM V Word8 IO (Maybe a)
dict_process_data ix dict iter = {-# SCC "dict_process_data" #-} case IM.lookup (fromEnum WAVE_DATA) dict of
  Just xs -> let (WAVEDE _ WAVE_DATA (WEN_DUB enum)) = (!!) xs ix in do
    e <- enum ==<< iter
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
normalize 8 a = ((fromIntegral a - 128)) / 128
normalize bd a = {-# SCC "normalize" #-} case (a > 0) of
  True ->  (fromIntegral a) / divPos
  False -> (fromIntegral a) / divNeg
  where
    divPos = (fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Int)) - 1
    divNeg = fromIntegral (1 `shiftL` fromIntegral (bd - 1) :: Int)

