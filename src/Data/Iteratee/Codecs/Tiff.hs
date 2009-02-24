{-# LANGUAGE Rank2Types #-}

-- Random and Binary IO with IterateeM

-- A general-purpose TIFF library

-- The library gives the user the TIFF dictionary, which the user
-- can search for specific tags and obtain the values associated with 
-- the tags, including the pixel matrix.
--
-- The overarching theme is incremental processing: initially,
-- only the TIFF dictionary is read. The value associated with a tag
-- is read only when that tag is looked up (unless the value was short
-- and was packed in the TIFF dictionary entry). The pixel matrix
-- (let alone the whole TIFF file) is not loaded in memory -- 
-- the pixel matrix is not even located before it is needed.
-- The matrix is processed incrementally, by a user-supplied 
-- iteratee.
--
-- The incremental processing is accomplished by iteratees and enumerators.
-- The enumerators are indeed first-class, they are stored
-- in the interned TIFF dictionary data structure. These enumerators
-- represent the values associated with tags; the values will be read
-- on demand, when the enumerator is applied to a user-given iteratee.
--
-- The library extensively uses nested streams, tacitly converting the 
-- stream of raw bytes from the file into streams of integers, 
-- rationals and other user-friendly items. The pixel matrix is
-- presented as a contiguous stream, regardless of its segmentation
-- into strips and physical arrangement.
-- The library exhibits random IO and binary parsing, reading
-- of multi-byte numeric data in big- or little-endian formats.
-- The library can be easily adopted for AIFF, RIFF and other
-- IFF formats.
--
-- We show a representative application of the library: reading a sample
-- TIFF file, printing selected values from the TIFF dictionary,
-- verifying the values of selected pixels and computing the histogram
-- of pixel values. The pixel verification procedure stops reading the
-- pixel matrix as soon as all specified pixel values are verified.
-- The histogram accumulation does read the entire matrix, but
-- incrementally. Neither pixel matrix processing procedure loads
-- the whole matrix in memory. In fact, we never read and retain
-- more than the IO-buffer-full of raw data.

-- This TIFF library is to be contrasted with the corresponding Scheme
-- code:
--     http://okmij.org/ftp/Scheme/binary-io.html#tiff
-- The main distinction is using iteratees for on-demand processing.

module Data.Iteratee.Codecs.Tiff where

import Data.Iteratee.Base (StreamG (..), IterateeG (..), IterateeGM, EnumeratorGMM, EnumeratorN, bindm, liftI, (==<<), (>.), (>>==), iterErr, enumErr, convStream, iterReportError, enumEof)
import qualified Data.Iteratee.Base as Iter
import qualified Data.Iteratee.Base.StreamChunk as SC
import Data.Iteratee.Binary
import Control.Monad.State
import Data.Char (chr)
import Data.Int
import Data.Word
import Data.Ratio
import Data.Maybe
import qualified Data.IntMap as IM


-- ========================================================================
-- Sample TIFF user code
-- The following is sample code using the TIFF library (whose implementation
-- is in the second part of this file).
-- Our sample code prints interesting information from the TIFF
-- dictionary (such as the dimensions, the resolution and the name
-- of the image)

-- The main user function. tiff_reader is the library function,
-- which builds the TIFF dictionary.
-- process_tiff is the user function, to extract useful data
-- from the dictionary
-- test_tiff :: IO (Maybe String)
-- test_tiff = test_driver_random (tiff_reader >>= process_tiff) "filename.tiff"

-- Sample TIFF processing function
process_tiff :: MonadIO m => Maybe (IM.IntMap TIFFDE) ->
  IterateeGM [] Word8 m (Maybe String)
process_tiff Nothing = return $ Just "No dictionary"
process_tiff (Just dict) = do
  note ["dict size: ", show $ IM.size dict]
  -- Check tag values against the known values for the sample image
  check_tag TG_IMAGEWIDTH  (flip dict_read_int dict) 129
  check_tag TG_IMAGELENGTH (flip dict_read_int dict) 122
  check_tag TG_BITSPERSAMPLE (flip dict_read_int dict) 8
  check_tag TG_IMAGEDESCRIPTION (flip dict_read_string dict)
		"JPEG:gnu-head-sm.jpg 129x122"
  check_tag TG_COMPRESSION (flip dict_read_int dict) 1
  check_tag TG_SAMPLESPERPIXEL (flip dict_read_int dict) 1
  check_tag TG_STRIPBYTECOUNTS (flip dict_read_int dict) 15738 -- nrows*ncols
  check_tag TG_XRESOLUTION (flip dict_read_rat dict) (72%1)
  check_tag TG_YRESOLUTION (flip dict_read_rat dict) (72%1)

  (n,hist) <- compute_hist dict
  note ["computed histogram over ", show n, " values\n", show hist]
  iterReportError >>= maybe (return ()) error
  note ["Verifying values of sample pixels"]
  verify_pixel_vals dict [(0,255), (17,248)]
  err <- iterReportError
  maybe (return ()) error err
  return err
 where check_tag tag action v = do
	   vc <- action tag
	   case vc of
	     Just v' | v' == v -> note ["Tag ",show tag, " value ", show v]
	     _ -> error $ unwords ["Tag", show tag, "unexpected:", show vc]

-- process_tiff Nothing = return Nothing

-- sample processing of the pixel matrix: computing the histogram
compute_hist :: MonadIO m =>
                TIFFDict ->
                IterateeGM [] Word8 m (Int,IM.IntMap Int)
compute_hist dict = Iter.joinI $ pixel_matrix_enum dict ==<< compute_hist' 0 IM.empty
 where
 compute_hist' count = liftI . Cont .step count
 step count hist (Chunk ch)
   | SC.null ch  = compute_hist' count hist
   | otherwise = compute_hist' (count + SC.length ch) (foldr accum hist ch)
 step count hist s        = liftI $ Done (count,hist) s
 accum e = IM.insertWith (+) (fromIntegral e) 1

-- Another sample processor of the pixel matrix: verifying values of
-- some pixels
-- This processor does not read the whole matrix; it stops as soon
-- as everything is verified or the error is detected
verify_pixel_vals :: MonadIO m =>
                     TIFFDict -> [(IM.Key, Word8)] -> IterateeGM [] Word8 m ()
verify_pixel_vals dict pixels = Iter.joinI $ pixel_matrix_enum dict ==<< 
				verify 0 (IM.fromList pixels)
 where
 verify _ m | IM.null m = return ()
 verify n m = liftI $ Cont (step n m)
 step n m (Chunk xs)
   | SC.null xs = verify n m
   | otherwise = let (h, t) = (SC.head xs, SC.tail xs) in
   case IM.updateLookupWithKey (\_k _e -> Nothing) n m of
    (Just v,m') -> if v == h then step (succ n) m' (Chunk t)
		     else iterErr $ unwords ["Pixel #",show n,
					      "expected:",show v,
					      "found", show h]
    (Nothing,m')->    step (succ n) m' (Chunk t)
 step _n _m s = liftI $ Done () s


-- ========================================================================
-- TIFF library code

-- A TIFF directory is a finite map associating a TIFF tag with
-- a record TIFFDE
type TIFFDict = IM.IntMap TIFFDE

data TIFFDE = TIFFDE{tiffde_count :: Int,        -- number of items
		     tiffde_enum  :: TIFFDE_ENUM -- enumerator to get values
		    }

data TIFFDE_ENUM =
  TEN_CHAR (forall a m. Monad m => EnumeratorGMM [] Word8 [] Char m a)
  | TEN_BYTE (forall a m. Monad m => EnumeratorGMM [] Word8 [] Word8 m a)
  | TEN_INT  (forall a m. Monad m => EnumeratorGMM [] Word8 [] Int m a)
  | TEN_RAT  (forall a m. Monad m => EnumeratorGMM [] Word8 [] (Ratio Int) m a)

-- Standard TIFF data types
data TIFF_TYPE = TT_NONE  -- 0
  | TT_byte      -- 1   8-bit unsigned integer
  | TT_ascii     -- 2   8-bit bytes with last byte null
  | TT_short     -- 3   16-bit unsigned integer
  | TT_long      -- 4   32-bit unsigned integer
  | TT_rational  -- 5   64-bit fractional (numer+denominator)
    				-- The following was added in TIFF 6.0
  | TT_sbyte     -- 6   8-bit signed (2s-complement) integer
  | TT_undefined -- 7   An 8-bit byte, "8-bit chunk"
  | TT_sshort    -- 8   16-bit signed (2s-complement) integer
  | TT_slong     -- 9   32-bit signed (2s-complement) integer
  | TT_srational -- 10  "signed rational",  two SLONGs (num+denominator)
  | TT_float     -- 11  "IEEE 32-bit float", single precision (4-byte)
  | TT_double    -- 12  "IEEE 64-bit double", double precision (8-byte)
 deriving (Eq, Enum, Ord, Bounded, Show)


-- Standard TIFF tags
data TIFF_TAG = TG_other Int		-- other than below
  | TG_SUBFILETYPE 	        -- subfile data descriptor
  | TG_OSUBFILETYPE             -- +kind of data in subfile
  | TG_IMAGEWIDTH	        -- image width in pixels
  | TG_IMAGELENGTH	        -- image height in pixels
  | TG_BITSPERSAMPLE	        -- bits per channel (sample)
  | TG_COMPRESSION	        -- data compression technique
  | TG_PHOTOMETRIC	        -- photometric interpretation
  | TG_THRESHOLDING		-- +thresholding used on data
  | TG_CELLWIDTH		-- +dithering matrix width
  | TG_CELLLENGTH	        -- +dithering matrix height
  | TG_FILLORDER		-- +data order within a byte
  | TG_DOCUMENTNAME	        -- name of doc. image is from
  | TG_IMAGEDESCRIPTION	        -- info about image
  | TG_MAKE			-- scanner manufacturer name
  | TG_MODEL			-- scanner model name/number
  | TG_STRIPOFFSETS		-- offsets to data strips
  | TG_ORIENTATION	        -- +image orientation
  | TG_SAMPLESPERPIXEL          -- samples per pixel
  | TG_ROWSPERSTRIP	        -- rows per strip of data
  | TG_STRIPBYTECOUNTS          -- bytes counts for strips
  | TG_MINSAMPLEVALUE	        -- +minimum sample value
  | TG_MAXSAMPLEVALUE           -- maximum sample value
  | TG_XRESOLUTION              -- pixels/resolution in x
  | TG_YRESOLUTION              -- pixels/resolution in y
  | TG_PLANARCONFIG             -- storage organization
  | TG_PAGENAME		        -- page name image is from
  | TG_XPOSITION		-- x page offset of image lhs
  | TG_YPOSITION		-- y page offset of image lhs
  | TG_FREEOFFSETS	        -- +byte offset to free block
  | TG_FREEBYTECOUNTS	        -- +sizes of free blocks
  | TG_GRAYRESPONSEUNIT         -- gray scale curve accuracy
  | TG_GRAYRESPONSECURVE	-- gray scale response curve
  | TG_GROUP3OPTIONS            -- 32 flag bits
  | TG_GROUP4OPTIONS 	        -- 32 flag bits
  | TG_RESOLUTIONUNIT           -- units of resolutions
  | TG_PAGENUMBER	        -- page numbers of multi-page
  | TG_COLORRESPONSEUNIT 	-- color scale curve accuracy
  | TG_COLORRESPONSECURVE       -- RGB response curve
  | TG_SOFTWARE			-- name & release
  | TG_DATETIME 		-- creation date and time
  | TG_ARTIST			-- creator of image
  | TG_HOSTCOMPUTER		-- machine where created
  | TG_PREDICTOR 		-- prediction scheme w/ LZW
  | TG_WHITEPOINT		-- image white point
  | TG_PRIMARYCHROMATICITIES    -- primary chromaticities
  | TG_COLORMAP 		-- RGB map for pallette image
  | TG_BADFAXLINES		-- lines w/ wrong pixel count
  | TG_CLEANFAXDATA		-- regenerated line info
  | TG_CONSECUTIVEBADFAXLINES   -- max consecutive bad lines
  | TG_MATTEING                 -- alpha channel is present
 deriving (Eq, Show)

tag_map :: Num t => [(TIFF_TAG, t)]
tag_map = [
   (TG_SUBFILETYPE,254),
   (TG_OSUBFILETYPE,255),
   (TG_IMAGEWIDTH,256),
   (TG_IMAGELENGTH,257),
   (TG_BITSPERSAMPLE,258),
   (TG_COMPRESSION,259),
   (TG_PHOTOMETRIC,262),
   (TG_THRESHOLDING,263),
   (TG_CELLWIDTH,264),
   (TG_CELLLENGTH,265),
   (TG_FILLORDER,266),
   (TG_DOCUMENTNAME,269),
   (TG_IMAGEDESCRIPTION,270),
   (TG_MAKE,271),
   (TG_MODEL,272),
   (TG_STRIPOFFSETS,273),
   (TG_ORIENTATION,274),
   (TG_SAMPLESPERPIXEL,277),
   (TG_ROWSPERSTRIP,278),
   (TG_STRIPBYTECOUNTS,279),
   (TG_MINSAMPLEVALUE,280),
   (TG_MAXSAMPLEVALUE,281),
   (TG_XRESOLUTION,282),
   (TG_YRESOLUTION,283),
   (TG_PLANARCONFIG,284),
   (TG_PAGENAME,285),
   (TG_XPOSITION,286),
   (TG_YPOSITION,287),
   (TG_FREEOFFSETS,288),
   (TG_FREEBYTECOUNTS,289),
   (TG_GRAYRESPONSEUNIT,290),
   (TG_GRAYRESPONSECURVE,291),
   (TG_GROUP3OPTIONS,292),
   (TG_GROUP4OPTIONS,293),
   (TG_RESOLUTIONUNIT,296),
   (TG_PAGENUMBER,297),
   (TG_COLORRESPONSEUNIT,300),
   (TG_COLORRESPONSECURVE,301),
   (TG_SOFTWARE,305),
   (TG_DATETIME,306),
   (TG_ARTIST,315),
   (TG_HOSTCOMPUTER,316),
   (TG_PREDICTOR,317),
   (TG_WHITEPOINT,318),
   (TG_PRIMARYCHROMATICITIES,319),
   (TG_COLORMAP,320),
   (TG_BADFAXLINES,326),
   (TG_CLEANFAXDATA,327),
   (TG_CONSECUTIVEBADFAXLINES,328),
   (TG_MATTEING,32995)
   ]

tag_map' :: IM.IntMap TIFF_TAG
tag_map' = IM.fromList $ map (\(tag,v) -> (v,tag)) tag_map

tag_to_int :: TIFF_TAG -> Int
tag_to_int (TG_other x) = x
tag_to_int x = fromMaybe (error $ "not found tag: " ++ show x) $ lookup x tag_map

int_to_tag :: Int -> TIFF_TAG
int_to_tag x = fromMaybe (TG_other x) $ IM.lookup x tag_map'


-- The library function to read the TIFF dictionary
tiff_reader :: IterateeGM [] Word8 IO (Maybe TIFFDict)
tiff_reader = do
  endian <- read_magic
  check_version
  case endian of
    Just e -> bindm (endian_read4 e) $ \dict_offset -> do
              Iter.seek (fromIntegral dict_offset)
              load_dict e
    Nothing -> return Nothing
 where
   -- Read the magic and set the endianness
   read_magic = do
     c1 <- Iter.head
     c2 <- Iter.head
     case (c1,c2) of
      (Just 0x4d, Just 0x4d) -> return $ Just MSB
      (Just 0x49, Just 0x49) -> return $ Just LSB
      _ -> (iterErr $ "Bad TIFF magic word: " ++ show [c1,c2])
           >> return Nothing

   -- Check the version in the header. It is always ...
   tiff_version = 42
   check_version = do
     v <- endian_read2 MSB
     case v of
      Just v' | v' == tiff_version -> return ()
      _ -> iterErr $ "Bad TIFF version: " ++ show v

-- A few conversion procedures
u32_to_float :: Word32 -> Double
u32_to_float _x = 		-- unsigned 32-bit int -> IEEE float
  error "u32->float is not yet implemented"

u32_to_s32 :: Word32 -> Int32   -- unsigned 32-bit int -> signed 32 bit
u32_to_s32 = fromIntegral
-- u32_to_s32 0x7fffffff == 0x7fffffff
-- u32_to_s32 0xffffffff == -1

u16_to_s16 :: Word16 -> Int16   -- unsigned 16-bit int -> signed 16 bit
u16_to_s16 = fromIntegral
-- u16_to_s16 32767 == 32767
-- u16_to_s16 32768 == -32768
-- u16_to_s16 65535 == -1

u8_to_s8 :: Word8 -> Int8   -- unsigned 8-bit int -> signed 8 bit
u8_to_s8 = fromIntegral
-- u8_to_s8 127 == 127
-- u8_to_s8 128 == -128
-- u8_to_s8 255 == -1

note :: (MonadIO m) => [String] -> IterateeGM [] el m ()
note = lift . liftIO . putStrLn . concat

-- An internal function to load the dictionary. It assumes that the stream
-- is positioned to read the dictionary
load_dict :: MonadIO m => Endian -> IterateeGM [] Word8 m (Maybe TIFFDict)
load_dict e =
  bindm (endian_read2 e) $ \nentries -> do
   dict <- foldr (const read_entry) (return (Just IM.empty)) [1..nentries]
   bindm (endian_read4 e) $ \next_dict -> do
   when (next_dict > 0) $
      note ["The TIFF file contains several images, ",
	         "only the first one will be considered"]
   return dict
 where
  read_entry dictM =
    bindm dictM $ \dict ->
     bindm (endian_read2 e) $ \tag ->    
     bindm (endian_read2 e) $ \typ' ->    
     bindm (convert_type (fromIntegral typ')) $ \typ ->    
     bindm (endian_read4 e) $ \count -> do
      -- we read the val-offset later. We need to check the size and the type
      -- of the datum, because val-offset may contain the value itself,
      -- in its lower-numbered bytes, regardless of the big/little endian
      -- order!

     note ["TIFFEntry: tag ",show . int_to_tag . fromIntegral $ tag, 
	   " type ", show typ, " count ", show count]
     enum_m <- read_value typ e (fromIntegral count)
     case enum_m of
      Just enum ->
       return . Just $ IM.insert (fromIntegral tag) 
		                 (TIFFDE (fromIntegral count) enum) dict
      _ -> return (Just dict)

  convert_type :: (Monad m) => Int -> IterateeGM [] el m (Maybe TIFF_TYPE)
  convert_type typ | typ > 0 && typ <= fromEnum (maxBound::TIFF_TYPE)
      = return . Just . toEnum $ typ
  convert_type typ = do
      iterErr $ "Bad type of entry: " ++ show typ
      return Nothing

  read_value :: MonadIO m => TIFF_TYPE -> Endian -> Int -> 
                IterateeGM [] Word8 m (Maybe TIFFDE_ENUM)

  read_value typ e' 0 =
    bindm (endian_read4 e') $ \_offset -> do
      iterErr $ "Zero count in the entry of type: " ++ show typ
      return Nothing

            		-- Read an ascii string from the offset in the
			-- dictionary. The last byte of
            		-- an ascii string is always zero, which is
            		-- included in 'count' but we don't need to read it
  read_value TT_ascii e' count | count > 4 = -- for sure, val-offset is offset
    bindm (endian_read4 e') $ \offset ->
      return . Just . TEN_CHAR $ \iter_char -> do
            Iter.seek (fromIntegral offset)
            let iter = convStream 
                         (bindm Iter.head (return. Just .(:[]). chr . fromIntegral))
                         iter_char
            Iter.joinI $ Iter.joinI $ Iter.takeR (pred count) ==<< iter

			-- Read the string of 0 to 3 characters long
                        -- The zero terminator is included in count, but
			-- we don't need to read it
  read_value TT_ascii _e count = do	-- count is within 1..4
    let len = pred count		-- string length
    let loop acc 0 = return . Just . reverse $ acc
        loop acc n = bindm Iter.head (\v -> loop ((chr . fromIntegral $ v):acc)
                                             (pred n))
    bindm (loop [] len) $ \str -> do
      Iter.drop (4-len)
      return . Just . TEN_CHAR $ immed_value str

			-- Read the array of signed or unsigned bytes
  read_value typ e' count | count > 4 && typ == TT_byte || typ == TT_sbyte =
    bindm (endian_read4 e') $ \offset ->
      return . Just . TEN_INT $ \iter_int -> do
            Iter.seek (fromIntegral offset)
            let iter = convStream 
                         (bindm Iter.head (return . Just . (:[]) . conv_byte typ))
                         iter_int
            Iter.joinI $ Iter.joinI $ Iter.takeR count ==<< iter

			-- Read the array of 1 to 4 bytes
  read_value typ _e count | typ == TT_byte || typ == TT_sbyte = do
    let loop acc 0 = return . Just . reverse $ acc
        loop acc n = bindm Iter.head (\v -> loop (conv_byte typ v:acc)
                                             (pred n))
    bindm (loop [] count) $ \str -> do
      Iter.drop (4-count)
      return . Just . TEN_INT $ immed_value str

			-- Read the array of Word8
  read_value TT_undefined e' count | count > 4 =
    bindm (endian_read4 e') $ \offset ->
      return . Just . TEN_BYTE $ \iter -> do
            Iter.seek (fromIntegral offset)
            Iter.joinI $ Iter.takeR count iter

			-- Read the array of Word8 of 1..4 elements,
			-- packed in the offset field
  read_value TT_undefined _e count = do 
    let loop acc 0 = return . Just . reverse $ acc
        loop acc n = bindm Iter.head (\v -> loop (v:acc) (pred n))
    bindm (loop [] count) $ \str -> do
      Iter.drop (4-count)
      return . Just . TEN_BYTE $ immed_value str

			-- Read the array of short integers

			-- of 1 element: the offset field contains the value
  read_value typ e' 1 | typ == TT_short || typ == TT_sshort =
    bindm (endian_read2 e') $ \item -> do
      Iter.drop 2				-- skip the padding
      return . Just . TEN_INT $ immed_value [conv_short typ item]

			-- of 2 elements: the offset field contains the value
  read_value typ e' 2 | typ == TT_short || typ == TT_sshort =
    bindm (endian_read2 e') $ \i1 -> 
     bindm (endian_read2 e') $ \i2 ->
      return . Just . TEN_INT $ 
	     immed_value [conv_short typ i1, conv_short typ i2]

			-- of n elements
  read_value typ e' count | typ == TT_short || typ == TT_sshort =
    bindm (endian_read4 e') $ \offset ->
      return . Just . TEN_INT $ \iter_int -> do
            Iter.seek (fromIntegral offset)
            let iter = convStream 
                         (bindm (endian_read2 e') 
			  (return . Just . (:[]) . conv_short typ))
                         iter_int
            Iter.joinI $ Iter.joinI $ Iter.takeR (2*count) ==<< iter


			-- Read the array of long integers
			-- of 1 element: the offset field contains the value
  read_value typ e' 1 | typ == TT_long || typ == TT_slong =
    bindm (endian_read4 e') $ \item ->
      return . Just . TEN_INT $ immed_value [conv_long typ item]

			-- of n elements
  read_value typ e' count | typ == TT_long || typ == TT_slong =
    bindm (endian_read4 e') $ \offset ->
      return . Just . TEN_INT $ \iter_int -> do
            Iter.seek (fromIntegral offset)
            let iter = convStream 
                         (bindm (endian_read4 e')  
			  (return . Just . (:[]) . conv_long typ))
                         iter_int
            Iter.joinI $ Iter.joinI $ Iter.takeR (4*count) ==<< iter

			-- Read the array of rationals. A rational can't
			-- be packed into the offset field
{-
  read_value typ e count | typ == TT_rational || typ == TT_srational = do 
    bindm (endian_read4 e) $ \offset ->
      return . Just . TEN_RAT $ \iter_rat -> do
            Iter.seek (fromIntegral offset)
            let iter = convStream 
                         (bindm (endian_read4 e) $ \i1 ->
			   bindm (endian_read4 e) $ \i2 ->
			    (return . Just . (:[]) $ conv_rat typ i1 i2))
                         iter_rat
            Iter.joinI $ Iter.joinI $ Iter.takeR (8*count) ==<< iter
-}


  read_value typ e' count = -- stub
    bindm (endian_read4 e') $ \_offset -> do
     note ["unhandled type: ", show typ, " with count ", show count]
     return Nothing

  immed_value :: (Monad m) => [el] -> EnumeratorGMM [] Word8 [] el m a
  immed_value item iter =
     (Iter.enumPure1Chunk item >. enumEof) iter >>== Iter.joinI . return

  conv_byte :: TIFF_TYPE -> Word8 -> Int
  conv_byte TT_byte  = fromIntegral
  conv_byte TT_sbyte = fromIntegral . u8_to_s8
  conv_byte _ = error "This should never happen"

  conv_short :: TIFF_TYPE -> Word16 -> Int
  conv_short TT_short  = fromIntegral
  conv_short TT_sshort = fromIntegral . u16_to_s16
  conv_short _ = error "This should never happen"

  conv_long :: TIFF_TYPE -> Word32 -> Int
  conv_long TT_long  = fromIntegral
  conv_long TT_slong = fromIntegral . u32_to_s32
  conv_long _ = error "This should never happen"

{- this code is never used...
  conv_rat :: TIFF_TYPE -> Word32 -> Word32 -> Rational
  conv_rat TT_rational v1 v2 = (fromIntegral v1) % (fromIntegral v2)
  conv_rat TT_srational v1 v2 = (fromIntegral (u32_to_s32 v1)) % 
				(fromIntegral (u32_to_s32 v2))
  conv_rat _tt _ _ = error "This should never happen"
-}

-- Reading the pixel matrix
-- For simplicity, we assume no compression and 8-bit pixels
pixel_matrix_enum :: MonadIO m => TIFFDict -> EnumeratorN [] Word8 [] Word8 m a
pixel_matrix_enum dict iter = validate_dict >>= proceed
 where
   -- Make sure we can handle this particular TIFF image
   validate_dict =
      dict_assert TG_COMPRESSION 1         `bindm`  \() ->
      dict_assert TG_SAMPLESPERPIXEL 1    `bindm`  \() ->
      dict_assert TG_BITSPERSAMPLE 8      `bindm`  \() ->
      dict_read_int TG_IMAGEWIDTH dict    `bindm`  \ncols ->
      dict_read_int TG_IMAGELENGTH dict   `bindm`  \nrows ->
      dict_read_ints TG_STRIPOFFSETS dict `bindm`  \strip_offsets -> do
        rps <- liftM (fromMaybe nrows) (dict_read_int TG_ROWSPERSTRIP dict)
	if ncols > 0 && nrows > 0 && rps > 0 
	   then return $ Just (ncols,nrows,rps,strip_offsets)
	   else return Nothing
	   
   dict_assert tag v = do
      vfound <- dict_read_int tag dict
      case vfound of
        Just v' | v' == v -> return $ Just ()
	_ -> iterErr (unwords ["dict_assert: tag:", show tag,
				"expected:", show v, "found:", show vfound]) >>
             return Nothing

   proceed Nothing = enumErr "Can't handle this TIFF" iter >>== return

   proceed (Just (ncols,nrows,rows_per_strip,strip_offsets)) = do
     let strip_size = rows_per_strip * ncols
	 image_size = nrows * ncols
     note ["Processing the pixel matrix, ", show image_size, " bytes"]
     let loop _pos _ iter'@Done{} = return iter'
         loop _pos [] iter'          = return iter'
         loop pos (strip:strips) iter' = do
	   Iter.seek (fromIntegral strip)
	   let len = min strip_size (image_size - pos)
	   iter'' <- Iter.takeR (fromIntegral len) iter'
	   loop (pos+len) strips iter''
     loop 0 strip_offsets iter


-- A few helpers for getting data from TIFF dictionary

dict_read_int :: Monad m => TIFF_TAG -> TIFFDict ->
                 IterateeGM [] Word8 m (Maybe Int)
dict_read_int tag dict = do
  els <- dict_read_ints tag dict
  case els of
   Just (e:_) -> return $ Just e
   _          -> return Nothing

dict_read_ints :: Monad m => TIFF_TAG -> TIFFDict -> 
		  IterateeGM [] Word8 m (Maybe [Int])
dict_read_ints tag dict = 
  case IM.lookup (tag_to_int tag) dict of
      Just (TIFFDE _ (TEN_INT enum)) -> do
	     e <- enum  ==<< Iter.stream2list
	     return (Just e)
      _ -> return Nothing

dict_read_rat :: Monad m => TIFF_TAG -> TIFFDict ->
                 IterateeGM [] Word8 m (Maybe (Ratio Int))
dict_read_rat tag dict = 
  case IM.lookup (tag_to_int tag) dict of
      Just (TIFFDE 1 (TEN_RAT enum)) -> do
	     [e] <- enum  ==<< Iter.stream2list
	     return (Just e)
      _ -> return Nothing

dict_read_string :: Monad m => TIFF_TAG -> TIFFDict ->
                    IterateeGM [] Word8 m (Maybe String)
dict_read_string tag dict = 
  case IM.lookup (tag_to_int tag) dict of
      Just (TIFFDE _ (TEN_CHAR enum)) -> do
	     e <- enum  ==<< Iter.stream2list
	     return (Just e)
      _ -> return Nothing
