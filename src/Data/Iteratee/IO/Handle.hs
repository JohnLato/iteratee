{-# LANGUAGE ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees.  These functions use Handles
-- for IO operations, and are provided for compatibility.  When available,
-- the File Descriptor based functions are preferred as these wastefully
-- allocate memory rather than running in constant space.

module Data.Iteratee.IO.Handle(
  -- * File enumerators
  enumHandle
  ,enumHandleCatch
  ,enumHandleRandom
  ,enumFile
  ,enumFileRandom
  -- * Iteratee drivers
  ,fileDriverHandle
  ,fileDriverRandomHandle
)

where

import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()

import Control.Exception
import Control.Monad
import Control.Monad.CatchIO as CIO
import Control.Monad.IO.Class

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

makeHandleCallback ::
  (MonadCatchIO m, NullPoint s, ReadableChunk s el) =>
  Ptr el
  -> Int
  -> Handle
  -> st
  -> m (Either SomeException ((Bool, st), s))
makeHandleCallback p bsize h st = do
  n' <- liftIO (CIO.try $ hGetBuf h p bsize :: IO (Either SomeException Int))
  case n' of
    Left e -> return $ Left e
    Right 0 -> return $ Right ((False, st), empty)
    Right n -> liftM (\s -> Right ((True, st), s)) $
                 readFromPtr p (fromIntegral n)


-- |The (monadic) enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
-- Data is read into a buffer of the specified size.
enumHandle ::
 forall s el m a.(NullPoint s, ReadableChunk s el, MonadCatchIO m) =>
  Int -- ^Buffer size (number of elements per read)
  -> Handle
  -> Enumerator s m a
enumHandle bs h i =
  let bufsize = bs * sizeOf (undefined :: el)
  in CIO.bracket (liftIO $ mallocBytes bufsize)
                 (liftIO . free)
                 (\p -> enumFromCallback (makeHandleCallback p bufsize h) () i)

-- |An enumerator of a file handle that catches exceptions raised by
-- the Iteratee.
enumHandleCatch
 ::
 forall e s el m a.(IException e,
                    NullPoint s,
                    ReadableChunk s el,
                    MonadCatchIO m)
  => Int -- ^Buffer size (number of elements per read)
  -> Handle
  -> (e -> m (Maybe EnumException))
  -> Enumerator s m a
enumHandleCatch bs h handler i =
  let bufsize = bs * sizeOf (undefined :: el)
  in CIO.bracket (liftIO $ mallocBytes bufsize)
                 (liftIO . free)
                 (\p -> enumFromCallbackCatch (makeHandleCallback p bufsize h) handler () i)


-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests).
-- Data is read into a buffer of the specified size.
enumHandleRandom ::
 forall s el m a.(NullPoint s, ReadableChunk s el, MonadCatchIO m) =>
  Int -- ^ Buffer size (number of elements per read)
  -> Handle
  -> Enumerator s m a
enumHandleRandom bs h i = enumHandleCatch bs h handler i
  where
    handler (SeekException off) =
       liftM (either
              (Just . EnumException :: IOException -> Maybe EnumException)
              (const Nothing))
             . liftIO . CIO.try $ hSeek h AbsoluteSeek $ fromIntegral off

-- ----------------------------------------------
-- File Driver wrapper functions.

enumFile' :: (NullPoint s, MonadCatchIO m, ReadableChunk s el) =>
  (Int -> Handle -> Enumerator s m a)
  -> Int -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFile' enumf bufsize filepath iter = CIO.bracket
  (liftIO $ openBinaryFile filepath ReadMode)
  (liftIO . hClose)
  (flip (enumf bufsize) iter)

enumFile ::
  (NullPoint s, MonadCatchIO m, ReadableChunk s el)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFile = enumFile' enumHandle

enumFileRandom ::
  (NullPoint s, MonadCatchIO m, ReadableChunk s el)
  => Int                 -- ^Buffer size
  -> FilePath
  -> Enumerator s m a
enumFileRandom = enumFile' enumHandleRandom

-- |Process a file using the given @Iteratee@.  This function wraps
-- @enumHandle@ as a convenience.
fileDriverHandle
  :: (NullPoint s, MonadCatchIO m, ReadableChunk s el) =>
     Int                      -- ^Buffer size (number of elements)
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverHandle bufsize iter filepath =
  enumFile bufsize filepath iter >>= run

-- |A version of @fileDriverHandle@ that supports seeking.
fileDriverRandomHandle
  :: (NullPoint s, MonadCatchIO m, ReadableChunk s el) =>
     Int                      -- ^ Buffer size (number of elements)
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomHandle bufsize iter filepath =
  enumFileRandom bufsize filepath iter >>= run

