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
  -- * Iteratee drivers
  ,fileDriverHandle
  ,fileDriverRandomHandle
)

where

import Data.Iteratee.Base.ReadableChunk
import Data.Iteratee.Iteratee
import Data.Iteratee.Binary()

import Data.Monoid
import Control.Exception
import Control.Monad
import Control.Monad.CatchIO as CIO
import Control.Monad.Trans

import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

import System.IO


-- ------------------------------------------------------------------------
-- Binary Random IO enumerators

makeHandleCallback ::
  ReadableChunk s el =>
  Ptr el
  -> Int
  -> Handle
  -> IO (Either SomeException (Bool, s))
makeHandleCallback p bufsize h = do
  n' <- CIO.try $ hGetBuf h p bufsize :: IO (Either SomeException Int)
  case n' of
    Left e -> return $ Left e
    Right 0 -> return $ Right (False, mempty)
    Right n -> liftM (\s -> Right (True, s)) $ readFromPtr p (fromIntegral n)


-- |The (monadic) enumerator of a file Handle.  This version enumerates
-- over the entire contents of a file, in order, unless stopped by
-- the iteratee.  In particular, seeking is not supported.
-- Data is read into a buffer of the specified size.
enumHandle :: forall s el m a.(ReadableChunk s el, Nullable s, MonadIO m) =>
  Int -- ^Buffer size (number of elements per read)
  -> Handle
  -> Enumerator s m a
enumHandle bs h i = do
  let bufsize = bs * (sizeOf (undefined :: el))
  p <- liftIO $ mallocBytes bufsize
  enumFromCallback (liftIO $ makeHandleCallback p bufsize h) i

-- |An enumerator of a file handle that catches exceptions raised by
-- the Iteratee.
enumHandleCatch
  :: forall e s el m a.(IException e, ReadableChunk s el, Nullable s, MonadIO m)
     => Int -- ^Buffer size (number of elements per read)
     -> Handle
     -> (e -> m (Maybe EnumException))
     -> Enumerator s m a
enumHandleCatch bs h handler i = do
  let bufsize = bs * (sizeOf (undefined :: el))
  p <- liftIO $ mallocBytes bufsize
  enumFromCallbackCatch (liftIO $ makeHandleCallback p bufsize h) handler i


-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests).
-- Data is read into a buffer of the specified size.
enumHandleRandom
  :: forall s el m a.(ReadableChunk s el, Nullable s, MonadIO m) =>
     Int -- ^Buffer size (number of elements per read)
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

fileDriver :: (MonadCatchIO m, Nullable s, ReadableChunk s el) =>
  (Int -> Handle -> Enumerator s m a)
  -> Int -- ^Buffer size
  -> Iteratee s m a
  -> FilePath
  -> m a
fileDriver enumf bufsize iter filepath = CIO.bracket
  (liftIO $ openBinaryFile filepath ReadMode)
  (liftIO . hClose)
  (run <=< flip (enumf bufsize) iter)

-- |Process a file using the given @Iteratee@.  This function wraps
-- @enumHandle@ as a convenience.
fileDriverHandle
  :: (MonadCatchIO m, Nullable s, ReadableChunk s el) =>
     Int -- ^Buffer size (number of elements)
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverHandle = fileDriver enumHandle

-- |A version of @fileDriverHandle@ that supports seeking.
fileDriverRandomHandle
  :: (MonadCatchIO m, Nullable s, ReadableChunk s el) =>
     Int
     -> Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomHandle = fileDriver enumHandleRandom

