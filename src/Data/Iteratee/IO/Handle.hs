{-# LANGUAGE ScopedTypeVariables #-}

-- |Random and Binary IO with generic Iteratees.  These functions use Handles
-- for IO operations, and are provided for compatibility.  When available,
-- the File Descriptor based functions are preferred as these wastefully
-- allocate memory rather than running in constant space.

module Data.Iteratee.IO.Handle(
  -- * File enumerators
  enumHandle
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
enumHandle :: forall s el m a.(ReadableChunk s el, Nullable s, MonadIO m) =>
  Handle ->
  Enumerator s m a
enumHandle h i = do
  let bufsize = 4096 - mod 4096 (sizeOf (undefined :: el))
  p <- liftIO $ mallocBytes bufsize
  enumFromCallback (liftIO $ makeHandleCallback p bufsize h) i


-- |The enumerator of a Handle: a variation of enumHandle that
-- supports RandomIO (seek requests)
enumHandleRandom
  :: forall s el m a.(ReadableChunk s el, Nullable s, MonadIO m) =>
     Handle
     -> Enumerator s m a
enumHandleRandom h i = do
  let bufsize = 4096 - mod 4096 (sizeOf (undefined :: el))
  let handler (SeekException off) =
       liftM (either
              (Just . EnumException :: IOException -> Maybe EnumException)
              (const Nothing))
             . liftIO . CIO.try $ hSeek h AbsoluteSeek $ fromIntegral off
  p <- liftIO $ mallocBytes bufsize
  enumFromCallbackCatch (liftIO $ makeHandleCallback p bufsize h) handler i


-- ----------------------------------------------
-- File Driver wrapper functions.

-- |Process a file using the given Iteratee.  This function wraps
-- enumHandle as a convenience.
fileDriverHandle
  :: (MonadCatchIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverHandle iter filepath = CIO.bracket
  (liftIO $ openBinaryFile filepath ReadMode)
  (liftIO . hClose)
  (run <=< flip enumHandle iter)

-- |Process a file using the given Iteratee.  This function wraps
-- enumHandleRandom as a convenience.
fileDriverRandomHandle
  :: (MonadCatchIO m, Nullable s, ReadableChunk s el) =>
     Iteratee s m a
     -> FilePath
     -> m a
fileDriverRandomHandle iter filepath = CIO.bracket
  (liftIO $ openBinaryFile filepath ReadMode)
  (liftIO . hClose)
  (run <=< flip enumHandleRandom iter)
