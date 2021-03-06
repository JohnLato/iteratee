{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

-- |Monadic and General Iteratees:
-- Messaging and exception handling.
--
-- Iteratees use an internal exception handling mechanism that is parallel to
-- that provided by 'Control.Exception'.  This allows the iteratee framework
-- to handle its own exceptions outside @IO@.
--
-- Iteratee exceptions are divided into two categories, 'IterException' and
-- 'EnumException'.  @IterExceptions@ are exceptions within an iteratee, and
-- @EnumExceptions@ are exceptions within an enumerator.
--
-- Enumerators can be constructed to handle an 'IterException' with
-- @Data.Iteratee.Iteratee.enumFromCallbackCatch@.  If the enumerator detects
-- an @iteratee exception@, the enumerator calls the provided exception handler.
-- The enumerator is then able to continue feeding data to the iteratee,
-- provided the exception was successfully handled.  If the handler could
-- not handle the exception, the 'IterException' is converted to an
-- 'EnumException' and processing aborts.
--
-- Exceptions can also be cleared by @Data.Iteratee.Iteratee.checkErr@,
-- although in this case the iteratee continuation cannot be recovered.
--
-- When viewed as Resumable Exceptions, iteratee exceptions provide a means
-- for iteratees to send control messages to enumerators.  The @seek@
-- implementation provides an example.  @Data.Iteratee.Iteratee.seek@ stores
-- the current iteratee continuation and throws a 'SeekException', which
-- inherits from 'IterException'.  @Data.Iteratee.IO.enumHandleRandom@ is
-- constructed with @enumFromCallbackCatch@ and a handler that performs
-- an @hSeek@.  Upon receiving the 'SeekException', @enumHandleRandom@ calls
-- the handler, checks that it executed properly, and then continues with
-- the stored continuation.
--
-- As the exception hierarchy is open, users can extend it with custom
-- exceptions and exception handlers to implement sophisticated messaging
-- systems based upon resumable exceptions.


module Data.Iteratee.Exception (
  -- * Exception types
  IFException (..)
  ,Exception (..)             -- from Control.Exception
  -- ** Enumerator exceptions
  ,EException (..)
  ,EnumException (..)
  ,DivergentException (..)
  ,EnumStringException (..)
  ,EnumUnhandledIterException (..)
  -- ** Iteratee exceptions
  ,IException (..)
  ,IterException (..)
  ,SeekException (..)
  ,EofException (..)
  ,IterStringException (..)
  ,IterUnhandledEnumException (..)
  -- * Functions
  ,enStrExc
  ,iterStrExc
  ,enumStrExc
  ,wrapIterExc
  ,wrapEnumExc
  ,iterExceptionToException
  ,iterExceptionFromException
  ,ifExcToIterExc
  ,isEofException
  -- * Exception handling helpers
  , IHandler (..)
  , eofHandler
)
where

import Data.Iteratee.IO.Base

import Control.Exception
import Data.Data


-- ----------------------------------------------
-- create exception type hierarchy

-- |Root of the Iteratee exception hierarchy.  @IFException@ derives from
-- @Control.Exception.SomeException@.  'EnumException', 'IterException',
-- and all inheritants are descendents of 'IFException'.
data IFException = forall e . Exception e => IFException e
  deriving Typeable

instance Show IFException where
  show (IFException e) = show e

instance Exception IFException

ifExceptionToException :: Exception e => e -> SomeException
ifExceptionToException = toException . IFException

ifExceptionFromException :: Exception e => SomeException -> Maybe e
ifExceptionFromException x = do
  IFException a <- fromException x
  cast a

-- Root of enumerator exceptions.
data EnumException = forall e . Exception e => EnumException e
  deriving Typeable

instance Show EnumException where
  show (EnumException e) = show e

class Exception e => EException e where
  toEnumException   :: e -> EnumException
  toEnumException   = EnumException
  fromEnumException :: EnumException -> Maybe e
  fromEnumException = fromException . toException

instance Exception EnumException where
  toException   = ifExceptionToException
  fromException = ifExceptionFromException

instance EException EnumException where
  toEnumException   = id
  fromEnumException = Just

enumExceptionToException :: Exception e => e -> SomeException
enumExceptionToException = toException . IterException

enumExceptionFromException :: Exception e => SomeException -> Maybe e
enumExceptionFromException x = do
  IterException a <- fromException x
  cast a

-- |The @iteratee@ diverged upon receiving 'EOF'.
data DivergentException = DivergentException
  deriving (Show, Typeable)

instance Exception DivergentException where
  toException   = enumExceptionToException
  fromException = enumExceptionFromException

instance EException DivergentException where

-- |Create an enumerator exception from a @String@.
data EnumStringException = EnumStringException String
  deriving (Show, Typeable)

instance Exception EnumStringException where
  toException   = enumExceptionToException
  fromException = enumExceptionFromException

instance EException EnumStringException where

-- |Create an 'EnumException' from a string.
enStrExc :: String -> EnumException
enStrExc = EnumException . EnumStringException

-- |The enumerator received an 'IterException' it could not handle.
data EnumUnhandledIterException = EnumUnhandledIterException IterException
  deriving (Show, Typeable)

instance Exception EnumUnhandledIterException where
  toException   = enumExceptionToException
  fromException = enumExceptionFromException

instance EException EnumUnhandledIterException where


-- |Convert an 'IterException' to an 'EnumException'.  Meant to be used
-- within an @Enumerator@ to signify that it could not handle the
-- @IterException@.
wrapIterExc :: IterException -> EnumException
wrapIterExc = EnumException . EnumUnhandledIterException

-- iteratee exceptions

-- |A class for @iteratee exceptions@.  Only inheritants of @IterException@
-- should be instances of this class.
class Exception e => IException e where
  toIterException   :: e -> IterException
  toIterException   = IterException
  fromIterException :: IterException -> Maybe e
  fromIterException = fromException . toException

-- |Root of iteratee exceptions.
data IterException = forall e . Exception e => IterException e
  deriving Typeable

instance Show IterException where
  show (IterException e) = show e

instance Exception IterException where
  toException   = ifExceptionToException
  fromException = ifExceptionFromException

iterExceptionToException :: Exception e => e -> SomeException
iterExceptionToException = toException . IterException

iterExceptionFromException :: Exception e => SomeException -> Maybe e
iterExceptionFromException x = do
  IterException a <- fromException x
  cast a

instance IException IterException where
  toIterException   = id
  fromIterException = Just

-- |A seek request within an @Iteratee@.
data SeekException = SeekException FileOffset
  deriving (Typeable, Show)

instance Exception SeekException where
  toException   = iterExceptionToException
  fromException = iterExceptionFromException

instance IException SeekException where

-- |The @Iteratee@ needs more data but received @EOF@.
data EofException = EofException String
  deriving (Typeable, Show)

instance Exception EofException where
  toException   = iterExceptionToException
  fromException = iterExceptionFromException

instance IException EofException where

isEofException :: Exception e => e -> Bool
isEofException e = case fromException $ toException e of
    Just (EofException _) -> True
    _                     -> False

-- |The iteratee received an 'EnumException' it could not handle.
data IterUnhandledEnumException = IterUnhandledEnumException EnumException
  deriving (Show, Typeable)

instance Exception IterUnhandledEnumException where
  toException   = iterExceptionToException
  fromException = iterExceptionFromException

-- |Convert an 'EnumException' to an 'IterException'.  Meant to be used
-- within an @Iteratee@ to signify that it could not handle the recieved
-- @EnumException@.
wrapEnumExc :: EnumException -> IterException
wrapEnumExc = IterException . IterUnhandledEnumException

-- |An @Iteratee exception@ specified by a @String@.
data IterStringException = IterStringException String deriving (Typeable, Show)

instance Exception IterStringException where
  toException   = iterExceptionToException
  fromException = iterExceptionFromException

instance IException IterStringException where

-- |Create an @iteratee exception@ from a string.
-- This convenience function wraps 'IterStringException' and 'toIterException'.
iterStrExc :: String -> IterException
iterStrExc= toIterException . IterStringException

enumStrExc :: String -> EnumException
enumStrExc = toEnumException . EnumStringException

ifExcToIterExc :: IFException -> IterException
ifExcToIterExc ife = let e = ifExceptionToException ife
                     in case (fromException e, fromException e) of
                        (Just i, _)  -> i
                        (_, Just e') -> wrapEnumExc e'
                        (_, _)       -> error "iteratee: couldn't convert an 'IFException' to either 'IterException' or 'EnumException'.  How'd that happen?"

-- | Wrap an exception handler in an existentially-quantified type.  This
-- exists so that multiple handlers can be passed to 'enumFromCallbackCatches'
data IHandler m where
  IHandler :: IException e => (e -> m (Maybe EnumException)) -> IHandler m

-- nearly every enumerator should be able to handle EofException
eofHandler :: Monad m => EofException -> m (Maybe EnumException)
eofHandler _ = return Nothing
