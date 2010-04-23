{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification #-}

-- |Monadic and General Iteratees:
-- Messaging and exception handling

module Data.Iteratee.Exception (
  -- * Exception types
  IFException (..)
  -- ** Enumerator exceptions
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
  -- * Functions
  ,enStrExc
  ,iterStrExc
  ,wrapIterExc
)
where

import Data.Iteratee.IO.Base

import Control.Exception
import Control.Failure
import Data.Data


-- ----------------------------------------------
-- create exception type hierarchy

-- |Root of Iteratee Framework exception hierarchy
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

-- enumerator exceptions
data EnumException = forall e . Exception e => EnumException e
  deriving Typeable

instance Show EnumException where
  show (EnumException e) = show e

instance Exception EnumException where
  toException   = ifExceptionToException
  fromException = ifExceptionFromException

enumExceptionToException :: Exception e => e -> SomeException
enumExceptionToException = toException . IterException

enumExceptionFromException :: Exception e => SomeException -> Maybe e
enumExceptionFromException x = do
  IterException a <- fromException x
  cast a

-- |The iteratee diverged upon receiving EOF.
data DivergentException = DivergentException
  deriving (Show, Typeable)

instance Exception DivergentException where
  toException   = enumExceptionToException
  fromException = enumExceptionFromException

-- |Unspecified enumerator exception
data EnumStringException = EnumStringException String
  deriving (Show, Typeable)

instance Exception EnumStringException where
  toException   = enumExceptionToException
  fromException = enumExceptionFromException

enStrExc :: String -> EnumException
enStrExc = EnumException . EnumStringException

-- |The enumerator received an IterException it could not handle
data EnumUnhandledIterException = EnumUnhandledIterException IterException
  deriving (Show, Typeable)

instance Exception EnumUnhandledIterException where
  toException   = enumExceptionToException
  fromException = enumExceptionFromException

wrapIterExc :: IterException -> EnumException
wrapIterExc = EnumException . EnumUnhandledIterException

-- iteratee exceptions

class Exception e => IException e where
  toIterException   :: e -> IterException
  toIterException   = IterException
  fromIterException :: IterException -> Maybe e
  fromIterException = fromException . toException

instance IException NullException where
instance IException NothingException where

-- |An exception within an iteratee.
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

-- |A seek request from an iteratee
data SeekException = SeekException FileOffset
  deriving (Typeable, Show)

instance Exception SeekException where
  toException   = iterExceptionToException
  fromException = iterExceptionFromException

instance IException SeekException where

-- |Iteratee needs more data to complete but received EOF.
data EofException = EofException
  deriving (Typeable, Show)

instance Exception EofException where
  toException   = iterExceptionToException
  fromException = iterExceptionFromException

instance IException EofException where

data IterStringException = IterStringException String deriving (Typeable, Show)

instance Exception IterStringException where
  toException   = iterExceptionToException
  fromException = iterExceptionFromException

instance IException IterStringException where

-- |Create an iteratee exception from a string.
iterStrExc :: String -> SomeException
iterStrExc= toException . IterStringException

