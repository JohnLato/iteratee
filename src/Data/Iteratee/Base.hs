{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, Rank2Types,
    DeriveDataTypeable #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base (
  -- * Types
  CtrlMsg (..)
  ,StreamG (..)
  ,StreamStatus (..)
  -- ** Iteratees
  ,Iteratee (..)
  ,run
  -- * Functions
  -- ** Iteratee Combinator
  ,idone
  ,icont
  ,idoneM
  ,icontM
  ,liftI
  -- ** Stream Functions
  ,strMap
  ,setEOF
  -- ** Error handling functions/utilities
  ,excEof
  ,excDivergent
  -- * Classes
  ,Nullable (..)
  ,module Data.Iteratee.Base.LooseMap
)
where

import Prelude hiding (null)
import Data.Iteratee.Base.LooseMap
import Data.Iteratee.IO.Base
import Data.Monoid
import qualified Data.ByteString as B

import Control.Monad.Trans
import Control.Applicative hiding (empty)
import Control.Exception
import Data.Data


-- |A stream is a (continuing) sequence of elements bundled in Chunks.
-- The first two variants indicate termination of the stream.
-- Chunk a gives the currently available part of the stream.
-- The stream is not terminated yet.
-- The case (null Chunk) signifies a stream with no currently available
-- data but which is still continuing. A stream processor should,
-- informally speaking, ``suspend itself'' and wait for more data
-- to arrive.

data StreamG c =
  EOF (Maybe SomeException)
  | Chunk c
  deriving (Show, Typeable)

instance Monoid c => Monoid (StreamG c) where
  mempty = Chunk mempty
  mappend (EOF mErr) _ = EOF mErr
  mappend _ (EOF mErr) = EOF mErr
  mappend (Chunk s1) (Chunk s2) = Chunk (s1 `mappend` s2)

-- |Map a function over a stream.
strMap :: (c -> c') -> StreamG c -> StreamG c'
strMap f (Chunk xs) = Chunk $ f xs
strMap _ (EOF mErr) = EOF mErr

-- |Describe the status of a stream of data.
data StreamStatus =
  DataRemaining
  | EofNoError
  | EofError SomeException
  deriving (Show, Typeable)

data CtrlMsg = Seek FileOffset
               | Err SomeException
               deriving (Show, Typeable)


-- |Produce the EOF error message.  If the stream was terminated because
-- of an error, keep the original error message.
setEOF :: StreamG c -> SomeException
setEOF (EOF (Just e)) = e
setEOF _              = excEof

excEof :: SomeException
excEof = toException $ ErrorCall "EOF"

excDivergent :: SomeException
excDivergent = toException $ ErrorCall "divergent iteratee"


-- ----------------------------------------------
-- Nullable container class
class Nullable c where
  null :: c -> Bool
  empty :: c

instance Nullable [a] where
  null [] = True
  null _  = False
  empty   = []

instance Nullable B.ByteString where
  null = B.null
  empty = B.empty

-- ----------------------------------------------
-- | Monadic iteratee
newtype Iteratee s m a = Iteratee{ runIter :: forall r.
          (a -> StreamG s -> m r) ->
          ((StreamG s -> Iteratee s m a) -> Maybe CtrlMsg -> m r) ->
          m r}

-- ----------------------------------------------

idone :: Monad m => a -> StreamG s -> Iteratee s m a
idone a s = Iteratee $ \onDone _ -> onDone a s

icont :: (StreamG s -> Iteratee s m a) -> Maybe CtrlMsg -> Iteratee s m a
icont k e = Iteratee $ \_ onCont -> onCont k e

liftI :: Monad m => (StreamG s -> Iteratee s m a) -> Iteratee s m a
liftI k = Iteratee $ \_ onCont -> onCont k Nothing

-- Monadic versions, frequently used by enumerators
idoneM :: Monad m => a -> StreamG s -> m (Iteratee s m a)
idoneM x str = return $ Iteratee $ \onDone _ -> onDone x str

icontM
  :: Monad m =>
     (StreamG s -> Iteratee s m a)
     -> Maybe CtrlMsg
     -> m (Iteratee s m a)
icontM k e = return $ Iteratee $ \_ onCont -> onCont k e

instance (Functor m, Monad m) => Functor (Iteratee s m) where
  fmap f m = Iteratee $ \onDone onCont ->
    let od   = onDone . f
        oc k = onCont (fmap f . k)
    in runIter m od oc

instance (Functor m, Monad m, Nullable s) => Applicative (Iteratee s m) where
    pure x  = idone x (Chunk empty)
    m <*> a = m >>= flip fmap a

instance (Monad m, Nullable s) => Monad (Iteratee s m) where
  {-# INLINE return #-}
  return x = Iteratee $ \onDone _ -> onDone x (Chunk empty)
  {-# INLINE (>>=) #-}
  m >>= f = Iteratee $ \onDone onCont -> 
     let m_done a (Chunk s)
           | null s      = runIter (f a) onDone onCont
         m_done a stream = runIter (f a) (\x _ -> onDone x stream) f_cont
           where f_cont k Nothing = runIter (k stream) onDone onCont
                 f_cont k e       = onCont k e
     in runIter m m_done (\k -> onCont ((>>= f) . k))

instance Monoid s => MonadTrans (Iteratee s) where
  lift m = Iteratee $ \onDone _ -> m >>= \x -> onDone x mempty

instance (MonadIO m, Nullable s, Monoid s) => MonadIO (Iteratee s m) where
  liftIO = lift . liftIO

-- |Send EOF to the Iteratee and disregard the unconsumed part of the stream.
-- It is an error to call run on an Iteratee that does not terminate on EOF.
run :: Monad m => Iteratee s m a -> m a
run iter = runIter iter onDone onCont
 where
   onDone  x _       = return x
   onCont  k Nothing = runIter (k (EOF Nothing)) onDone onCont'
   onCont  _ e       = error $ "control message: " ++ show e
   onCont' _ e       = error $ "control message: " ++ show e

