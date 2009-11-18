{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base (
  -- * Types
  ErrMsg (..)
  ,StreamG (..)
  ,StreamStatus (..)
  -- ** Iteratees
  ,IterV (..)
  ,Iteratee (..)
  ,IterateePure (..)
  -- * Functions
  -- ** Stream Functions
  ,strMap
  ,setEOF
  -- ** Iteratee and IterateePure functions
  ,unIV
  ,unIVP
  ,run
  ,runPure
  -- * Classes
  ,IterateeC (..)
  ,Nullable (..)
  ,module Data.Iteratee.Base.LooseMap
)
where

import Prelude hiding (null)
import Data.Iteratee.Base.LooseMap
import Data.Iteratee.IO.Base
import Data.Monoid
import qualified Data.ByteString as B

import Control.Monad
import Control.Monad.Trans
import Control.Applicative hiding (empty)


-- |A stream is a (continuing) sequence of elements bundled in Chunks.
-- The first two variants indicate termination of the stream.
-- Chunk a gives the currently available part of the stream.
-- The stream is not terminated yet.
-- The case (null Chunk) signifies a stream with no currently available
-- data but which is still continuing. A stream processor should,
-- informally speaking, ``suspend itself'' and wait for more data
-- to arrive.

data StreamG c =
  EOF (Maybe ErrMsg)
  | Chunk c

instance Eq c => Eq (StreamG c) where
  EOF mErr1 == EOF mErr2 = mErr1 == mErr2
  Chunk xs == Chunk ys   = xs == ys
  _ == _ = False

instance Show c => Show (StreamG c) where
  show (EOF mErr) = "StreamG: EOF " ++ show mErr
  show (Chunk xs) = "StreamG: Chunk " ++ show xs

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
  | EofError ErrMsg
  deriving (Show, Eq)

data ErrMsg = Err String
              | Seek FileOffset
              deriving (Show, Eq)

instance Monoid ErrMsg where
  mempty = Err ""
  mappend (Err s1) (Err s2)  = Err (s1 ++ s2)
  mappend e@(Err _) _        = e
  mappend _        e@(Err _) = e
  mappend (Seek _) (Seek b)  = Seek b


-- |Produce the EOF error message.  If the stream was terminated because
-- of an error, keep the original error message.
setEOF :: StreamG c -> ErrMsg
setEOF (EOF (Just e)) = e
setEOF _              = Err "EOF"

data IterV i s m a =
  Done a (StreamG s)
  | Cont (StreamG s -> i s m a) (Maybe ErrMsg)

instance (Show a) => Show (IterV i s m a) where
  show (Done a str) = "IterV :: Done " ++ show a
  show (Cont k mErr) = "IterV :: Cont, mErr :: " ++ show mErr

-- ----------------------------------------------
-- Nullable container class
class Monoid c => Nullable c where
  null :: c -> Bool
  empty :: c
  empty = mempty

instance Nullable [a] where
  null [] = True
  null _  = False
  empty   = []

instance Nullable B.ByteString where
  null = B.null
  empty = B.empty

-- ----------------------------------------------
-- | Monadic iteratee
newtype Iteratee s m a = Iteratee{ runIter :: m (IterV Iteratee s m a)}

-- |Pure iteratee
newtype IterateePure s m a = IterateePure{
  runIterPure :: IterV IterateePure s m a
  }

-- ----------------------------------------------

-- |Type class of iteratees
class IterateeC i where
  data IV i   :: * -> (* -> *) -> * -> *
  toIter      :: IV i s m a -> i s m a
  runIterC    :: i s m a -> IV i s m a
  done        :: Monad m => a -> StreamG s -> IV i s m a
  cont        :: Monad m => (StreamG s -> i s m a) -> Maybe ErrMsg -> IV i s m a
  infixl 1 >>==
  (>>==)      :: Monad m =>
                 i s m a
                 -> (IterV i s m a -> IV i s' m b)
                 -> i s' m b
  joinI       :: Monad m => i s m (i s' m a) -> i s m a

instance IterateeC Iteratee where
  data IV Iteratee s m a = IterateeV (m (IterV Iteratee s m a))
  toIter       = Iteratee . unIV
  runIterC     = IterateeV . runIter
  done a s     = IterateeV (return $ Done a s)
  cont k mErr  = IterateeV (return $ Cont k mErr)
  m >>== f     = Iteratee (runIter m >>= unIV . f)
  joinI        = joinIM

instance IterateeC IterateePure where
  data IV IterateePure s m a = IterateePV (IterV IterateePure s m a)
  toIter        = IterateePure . unIVP
  runIterC      = IterateePV . runIterPure
  done a s      = IterateePV (Done a s)
  cont k mErr   = IterateePV (Cont k mErr)
  m >>== f      = IterateePure . unIVP . f . runIterPure $ m
  joinI         = joinIPure

-- |Convert the data family IterV type to a normal IterV (monadic version)
unIV :: IV Iteratee s m a -> m (IterV Iteratee s m a)
unIV (IterateeV v) = v

-- |Convert the data family IterV type to a normal IterV (pure version)
unIVP :: IV IterateePure s m a -> IterV IterateePure s m a
unIVP (IterateePV v) = v


instance (IterateeC i, Functor m, Monad m) => Functor (i s m) where
  fmap f m = m >>== docase
    where
      docase (Done a str)  = done (f a) str
      docase (Cont k mErr) = cont (fmap f . k) mErr

instance (IterateeC i, Functor m, Monad m, Nullable s) =>
  Applicative (i s m) where
    pure x  = toIter $ done x (Chunk empty)
    m <*> a = m >>= flip fmap a

instance (IterateeC i, Monad m, Nullable s) => Monad (i s m) where
  return x = toIter $ done x (Chunk empty)
  (>>=)    = iterBind

iterBind
  :: (IterateeC i, Monad m, Nullable s) =>
     i s m a
     -> (a -> i s m b)
     -> i s m b
iterBind m f = m >>== docase
  where
    -- the null case isn't required, but it is more efficient
    docase (Done a (Chunk s)) | null s = runIterC $ f a
    docase (Done a str)  = runIterC $ (f a) >>== check
      where
        check (Done x _str) = done x str
        check (Cont k mErr) = runIterC (k str)
    docase (Cont k mErr) = cont ((`iterBind` f) . k) mErr

{-# INLINE iterBind #-}

instance Monoid s => MonadTrans (Iteratee s) where
  lift m = Iteratee (m >>= return . flip Done mempty)

instance (Nullable s, MonadIO m) => MonadIO (Iteratee s m) where
  liftIO = lift . liftIO

-- ----------------------------------------------
-- non-typeclass Iteratee and IterateePure functions

-- | Run an 'Iteratee' and get the result.  An 'EOF' is sent to the
-- iteratee as it is run.
-- If the iteratee does not complete, an error is raised.
run :: (Monad m) => Iteratee s m a -> m a
run iter = runIter iter >>= check1
  where
    check1 (Done x _)       = return x
    check1 (Cont k Nothing) = runIter (k $ EOF Nothing) >>= check2
    check1 (Cont _ e)       = error $ "control message: " ++ show e
    check2 (Done x _)       = return x
    check2 (Cont k Nothing) = error
      "control message: iteratee did not terminate on EOF"
    check2 (Cont k e)       = error $ "control message: " ++ show e

-- | Run an 'IterateePure' and get the result.  An 'EOF' is sent to the
-- iteratee as it is run.
-- If the iteratee does not complete, an error is raised.
runPure :: IterateePure s m a -> a
runPure = check1 . runIterPure
  where
    check1 (Done x _)       = x
    check1 (Cont k Nothing) = check2 $ runIterPure (k (EOF Nothing))
    check1 (Cont _ e)       = error $ "control message: " ++ show e
    check2 (Done x _)       = x
    check2 (Cont _ Nothing) = error
      "control message: iteratee did not terminate on EOF"
    check2 (Cont _ e)       = error $ "control message: " ++ show e

-- the join functions are similar to monadic join, with one difference.
-- the inner stream may be of a different type than the outer stream.
-- In this case, there are no more elements of the inner stream to feed to
-- the inner iteratee after the join operation, so the inner iteratee
-- is terminated with EOF.
joinIM
  :: (Monad m) =>
     Iteratee s m (Iteratee s' m a)
     -> Iteratee s m a
joinIM = Iteratee . (docase <=< runIter)
  where
    docase (Done ma str) = liftM (flip Done str) $ run ma
    docase (Cont k mErr) = return $ Cont (joinIM . k) mErr

joinIPure
  :: 
     IterateePure s m (IterateePure s' m a)
     -> IterateePure s m a
joinIPure = IterateePure . docase . runIterPure
  where
    docase (Done ma str) = (flip Done str) (runPure ma)
    docase (Cont k mErr) = Cont (joinIPure . k) mErr

