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
  ,module Data.Iteratee.Base.LooseMap
)
where

import Data.Iteratee.Base.LooseMap
import Data.Iteratee.IO.Base
import Data.Monoid

import Control.Monad
import Control.Applicative


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
  | Cont (i s m a) (Maybe ErrMsg)

instance (Show a) => Show (IterV i s m a) where
  show (Done a str) = "IterV :: Done " ++ show a
  show (Cont k mErr) = "IterV :: Cont, mErr :: " ++ show mErr

-- ----------------------------------------------
-- | Monadic iteratee
newtype Iteratee s m a = Iteratee{
  runIter :: StreamG s -> m (IterV Iteratee s m a)
  }

-- |Pure iteratee
newtype IterateePure s m a = IterateePure{
  runIterPure :: StreamG s -> IterV IterateePure s m a
  }

-- ----------------------------------------------

-- |Type class of iteratees
class IterateeC i where
  data IV i   :: * -> (* -> *) -> * -> *
  toIter      :: (StreamG s -> IV i s m a) -> i s m a
  runIterC    :: i s m a -> StreamG s -> IV i s m a
  done        :: Monad m => a -> StreamG s -> IV i s m a
  cont        :: Monad m => i s m a -> Maybe ErrMsg -> IV i s m a
  mapv        :: Monad m =>
                 (IterV i s m a -> IV i s' m b)
                 -> IV i s m a -> IV i s' m b
  joinI       :: Monad m => i s m (i s' m a) -> i s m a
  checkIfDone :: Monad m => (i s m a -> i s m a) -> IV i s m a -> i s m a

instance IterateeC Iteratee where
  data IV Iteratee s m a = IterateeV (m (IterV Iteratee s m a))
  toIter f     = Iteratee (unIV . f)
  runIterC i s = IterateeV (runIter i s)
  done a s     = IterateeV (return $ Done a s)
  cont k mErr  = IterateeV (return $ Cont k mErr)
  mapv f iv    = IterateeV (unIV iv >>= unIV . f)
  joinI        = joinIM
  checkIfDone e iv = checkIfDoneM e (unIV iv)

instance IterateeC IterateePure where
  data IV IterateePure s m a = IterateePV (IterV IterateePure s m a)
  toIter f      = IterateePure (unIVP . f)
  runIterC i s  = IterateePV (runIterPure i s)
  done a s      = IterateePV (Done a s)
  cont k mErr   = IterateePV (Cont k mErr)
  mapv f        = f . unIVP
  joinI         = joinIPure
  checkIfDone e = checkIfDoneP e . unIVP

-- |Convert the data family IterV type to a normal IterV (monadic version)
unIV :: IV Iteratee s m a -> m (IterV Iteratee s m a)
unIV (IterateeV v) = v

-- |Convert the data family IterV type to a normal IterV (pure version)
unIVP :: IV IterateePure s m a -> IterV IterateePure s m a
unIVP (IterateePV v) = v

instance (IterateeC i, Functor m, Monad m) => Functor (i s m) where
  fmap f m = toIter (mapv docase . runIterC m)
    where
      docase (Done a stream) = done (f a) stream
      docase (Cont k mErr)   = cont (fmap f k) mErr

instance (IterateeC i, Functor m, Monad m) => Applicative (i s m) where
  pure    = return
  m <*> a = m >>= flip fmap a

instance (IterateeC i, Monad m) => Monad (i s m) where
  return x = toIter (done x)
  (>>=)    = iterBind

iterBind
  :: (IterateeC i, Monad m) =>
     i s m a
     -> (a -> i s m b)
     -> i s m b
iterBind m f = toIter (mapv docase . runIterC m)
  where
    docase (Done a str)  = runIterC (f a) str
    docase (Cont k mErr) = cont (k `iterBind` f) mErr

{-# INLINE iterBind #-}

-- ----------------------------------------------
-- non-typeclass Iteratee and IterateePure functions

-- | Run an 'Iteratee' and get the result.  An 'EOF' is sent to the
-- iteratee as it is run.
-- If the iteratee does not complete, an error is raised.
run :: (Monad m) => Iteratee s m a -> m a
run iter = liftM docase $ runIter iter (EOF Nothing)
  where
    docase iv = case iv of
      Done x _ -> x
      Cont _ e -> error $ "control message: " ++ show e

-- | Run an 'IterateePure' and get the result.  An 'EOF' is sent to the
-- iteratee as it is run.
-- If the iteratee does not complete, an error is raised.
runPure :: IterateePure s m a -> a
runPure iter = case runIterPure iter (EOF Nothing) of
  Done x _ -> x
  Cont _ e -> error $ "control message: " ++ show e

-- the join functions are similar to monadic join, with one difference.
-- the inner stream may be of a different type than the outer stream.
-- In this case, there are no more elements of the inner stream to feed to
-- the inner iteratee after the join operation, so the inner iteratee
-- is terminated with EOF.
joinIM
  :: (Monad m) =>
     Iteratee s m (Iteratee s' m a)
     -> Iteratee s m a
joinIM m = Iteratee (docase <=< runIter m)
  where
    docase (Done ma str) = liftM (flip Done str) (run ma)
    docase (Cont k mErr) = return $ Cont (joinIM k) mErr

joinIPure
  :: 
     IterateePure s m (IterateePure s' m a)
     -> IterateePure s m a
joinIPure m = IterateePure $ docase . runIterPure m
  where
    docase (Done ma str) = (flip Done str) (runPure ma)
    docase (Cont k mErr) = Cont (joinIPure k) mErr

checkIfDoneM
  :: (Monad m) =>
     (Iteratee s m a -> Iteratee s m a)
     -> m (IterV Iteratee s m a)
     -> Iteratee s m a
checkIfDoneM k mIv = Iteratee step
  where
    step str = mIv >>= (\iv -> case iv of
      Done x _       -> return $ Done x str
      Cont x Nothing -> runIter (k x) str
      Cont x err     -> return $ Cont (k x) err
      )

checkIfDoneP
  :: (Monad m) =>
     (IterateePure s m a -> IterateePure s m a)
     -> IterV IterateePure s m a
     -> IterateePure s m a
checkIfDoneP _ (Done x _)       = return x
checkIfDoneP k (Cont x Nothing) = k x
checkIfDoneP k (Cont x err)     = IterateePure (const (Cont (k x) err))
