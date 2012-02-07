{-# LANGUAGE TypeFamilies
            ,MultiParamTypeClasses
            ,FlexibleContexts
            ,FlexibleInstances
            ,UndecidableInstances
            ,RankNTypes
            ,DeriveDataTypeable
            ,ScopedTypeVariables
            ,ExistentialQuantification #-}

-- |Monadic Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base (
  -- * Types
  Stream (..)
  -- ** Exception types
  ,module Data.Iteratee.Exception
  -- ** Iteratees
  ,Iteratee (..)
  -- * Functions
  -- ** Control functions
  ,run
  ,tryRun
  ,ilift
  ,ifold
  -- ** Creating Iteratees
  ,idone
  ,icont
  ,icontP
  ,ierr
  ,ireq
  ,liftI
  ,idoneM
  ,ierrM
  -- ** Stream Functions
  ,setEOF
  -- * Classes
  ,module X
)
where

import Prelude hiding (null, catch)
import Data.Iteratee.Exception
import Data.Iteratee.Base.LooseMap as X
import Data.Nullable               as X
import Data.NullPoint              as X

import Data.Maybe
import Data.Monoid

import Control.Arrow (first)
import Control.Monad (liftM, join)
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.CatchIO (MonadCatchIO (..), Exception (..),
  catch, block, toException, fromException)
import Control.Monad.Trans.Control
import Control.Applicative hiding (empty)
import Control.Exception (SomeException)
import qualified Control.Exception as E
import Data.Data


-- |A stream is a (continuing) sequence of elements bundled in Chunks.
-- The first variant indicates termination of the stream.
-- Chunk a gives the currently available part of the stream.
-- The stream is not terminated yet.
-- The case (null Chunk) signifies a stream with no currently available
-- data but which is still continuing. A stream processor should,
-- informally speaking, ``suspend itself'' and wait for more data
-- to arrive.

data Stream c =
  EOF (Maybe SomeException)
  | Chunk c
  deriving (Show, Typeable)

instance (Eq c) => Eq (Stream c) where
  (Chunk c1) == (Chunk c2)           = c1 == c2
  (EOF Nothing) == (EOF Nothing)     = True
  (EOF (Just e1)) == (EOF (Just e2)) = typeOf e1 == typeOf e2
  _ == _                             = False

instance Monoid c => Monoid (Stream c) where
  mempty = Chunk mempty
  mappend (EOF mErr) _ = EOF mErr
  mappend _ (EOF mErr) = EOF mErr
  mappend (Chunk s1) (Chunk s2) = Chunk (s1 `mappend` s2)

-- |Map a function over a stream.
instance Functor Stream where
  fmap f (Chunk xs) = Chunk $ f xs
  fmap _ (EOF mErr) = EOF mErr

-- ----------------------------------------------
-- create exception type hierarchy

-- |Produce the 'EOF' error message.  If the stream was terminated because
-- of an error, keep the error message.
setEOF :: Stream c -> SomeException
setEOF (EOF (Just e)) = e
setEOF _              = toException EofException

-- ----------------------------------------------
-- | Monadic iteratee
newtype Iteratee s m a = Iteratee{ runIter :: forall r.
          (a -> r) ->
          ((Stream s -> m (Iteratee s m a, Stream s)) -> r) ->
          (Iteratee s m a -> SomeException -> r) ->
          (forall b. m b -> (b -> (Iteratee s m a)) -> r) ->
          r}

-- ----------------------------------------------

idone :: a -> Iteratee s m a
idone a = Iteratee $ \onDone _ _ _ -> onDone a

icont :: (Stream s -> m (Iteratee s m a, Stream s)) -> Iteratee s m a
icont k = Iteratee $ \_ onCont _ _ -> onCont k

icontP :: Monad m => (Stream s -> (Iteratee s m a, Stream s)) -> Iteratee s m a
icontP k = Iteratee $ \_ onCont _ _ -> onCont (return . k)

-- | identical to icont, left in for compatibility-ish reasons
liftI :: Monad m => (Stream s -> (Iteratee s m a, Stream s)) -> Iteratee s m a
liftI = icontP

ierr :: Iteratee s m a -> SomeException -> Iteratee s m a
ierr i e = Iteratee $ \_ _ onErr _ -> onErr i e

ireq :: m b -> (b -> Iteratee s m a) -> Iteratee s m a
ireq mb bf = Iteratee $ \_ _ _ onReq -> onReq mb bf
{-# INLINE ireq #-}

-- Monadic versions, frequently used by enumerators
idoneM :: Monad m => a -> m (Iteratee s m a)
idoneM x = return $ idone x

ierrM :: Monad m => Iteratee s m a -> SomeException -> m (Iteratee s m a)
ierrM i e = return $ ierr i e

instance forall s m. (Functor m) => Functor (Iteratee s m) where
  fmap f m = runIter m (idone . f) onCont onErr (onReq f)
    where
      onCont k      = icont $ fmap (first (fmap f)) . k
      onErr i e     = ierr (fmap f i) e
      onReq :: (a -> b) -> m x -> (x -> Iteratee s m a) -> Iteratee s m b
      onReq f mb doB = ireq mb (fmap f . doB)

instance (Functor m, Monad m) => Applicative (Iteratee s m) where
    pure x  = idone x
    m <*> a = m >>= flip fmap a

instance (Monad m) => Monad (Iteratee s m) where
  {-# INLINE return #-}
  return = idone
  {-# INLINE (>>=) #-}
  (>>=) = bindIter

{-# INLINE bindIter #-}
bindIter :: forall s m a b. (Monad m)
    => Iteratee s m a
    -> (a -> Iteratee s m b)
    -> Iteratee s m b
bindIter m f = runIter m f onCont onErr onReq
  where
    push i str = runIter i
                         (\a         -> return (idone a, str))
                         (\k         -> k str)
                         (\iResume e -> return (ierr iResume e, EOF (Just e)))
                         (\mb doB    -> mb >>= \b -> push (doB b) str)
    onCont k  = icont $ \str -> do
                  (i', strRem) <- k str
                  let oD a  = push (f a) strRem
                      oC k' = return (i' `bindIter` f, strRem)
                      oE iResume e = return (ierr (bindIter iResume f) e
                                             ,EOF (Just e))
                      oR :: m x -> (x -> Iteratee s m a) -> m (Iteratee s m b, Stream s)
                      oR mb doB    = mb >>= \b ->
                                      push (doB b `bindIter` f) strRem
                  runIter i' oD oC oE oR
    onErr i e = ierr (i >>= f) e
    onReq :: m x -> (x -> Iteratee s m a) -> Iteratee s m b
    onReq mb doB = ireq mb (( `bindIter` f) . doB)

instance MonadTrans (Iteratee s) where
  lift = flip ireq idone

instance (MonadBase b m) => MonadBase b (Iteratee s m) where
  liftBase = lift . liftBase

instance (MonadIO m) => MonadIO (Iteratee s m) where
  liftIO = lift . liftIO

instance forall s m. (MonadCatchIO m) =>
  MonadCatchIO (Iteratee s m) where
    m `catch` f = let oC k    = icont $ \s -> k s `catch`
                                  (\e -> return (f e
                                          , EOF . Just $ toException e))
                      oE i' e = ierr (i' `catch` f) e
                  in runIter m idone oC oE (catchOR m f)
    block       = ilift block
    unblock     = ilift unblock

-- This definition is pulled out from the MonadCatchIO.catch instance because
-- it's the simplest way to make the type signatures work out...
catchOR :: (Exception e, MonadCatchIO m) => Iteratee s m a -> (e -> Iteratee s m a) -> m b -> (b -> Iteratee s m a) -> Iteratee s m a
catchOR _ f mb doB = ireq ((doB `liftM` mb) `catch` (\e -> return (f e) )) id

instance forall s. (NullPoint s, Nullable s) => MonadTransControl (Iteratee s) where
  newtype StT (Iteratee s) x =
    StIter { unStIter :: Either x (Maybe SomeException) }
  liftWith f = lift $ f $ \t -> liftM StIter (runIter t
                                 (return . Left)
                                 (const . return $ Right Nothing)
                                 (\_ e -> return $ Right (Just e))
                                 (\mb doB -> pushoR mb doB))
  restoreT = join . lift . liftM
               (either idone
                       (te . fromMaybe (iterStrExc
                          "iteratee: error in MonadTransControl instance"))
                      . unStIter )
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

pushoR :: Monad m =>
          m x
       -> (x -> Iteratee s m a)
       -> m (Either a (Maybe SomeException))
pushoR mb doB = mb >>= \b -> runIter (doB b)
   (return . Left)
   (const . return $ Right Nothing)
   (\_ e -> return $ Right (Just e))
   pushoR

te :: SomeException -> Iteratee s m a
te e = ierr (te e) e

instance (MonadBaseControl b m, Nullable s) => MonadBaseControl b (Iteratee s m) where
  newtype StM (Iteratee s m) a =
    StMIter { unStMIter :: ComposeSt (Iteratee s) m a}
  liftBaseWith = defaultLiftBaseWith StMIter
  restoreM     = defaultRestoreM unStMIter

-- |Send 'EOF' to the @Iteratee@ and disregard the unconsumed part of the
-- stream.  If the iteratee is in an exception state, that exception is
-- thrown with 'Control.Exception.throw'.  Iteratees that do not terminate
-- on @EOF@ will throw 'EofException'.
run :: forall s m a. Monad m => Iteratee s m a -> m a
run iter = runIter iter onDone onCont onErr onReq
 where
   onDone  x     = return x
   onCont  k     = k (EOF Nothing) >>= \(i,_) ->
                     runIter i onDone onCont' onErr onReq
   onCont' _     = E.throw EofException
   onErr _ e     = E.throw e
   onReq :: m x -> (x -> Iteratee s m a) -> m a
   onReq mb doB  = mb >>= run . doB

-- |Run an iteratee, returning either the result or the iteratee exception.
-- Note that only internal iteratee exceptions will be returned; exceptions
-- thrown with @Control.Exception.throw@ or @Control.Monad.CatchIO.throw@ will
-- not be returned.
-- 
-- See 'Data.Iteratee.Exception.IFException' for details.
tryRun :: forall s m a e. (Exception e, Monad m)
  => Iteratee s m a
  -> m (Either e a)
tryRun iter = runIter iter onD onC onE onR
  where
    onD  x     = return $ Right x
    onC  k     = k (EOF Nothing) >>= \(i,_) -> runIter i onD onC' onE onR
    onC' _     = return $ maybeExc (toException EofException)
    onE   _ e  = return $ maybeExc e
    onR :: m x -> (x -> Iteratee s m a) -> m (Either e a)
    onR mb doB = mb >>= tryRun . doB
    maybeExc e = maybe (Left (E.throw e)) Left (fromException e)

-- | Lift a computation in the inner monad of an iteratee.
-- 
-- A simple use would be to lift a logger iteratee to a monad stack.
-- 
-- > logger :: Iteratee String IO ()
-- > logger = mapChunksM_ putStrLn
-- > 
-- > loggerG :: MonadIO m => Iteratee String m ()
-- > loggerG = ilift liftIO logger
-- 
-- A more complex example would involve lifting an iteratee to work with
-- interleaved streams.  See the example at 'Data.Iteratee.ListLike.merge'.
ilift :: forall m n s a.
  (Monad m, Monad n)
  => (forall r. m r -> n r)
  -> Iteratee s m a
  -> Iteratee s n a
ilift f i = runIter i idone onCont onErr  onReq
 where
  onCont k = icont $ \str -> first (ilift f) `liftM` f (k str)
  onErr = ierr . ilift f
  onReq :: m x -> (x -> Iteratee s m a) -> Iteratee s n a
  onReq mb doB = ireq (liftM (ilift f . doB) (f mb)) id

-- | Lift a computation in the inner monad of an iteratee, while threading
-- through an accumulator.
ifold :: forall m n acc s a. (Monad m, Monad n)
  => (forall r. m r -> acc -> n (r, acc))
  -> acc
  -> Iteratee s m a
  -> Iteratee s n (a, acc)
ifold f acc i = runIter i onDone onCont onErr onReq
 where
  onDone x = ireq (f (return x) acc) idone
  onCont k = icont $ \str -> do
               ((i', strRes), acc') <- f (k str) acc
               return (ifold f acc' i', strRes)
  onErr i' e = ierr (ifold f acc i') e
  onReq :: m x -> (x -> Iteratee s m a) -> Iteratee s n (a, acc)
  onReq mb doB = ireq (f mb acc) (\(b', acc') -> ifold f acc' (doB b'))
