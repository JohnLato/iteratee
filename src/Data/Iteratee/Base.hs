{-# LANGUAGE TypeFamilies
            ,MultiParamTypeClasses
            ,FlexibleContexts
            ,FlexibleInstances
            ,UndecidableInstances
            ,RankNTypes
            ,DeriveDataTypeable
            ,DeriveFunctor
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
  -- ** Iteratee continuation returns
  ,ContReturn (..)
  ,Cont
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
  -- ** Returning from continuations
  ,continue
  ,continueErr
  ,continueP
  ,continueErrP
  ,wrapCont
  -- ** Continuation-return handling
  ,contDoneM
  ,contMoreM
  ,contErrM
  ,nextStep
  ,mapCont
  ,mapContRet
  ,doContIteratee
  ,doContEtee
  ,doCont
  -- ** Stream Functions
  -- * Classes
  ,module X
  -- * Debugging utilities
  ,traceContIteratee
  ,traceContEtee
)
where

import Prelude hiding (null)
import Data.Iteratee.Exception
import Data.Iteratee.Base.LooseMap as X

import Data.Maybe
import Data.Monoid

import Control.Monad (liftM, join)
import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Applicative hiding (empty)
import qualified Control.Exception as E
import Data.Data

import Debug.Trace


-- |A stream is a (continuing) sequence of elements bundled in Chunks.
-- The first variant indicates termination of the stream.
-- Chunk a gives the currently available part of the stream.
-- The stream is not terminated yet.
-- The case (null Chunk) signifies a stream with no currently available
-- data but which is still continuing. A stream processor should,
-- informally speaking, ``suspend itself'' and wait for more data
-- to arrive.

data Stream c =
  EOF (Maybe EnumException)
  | NoData
  | Chunk c
  deriving (Show, Typeable)

instance (Eq c) => Eq (Stream c) where
  (Chunk c1) == (Chunk c2)           = c1 == c2
  NoData     == NoData               = True
  (EOF Nothing) == (EOF Nothing)     = True
  (EOF (Just e1)) == (EOF (Just e2)) = typeOf e1 == typeOf e2
  _ == _                             = False

instance Monoid c => Monoid (Stream c) where
  mempty = NoData
  mappend (EOF mErr) _ = EOF mErr
  mappend _ (EOF mErr) = EOF mErr
  mappend NoData     b = b
  mappend a     NoData = a
  mappend (Chunk s1) (Chunk s2) = Chunk (s1 `mappend` s2)

-- |Map a function over a stream.
instance Functor Stream where
  fmap f (Chunk xs) = Chunk $ f xs
  fmap _ NoData     = NoData
  fmap _ (EOF mErr) = EOF mErr


-- | The continuation type of an incomplete iteratee.
type Cont s m a = Stream s -> m (ContReturn s m a)


-- ----------------------------------------------
-- continuation creation and handling


data ContReturn s m a =
    ContDone a (Stream s)
  | ContMore (Iteratee s m a)
  | ContErr  (Iteratee s m a) IterException
  deriving (Functor)

-- | Create a @ContReturn@ in a monad
--
-- contDoneM a s === return (ContDone a s)
contDoneM :: Monad m => a -> Stream s -> m (ContReturn s m a)
contDoneM a = return . ContDone a
{-# INLINE contDoneM #-}


-- | Create a @ContReturn@ in a monad
--
-- contMoreM === return . ContMore
contMoreM :: Monad m => Iteratee s m a -> m (ContReturn s m a)
contMoreM = return . ContMore
{-# INLINE contMoreM #-}

-- | Create a @ContReturn@ from a step function.
nextStep :: Monad m => Cont s m a -> m (ContReturn s m a)
nextStep = contMoreM . icont
{-# INLINE nextStep #-}

-- | Create a @ContReturn@ in a monad
--
-- contErrM i e === return (ContErr i e)
contErrM :: (Monad m, IException e) => Iteratee s m a -> e -> m (ContReturn s m a)
contErrM i = return . ContErr i . toIterException
{-# INLINE contErrM #-}

-- | Map over an iteratee continuation.
--
-- Sometimes useful for creating iteratees
mapCont
  :: Monad m
  => (Iteratee s m a -> Iteratee s m b)
  -> (a -> b)
  -> Cont s m a
  -> Iteratee s m b
mapCont fi f k = icont $ \str -> k str >>= \res -> return $ case res of
          ContDone a str' -> ContDone (f a) str'
          ContMore i'     -> ContMore (fi i')
          ContErr  i' e   -> ContErr  (fi i') e
{-# INLINE mapCont #-}

mapContRet
  :: Monad m
  => (Iteratee s m a -> Iteratee s m b)
  -> (a -> Stream s -> b)
  -> ContReturn s m a
  -> ContReturn s m b
mapContRet fi f ret = case ret of
    ContDone a str' -> ContDone (f a str') str'
    ContMore i'     -> ContMore (fi i')
    ContErr  i' e   -> ContErr (fi i') e
{-# INLINE mapContRet #-}


-- | Create an iteratee from a @ContReturn@.  Sometimes used when writing
-- enumerators.
doContIteratee
  :: Monad m
  => Cont s m a
  -> Stream s
  -> m (Iteratee s m a)
doContIteratee k s = k s >>= \res -> case res of
    ContDone a _ -> idoneM a
    ContMore i   -> return i
    ContErr  i e -> ierrM i e
{-# INLINE doContIteratee #-}

-- | Create a @ContReturn@ from a stepping function, a continuation, and
-- an input stream.  Useful when writing enumeratees.
doContEtee
  :: Monad m
  => (Iteratee sInner m a -> Iteratee sOuter m (Iteratee sInner m a))
  -> (Stream sInner -> m (ContReturn sInner m a))
  -> sInner
  -> m (ContReturn sOuter m (Iteratee sInner m a))
doContEtee go k s = k (Chunk s) >>= \ret -> case ret of
    ContDone a _ -> contDoneM (idone a) NoData
    ContMore i   -> contMoreM (go i)
    ContErr  i e -> contErrM (go i) e
{-# INLINE doContEtee #-}

-- | A generic @ContReturn@ handler, if you don't want to write a bunch of
-- case statements.
doCont
  :: Monad m
  => Cont s m a
  -> Stream s
  -> (a -> Stream s -> m b)
  -> (Iteratee s m a -> m b)
  -> (Iteratee s m a -> IterException -> m b)
  -> m b
doCont k str oD oC oE = k str >>= \res -> case res of
    ContDone a s -> oD a s
    ContMore i   -> oC i
    ContErr  i e -> oE i e
{-# INLINE doCont #-}


-- ----------------------------------------------
-- | Monadic iteratee
newtype Iteratee s m a = Iteratee{ runIter :: forall r.
          (a -> r) ->
          ((Stream s -> m (ContReturn s m a)) -> r) ->
          (Iteratee s m a -> IterException -> r) ->
          r}


-- invariants:
-- 1.  The returned @Stream s@ of the second (continuation) parameter must
--       be a subset of the input stream.

-- ----------------------------------------------

idone :: a -> Iteratee s m a
idone a = Iteratee $ \onDone _ _ -> onDone a
{-# INLINE idone #-}

-- | Create an iteratee from a continuation
icont :: (Stream s -> m (ContReturn s m a)) -> Iteratee s m a
icont k = Iteratee $ \_ onCont _ -> onCont k
{-# INLINE icont #-}

-- | Create an iteratee from a pure continuation
icontP :: Monad m => (Stream s -> (ContReturn s m a)) -> Iteratee s m a
icontP k = Iteratee $ \_ onCont _ -> onCont (return . k)
{-# INLINE icontP #-}

-- | Create a continuation return value from a continuation
continue
  :: Monad m
  => Cont s m a
  -> m (ContReturn s m a)
continue = contMoreM . icont
{-# INLINE continue #-}

-- | Create a continuation return value from a pure continuation function
continueP
  :: Monad m
  => (Stream s -> ContReturn s m a)
  -> ContReturn s m a
continueP = ContMore . icontP
{-# INLINE continueP #-}

-- | Wrap a continuation return into an iteratee.  This may cause data loss
-- if not used properly.
wrapCont :: ContReturn s m a -> Iteratee s m a
wrapCont (ContDone a _) = idone a
wrapCont (ContMore i)   = i
wrapCont (ContErr i e)  = ierr i e
{-# INLINE wrapCont #-}

continueErr
  :: (IException e, Monad m)
  => e
  -> Cont s m a
  -> m (ContReturn s m a)
continueErr e k = return $ ContErr (icont k) (toIterException e)
{-# INLINE continueErr #-}

continueErrP
  :: (IException e, Monad m)
  => e
  -> (Stream s -> ContReturn s m a)
  -> ContReturn s m a
continueErrP e k = ContErr (icontP k) (toIterException e)
{-# INLINE continueErrP #-}


-- | identical to icont, left in for compatibility-ish reasons
liftI :: Monad m => (Stream s -> ContReturn s m a) -> Iteratee s m a
liftI = icontP
{-# INLINE liftI #-}

ierr :: IException e => Iteratee s m a -> e -> Iteratee s m a
ierr i e = Iteratee $ \_ _ onErr -> onErr i (toIterException e)
{-# INLINE ierr #-}

ireq :: Monad m => m b -> (b -> Iteratee s m a) -> Iteratee s m a
ireq mb bf = icont $ \str -> do
  b <- mb
  runIter (bf b) (\a -> contDoneM a str)
                 (\k -> k str)
                 (\i' e -> contErrM i' e)
{-# INLINE ireq #-}

-- Monadic versions, frequently used by enumerators
idoneM :: Monad m => a -> m (Iteratee s m a)
idoneM x = return $ idone x
{-# INLINE idoneM #-}

ierrM :: (IException e, Monad m) => Iteratee s m a -> e -> m (Iteratee s m a)
ierrM i e = return $ ierr i e
{-# INLINE ierrM #-}

instance forall s m. (Functor m) => Functor (Iteratee s m) where
  {-# INLINE fmap #-}
  fmap f m = runIter m (idone . f) onCont onErr
    where
      onCont k      = icont $ fmap (fmap f) . k
      onErr i e     = ierr (fmap f i) e

instance (Functor m, Monad m) => Applicative (Iteratee s m) where
    pure x  = idone x
    {-# INLINE (<*>) #-}
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
bindIter m f = go m f
  where
    go l r = runIter l r onCont onErr
    push :: Iteratee s m b -> Stream s -> m (ContReturn s m b)
    push i str = runIter i
                         (\a         -> return $ ContDone a str)
                         (\k         -> k str)
                         (\iResume e -> return $ ContErr (ierr iResume e) e)
    onCont :: (Stream s -> m (ContReturn s m a)) -> Iteratee s m b
    onCont k  = icont $ \str -> do
                  res <- k str
                  case res of
                      ContDone a  strRem -> push (f a) strRem
                      ContMore i'        -> return $ ContMore (i' `go` f)
                      ContErr  i' e      -> contErrM (i' `go` f) e

    onErr i e = ierr (i `go` f) e

instance MonadTrans (Iteratee s) where
  lift = flip ireq idone

instance (MonadBase b m) => MonadBase b (Iteratee s m) where
  liftBase = lift . liftBase

instance (MonadIO m) => MonadIO (Iteratee s m) where
  liftIO = lift . liftIO


instance forall s. MonadTransControl (Iteratee s) where
  newtype StT (Iteratee s) x =
    StIter { unStIter :: Either x (Maybe IterException) }
  liftWith f = lift $ f $ \t -> liftM StIter (runIter t
                                 (return . Left)
                                 (const . return $ Right Nothing)
                                 (\_ e -> return $ Right (Just e)) )
  restoreT = join . lift . liftM
               (either idone
                       (te . fromMaybe (iterStrExc
                          "iteratee: error in MonadTransControl instance"))
                      . unStIter )
  {-# INLINE liftWith #-}
  {-# INLINE restoreT #-}

te :: IterException -> Iteratee s m a
te e = ierr (te e) e

instance (MonadBaseControl b m) => MonadBaseControl b (Iteratee s m) where
  newtype StM (Iteratee s m) a =
    StMIter { unStMIter :: ComposeSt (Iteratee s) m a}
  {-# INLINE liftBaseWith #-}
  liftBaseWith = defaultLiftBaseWith StMIter
  {-# INLINE restoreM #-}
  restoreM     = defaultRestoreM unStMIter

-- |Send 'EOF' to the @Iteratee@ and disregard the unconsumed part of the
-- stream.  If the iteratee is in an exception state, that exception is
-- thrown with 'Control.Exception.throw'.  Iteratees that do not terminate
-- on @EOF@ will throw 'EofException'.
run :: forall s m a. Monad m => Iteratee s m a -> m a
run iter = runIter iter onDone onCont onErr
 where
   onDone  x     = return x
   onCont  k     = k (EOF Nothing) >>= \res -> case res of
                      ContDone a _ -> return a
                      ContMore _   -> E.throw EofException
                      ContErr  _ e -> E.throw e
   onErr _ e     = E.throw e
{-# INLINE run #-}

-- |Run an iteratee, returning either the result or the iteratee exception.
-- Note that only internal iteratee exceptions will be returned; exceptions
-- thrown with @Control.Exception.throw@ or @Control.Monad.CatchIO.throw@ will
-- not be returned, and instead will be thrown in the 'm' monad.
-- 
-- See 'Data.Iteratee.Exception.IFException' for details.
tryRun :: forall s m a. (Monad m)
  => Iteratee s m a
  -> m (Either IFException a)
tryRun iter = runIter iter onD onC onE
  where
    onD        = return . Right
    onC  k     = doCont k (EOF Nothing) (\a _ -> return $ Right a)
                                        (const . return $ maybeExc EofException)
                                        (\_ e -> return $ maybeExc e)
    onE   _ e  = return $ maybeExc e
    maybeExc e = maybe (Left (E.throw e)) Left (fromException $ toException e)

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
ilift f i = runIter i idone onCont onErr
 where
  onCont k = icont $ \str -> f (k str) >>= \res -> case res of
                ContDone a str' -> return $ ContDone a str'
                ContMore i'     -> return $ ContMore (ilift f i')
                ContErr  i' e   -> return $ ContErr (ilift f i') e
  onErr = ierr . ilift f

-- | Lift a computation in the inner monad of an iteratee, while threading
-- through an accumulator.
ifold :: forall m n acc s a. (Monad m, Monad n)
  => (forall r. m r -> acc -> n (r, acc))
  -> acc
  -> Iteratee s m a
  -> Iteratee s n (a, acc)
ifold f acc i = runIter i onDone onCont onErr
 where
  onDone x = ireq (f (return x) acc) idone
  onCont k = icont $ \str -> f (k str) acc >>= \res -> return $ case res of
                  (ContDone a str', acc') -> ContDone (a, acc') str'
                  (ContMore i', acc')     -> ContMore (ifold f acc' i')
                  (ContErr  i' e, acc')   -> ContErr  (ifold f acc' i') e

  onErr i' e = ierr (ifold f acc i') e


-- some useful debugging tools

-- | like 'doContIteratee'
traceContIteratee
  :: Monad m
  => String
  -> Cont s m a
  -> Stream s
  -> m (Iteratee s m a)
traceContIteratee lbl k s = k s >>= \res -> case res of
    ContDone a _ -> trace (lbl ++ ": ContDone") $ idoneM a
    ContMore i   -> trace (lbl ++ ": ContMore") $ return i
    ContErr  i e -> trace (lbl ++ ": ContErr " ++ show e) $ ierrM i e

-- | like 'doContEtee'
traceContEtee
  :: Monad m
  => String
  -> (Iteratee sInner m a -> Iteratee sOuter m (Iteratee sInner m a))
  -> (Stream sInner -> m (ContReturn sInner m a))
  -> sInner
  -> m (ContReturn sOuter m (Iteratee sInner m a))
traceContEtee lbl go k s = k (Chunk s) >>= \ret -> case ret of
    ContDone a _ -> trace (lbl ++ ": ContDone") $ contDoneM (idone a) NoData
    ContMore i   -> trace (lbl ++ ": ContMore") $ contMoreM (go i)
    ContErr  i e -> trace (lbl ++ ": ContErr " ++ show e) $ contErrM (go i) e
