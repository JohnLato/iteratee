{-# LANGUAGE NoMonomorphismRestriction, BangPatterns, ScopedTypeVariables #-}

module Data.Iteratee.Parallel (
  psequence_
 -- ,psequence
 ,parE
 ,parI
 ,liftParI
 ,mapReduce
)

where

import           Control.Monad.IO.Class
import           Data.Iteratee as I hiding (mapM_, zip, filter)
import qualified Data.ListLike as LL

import           Data.Monoid

import           Control.Concurrent
import           Control.Parallel
import           Control.Monad

-- | Transform usual Iteratee into parallel composable one, introducing
-- one step extra delay.
-- 
-- Ex - time spent in Enumerator working on x'th packet
-- Ix - time spent in Iteratee working on x'th packet
-- z - last packet, y = (z-1)'th packet
-- 
-- regular  Iteratee: E0 - I0,  E1 - I1,  E2 - I2        .. Ez -> Iz
-- parallel Iteratee: E0,   E1,  E2,       .. Ez
--                 \_ I0\_ I1\_ .. Iy\__ Iz
-- 
parI :: forall s a. (Monoid s) => Iteratee s IO a -> Iteratee s IO a
parI  iter0 = do
    var <- liftIO $ newEmptyMVar
    liftIO . void . forkIO $ consumer var iter0
    icont $ reader var
  where
    reader var NoData = continue $ reader var
    reader var c@Chunk{} = do
        (innervar,res) <- takeMVar var
        case res of
            ContDone a s' -> contDoneM a (s' `mappend` c)
            ContMore _    -> putMVar innervar c >> continue (reader var)
            ContErr _ e   -> continueErr e $ \c' -> putMVar innervar c'
                                                    >> continue (reader var)
    reader var str@(EOF Nothing) = do
        (innervar,res) <- takeMVar var
        case res of
            ContDone a s' -> contDoneM a (s' `mappend` str)
            ContMore _    -> putMVar innervar str >> final var
            ContErr _ e   -> continueErr e $ \c' -> putMVar innervar c'
                                                    >> final var
    -- treat this case exactly like receiving a chunk, since the enumerator
    -- should be expecting the iteratee to recover and continue
    reader var c@(EOF (Just _)) = do
        (innervar,res) <- takeMVar var
        case res of
            ContDone a s' -> contDoneM a (s' `mappend` c)
            ContMore _    -> putMVar innervar c >> continue (reader var)
            ContErr _ e   -> continueErr e $ \c' -> putMVar innervar c'
                                                    >> continue (reader var)

    -- we received EOF, so we need to send either ContDone or ContErr
    -- at this time.  It is still possible to recover via ContErr
    final var = do
        (innervar,res) <- takeMVar var
        case res of
            ContDone a s -> contDoneM a s
            ContMore _   -> contErrM (icont (reader var)) EofException
            ContErr _ e  -> continueErr e $ \c' -> putMVar innervar c'
                                                   >> final var
    consumer var iter = runIter iter onDone onCont onErr
      where
        onDone a = do
            unused <- newEmptyMVar
            putMVar var (unused, ContDone a mempty)
        onCont k = do
            innervar <- newEmptyMVar
            putMVar var (innervar, ContMore (icont k))
            str <- takeMVar innervar
            res <- k str
            case res of
                ContDone a str' -> putMVar var (innervar, (ContDone a str'))
                ContMore i'     -> consumer var i'
                ContErr i' e    -> consumer var $ ierr i' e
        onErr i' e = do
            innervar <- newEmptyMVar
            putMVar var (innervar, ContErr i' e)
            str <- takeMVar innervar
            iRecovered <- enumChunk str i'
            consumer var iRecovered


-- | Transform an Enumeratee into a parallel composable one, introducing
--  one step extra delay, see 'parI'.
parE ::
  (Monoid s1)
  => Enumeratee s1 s2 IO r
  -> Enumeratee s1 s2 IO r
parE outer inner = parI (outer inner)

-- | Enumerate a list of iteratees over a single stream simultaneously
-- and discard the results. Each iteratee runs in a separate forkIO thread,
-- passes all errors from iteratees up.
psequence_ ::
  (LL.ListLike s el)
  => [Iteratee s IO a]
  -> Iteratee s IO ()
psequence_ = I.sequence_ . map parI


{-
-- | Enumerate a list of iteratees over a single stream simultaneously
-- and keeps the results. Each iteratee runs in a separete forkIO thread, passes all
-- errors from iteratees up.
psequence = I.sequence . map parI
-}

-- | A variant of 'parI' with the parallelized iteratee lifted into an
-- arbitrary MonadIO.
liftParI ::
  (Monoid s, MonadIO m)
  => Iteratee s IO a
  -> Iteratee s m a
liftParI = ilift liftIO . parI

-- | Perform a parallel map/reduce.  The `bufsize` parameter controls
-- the maximum number of chunks to read at one time.  A larger bufsize
-- allows for greater parallelism, but will require more memory.
--
-- Implementation of `sum`
--
-- > sum :: (Monad m, LL.ListLike s) => Iteratee s m Int64
-- > sum = getSum <$> mapReduce 4 (Sum . LL.sum)
mapReduce ::
  (Monad m, Monoid b)
  => Int               -- ^ maximum number of chunks to read
  -> (s -> b)          -- ^ map function
  -> Iteratee s m b
mapReduce bufsize f = icontP (step (0, []))
 where
  step (!buf,acc) (Chunk xs)
    | buf >= bufsize =
        let acc'  = mconcat acc
            b'    = f xs
        in b' `par` acc' `pseq` continueP (step (0,[b' `mappend` acc']))
    | otherwise     =
        let b' = f xs
        in b' `par` continueP (step (succ buf,b':acc))
  step a NoData =
    continueP (step a)
  step (_,acc) s@(EOF Nothing) =
    ContDone (mconcat acc) s
  step acc     (EOF (Just err))  =
    ContErr (icontP $ step acc) (wrapEnumExc err)

