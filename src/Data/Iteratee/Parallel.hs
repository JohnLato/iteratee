{-# LANGUAGE NoMonomorphismRestriction, BangPatterns #-}

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
import           Control.Monad.Trans.Class
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
parI :: (Nullable s, Monoid s) => Iteratee s IO a -> Iteratee s IO a
parI = liftI . firstStep
  where
    -- first step, here we fork separete thread for the next chain and at the
    -- same time ask for more date from the previous chain
    firstStep iter chunk = do
        var <- liftIO newEmptyMVar
        _   <- sideStep var chunk iter
        liftI $ go var

    -- somewhere in the middle, we are getting iteratee from previous step,
    -- feeding it with some new data, asking for more data and starting
    -- more processing in separete thread
    go var chunk@(Chunk _) = do
        iter <- liftIO $ takeMVar var
        _    <- sideStep var chunk iter
        liftI $ go var

    -- final step - no more data, so  we need to inform our consumer about it
    go var e = do
        iter <- liftIO $ takeMVar var
        join . lift $ enumChunk e iter

    -- forks away from the main computation, return results via MVar
    sideStep var chunk iter = liftIO . forkIO $ runIter iter onDone onCont
        where
            onDone a s = putMVar var $ idone a s
            onCont k _ = runIter (k chunk) onDone onFina
            onFina k e = putMVar var $ icont k e

-- | Transform an Enumeratee into a parallel composable one, introducing
--  one step extra delay, see 'parI'.
parE ::
  (Nullable s1, Nullable s2, Monoid s1)
  => Enumeratee s1 s2 IO r
  -> Enumeratee s1 s2 IO r
parE outer inner = parI (outer inner)

-- | Enumerate a list of iteratees over a single stream simultaneously
-- and discard the results. Each iteratee runs in a separate forkIO thread,
-- passes all errors from iteratees up.
psequence_ ::
  (LL.ListLike s el, Nullable s)
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
  (Nullable s, Monoid s, MonadIO m)
  => Iteratee s IO a
  -> Iteratee s m a
liftParI = ilift liftIO . parI

-- | Perform a parallel map/reduce.  The `bufsize` parameter controls
-- the maximum number of chunks to read at one time.  A larger bufsize
-- allows for greater parallelism, but will require more memory.
--
-- Implementation of `sum`
--
-- > sum :: (Monad m, LL.ListLike s, Nullable s) => Iteratee s m Int64
-- > sum = getSum <$> mapReduce 4 (Sum . LL.sum)
mapReduce ::
  (Monad m, Nullable s, Monoid b)
  => Int               -- ^ maximum number of chunks to read
  -> (s -> b)          -- ^ map function
  -> Iteratee s m b
mapReduce bufsize f = liftI (step (0, []))
 where
  step a@(!buf,acc) (Chunk xs)
    | nullC xs = liftI (step a)
    | buf >= bufsize =
        let acc'  = mconcat acc
            b'    = f xs
        in b' `par` acc' `pseq` liftI (step (0,[b' `mappend` acc']))
    | otherwise     =
        let b' = f xs
        in b' `par` liftI (step (succ buf,b':acc))
  step (_,acc) s@(EOF Nothing) =
    idone (mconcat acc) s
  step acc       (EOF (Just err))  =
    throwRecoverableErr err (step acc)

