{-# LANGUAGE RankNTypes
            ,FlexibleContexts
            ,ScopedTypeVariables #-}

-- | Enumeratees - pass terminals variant.
-- 
-- Provides enumeratees that pass terminal markers ('EOF') to the inner
-- 'iteratee'.
-- 
-- Most enumeratees, upon receipt of @EOF@, will enter a done state and return
-- the inner iteratee without sending @EOF@ to it.  This allows for composing
-- enumerators as in:
-- 
-- > myEnum extraData i = do
-- > nested <- enumFile "file" (mapChunks unpacker i)
-- > inner <- run nested
-- > enumList extraData inner
-- 
-- if @mapChunks unpacker@ sent 'EOF' to the inner iteratee @i@, there would
-- be no way to submit extra data to it after 'run'ing the result from
-- @enumFile@.
-- 
-- In certain cases, this is not the desired behavior.  Consider:
-- 
-- > consumer :: Iteratee String IO ()
-- > consumer = icont (go 0)
-- >   where
-- >     go c (Chunk xs) = liftIO (putStr s) >> icont (go c)
-- >     go 10 e         = liftIO (putStr "10 loops complete")
-- >                         >> idone () (Chunk "")
-- >     go n  e         = I.seek 0 >> icont (go (n+1))
--
-- The @consumer@ iteratee does not complete until after it has received 
-- 10 @EOF@s.  If you attempt to use it in a standard enumeratee, it will
-- never terminate.  When the outer enumeratee is terminated, the inner
-- iteratee will remain in a @cont@ state, but in general there is no longer
-- any valid data for the continuation.  The enumeratee itself must pass the
-- EOF marker to the inner iteratee and remain in a cont state until the inner
-- iteratee signals its completion.
-- 
-- All enumeratees in this module will pass 'EOF' terminators to the inner
-- iteratees.

module Data.Iteratee.PTerm (
  -- * Nested iteratee combinators
   mapChunksPT
  ,mapChunksMPT
  ,convStreamPT
  ,unfoldConvStreamPT
  ,unfoldConvStreamCheckPT
  -- * ListLike analog functions
  ,breakEPT
  ,takePT
  ,takeUpToPT
  ,takeWhileEPT
  ,mapStreamPT
  ,rigidMapStreamPT
  ,filterPT
)
where

import Prelude hiding (head, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product)

import           Data.Iteratee.Iteratee
import           Data.Iteratee.ListLike (drop,dropWhile)

import qualified Data.ListLike as LL

import           Control.Monad.Trans.Class
import           Control.Monad

import qualified Data.ByteString as B
import           Data.Word (Word8)

(<$>) :: Monad m => (a1 -> r) -> m a1 -> m r
(<$>) = liftM

-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- | Convert one stream into another with the supplied mapping function.
-- 
-- A version of 'mapChunks' that sends 'EOF's to the inner iteratee.
-- 
mapChunksPT :: (Monad m) => (s -> s') -> Enumeratee s s' m a
mapChunksPT f = eneeCheckIfDonePass (icont . step)
 where
  go = eneeCheckIfDonePass (icont . step)
  step k (Chunk xs)   = doContEtee go k (f xs)
  step k NoData       = continue (step k)
  step k s@(EOF mErr) = k (EOF mErr) >>= \ret -> case ret of
      ContDone a _  -> contDoneM (return a) s
      ContMore i'   -> contMoreM $ go i'
      ContErr i' e' -> contErrM (go i') e'
{-# INLINE mapChunksPT #-}

-- | Convert a stream of @s@ to a stream of @s'@ using the supplied function.
-- 
-- A version of 'mapChunksM' that sends 'EOF's to the inner iteratee.
mapChunksMPT
  :: (Monad m)
  => (s -> m s')
  -> Enumeratee s s' m a
mapChunksMPT f = eneeCheckIfDonePass (icont . step)
 where
  go = eneeCheckIfDonePass (icont . step)
  -- step :: (Stream s' -> m (ContReturn s' m a))-> Stream s -> m (ContReturn s m (Iteratee s' m a))
  step k (Chunk xs) = f xs >>= doContEtee go k
  step k NoData     = continue $ step k
  step k s@EOF{}    = k (EOF Nothing) >>= \ret -> case ret of
                          ContDone a _ -> contDoneM (idone a) s
                          ContMore i   -> contMoreM (go i)
                          ContErr  i e -> contErrM (go i) e
{-# INLINE mapChunksMPT #-}

-- |Convert one stream into another, not necessarily in lockstep.
-- 
-- A version of 'convStream' that sends 'EOF's to the inner iteratee.
convStreamPT
  :: (Monad m, LL.ListLike s el)
  =>  Iteratee s m s'
  -> Enumeratee s s' m a
convStreamPT fi = eneeCheckIfDonePass check
  where
    go = eneeCheckIfDonePass check
    check k = isStreamFinished >>= maybe (step k) (hndl k)
    hndl k (EOF e) = lift (k (EOF e)) >>= go . wrapCont
    hndl _ _str    = error "iteratee: internal error in convStreamPT"
    step k = fi >>= lift . doContIteratee k . Chunk >>= go
{-# INLINE convStreamPT #-}

-- |The most general stream converter.
-- 
-- A version of 'unfoldConvStream' that sends 'EOF's to the inner iteratee.
unfoldConvStreamPT ::
 (Monad m, LL.ListLike s el) =>
  (acc -> Iteratee s m (acc, s'))
  -> acc
  -> Enumeratee s s' m a
unfoldConvStreamPT fi acc0 = unfoldConvStreamCheckPT eneeCheckIfDonePass fi acc0
{-# INLINE unfoldConvStreamPT #-}

-- | A version of 'unfoldConvStreamCheck' that sends 'EOF's
-- to the inner iteratee.
unfoldConvStreamCheckPT
  :: (Monad m, LL.ListLike fromStr elo)
  => ((Cont toStr m a -> Iteratee fromStr m (Iteratee toStr m a))
      -> Enumeratee fromStr toStr m a
     )
  -> (acc -> Iteratee fromStr m (acc, toStr))
  -> acc
  -> Enumeratee fromStr toStr m a
unfoldConvStreamCheckPT checkDone f acc0 = checkDone (check acc0)
  where
    go acc = checkDone (check acc)
    check acc k = isStreamFinished >>= maybe (step acc k) (hndl acc k)
    hndl acc k (EOF e) = lift (k (EOF e)) >>= go acc . wrapCont
    hndl _   _ _str    = error "iteratee: internal error in unfoldConvStreamCheckPT"

    step acc k = do
      (acc',s') <- f acc
      i' <- lift . doContIteratee k $ Chunk s'
      go acc' i'
{-# INLINE unfoldConvStreamCheckPT #-}

-- -------------------------------------
-- ListLike variants

-- | A variant of 'Data.Iteratee.ListLike.breakE' that passes 'EOF's.
breakEPT
  :: (LL.ListLike s el, Monad m)
  => (el -> Bool)
  -> Enumeratee s s m a
breakEPT cpred = eneeCheckIfDonePass (icont . step) >=>
                  \i' -> dropWhile (not . cpred) >> return i'
 where
  go = eneeCheckIfDonePass (icont . step)
  step k (Chunk s)
      | LL.null s  = contMoreM (icont (step k))
      | otherwise  = case LL.break cpred s of
        (str', tail')
          | LL.null tail' -> doContEtee go k str'
          | otherwise     -> k (Chunk str') >>= \ret -> case ret of
                              ContDone a _ -> contDoneM (idone a) (Chunk tail')
                              ContMore i   -> contDoneM i (Chunk tail')
                              ContErr  i e -> contDoneM (ierr i e) (Chunk tail')
  step k NoData         =  continue (step k)
  step k stream@(EOF{}) =  k stream >>= \ret -> case ret of
                              ContDone a _ -> contDoneM (idone a) stream
                              ContMore i   -> contMoreM (go i)
                              ContErr  i e -> contErrM (go i) e
{-# INLINE breakEPT #-}

-- | A variant of 'Data.Iteratee.ListLike.take' that passes 'EOF's.
takePT ::
  (Monad m, LL.ListLike s el)
  => Int   -- ^ number of elements to consume
  -> Enumeratee s s m a
takePT = go
 where
  go n iter
    | n <= 0 = return iter
    | otherwise = runIter iter (onDone n) (onCont n) (onErr n)

  onDone n x = drop n >> idone (idone x)
  onCont n k = icont (step n k)
  onErr n i  = ierr (go n i)

  step n k (Chunk str)
      | LL.null str        = continue (step n k)
      | LL.length str <= n = k (Chunk str) >>= \ret -> case ret of
                              ContDone a _ -> contMoreM (go (n-LL.length str)
                                                                (idone a))
                              ContMore i  -> contMoreM (go (n-LL.length str) i)
                              ContErr i e -> contErrM (go (n-LL.length str) i)
                                                      e
      | otherwise          = k (Chunk s1) >>= \ret -> case ret of
                              ContDone a _ -> contDoneM (idone a) (Chunk s2)
                              ContMore i   -> contDoneM i (Chunk s2)
                              ContErr i e  -> contErrM (idone i) e
      where (s1, s2) = LL.splitAt n str
  step n k NoData       = continue (step n k)
  step n k stream@EOF{} = k stream >>= \rk -> case rk of
         ContDone a _str' -> contDoneM (idone a) stream
         ContMore inner   -> contMoreM (go n inner)
         ContErr inner e  -> contErrM  (go n inner) e
{-# INLINE takePT #-}

-- | A variant of 'Data.Iteratee.ListLike.takeUpTo' that passes 'EOF's.
takeUpToPT :: (Monad m, LL.ListLike s el) => Int -> Enumeratee s s m a
takeUpToPT = go
  where
    go count iter
      | count <= 0 = idone iter
      | otherwise  = runIter iter onDone (onCont count) (onErr count)

    onDone x       = idone (idone x)
    onCont count k = icont (step count k)
    onErr count i' = ierr (go count i')

    step n k (Chunk str)
      | LL.null str       = continue (step n k)
      | LL.length str < n = k (Chunk str) >>= \ret -> case ret of
                              ContDone a str' -> contDoneM (idone a) str'
                              ContMore i -> contMoreM (go
                                                        (n - LL.length str)
                                                        i)
                              ContErr i e -> contErrM (go
                                                        (n - LL.length str)
                                                        i)
                                                      e

      | otherwise         = do
         -- check to see if the inner iteratee has completed, and if so,
         -- grab any remaining stream to put it in the outer iteratee.
         -- the outer iteratee is always complete at this stage, although
         -- the inner may not be.
         let (s1, s2) = LL.splitAt n str
         ret <- k (Chunk s1)
         case ret of
            ContDone a preStr -> case preStr of
                (Chunk pre)
                  | LL.null pre -> contDoneM (idone a) $ Chunk s2
                  | otherwise   -> contDoneM (idone a) $ Chunk $ pre `LL.append` s2
                NoData          -> contDoneM (idone a) $ Chunk s2

                -- this case shouldn't ever happen, except possibly
                -- with broken iteratees
                EOF{}           -> contDoneM (idone a) preStr

            ContMore i   -> contDoneM i (Chunk s2)
            ContErr  i e -> contErrM (idone i) e
    step n k NoData         = continue (step n k)
    step n k stream@EOF{} = k stream >>= \rk -> case rk of
           ContDone a _str' -> contDoneM (idone a) stream
           ContMore inner   -> contMoreM (go n inner)
           ContErr inner e  -> contErrM (go n inner) e
{-# INLINE takeUpToPT #-}

-- | A variant of 'Data.Iteratee.ListLike.takeWhileE' that passes 'EOF's.
takeWhileEPT
 :: (LL.ListLike s el, Monad m)
 => (el -> Bool)
 -> Enumeratee s s m a
takeWhileEPT = breakEPT . (not .)
{-# INLINE takeWhileEPT #-}

-- | A variant of 'Data.Iteratee.ListLike.mapStream' that passes 'EOF's.
mapStreamPT
  :: (LL.ListLike (s el) el
     ,LL.ListLike (s el') el'
     ,Monad m
     ,LooseMap s el el')
  => (el -> el')
  -> Enumeratee (s el) (s el') m a
mapStreamPT f = mapChunksPT (lMap f)
{-# INLINE mapStreamPT #-}

-- | A variant of 'Data.Iteratee.ListLike.rigidMapStream' that passes 'EOF's.
rigidMapStreamPT
  :: (LL.ListLike s el, Monad m)
  => (el -> el)
  -> Enumeratee s s m a
rigidMapStreamPT f = mapChunksPT (LL.rigidMap f)
{-# INLINE rigidMapStreamPT #-}

-- | A variant of 'Data.Iteratee.ListLike.filter' that passes 'EOF's.
filterPT
  :: (Monad m, LL.ListLike s el)
  => (el -> Bool)
  -> Enumeratee s s m a
filterPT p = convStreamPT (LL.filter p <$> getChunk)
{-# INLINE filterPT #-}
