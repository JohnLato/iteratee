{-# LANGUAGE KindSignatures
            ,RankNTypes
            ,FlexibleContexts
            ,ScopedTypeVariables
            ,BangPatterns
            ,DeriveDataTypeable #-}

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
-- > consumer = liftI (go 0)
-- >   where
-- >     go c (Chunk xs) = liftIO (putStr s) >> liftI (go c)
-- >     go 10 e         = liftIO (putStr "10 loops complete")
-- >                         >> idone () (Chunk "")
-- >     go n  e         = I.seek 0 >> liftI (go (n+1))
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
import           Data.Iteratee.ListLike (drop)

import qualified Data.ListLike as LL

import           Control.Applicative ((<$>))
import           Control.Exception
import           Control.Monad.Trans.Class

import qualified Data.ByteString as B
import           Data.Monoid
import           Data.Word (Word8)

-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- | Convert one stream into another with the supplied mapping function.
-- 
-- A version of 'mapChunks' that sends 'EOF's to the inner iteratee.
-- 
mapChunksPT :: (NullPoint s) => (s -> s') -> Enumeratee s s' m a
mapChunksPT f = eneeCheckIfDonePass (icont . step)
 where
  step k (Chunk xs) = eneeCheckIfDonePass (icont . step) . k . Chunk $ f xs
  step k (EOF mErr) = eneeCheckIfDonePass (icont . step) . k $ EOF mErr
{-# INLINE mapChunksPT #-}

-- | Convert a stream of @s@ to a stream of @s'@ using the supplied function.
-- 
-- A version of 'mapChunksM' that sends 'EOF's to the inner iteratee.
mapChunksMPT
  :: (Monad m, NullPoint s, Nullable s)
  => (s -> m s')
  -> Enumeratee s s' m a
mapChunksMPT f = eneeCheckIfDonePass (icont . step)
 where
  step k (Chunk xs) = lift (f xs) >>=
                        eneeCheckIfDonePass (icont . step) . k . Chunk
  step k (EOF mErr) = eneeCheckIfDonePass (icont . step) . k $ EOF mErr
{-# INLINE mapChunksMPT #-}

-- |Convert one stream into another, not necessarily in lockstep.
-- 
-- A version of 'convStream' that sends 'EOF's to the inner iteratee.
convStreamPT
  :: (Monad m, Nullable s, NullPoint s')
  =>  Iteratee s m s'
  -> Enumeratee s s' m a
convStreamPT fi = go
  where
    go = eneeCheckIfDonePass check
    check k (Just e) = throwRecoverableErr e (const identity)
                       >> go (k $ Chunk empty)
    check k _ = isStreamFinished >>= maybe (step k)
                  (\e -> case fromException e of
                    Just EofException -> go . k $ EOF Nothing
                    Nothing -> go . k . EOF $ Just e)
    step k = fi >>= go . k . Chunk
{-# INLINABLE convStreamPT #-}

-- |The most general stream converter.
-- 
-- A version of 'unfoldConvStream' that sends 'EOF's to the inner iteratee.
unfoldConvStreamPT ::
 (Monad m, Nullable s, NullPoint s') =>
  (acc -> Iteratee s m (acc, s'))
  -> acc
  -> Enumeratee s s' m a
unfoldConvStreamPT f acc0 = go acc0
  where
    go acc = eneeCheckIfDonePass (check acc)
    check acc k (Just e) = throwRecoverableErr e (const identity)
                           >> go acc (k $ Chunk empty)
    check acc k _ = isStreamFinished >>= maybe (step acc k)
                      (\e -> case fromException e of
                        Just EofException -> go acc . k $ EOF Nothing
                        Nothing -> go acc . k . EOF $ Just e)
    step acc k = f acc >>= \(acc', s') -> go acc' . k $ Chunk s'
{-
    check acc k _ = isStreamFinished >>=
                    maybe (step acc k) (idone (liftI k) . EOF . Just)
    step acc k = f acc >>= \(acc', s') ->
                    go acc' . k . Chunk $ s'
-}

-- | A version of 'unfoldConvStreamCheck' that sends 'EOF's
-- to the inner iteratee.
unfoldConvStreamCheckPT
  :: (Monad m, Nullable elo)
  => (((Stream eli -> Iteratee eli m a)
        -> Maybe SomeException
        -> Iteratee elo m (Iteratee eli m a)
      )
      -> Enumeratee elo eli m a
     )
  -> (acc -> Iteratee elo m (acc, eli))
  -> acc
  -> Enumeratee elo eli m a
unfoldConvStreamCheckPT checkDone f acc0 = checkDone (check acc0)
  where
    check acc k mX = step acc k mX
    step acc k Nothing = f acc >>= \(acc', s') ->
                  (checkDone (check acc') . k $ Chunk s')
    step acc k (Just ex) = throwRecoverableErr ex $ \str' ->
      let i = f acc >>= \(acc', s') ->
                           (checkDone (check acc') . k $ Chunk s')
      in joinIM $ enumChunk str' i
{-# INLINABLE unfoldConvStreamCheckPT #-}

-- -------------------------------------
-- ListLike variants

-- | A variant of 'Data.Iteratee.ListLike.breakE' that passes 'EOF's.
breakEPT
  :: (LL.ListLike s el, NullPoint s)
  => (el -> Bool)
  -> Enumeratee s s m a
breakEPT cpred = eneeCheckIfDonePass (icont . step)
 where
  step k (Chunk s)
      | LL.null s  = liftI (step k)
      | otherwise  = case LL.break cpred s of
        (str', tail')
          | LL.null tail' -> eneeCheckIfDonePass (icont . step) . k $ Chunk str'
          | otherwise     -> idone (k $ Chunk str') (Chunk tail')
  step k stream           =  idone (k stream) stream
{-# INLINE breakEPT #-}

-- | A variant of 'Data.Iteratee.ListLike.take' that passes 'EOF's.
takePT ::
  (Monad m, Nullable s, LL.ListLike s el)
  => Int   -- ^ number of elements to consume
  -> Enumeratee s s m a
takePT n' iter
 | n' <= 0   = return iter
 | otherwise = Iteratee $ \od oc -> runIter iter (on_done od oc) (on_cont od oc)
  where
    on_done od oc x _ = runIter (drop n' >> return (return x)) od oc
    on_cont od oc k Nothing = if n' == 0 then od (liftI k) (Chunk mempty)
                                 else runIter (liftI (step n' k)) od oc
    on_cont od oc _ (Just e) = runIter (drop n' >> throwErr e) od oc
    step n k (Chunk str)
      | LL.null str        = liftI (step n k)
      | LL.length str <= n = takePT (n - LL.length str) $ k (Chunk str)
      | otherwise          = idone (k (Chunk s1)) (Chunk s2)
      where (s1, s2) = LL.splitAt n str
    step _n k stream       = idone (k stream) stream
{-# INLINE takePT #-}

-- | A variant of 'Data.Iteratee.ListLike.takeUpTo' that passes 'EOF's.
takeUpToPT :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Enumeratee s s m a
takeUpToPT i iter
 | i <= 0    = idone iter (Chunk empty)
 | otherwise = Iteratee $ \od oc ->
    runIter iter (onDone od oc) (onCont od oc)
  where
    onDone od oc x str      = runIter (idone (return x) str) od oc
    onCont od oc k Nothing  = if i == 0 then od (liftI k) (Chunk mempty)
                                 else runIter (liftI (step i k)) od oc
    onCont od oc _ (Just e) = runIter (throwErr e) od oc
    step n k (Chunk str)
      | LL.null str       = liftI (step n k)
      | LL.length str < n = takeUpToPT (n - LL.length str) $ k (Chunk str)
      | otherwise         =
         -- check to see if the inner iteratee has completed, and if so,
         -- grab any remaining stream to put it in the outer iteratee.
         -- the outer iteratee is always complete at this stage, although
         -- the inner may not be.
         let (s1, s2) = LL.splitAt n str
         in Iteratee $ \od' _ -> do
              res <- runIter (k (Chunk s1)) (\a s  -> return $ Left  (a, s))
                                            (\k' e -> return $ Right (k',e))
              case res of
                Left (a,Chunk s1') -> od' (return a)
                                          (Chunk $ s1' `LL.append` s2)
                Left  (a,s')       -> od' (idone a s') (Chunk s2)
                Right (k',e)       -> od' (icont k' e) (Chunk s2)
    step _ k stream       = idone (k stream) stream
{-# INLINE takeUpToPT #-}

-- | A variant of 'Data.Iteratee.ListLike.takeWhileE' that passes 'EOF's.
takeWhileEPT
 :: (LL.ListLike s el, NullPoint s)
 => (el -> Bool)
 -> Enumeratee s s m a
takeWhileEPT = breakEPT . (not .)
{-# INLINEABLE takeWhileEPT #-}

-- | A variant of 'Data.Iteratee.ListLike.mapStream' that passes 'EOF's.
mapStreamPT
  :: (LL.ListLike (s el) el
     ,LL.ListLike (s el') el'
     ,NullPoint (s el)
     ,LooseMap s el el')
  => (el -> el')
  -> Enumeratee (s el) (s el') m a
mapStreamPT f = mapChunksPT (lMap f)
{-# SPECIALIZE mapStreamPT :: (el -> el') -> Enumeratee [el] [el'] m a #-}

-- | A variant of 'Data.Iteratee.ListLike.rigidMapStream' that passes 'EOF's.
rigidMapStreamPT
  :: (LL.ListLike s el, NullPoint s)
  => (el -> el)
  -> Enumeratee s s m a
rigidMapStreamPT f = mapChunksPT (LL.rigidMap f)
{-# SPECIALIZE rigidMapStreamPT :: (el -> el) -> Enumeratee [el] [el] m a #-}
{-# SPECIALIZE rigidMapStreamPT :: (Word8 -> Word8) -> Enumeratee B.ByteString B.ByteString m a #-}

-- | A variant of 'Data.Iteratee.ListLike.filter' that passes 'EOF's.
filterPT
  :: (Monad m, Functor m, Nullable s, LL.ListLike s el)
  => (el -> Bool)
  -> Enumeratee s s m a
filterPT p = convStreamPT (LL.filter p <$> getChunk)
{-# INLINE filterPT #-}
