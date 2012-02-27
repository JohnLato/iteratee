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

import           Control.Arrow ((***), first)
import           Control.Monad.Trans.Class
import           Control.Monad

import qualified Data.ByteString as B
import           Data.Monoid
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
mapChunksPT :: (NullPoint s, Monad m) => (s -> s') -> Enumeratee s s' m a
mapChunksPT f = go
 where
  step k (Chunk xs)     = eneeCheckIfDonePass (icont . step) . k . Chunk $ f xs
  step k str@(EOF mErr) = eneeCheckIfDonePass (icont . step) . k $ EOF mErr
{-# INLINE mapChunksPT #-}

-- | Convert a stream of @s@ to a stream of @s'@ using the supplied function.
-- 
-- A version of 'mapChunksM' that sends 'EOF's to the inner iteratee.
mapChunksMPT
  :: (Monad m, NullPoint s, Nullable s)
  => (s -> m s')
  -> Enumeratee s s' m a
mapChunksMPT f = go
 where
  step k (Chunk xs)     = lift (f xs) >>=
                          eneeCheckIfDonePass (icont . step) . k . Chunk
  step k str@(EOF mErr) = eneeCheckIfDonePass (icont . step) . k $ EOF mErr
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
    check k = isStreamFinished >>= maybe (step k)
                  (\e -> case fromException e of
                    Just EofException -> lift (k (EOF Nothing))  >>= go . fst
                    Nothing           -> lift (k (EOF (Just e))) >>= go . fst)
    step k = fi >>= lift . k . Chunk >>= go . fst
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
    check acc k = isStreamFinished >>= maybe (step acc k)
                      (\e -> case fromException e of
                        Just EofException -> lift (k (EOF Nothing))
                                              >>= go acc . fst
                        Nothing -> lift (k (EOF (Just e)))
                                     >>= go acc . fst )
    step acc k = f acc
                   >>= \(acc',s') -> lift (k (Chunk s'))
                     >>= go acc' . fst
{-# INLINABLE unfoldConvStreamPT #-}

-- | A version of 'unfoldConvStreamCheck' that sends 'EOF's
-- to the inner iteratee.
unfoldConvStreamCheckPT
  :: (Monad m, Nullable elo)
  => ((Cont eli m a -> Iteratee elo m (Iteratee eli m a))
      -> Enumeratee elo eli m a
     )
  -> (acc -> Iteratee elo m (acc, eli))
  -> acc
  -> Enumeratee elo eli m a
unfoldConvStreamCheckPT checkDone f acc0 = go acc0
  where
    go acc = checkDone (check acc)
    check acc k = isStreamFinished >>= maybe (step acc k)
                      (\e -> case fromException e of
                        Just EofException -> lift (k (EOF Nothing))
                                              >>= go acc . fst
                        Nothing -> lift (k (EOF (Just e))) >>= go acc . fst )
    step acc k = do
      (acc',s') <- f acc
      lift (k (Chunk s')) >>= go acc' . fst
{-# INLINABLE unfoldConvStreamCheckPT #-}

-- -------------------------------------
-- ListLike variants

-- | A variant of 'Data.Iteratee.ListLike.breakE' that passes 'EOF's.
breakEPT
  :: (LL.ListLike s el, NullPoint s, Monad m)
  => (el -> Bool)
  -> Enumeratee s s m a
breakEPT cpred = go
 where
  go = eneeCheckIfDonePass (icont . step)
  step k s'@(Chunk s)
      | LL.null s  = return (icont (step k), s')
      | otherwise  = case LL.break cpred s of
        (str', tail')
          | LL.null tail' -> (go *** const mempty) <$> k (Chunk str')
          | otherwise     -> (idone *** const (Chunk tail')) <$> k (Chunk str')
  step k stream           =  (idone *** const stream) <$> k stream
{-# INLINE breakEPT #-}

-- | A variant of 'Data.Iteratee.ListLike.take' that passes 'EOF's.
takePT ::
  (Monad m, Nullable s, LL.ListLike s el)
  => Int   -- ^ number of elements to consume
  -> Enumeratee s s m a
takePT n' iter
  | n' <= 0   = return iter
  | otherwise = runIter iter onDone onCont onErr onReq
 where
  onDone x = drop n' >> idone (idone x)
  onCont k = if n' == 0 then idone (icont k)
                else icont (step n' k)
  onErr i  = ierr (takePT n' i)
  onReq mb doB = ireq mb (takePT n' . doB)

  step n k c@(Chunk str)
      | LL.null str        = return (icont (step n k), c)
      | LL.length str <= n = (takePT (n - LL.length str) *** const mempty)
                             <$> k (Chunk str)
      | otherwise          = (idone *** const (Chunk s2)) <$> k (Chunk s1)
      where (s1, s2) = LL.splitAt n str
  step _n k stream       = (idone *** const stream) <$> k stream
{-# INLINE takePT #-}

-- | A variant of 'Data.Iteratee.ListLike.takeUpTo' that passes 'EOF's.
takeUpToPT :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Enumeratee s s m a
takeUpToPT i iter
 | i <= 0    = idone iter
 | otherwise = runIter iter onDone onCont onErr onReq
  where
    onDone x = idone (idone x)
    onCont k = if i == 0 then idone (icont k)
                         else icont (step i k)
    onErr i' = ierr (takeUpToPT i i')
    onReq mb doB = ireq mb (takeUpToPT i . doB)

    step n k c@(Chunk str)
      | LL.null str       = return (icont (step n k), c)
      | LL.length str < n = first (takeUpToPT (n - LL.length str))
                            <$> k (Chunk str)
      | otherwise         = do
         -- check to see if the inner iteratee has completed, and if so,
         -- grab any remaining stream to put it in the outer iteratee.
         -- the outer iteratee is always complete at this stage, although
         -- the inner may not be.
         let (s1, s2) = LL.splitAt n str
         (iter', preStr) <- k (Chunk s1)
         case preStr of
              (Chunk preC)
                | LL.null preC -> return (idone iter', Chunk s2)
                | otherwise    -> return (idone iter'
                                     , Chunk $ preC `LL.append` s2)
              -- this case shouldn't ever happen, except possibly
              -- with broken iteratees
              _                -> return (idone iter', preStr)
    step _ k stream       = (idone *** const stream) <$> k stream
{-# INLINE takeUpToPT #-}

-- | A variant of 'Data.Iteratee.ListLike.takeWhileE' that passes 'EOF's.
takeWhileEPT
 :: (LL.ListLike s el, NullPoint s, Monad m)
 => (el -> Bool)
 -> Enumeratee s s m a
takeWhileEPT = breakEPT . (not .)
{-# INLINEABLE takeWhileEPT #-}

-- | A variant of 'Data.Iteratee.ListLike.mapStream' that passes 'EOF's.
mapStreamPT
  :: (LL.ListLike (s el) el
     ,LL.ListLike (s el') el'
     ,NullPoint (s el)
     ,Monad m
     ,LooseMap s el el')
  => (el -> el')
  -> Enumeratee (s el) (s el') m a
mapStreamPT f = mapChunksPT (lMap f)
{-# SPECIALIZE mapStreamPT :: Monad m => (el -> el') -> Enumeratee [el] [el'] m a #-}

-- | A variant of 'Data.Iteratee.ListLike.rigidMapStream' that passes 'EOF's.
rigidMapStreamPT
  :: (LL.ListLike s el, Monad m, NullPoint s)
  => (el -> el)
  -> Enumeratee s s m a
rigidMapStreamPT f = mapChunksPT (LL.rigidMap f)
{-# SPECIALIZE rigidMapStreamPT :: Monad m => (el -> el) -> Enumeratee [el] [el] m a #-}
{-# SPECIALIZE rigidMapStreamPT :: Monad m => (Word8 -> Word8) -> Enumeratee B.ByteString B.ByteString m a #-}

-- | A variant of 'Data.Iteratee.ListLike.filter' that passes 'EOF's.
filterPT
  :: (Monad m, Nullable s, LL.ListLike s el)
  => (el -> Bool)
  -> Enumeratee s s m a
filterPT p = convStreamPT (LL.filter p <$> getChunk)
{-# INLINE filterPT #-}
