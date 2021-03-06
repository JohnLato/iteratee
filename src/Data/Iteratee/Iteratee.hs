{-# LANGUAGE KindSignatures
            ,RankNTypes
            ,BangPatterns
            ,FlexibleContexts
            ,NoMonomorphismRestriction
            ,ScopedTypeVariables
            ,TupleSections
            ,DeriveFunctor
            ,DeriveDataTypeable #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Iteratee (
  -- * Types
  EnumerateeHandler
  -- ** Error handling
  ,throwErr
  ,throwRecoverableErr
  ,throwRec
  ,checkErr
  -- ** Basic Iteratees
  ,identity
  ,skipToEof
  ,isStreamFinished
  ,isIterFinished
  -- ** Chunkwise Iteratees
  ,mapChunksM_
  ,foldChunksM
  ,getChunk
  ,getChunks
  -- ** Nested iteratee combinators
  ,mapChunks
  ,mapChunksM
  ,mapAccumChunks
  ,mapAccumChunksM
  ,convStream
  ,unfoldConvStream
  ,unfoldConvStreamCheck
  ,joinI
  ,joinIM
  -- ** Nested iteratee combinators with leftover handling
  ,bimapChunks
  ,bimapChunksM
  ,bimapAccumChunks
  ,bimapAccumChunksM
  -- * Enumerators
  ,Enumerator
  ,Enumeratee
  -- ** Basic enumerators
  ,enumChunk
  ,enumEof
  ,enumErr
  ,enumPure1Chunk
  ,enumChunkRemaining
  ,enumList
  ,enumCheckIfDone
  ,enumFromCallback
  ,enumFromCallbackCatch
  ,enumFromCallbackCatches
  -- ** Enumerator Combinators
  ,(>>>)
  ,eneeCheckIfDone
  ,eneeCheckIfDoneHandle
  ,eneeCheckIfDonePass
  ,eneeCheckIfDoneIgnore
  ,mergeEnums
  -- ** Enumeratee Combinators
  ,($=)
  ,(=$)
  ,(><>)
  ,(<><)
  -- ** Enumerator creation callbacks
  ,CBState (..)
  ,Callback
  -- * Misc.
  ,seek
  ,FileOffset
  -- ** Debugging utils
  ,traceEteeExc
  ,fuseEneeHandle
  -- * Classes
  ,module Data.Iteratee.Base
)
where

import Prelude hiding (head, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product)

import Data.Iteratee.IO.Base
import Data.Iteratee.Base
import qualified Data.ListLike as LL

import Control.Monad.Trans.Class
import Control.Monad ((<=<), liftM)
import Data.Monoid
import Data.Traversable as Tr

import Debug.Trace


-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Report and propagate an unrecoverable error.
--  Disregard the input first and then propagate the error.  This error
-- cannot be handled by 'enumFromCallbackCatch', although it can be cleared
-- by 'checkErr'.
throwErr :: IException e => e -> Iteratee s m a
throwErr e = ierr (throwErr e) e

-- |Report and propagate a recoverable error.  This error can be handled by
-- both 'enumFromCallbackCatch' and 'checkErr'.
throwRecoverableErr :: (IException e) => e -> Iteratee s m a -> Iteratee s m a
throwRecoverableErr e i = ierr i e

-- | A shorter name for 'throwRecoverableErr'
throwRec :: (IException e) => e -> Iteratee s m a -> Iteratee s m a
throwRec = throwRecoverableErr


-- |Check if an iteratee produces an error.
-- Returns @Right a@ if it completes without errors, otherwise
-- @Left SomeException@. 'checkErr' is useful for iteratees that may not
-- terminate, such as @Data.Iteratee.head@ with an empty stream.
checkErr :: Monad m => Iteratee s m a -> Iteratee s m (Either IterException a)
checkErr iter = runIter iter (idone . Right) oc oe
 where
  oc     = mapCont checkErr Right
  oe _ e = idone (Left e)

-- ------------------------------------------------------------------------
-- Parser combinators

-- |The identity iteratee.  Doesn't do any processing of input.
identity :: Iteratee s m ()
identity = idone ()

-- |Get the stream status in an iteratee.
--
-- The returned 'Stream s' value, if it exists, will be at @EOF@
-- if 'Nothing' is returned, there is a valid Chunk with at least one
-- element available.
isStreamFinished :: (Monad m, LL.ListLike s el) => Iteratee s m (Maybe (Stream s))
isStreamFinished = icontP check
  where
    check s@(Chunk xs)
        | LL.null xs = continueP check
        | otherwise  = ContDone Nothing s
    check NoData     = continueP check
    check s@(EOF _)  = ContDone (Just s) s
{-# INLINE isStreamFinished #-}

-- | Check if an iteratee is finished.  Returns True when the iteratee is
-- complete, returns False if the iteratee is in an error condition or expects
-- more data.
isIterFinished :: Iteratee s m a -> Bool
isIterFinished iter = runIter iter (const True) (const False) (\_ _ -> False)

-- |Skip the rest of the stream
skipToEof :: (Monad m) => Iteratee s m ()
skipToEof = icontP check
  where
    check (Chunk _) = continueP check
    check s         = ContDone () s


-- |Seek to a position in the stream
seek :: FileOffset -> Iteratee s m ()
seek o = throwRec (SeekException o) identity

-- | Map a monadic function over the chunks of the stream and ignore the
-- result.  Useful for creating efficient monadic iteratee consumers, e.g.
-- 
-- >  logger = mapChunksM_ (liftIO . putStrLn)
-- 
-- these can be efficiently run in parallel with other iteratees via
-- @Data.Iteratee.ListLike.zip@.
mapChunksM_ :: (Monad m)
  => (s -> m b)
  -> Iteratee s m ()
mapChunksM_ f = go
  where
    go = icont step
    step (Chunk xs) = f xs >> contMoreM go
    step NoData     = contMoreM go
    step s@(EOF _)  = contDoneM () s
{-# INLINE mapChunksM_ #-}

-- | A fold over chunks
foldChunksM :: (Monad m)
  => (a -> s -> m a)
  -> a
  -> Iteratee s m a
foldChunksM f = icont . go
  where
    go a (Chunk c) = f a c >>= \a' -> continue (go a')
    go a NoData    = continue (go a)
    go a e@(EOF{}) = contDoneM a e
{-# INLINE foldChunksM #-}

-- | Remove the current (possibly empty) chunk from the stream and return it.
getChunk :: (Monad m) => Iteratee s m s
getChunk = icontP step
 where
  step (Chunk xs)     = ContDone xs NoData
  step NoData         = continueP step
  step (EOF Nothing)  = continueErrP (EofException "getChunk") step
  step (EOF (Just e)) = continueErrP (wrapEnumExc e) step
{-# INLINE getChunk #-}

-- | Get a list of all chunks from the stream.
getChunks :: (Monad m) => Iteratee s m [s]
getChunks = icontP (step id)
 where
  step acc (Chunk xs) = continueP $ step $ acc . (xs:)
  step acc NoData     = continueP (step acc)
  step acc s@(EOF{})  = ContDone (acc []) s
{-# INLINE getChunks #-}

-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

type Enumeratee sFrom sTo (m :: * -> *) a =
  Iteratee sTo m a
  -> Iteratee sFrom m (Iteratee sTo m a)

-- The following pattern appears often in Enumeratee code
{-# INLINE eneeCheckIfDone #-}

-- | Utility function for creating enumeratees.  Typical usage is demonstrated
-- by the @breakE@ definition.
-- 
-- > breakE
-- >   :: (LL.ListLike s el, Monad m)
-- >   => (el -> Bool)
-- >   -> Enumeratee s s m a
-- > breakE cpred = eneeCheckIfDonePass (icont . step) CM.>=>
-- >                 \i' -> dropWhile (not . cpred) >> return i'
-- >  where
-- >   go = eneeCheckIfDonePass (icont . step)
-- >   step k (Chunk s)
-- >     | LL.null s = continue (step k)
-- >     | otherwise = case LL.break cpred s of
-- >         (str', tail')
-- >           | LL.null tail' ->
-- >               doContEtee go k str'
-- >                                -- if the inner iteratee completes before
-- >                                -- the predicate is met, elements still
-- >                                -- need to be dropped.
-- >           | otherwise -> k (Chunk str') >>= \ret ->
-- >                             contDoneM (wrapCont ret) (Chunk tail')
-- >   step k NoData       =  continue (step k)
-- >   step k stream@EOF{} =  contDoneM (icont k) stream
eneeCheckIfDone ::
 (Monad m) =>
  (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDone = eneeCheckIfDonePass

type EnumerateeHandler eli elo m a =
  Iteratee elo m (Iteratee eli m a)
  -> IterException
  -> Iteratee elo m (Iteratee eli m a)

fuseEneeHandle
  :: (Monad m)
  => EnumerateeHandler s1 s2 m a
  -> (Cont s1 m a -> Iteratee s2 m (Iteratee s1 m a))
  -> EnumerateeHandler s2 s3 m (Iteratee s1 m a)
  -> (Cont s2 m (Iteratee s1 m a) -> Iteratee s3 m (Iteratee s2 m (Iteratee s1 m a)))
  -> Enumeratee s3 s1 m a
fuseEneeHandle h1 k1 h2 k2 = eneeCheckIfDoneHandle hFuse kFuse
  where
    go = eneeCheckIfDoneHandle hFuse kFuse
    hFuse fusedRec exc = do
        rec1 <- fusedRec
        let hD a = return a
            hC k = innerC k
            hE i' e = do
                i21 <- h2 (return i') e
                runIter i21 return innerC innerE
        rec2 <- runIter (h1 (return rec1) exc) hD hC hE
        go rec2
    -- kFuse :: Cont s1 m a -> Iteratee s3 m (Iteratee s1 m a)
    kFuse k = runIter (k1 k) return innerC innerE
    innerC kS2 = push2 (k2 kS2)
    innerE i2rec exc = push2 . return $ h1 i2rec exc

    -- push2 :: Iteratee s3 m (Iteratee s2 m (Iteratee s1 m a)) -> Iteratee s3 m (Iteratee s1 m a)
    push2 i321 = do
        i2 <- i321
        runIter i2 return (icont . step) handleInner
          where
            handleInner rec2 exc = push2 $ h2 (return rec2) exc

            step k NoData = continue (step k)
            step k (Chunk xs_3) = do
                (i321', rest) <- enumChunkRemaining xs_3 (k2 k)
                runIter i321' (\i' -> runIter i'
                                        (\i1  -> contDoneM i1 rest)
                                        (\k'  -> contMoreM (push2 $ return $ icont k'))
                                        (\_ _ -> contMoreM (push2 i321'))
                                        )
                                 (\k'  -> contMoreM (push2 (icont k')))
                                 (\_ _ -> contMoreM (push2 i321'))
            step k (EOF mExc) = do
                let enum = maybe enumEof enumErr mExc
                i321' <- enum (k2 k)
                runIter i321' (\i' -> enum i' >>= \i'2 -> runIter i'2
                                  (\i1  -> contDoneM i1 (EOF Nothing))
                                  (\k'  -> contMoreM (push2 (k2 k')))
                                  (\rec exc -> contMoreM (push2 (return (h1 rec exc))))
                                  )
                               (\_       -> contMoreM (push2 i321'))
                               (\rec exc -> contMoreM (push2 (h2 rec exc)))

{-# INLINE[1] fuseEneeHandle #-}

-- the fusion workhorse.  Most enumeratees are defined in terms of
-- eneeCheckIfDoneHandle at some level.
{-# RULES
"fuseEneeHandle" forall h1 k1 h2 k2 i. eneeCheckIfDoneHandle h2 k2 (eneeCheckIfDoneHandle h1 k1 i)
                      = push (fuseEneeHandle h1 k1 h2 k2 i)
  #-}

-- | The same as eneeCheckIfDonePass, with one extra argument:
-- a handler which is used
-- to process any exceptions in a separate method.
eneeCheckIfDoneHandle
  :: forall m eli elo a. Monad m
  => EnumerateeHandler eli elo m a
  -> (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDoneHandle h fc inner = runIter (worker inner) return ckCont h
 where
  worker i     = runIter i onDone fc handle
  handle recInner e = throwRec e (return recInner)
  onDone x     = idone (idone x)
  ckCont k     = icont $ \str -> do
        res <- k str
        case res of
            ContDone a rest -> contDoneM a rest
            ContMore i'     -> contMoreM i'
            ContErr i' e    -> contMoreM (h i' e)
{-# NOINLINE eneeCheckIfDoneHandle #-}

-- | Create enumeratees that pass all errors through the outer iteratee.
eneeCheckIfDonePass
  :: Monad m
  => (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDonePass f = eneeCheckIfDoneHandle handler f
 where
  worker    = eneeCheckIfDoneHandle handler f
  handler i = ierr (i >>= worker)
{-# INLINE eneeCheckIfDonePass #-}

-- | Create an enumeratee that ignores all errors from the inner iteratee
eneeCheckIfDoneIgnore
  :: Monad m
  => (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDoneIgnore f = eneeCheckIfDoneHandle handler f
 where
  worker = eneeCheckIfDoneHandle handler f
  handler i _e = i >>= worker
{-# INLINE eneeCheckIfDoneIgnore #-}


-- | Convert one stream into another with the supplied mapping function.
-- This function operates on whole chunks at a time, contrasting to
-- @mapStream@ which operates on single elements.
-- 
-- 'mapChunks' is useful for creating high-performance iteratees, however
-- the entire input chunk will be consumed even if the inner iteratee doesn't
-- make use of all of it.
-- 
-- > unpacker :: Enumeratee B.ByteString [Word8] m a
-- > unpacker = mapChunks B.unpack
-- 
mapChunks :: (Monad m) => (s -> s') -> Enumeratee s s' m a
mapChunks f i = eneeCheckIfDonePass (icont . step) i
 where
  go = eneeCheckIfDonePass (icont . step)

  -- After running the inner iteratee continuation 'k', if the result is
  -- done, the rest of the output won't be used and can therefore be
  -- dropped here.  If the result isn't done, there shouldn't be any
  -- returned output, so again it can be dropped.
  step k (Chunk xs) = k (Chunk (f xs)) >>= \ret -> case ret of
                        ContMore i'      -> contMoreM (go i')
                        ContErr  i' e    -> contErrM (go i') e
                        ContDone a _str' -> contDoneM (idone a) NoData
  step k NoData     = continue (step k)

  step k s@(EOF Nothing) = contDoneM (icont k) s
  step k s@(EOF (Just e)) = k (EOF (Just e)) >>= \ret -> case ret of
          ContDone a _  -> contDoneM (return a) s
          ContMore i'   -> contMoreM (go i')
          ContErr i' e' -> contErrM (go i') e'
{-# INLINE[1] mapChunks #-}

{-# RULES
"joinI-collapse"         forall i.     joinI (push i) = i

"mapChunks/mapChunks"    forall f g i. mapChunks  f (mapChunks  g i) = push (mapChunks  (g . f) i)
"mapChunks/mapChunksM"   forall f g i. mapChunks  f (mapChunksM g i) = push (mapChunksM (g . f) i)
"mapChunksM/mapChunks"   forall f g i. mapChunksM f (mapChunks  g i) = push (mapChunksM (return g <=< f) i)
"mapChunksM/mapChunksM"  forall f g i. mapChunksM f (mapChunksM g i) = push (mapChunksM (g <=< f) i)
  #-}

-- This function exists to aid enumeratee fusion.  When fusing enumeratees,
-- an artificial stream level must be created, which will then be terminated by
-- joinI.  The 'joinI-collapse' rule removes the joinI/push pair, completely
-- removing the nested layer.
push :: Monad m => Iteratee s m a -> Iteratee s m (Iteratee s' m a)
push = liftM return
{-# INLINE[0] push #-}

-- | Convert a stream of @s@ to a stream of @s'@ using the supplied function.
mapChunksM
  :: forall m s s' a. (Monad m)
  => (s -> m s')
  -> Enumeratee s s' m a
mapChunksM f i = eneeCheckIfDonePass (icont . step) i
 where
  go = eneeCheckIfDonePass (icont . step)
  step :: (Stream s' -> m (ContReturn s' m a))-> Stream s -> m (ContReturn s m (Iteratee s' m a))
  step k (Chunk xs)   = f xs >>= doContEtee go k
  step k NoData       = continue (step k)
  step k s@(EOF Nothing) = contDoneM (icont k) s
  step k (EOF (Just e))  = k (EOF (Just e)) >>= \iret -> case iret of
      ContDone x _  -> contDoneM (return x) NoData
      ContMore i'   -> contMoreM (go i')
      ContErr i' e' -> contErrM (go i') e'
{-# INLINE[1] mapChunksM #-}

mapAccumChunks
  :: (Monad m)
  => (acc -> s -> (acc, s'))
  -> acc
  -> Enumeratee s s' m a
mapAccumChunks f = mapAccumChunksM (\acc -> return . f acc)
{-# INLINE mapAccumChunks #-}

mapAccumChunksM
  :: (Monad m)
  => (acc -> s -> m (acc, s'))
  -> acc
  -> Enumeratee s s' m a
mapAccumChunksM f acc0 i = go acc0 i
 where
  go acc = eneeCheckIfDonePass (icont . step acc)
  step !acc k (Chunk xs)   = f acc xs >>= \(acc',a') -> doContEtee (go acc') k a'
  step !acc k NoData       = contMoreM (go acc (icont k))
  step _acc k s@(EOF Nothing) = contDoneM (icont k) s
  step !acc k (EOF (Just e))  = k (EOF (Just e)) >>= \iret -> case iret of
      ContDone x _  -> contDoneM (return x) NoData
      ContMore i'   -> contMoreM (go acc i')
      ContErr i' e' -> contErrM (go acc i') e'
{-# INLINE[1] mapAccumChunksM #-}

bimapChunks
  :: forall m s s' a. (Monad m, Monoid s)
  => (s  -> s')
  -> (s -> s' -> s)
  -> Enumeratee s s' m a
bimapChunks f unF = bimapChunksM (return . f) ((return .) . unF)
{-# INLINE bimapChunks #-}

bimapChunksM
  :: forall m s s' a. (Monad m, Monoid s)
  => (s  -> m s')
  -> (s -> s' -> m s)
  -> Enumeratee s s' m a
bimapChunksM f unF i = go i
 where
  go = eneeCheckIfDonePass (icont . step)
  step :: (Stream s' -> m (ContReturn s' m a))-> Stream s -> m (ContReturn s m (Iteratee s' m a))
  step k (Chunk xs)   = f xs >>= doContEteeBi go k (unF xs)
  step k NoData       = continue (step k)
  step k s@(EOF Nothing) = contDoneM (icont k) s
  step k (EOF (Just e))  = k (EOF (Just e)) >>= \iret -> case iret of
      ContDone x (EOF exc) -> trace ("bimapChunksM: inner continuation returned EOF in contDone: " ++ show exc)
                            $ contDoneM (return x) NoData
      ContDone x c  -> Tr.mapM (unF mempty) c >>= contDoneM (return x)
      ContMore i'   -> contMoreM (go i')
      ContErr i' e' -> contErrM (go i') e'
{-# INLINE bimapChunksM #-}

bimapAccumChunks
  :: (Monad m)
  => (acc -> s -> (acc, s'))
  -> (acc -> s' -> s)
  -> acc
  -> Enumeratee s s' m a
bimapAccumChunks f unF =
  bimapAccumChunksM (\a s -> return $! f a s) (\a s' -> return $! unF a s')
{-# INLINE bimapAccumChunks #-}

bimapAccumChunksM
  :: (Monad m)
  => (acc -> s -> m (acc, s'))
  -> (acc -> s' -> m s)
  -> acc
  -> Enumeratee s s' m a
bimapAccumChunksM f unF acc0 i = go acc0 i
 where
  go acc = eneeCheckIfDonePass (icont . step acc)
  step !acc k (Chunk xs)   = f acc xs >>= \(acc',a') -> doContEteeBi (go acc') k (unF acc') a'
  step !acc k NoData       = contMoreM (go acc (icont k))
  step _acc k s@(EOF Nothing) = contDoneM (icont k) s
  step !acc k (EOF (Just e))  = k (EOF (Just e)) >>= \iret -> case iret of
      -- what if iret is EOF?  This is probably an error in the iteratee.  But
      -- we have a value, so we can continue working at least.
      ContDone x (EOF exc) -> trace ("bimapAccumChunksM: inner continuation returned EOF in contDone: " ++ show exc)
                            $ contDoneM (return x) NoData
      ContDone x c  -> Tr.mapM (unF acc) c >>= contDoneM (return x)
      ContMore i'   -> contMoreM (go acc i')
      ContErr i' e' -> contErrM (go acc i') e'
{-# INLINE[1] bimapAccumChunksM #-}

-- |Convert one stream into another, not necessarily in lockstep.
-- 
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- Iteratee s m s'.
convStream
  :: (Monad m, LL.ListLike s elo)
  => Iteratee s m s'
  -> Enumeratee s s' m a
convStream fi = eneeCheckIfDonePass check
  where
    go = eneeCheckIfDonePass check
    check k = isStreamFinished >>= maybe (step k) (hndl k)
    hndl k (EOF Nothing)  = idone (icont k)
    hndl k (EOF (Just e)) = ierr (check k) (wrapEnumExc e)
    hndl _ NoData         = error "iteratee: internal error in convStream"
    hndl _ (Chunk _)      = error "iteratee: internal error in convStream"
    step k = fi >>= lift . doContIteratee k . Chunk >>= go
{-# INLINE convStream #-}


-- |The most general stream converter.  Given a function to produce iteratee
-- transformers and an initial state, convert the stream using iteratees
-- generated by the function while continually updating the internal state.
unfoldConvStream ::
 (Monad m, LL.ListLike s elo) =>
  (acc -> Iteratee s m (acc, s'))
  -> acc
  -> Enumeratee s s' m a
unfoldConvStream fi acc0 = unfoldConvStreamCheck eneeCheckIfDonePass fi acc0
{-# INLINE unfoldConvStream #-}

unfoldConvStreamCheck
  :: (Monad m, LL.ListLike fromStr elo)
  => ((Cont toStr m a -> Iteratee fromStr m (Iteratee toStr m a))
      -> Enumeratee fromStr toStr m a
     )
  -> (acc -> Iteratee fromStr m (acc, toStr))
  -> acc
  -> Enumeratee fromStr toStr m a
unfoldConvStreamCheck checkDone f acc0 = checkDone (check acc0)
  where
    go acc = checkDone (check acc)
    check acc k  = isStreamFinished >>= maybe (step acc k) (hndl acc k)
    hndl _   k (EOF Nothing)  = idone (icont k)
    hndl acc k (EOF (Just e)) = ierr (check acc k) (wrapEnumExc e)
    hndl _   _ NoData         = error "iteratee: internal error in unfoldConvStreamCheck"
    hndl _   _ (Chunk _)      = error "iteratee: internal error in unfoldConvStreamCheck"
    step acc k = do
      (acc', s') <- f acc
      i' <- lift . doContIteratee k $ Chunk s'
      go acc' i'
{-# INLINE unfoldConvStreamCheck #-}

-- | Collapse a nested iteratee.  The inner iteratee is terminated by @EOF@.
--   Errors are propagated through the result.
-- 
--  The stream resumes from the point of the outer iteratee; any remaining
--  input in the inner iteratee will be lost.
-- 
--  Differs from 'Control.Monad.join' in that the inner iteratee is terminated,
--  and may have a different stream type than the result.
joinI :: forall m s s' a.
 (Monad m) =>
  Iteratee s m (Iteratee s' m a)
  -> Iteratee s m a
joinI i = i >>= lift . run
{-
joinI i = i >>= lift . tryRun >>= either (throwErr . wrapExc) idone
  where
  wrapExc e = let e' = toException e
              in case (fromException e', fromException e') of
                  (Just a, _) -> a
                  (_, Just b) -> wrapEnumExc b
                  _           -> error ("iteratee/joinI: internal exception error."
                                     ++ "\nPlease report this as a bug.")
-}
{-# INLINE[0] joinI #-}

-- | Lift an iteratee inside a monad to an iteratee.
--
-- > joinIM === Control.Monad.join . lift
joinIM :: (Monad m) => m (Iteratee s m a) -> Iteratee s m a
joinIM mIter = ireq mIter id


-- ------------------------------------------------------------------------
-- Enumerators
-- | Each enumerator takes an iteratee and returns an iteratee
-- 
-- an Enumerator is an iteratee transformer.
-- The enumerator normally stops when the stream is terminated
-- or when the iteratee moves to the done state, whichever comes first.
-- When to stop is of course up to the enumerator...

type Enumerator s m a = Iteratee s m a -> m (Iteratee s m a)

-- |Applies the iteratee to the given stream.  This wraps 'enumEof',
-- 'enumErr', and 'enumPure1Chunk', calling the appropriate enumerator
-- based upon 'Stream'.
enumChunk :: (Monad m) => Stream s -> Enumerator s m a
enumChunk (Chunk xs)     = enumPure1Chunk xs
enumChunk NoData         = return
enumChunk (EOF Nothing)  = enumEof
enumChunk (EOF (Just e)) = enumErr e
{-# INLINE enumChunk #-}

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee in the Done state.  It is an error
-- if the iteratee does not terminate on EOF.
enumEof :: (Monad m) => Enumerator s m a
enumEof iter = runIter iter idoneM onC ierrM
  where
    onC  k     = doContIteratee k (EOF Nothing)
{-# INLINE enumEof #-}

-- |Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error.
-- 
-- If the iteratee is already in an error state, the previous error is
-- preserved.
enumErr :: (EException e, Monad m) => e -> Enumerator s m a
enumErr e iter = runIter iter idoneM onCont ierrM
  where
    onCont  k  = doContIteratee k (EOF $ Just (toEnumException e))
{-# INLINE enumErr #-}


-- |The composition of two enumerators: essentially the functional composition
-- 
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >>> e2, e1 is executed first

(>>>) :: (Monad m) => Enumerator s m a -> Enumerator s m a -> Enumerator s m a
(e1 >>> e2) i =  e1 i >>= e2
  -- I think (>>>) is identical to (>=>)...

infixr 0 =$

-- | Combines an Enumeratee from @s@ to @s'@ and an Iteratee that
--  consumes @s'@ into an Iteratee which consumes @s@
(=$)
  :: (Monad m)
  => Enumeratee s s' m a
  -> Iteratee s' m a
  -> Iteratee s m a
(=$) = (.) joinI
{-# INLINE (=$) #-}

infixl 1 $=

-- | Combines Enumerator which produces stream of @s@ and @Enumeratee@
--  which transforms stream of @s@ to stream
--  of @s'@ to into Enumerator which produces stream of @s'@
($=)
  :: (Monad m)
  => (forall a. Enumerator s m a)
  -> Enumeratee s s' m b
  -> Enumerator s' m b
($=) enum enee = \iter -> enum (enee iter) >>= run
{-# INLINE ($=) #-}

-- | Enumeratee composition
-- Run the second enumeratee within the first.  In this example, stream2list
-- is run within the 'take 10', which is itself run within 'take 15', resulting
-- in 15 elements being consumed
-- 
-- >>> run =<< enumPure1Chunk [1..1000 :: Int] (joinI $ (I.take 15 ><> I.take 10) I.stream2list)
-- [1,2,3,4,5,6,7,8,9,10]
-- 
(><>) ::
 (Monad m)
  => (forall x . Enumeratee s1 s2 m x)
  -> Enumeratee s2 s3 m a
  -> Enumeratee s1 s3 m a
f ><> g = \i -> joinI (f (g i))
{-# INLINE (><>) #-}

-- | enumeratee composition with the arguments flipped, see '><>'
(<><) ::
 (Monad m)
  => Enumeratee s2 s3 m a
  -> (forall x. Enumeratee s1 s2 m x)
  -> Enumeratee s1 s3 m a
f <>< g = \i -> joinI (g (f i))
{-# INLINE (<><) #-}

-- | Combine enumeration over two streams.  The merging enumeratee would
-- typically be the result of 'Data.Iteratee.ListLike.merge' or
-- 'Data.Iteratee.ListLike.mergeByChunks' (see @merge@ for example).
mergeEnums :: 
  (Monad m)
  => Enumerator s1 m a                   -- ^ inner enumerator
  -> Enumerator s2 (Iteratee s1 m) a     -- ^ outer enumerator
  -> Enumeratee s2 s1 (Iteratee s1 m) a  -- ^ merging enumeratee
  -> Enumerator s1 m a
mergeEnums e1 e2 etee i = e1 $ e2 (joinI . etee $ ilift lift i) >>= run
{-# INLINE mergeEnums #-}

-- | The pure 1-chunk enumerator
-- 
-- It passes a given list of elements to the iteratee in one chunk
-- The enumerator performs no monadic action, but monadic effects embedded in
-- the iteratee can be performed.
enumPure1Chunk :: (Monad m) => s -> Enumerator s m a
enumPure1Chunk str = runIt
  where
    runIt i = runIter i idoneM onC onE
    onC k      = doContIteratee k (Chunk str)
    onE i e | isEofException e = runIt i
            | otherwise = ierrM i e
{-# INLINE enumPure1Chunk #-}

-- | A 1-chunk enumerator
--
-- It passes a given list of elements to the iteratee in one chunk
-- The enumerator performs no monadic action, but monadic effects embedded in
-- the iteratee can be performed.
--
-- Like @enumPure1Chunk@, but any leftover stream data is also returned.
-- If the returned leftover stream data is a Chunk, the iteratee will be in the
-- ContDone state.
enumChunkRemaining
  :: (Monad m)
  => s
  -> Iteratee s m a
  -> m (Iteratee s m a, Stream s)
enumChunkRemaining str iter = runIt iter
  where
    runIt i = runIter i onD onC onE
    onD a      = return (idone a, Chunk str)
    onC k      = k (Chunk str) >>= \res -> case res of
                    ContDone a str' -> return (idone a, str')
                    ContMore i'     -> return (i', NoData)
                    ContErr  i' e   -> return (ierr i' e, NoData)
    onE i err | isEofException err = runIt i
              | otherwise = return (ierr i err, Chunk str)
{-# INLINE enumChunkRemaining #-}

-- | Enumerate chunks from a list
-- 
enumList :: (Monad m) => [s] -> Enumerator s m a
enumList chunks = go chunks
 where
  go [] i  = return i
  go xs' i = runIter i idoneM (onCont xs') onErr
   where
    onCont (x:xs) k = k (Chunk x) >>= \res -> case res of
                        ContMore i'  -> go xs i'
                        ContDone a _ -> return (idone a)
                        ContErr i' e -> ierrM i' e
    onCont []     k = return $ icont k
    onErr iRes e | isEofException e = go xs' iRes
                 | otherwise = return $ throwRec e iRes
{-# INLINABLE enumList #-}

-- | Checks if an iteratee has finished.
-- 
-- This enumerator runs the iteratee, performing any monadic actions.
-- If the result is True, the returned iteratee is done.
enumCheckIfDone :: (Monad m) => Iteratee s m a -> m (Bool, Iteratee s m a)
enumCheckIfDone iter = runIter iter onDone onCont onErr
  where
    onDone x  = return (True, idone x)
    onCont k  = return (False, icont k)
    onErr i e = return (False, ierr i e)
{-# INLINE enumCheckIfDone #-}


-- |Create an enumerator from a callback function
enumFromCallback ::
 (Monad m) =>
  Callback st m s
  -> st
  -> Enumerator s m a
enumFromCallback c =
  enumFromCallbackCatch c eofHandler
{-# INLINE enumFromCallback #-}

-- | Indicate if a callback should be called again to produce more data.
--
-- In iteratee < 0.9, callbacks used Bool equivalent to
-- HasMore : True
-- Finished : False
data CBState st s = HasMore st s | Finished st s
    deriving (Eq, Show, Functor)

-- | The type of callback functions to create enumerators.
type Callback st m s = st -> m (CBState st s)

cbChunk :: CBState st s -> s
cbChunk (HasMore _ s)  = s
cbChunk (Finished _ s) = s

-- |Create an enumerator from a callback function with an exception handler.
-- The exception handler is called if an iteratee reports an exception.
--
-- If the callback may fail, those failures should be handled in the 'm' monad,
-- using e.g. 'ErrorT SomeException Identity', 'Control.Exception' functions
-- for 'IO', etc.
enumFromCallbackCatch
  :: forall e m s st a. (IException e, Monad m)
  => Callback st m s
  -> (e -> m (Maybe EnumException))
  -> st
  -> Enumerator s m a
enumFromCallbackCatch c handler = loop
  where
    loop st iter = runIter iter idoneM (onCont st) (onErr st)
    onCont st k  = c st >>= check k
    onErr st i e = case fromIterException e of
      Just e' -> handler e' >>=
                   maybe (loop st i)
                         (return . ierr i) . fmap wrapEnumExc
      Nothing -> return (ierr i e)
      
    doNext (HasMore st' _) = loop st'
    doNext (Finished _  _) = return
    check :: Cont s m a
             -> CBState st s
             -> m (Iteratee s m a)
    check k cbstate = k (Chunk $! cbChunk cbstate) >>= \res -> case res of
                               ContDone a _   -> return (idone a)
                               ContMore i'    -> doNext cbstate i'
                               ContErr  i' e' -> doNext cbstate (ierr i' e')
{-# INLINE enumFromCallbackCatch #-}

-- |Create an enumerator from a callback function with an exception handler.
-- The exception handler is called if an iteratee reports an exception.
--
-- If the callback may fail, those failures should be handled in the 'm' monad,
-- using e.g. 'ErrorT SomeException Identity', 'Control.Exception' functions
-- for 'IO', etc.
enumFromCallbackCatches
  :: forall m s st a. (Monad m)
  => Callback st m s
  -> [IHandler m]
  -> st
  -> Enumerator s m a
enumFromCallbackCatches c handlers = loop
  where
    loop st iter = runIter iter idoneM (onCont st) (onErr handlers st)
    onCont st k  = c st >>= check k
    onErr [] _ i e = return (ierr i e)
    onErr (IHandler handler:remHandlers) st i e = case fromIterException e of
      Just e' -> handler e' >>=
                   maybe (loop st i)
                         (return . ierr i) . fmap wrapEnumExc
      Nothing -> onErr remHandlers st i e

    doNext (HasMore st' _) = loop st'
    doNext (Finished _  _) = return
    check :: Cont s m a
             -> CBState st s
             -> m (Iteratee s m a)
    check k cbstate = k (Chunk $! cbChunk cbstate) >>= \res -> case res of
                               ContDone a _   -> return (idone a)
                               ContMore i'    -> doNext cbstate i'
                               ContErr  i' e' -> doNext cbstate (ierr i' e')
{-# INLINE enumFromCallbackCatches #-}

-- | trace exceptions through enumeratees
traceEteeExc :: String -> Enumeratee s1 s2 m a -> Enumeratee s1 s2 m a
traceEteeExc lbl etee inner = runIter outer onDone (oc outer) onOuterErr
  where
    outer = etee inner'
    inner' = runIter inner (od inner) (oc inner) (oe inner)

    onDone innerRet = runIter innerRet (od outer) (oc outer)
                                       (showRec "output inner done" outer)
    onOuterErr outerRec e = showRec "output outer" outer outerRec e

    od i _   = i
    oc i _   = i
    oe       = showRec "input iteratee"
    showRec lbl' i rec e = trace (lbl ++ " - " ++ lbl' ++ ": " ++ show e) $
                 runIter rec (const i) (const i)
                   (\_ e' -> trace (lbl ++ " - *** " ++ lbl' ++ " recovery!!: " ++ show e') i)
