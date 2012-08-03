{-# LANGUAGE KindSignatures
            ,RankNTypes
            ,FlexibleContexts
            ,ScopedTypeVariables
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
  -- ** Chunkwise Iteratees
  ,mapChunksM_
  ,foldChunksM
  ,getChunk
  ,getChunks
  -- ** Nested iteratee combinators
  ,mapChunks
  ,mapChunksM
  ,convStream
  ,unfoldConvStream
  ,joinI
  ,joinIM
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
  -- * Classes
  ,module Data.Iteratee.Base
)
where

import Prelude hiding (head, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product)

import Data.Iteratee.IO.Base
import Data.Iteratee.Base
import qualified Data.ListLike as LL

import Control.Monad.Trans.Class
import Data.Typeable

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
  step (EOF Nothing)  = continueErrP EofException step
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
-- >   :: (Monad m, LL.ListLike s el)
-- >   => (el -> Bool)
-- >   -> Enumeratee s s m a
-- > breakE cpred = eneeCheckIfDone (icont . step)
-- >  where
-- >  step k (Chunk s)
-- >    | LL.null s = return (continue (step k))
-- >    | otherwise = case LL.break cpred s of
-- >         (str', tail')
-- >           | LL.null tail' -> do
-- >               (i', _) <- k (Chunk str')
-- >               return (go i' <* dropWhile (not . cpred), Chunk tail')
-- >                                -- if the inner iteratee completes before
-- >                                -- the predicate is met, elements still
-- >                                -- need to be dropped.
-- >           | otherwise -> (idone *** const (Chunk tail')) `liftM` k (Chunk str')
-- >   step k NoData       =  return (continue (step k))
-- >   step k stream       =  return (idone (icont k), stream)
-- 
eneeCheckIfDone ::
 (Monad m) =>
  (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDone = eneeCheckIfDonePass

type EnumerateeHandler eli elo m a =
  Iteratee elo m (Iteratee eli m a)
  -> IterException
  -> Iteratee elo m (Iteratee eli m a)

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
{-# INLINABLE eneeCheckIfDoneHandle #-}

-- | Create enumeratees that pass all errors through the outer iteratee.
eneeCheckIfDonePass
  :: Monad m
  => (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDonePass f = worker
 where
  worker    = eneeCheckIfDoneHandle handler f
  handler i = ierr (i >>= worker)
{-# INLINABLE eneeCheckIfDonePass #-}

-- | Create an enumeratee that ignores all errors from the inner iteratee
eneeCheckIfDoneIgnore
  :: Monad m
  => (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDoneIgnore f = worker
 where
  worker = eneeCheckIfDoneHandle handler f
  handler i _e = i >>= worker
{-# INLINABLE eneeCheckIfDoneIgnore #-}


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
mapChunks f = go
 where
  go = eneeCheckIfDonePass (icont . step)

  -- After running the inner iteratee continuation 'k', if the result is
  -- done, the rest of the output won't be used and can therefore be
  -- dropped here.  If the result isn't done, there shouldn't be any
  -- returned output, so again it can be dropped.
  step k (Chunk xs) = k (Chunk (f xs)) >>= \ret -> return $ case ret of
                        ContMore i       -> ContMore (go i)
                        ContErr  i e     -> ContErr (go i) e
                        ContDone a _str' -> ContDone (idone a) NoData
  step k NoData     = contMoreM (go (icont k))

  step k s@EOF{}    = contDoneM (icont k) s
{-# INLINE mapChunks #-}

-- | Convert a stream of @s@ to a stream of @s'@ using the supplied function.
mapChunksM
  :: forall m s s' a. (Monad m)
  => (s -> m s')
  -> Enumeratee s s' m a
mapChunksM f = go
 where
  go = eneeCheckIfDonePass (icont . step)
  step :: (Stream s' -> m (ContReturn s' m a))-> Stream s -> m (ContReturn s m (Iteratee s' m a))
  step k (Chunk xs)   = f xs >>= doContEtee go k
  step k NoData       = contMoreM (go (icont k))
  step k s@(EOF Nothing) = contDoneM (icont k) s
  step k (EOF (Just e))  = k (EOF (Just e)) >>= \iret -> case iret of
      ContDone x _  -> contDoneM (return x) NoData
      ContMore i'   -> contMoreM (go i')
      ContErr i' e' -> contErrM (go i') e'
{-# INLINE mapChunksM #-}

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
convStream fi = go
  where
    go = eneeCheckIfDonePass check
    check k = isStreamFinished >>= maybe (step k) (hndl k)
    hndl k (EOF Nothing)  = idone (icont k)
    hndl k (EOF (Just e)) = ierr (check k) (wrapEnumExc e)
    hndl _ NoData         = error "iteratee: internal error in convStream"
    hndl _ (Chunk _)      = error "iteratee: internal error in convStream"
    step k = fi >>= lift . doContIteratee k . Chunk >>= go
{-# INLINABLE convStream #-}


-- |The most general stream converter.  Given a function to produce iteratee
-- transformers and an initial state, convert the stream using iteratees
-- generated by the function while continually updating the internal state.
unfoldConvStream ::
 (Monad m, LL.ListLike s elo) =>
  (acc -> Iteratee s m (acc, s'))
  -> acc
  -> Enumeratee s s' m a
unfoldConvStream fi acc0 = unfoldConvStreamCheck eneeCheckIfDonePass fi acc0
{-# INLINABLE unfoldConvStream #-}

unfoldConvStreamCheck
  :: (Monad m, LL.ListLike fromStr elo)
  => ((Cont toStr m a -> Iteratee fromStr m (Iteratee toStr m a))
      -> Enumeratee fromStr toStr m a
     )
  -> (acc -> Iteratee fromStr m (acc, toStr))
  -> acc
  -> Enumeratee fromStr toStr m a
unfoldConvStreamCheck checkDone f acc0 = go acc0
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
{-# INLINABLE unfoldConvStreamCheck #-}

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
{-
joinI i = i >>= lift . tryRun >>= either (throwErr . wrapExc) idone
  where
 -- TODO: either use this def. or toss it.
  -}
joinI i = runIter i onDone onCont onErr
 where
  onDone i'   = ireq (tryRun i') (either (throwErr . wrapExc) return)
  onCont k    = icont $ \str -> doCont k str
                    (\i' str' -> tryRun i' >>= either (onExc . wrapExc)
                                                      (`contDoneM` str') )
                    (contMoreM . joinI)
                    (contErrM  . joinI)

  onErr i' e  = throwRec e (joinI i')
  onExc e   = contErrM (throwErr e) e
  wrapExc e = let e' = toException e
              in case (fromException e', fromException e') of
                  (Just a, _) -> a
                  (_, Just b) -> wrapEnumExc b
                  _           -> error ("iteratee/joinI: internal exception error."
                                     ++ "\nPlease report this as a bug.")
{-# INLINE joinI #-}

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

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee in the Done state.  It is an error
-- if the iteratee does not terminate on EOF.
enumEof :: (Monad m) => Enumerator s m a
enumEof iter = runIter iter idoneM onC ierrM
  where
    onC  k     = doContIteratee k (EOF Nothing)

-- |Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error.
-- 
-- If the iteratee is already in an error state, the previous error is
-- preserved.
enumErr :: (EException e, Monad m) => e -> Enumerator s m a
enumErr e iter = runIter iter idoneM onCont ierrM
  where
    onCont  k  = doContIteratee k (EOF $ Just (toEnumException e))


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

infixl 1 $=

-- | Combines Enumerator which produces stream of @s@ and @Enumeratee@
--  which transforms stream of @s@ to stream
--  of @s'@ to into Enumerator which produces stream of @s'@
($=)
  :: (Monad m)
  => (forall a. Enumerator s m a)
  -> Enumeratee s s' m b
  -> Enumerator s' m b
($=) enum enee iter = enum (enee iter) >>= run


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
f ><> g = joinI . f . g

-- | enumeratee composition with the arguments flipped, see '><>'
(<><) ::
 (Monad m)
  => Enumeratee s2 s3 m a
  -> (forall x. Enumeratee s1 s2 m x)
  -> Enumeratee s1 s3 m a
f <>< g = joinI . g . f

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
enumPure1Chunk str iter = runIter iter idoneM onC ierrM
  where
    onC k      = doContIteratee k (Chunk str)


-- | A 1-chunk enumerator
--
-- It passes a given list of elements to the iteratee in one chunk
-- The enumerator performs no monadic action, but monadic effects embedded in
-- the iteratee can be performed.
--
-- Like @enumPure1Chunk@, but any leftover stream data is also returned.
enumChunkRemaining
  :: (Monad m)
  => s
  -> Iteratee s m a
  -> m (Iteratee s m a, Stream s)
enumChunkRemaining str iter = runIter iter onD onC onE
  where
    onD a      = return (idone a, Chunk str)
    onC k      = k (Chunk str) >>= \res -> case res of
                    ContDone a str' -> return (idone a, str')
                    ContMore i'     -> return (i', NoData)
                    ContErr  i' e   -> return (ierr i' e, NoData)
    onE i err  = return (ierr i err, Chunk str)

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
    onErr iRes e    = return $ throwRec e iRes
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
  enumFromCallbackCatch c (\NotAnException -> return Nothing)

-- Dummy exception to catch in enumFromCallback
-- This never gets thrown, but it lets us
-- share plumbing
data NotAnException = NotAnException
 deriving (Show, Typeable)

instance Exception NotAnException where
instance IException NotAnException where

-- | Indicate if a callback should be called again to produce more data.
--
-- In iteratee < 0.9, callbacks used Bool equivalent to
-- HasMore : True
-- Finished : False
data CBState = HasMore | Finished deriving (Eq, Show, Ord, Enum)

-- | The type of callback functions to create enumerators.
type Callback st m s = st -> m ((CBState, st), s)

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
      
    doNext (HasMore, st') = loop st'
    doNext (Finished,_)   = return
    check :: Cont s m a
             -> ((CBState, st), s)
             -> m (Iteratee s m a)
    check k (cbstate, s) = k (Chunk s) >>= \res -> case res of
                               ContDone a _   -> return (idone a)
                               ContMore i'    -> doNext cbstate i'
                               ContErr  i' e' -> doNext cbstate (ierr i' e')
{-# INLINE enumFromCallbackCatch #-}


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
