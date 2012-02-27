{-# LANGUAGE KindSignatures
            ,RankNTypes
            ,FlexibleContexts
            ,ScopedTypeVariables
            ,DeriveDataTypeable #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Iteratee (
  -- * Types
  Cont
  ,EnumerateeHandler
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
  -- * Classes
  ,module Data.Iteratee.Base
)
where

import Prelude hiding (head, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product)

import Data.Iteratee.IO.Base
import Data.Iteratee.Base

import Control.Arrow (first, (***))
import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Data.Maybe
import Data.Typeable

-- exception helpers
excDivergent :: SomeException
excDivergent = toException DivergentException

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Report and propagate an unrecoverable error.
--  Disregard the input first and then propagate the error.  This error
-- cannot be handled by 'enumFromCallbackCatch', although it can be cleared
-- by 'checkErr'.
throwErr :: SomeException -> Iteratee s m a
throwErr e = ierr (throwErr e) e

-- |Report and propagate a recoverable error.  This error can be handled by
-- both 'enumFromCallbackCatch' and 'checkErr'.
throwRecoverableErr :: (Exception e) => e -> Iteratee s m a -> Iteratee s m a
throwRecoverableErr e i = ierr i (toException e)

-- | A shorter name for 'throwRecoverableErr'
throwRec :: (Exception e) => e -> Iteratee s m a -> Iteratee s m a
throwRec = throwRecoverableErr


-- |Check if an iteratee produces an error.
-- Returns @Right a@ if it completes without errors, otherwise
-- @Left SomeException@. 'checkErr' is useful for iteratees that may not
-- terminate, such as @Data.Iteratee.head@ with an empty stream.
checkErr :: Monad m => Iteratee s m a -> Iteratee s m (Either SomeException a)
checkErr iter = runIter iter (idone . Right) oc oe oR
 where
  oc k   = icont (liftM (first checkErr) . k)
  oe _ e = idone (Left e)
  oR mb doB = ireq mb (checkErr . doB)

-- ------------------------------------------------------------------------
-- Parser combinators

-- |The identity iteratee.  Doesn't do any processing of input.
identity :: Iteratee s m ()
identity = idone ()

-- |Get the stream status of an iteratee.
isStreamFinished :: (Nullable s, Monad m) => Iteratee s m (Maybe SomeException)
isStreamFinished = icontP check
  where
    check s@(Chunk xs)
      | nullC xs  = (isStreamFinished, s)
      | otherwise = (idone Nothing, s)
    check s@(EOF e) = (idone (Just $ fromMaybe (toException EofException) e), s)
{-# INLINE isStreamFinished #-}


-- |Skip the rest of the stream
skipToEof :: (NullPoint s, Monad m) => Iteratee s m ()
skipToEof = icontP check
  where
    check (Chunk _) = (skipToEof, Chunk empty)
    check s         = (idone (), s)


-- |Seek to a position in the stream
seek :: (NullPoint s) => FileOffset -> Iteratee s m ()
seek o = throwRec (SeekException o) identity

-- | Map a monadic function over the chunks of the stream and ignore the
-- result.  Useful for creating efficient monadic iteratee consumers, e.g.
-- 
-- >  logger = mapChunksM_ (liftIO . putStrLn)
-- 
-- these can be efficiently run in parallel with other iteratees via
-- @Data.Iteratee.ListLike.zip@.
mapChunksM_ :: (Monad m, Nullable s, NullPoint s)
  => (s -> m b)
  -> Iteratee s m ()
mapChunksM_ f = icont step
  where
    step s@(Chunk xs)
      | nullC xs   = return (icont step, s)
      | otherwise  = f xs >> return (icont step, Chunk empty)
    step s@(EOF _) = return (idone (), s)
{-# INLINE mapChunksM_ #-}

-- | A fold over chunks
foldChunksM :: (Monad m, Nullable s, NullPoint s)
  => (a -> s -> m a)
  -> a
  -> Iteratee s m a
foldChunksM f = icont . go
  where
    go a (Chunk c) = f a c >>= \a' -> return (icont (go a'), Chunk empty)
    go a e = return (idone a, e)
{-# INLINE foldChunksM #-}

-- | Get the current chunk from the stream.
getChunk :: (Monad m, Nullable s, NullPoint s) => Iteratee s m s
getChunk = icontP step
 where
  step s@(Chunk xs)
    | nullC xs  = (icontP step, s)
    | otherwise = (idone xs, Chunk empty)
  step s@(EOF Nothing)  = (throwRec EofException getChunk, s)
  step s@(EOF (Just e)) = (throwRec e getChunk, s)
{-# INLINE getChunk #-}

-- | Get a list of all chunks from the stream.
getChunks :: (Monad m, Nullable s, NullPoint s) => Iteratee s m [s]
getChunks = icontP (step id)
 where
  step acc s@(Chunk xs)
    | nullC xs    = (icontP (step acc), s)
    | otherwise   = (icontP (step $ acc . (xs:)), s)
  step acc stream = (idone (acc []), stream)
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
-- >   :: (Monad m, LL.ListLike s el, NullPoint s)
-- >   => (el -> Bool)
-- >   -> Enumeratee s s m a
-- > breakE cpred = eneeCheckIfDone (icont . step)
-- >  where
-- >   step k (Chunk s)
-- >       | LL.null s  = icont (step k)
-- >       | otherwise  = case LL.break cpred s of
-- >         (str', tail')
-- >           | LL.null tail' -> eneeCheckIfDone (icont . step) . k $ Chunk str'
-- >           | otherwise     -> idone (k $ Chunk str') (Chunk tail')
-- >   step k stream           =  idone (k stream) stream
-- 
eneeCheckIfDone ::
 (Monad m, NullPoint elo) =>
  (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDone = eneeCheckIfDonePass

-- | The continuation type of an incomplete iteratee
type Cont s m a = Stream s -> m (Iteratee s m a, Stream s)

type EnumerateeHandler eli elo m a =
  Iteratee eli m a
  -> SomeException
  -> Iteratee elo m (Iteratee eli m a)

-- | The same as eneeCheckIfDonePass, with one extra argument:
-- a handler which is used
-- to process any exceptions in a separate method.
eneeCheckIfDoneHandle
  :: (NullPoint elo)
  => EnumerateeHandler eli elo m a
  -> (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDoneHandle h fc inner = runIter inner onDone fc h onReq
 where
  onDone x     = idone (idone x)
  onReq mb doB = ireq mb (eneeCheckIfDoneHandle h fc . doB)
{-# INLINABLE eneeCheckIfDoneHandle #-}

-- | Create enumeratees that pass all errors through the outer iteratee.
eneeCheckIfDonePass
  :: (NullPoint elo)
  => (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDonePass f = eneeCheckIfDoneHandle handler f
 where
  handler i = ierr (eneeCheckIfDonePass f i)
{-# INLINABLE eneeCheckIfDonePass #-}

-- | Create an enumeratee that ignores all errors from the inner iteratee
eneeCheckIfDoneIgnore
  :: (NullPoint elo)
  => (Cont eli m a -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDoneIgnore f = eneeCheckIfDoneHandle handler f
 where
  handler i e = eneeCheckIfDoneIgnore f i
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
mapChunks :: (Monad m, NullPoint s) => (s -> s') -> Enumeratee s s' m a
mapChunks f = go
 where
  go = eneeCheckIfDonePass (icont . step)
  step k (Chunk xs) = k (Chunk (f xs)) >>= \(i',_) ->
                        return (go i', Chunk empty)
  step k (EOF mErr) = (idone *** const (EOF mErr)) `liftM` k (EOF mErr)
{-# INLINE mapChunks #-}

-- | Convert a stream of @s@ to a stream of @s'@ using the supplied function.
mapChunksM
  :: (Monad m, NullPoint s, Nullable s)
  => (s -> m s')
  -> Enumeratee s s' m a
mapChunksM f = go
 where
  go = eneeCheckIfDonePass (icont . step)
  step k (Chunk xs) = f xs >>= k . Chunk >>= \(i', _str) ->
                            return (go i', Chunk empty)
  step k (EOF mErr) = (idone *** const (EOF mErr)) `liftM` k (EOF mErr)
{-# INLINE mapChunksM #-}

-- |Convert one stream into another, not necessarily in lockstep.
-- 
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- Iteratee s m s'.
convStream :: forall s s' m a.
 (Monad m, Nullable s) =>
  Iteratee s m s'
  -> Enumeratee s s' m a
convStream fi = go
  where
    go = eneeCheckIfDonePass check
    check k = isStreamFinished >>= maybe (step k) (hndl k)
    hndl k e = case fromException e of
      Just EofException -> idone (icont k)
      _                 -> ierr (step k) e
    step k = fi >>= lift . k . Chunk >>= go . fst
{-# INLINABLE convStream #-}


-- |The most general stream converter.  Given a function to produce iteratee
-- transformers and an initial state, convert the stream using iteratees
-- generated by the function while continually updating the internal state.
unfoldConvStream ::
 (Monad m, Nullable s) =>
  (acc -> Iteratee s m (acc, s'))
  -> acc
  -> Enumeratee s s' m a
unfoldConvStream fi acc0 = go acc0
  where
    go acc = eneeCheckIfDonePass (check acc)
    check acc k  = isStreamFinished >>= maybe (step acc k) (hndl acc k)
    hndl acc k e = case fromException e of
      Just EofException -> idone (icont k)
      _                 -> ierr (step acc k) e
    step acc k = do
      (acc', s') <- fi acc
      (i', _)    <- lift . k $ Chunk s'
      go acc' i'
{-# INLINABLE unfoldConvStream #-}

unfoldConvStreamCheck
  :: (Monad m, Nullable elo)
  => ((Cont eli m a -> Iteratee elo m (Iteratee eli m a))
      -> Enumeratee elo eli m a
     )
  -> (acc -> Iteratee elo m (acc, eli))
  -> acc
  -> Enumeratee elo eli m a
unfoldConvStreamCheck checkDone f acc0 = go acc0
  where
    go acc = checkDone (check acc)
    check acc k  = isStreamFinished >>= maybe (step acc k) (hndl acc k)
    hndl acc k e = case fromException e of
      Just EofException -> idone (icont k)
      _                 -> ierr (step acc k) e
    step acc k = do
      (acc', s') <- f acc
      (i', _)    <- lift . k $ Chunk s'
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
joinI ::
 (Monad m, Nullable s) =>
  Iteratee s m (Iteratee s' m a)
  -> Iteratee s m a
joinI i = runIter i onDone onCont onErr onR
 where
  onDone i'   = ireq (tryRun i') (either throwErr return)
  onCont k    = icont $ \str -> first joinI `liftM` k str
  onErr i' e  = throwRec e (joinI i')
  onR mb doB  = lift mb >>= joinI . doB
{-# INLINE joinI #-}

-- | Lift an iteratee inside a monad to an iteratee.
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
enumChunk (EOF Nothing)  = enumEof
enumChunk (EOF (Just e)) = enumErr e

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee in the Done state.  It is an error
-- if the iteratee does not terminate on EOF.
enumEof :: (Monad m) => Enumerator s m a
enumEof iter = runIter iter idoneM onC ierrM onR
  where
    onC  k     = k (EOF Nothing) >>= \(i,_) -> runIter i idoneM onC' ierrM onR
    onC' _k    = return $ throwErr excDivergent
    onR mb doB = mb >>= enumEof . doB

-- |Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error.
-- 
-- If the iteratee is already in an error state, the previous error is
-- preserved.
enumErr :: (Exception e, Monad m) => e -> Enumerator s m a
enumErr e iter = runIter iter idoneM onCont ierrM onR
  where
    onCont  k  = do
      (i',_) <- k . EOF . Just $ toException e 
      runIter i' idoneM onCont' ierrM onR
    onCont' _  = return $ throwErr excDivergent
    onR mb doB = mb >>= enumErr e . doB


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
  :: (Nullable s, Nullable s', Monad m)
  => Enumeratee s s' m a
  -> Iteratee s' m a
  -> Iteratee s m a
(=$) = (.) joinI

infixl 1 $=

-- | Combines Enumerator which produces stream of @s@ and @Enumeratee@
--  which transforms stream of @s@ to stream
--  of @s'@ to into Enumerator which produces stream of @s'@
($=)
  :: (Nullable s, Nullable s', Monad m)
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
 (Nullable s1, Monad m)
  => (forall x . Enumeratee s1 s2 m x)
  -> Enumeratee s2 s3 m a
  -> Enumeratee s1 s3 m a
f ><> g = joinI . f . g

-- | enumeratee composition with the arguments flipped, see '><>'
(<><) ::
 (Nullable s1, Monad m)
  => Enumeratee s2 s3 m a
  -> (forall x. Enumeratee s1 s2 m x)
  -> Enumeratee s1 s3 m a
f <>< g = joinI . g . f

-- | Combine enumeration over two streams.  The merging enumeratee would
-- typically be the result of 'Data.Iteratee.ListLike.merge' or
-- 'Data.Iteratee.ListLike.mergeByChunks' (see @merge@ for example).
mergeEnums :: 
  (Nullable s2, Nullable s1, Monad m)
  => Enumerator s1 m a                   -- ^ inner enumerator
  -> Enumerator s2 (Iteratee s1 m) a     -- ^ outer enumerator
  -> Enumeratee s2 s1 (Iteratee s1 m) a  -- ^ merging enumeratee
  -> Enumerator s1 m a
mergeEnums e1 e2 etee i = e1 $ e2 (joinI . etee $ ilift lift i) >>= run
{-# INLINE mergeEnums #-}

-- | The pure 1-chunk enumerator
-- 
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enumPure1Chunk :: (Monad m) => s -> Enumerator s m a
enumPure1Chunk str iter = runIter iter idoneM onC ierrM onR
  where
    onC k      = fst `liftM` k (Chunk str)
    onR mb doB = mb >>= enumPure1Chunk str . doB

-- | Enumerate chunks from a list
-- 
enumList :: (Monad m) => [s] -> Enumerator s m a
enumList chunks = go chunks
 where
  go [] i  = return i
  go xs' i = runIter i idoneM (onCont xs') onErr (onReq xs')
   where
    onCont (x:xs) k = k (Chunk x) >>= go xs . fst
    onCont []     k = return $ icont k
    onErr iRes e    = return $ throwRec e iRes
    onReq xs mb doB = mb >>= go xs . doB
{-# INLINABLE enumList #-}

-- | Checks if an iteratee has finished.
-- 
-- This enumerator runs the iteratee, performing any monadic actions.
-- If the result is True, the returned iteratee is done.
enumCheckIfDone :: (Monad m) => Iteratee s m a -> m (Bool, Iteratee s m a)
enumCheckIfDone iter = runIter iter onDone onCont onErr onReq
  where
    onDone x  = return (True, idone x)
    onCont k  = return (False, icont k)
    onErr i e = return (False, ierr i e)
    onReq mb doB = mb >>= enumCheckIfDone . doB
{-# INLINE enumCheckIfDone #-}


-- |Create an enumerator from a callback function
enumFromCallback ::
 (Monad m, NullPoint s) =>
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
data CBState = HasMore | Finished deriving (Eq, Show, Ord, Enum)

-- | The type of callback functions to create enumerators.
type Callback st m s = st -> m (Either SomeException ((CBState, st), s))

-- |Create an enumerator from a callback function with an exception handler.
-- The exception handler is called if an iteratee reports an exception.
enumFromCallbackCatch
  :: forall e m s st a. (IException e, Monad m, NullPoint s)
  => Callback st m s
  -> (e -> m (Maybe EnumException))
  -> st
  -> Enumerator s m a
enumFromCallbackCatch c handler = loop
  where
    loop st iter = runIter iter idoneM (onCont st) (onErr st) (onReq st)
    onCont st k  = c st >>= either (liftM fst . k . EOF . Just) (check k)
    onErr st i e = case fromException e of
      Just e' -> handler e' >>=
                   maybe (loop st i)
                         (return . ierr i) . fmap toException
      Nothing -> return (ierr i e)
    onReq :: st -> m x -> (x -> Iteratee s m a) -> m (Iteratee s m a)
    onReq st mb doB = mb >>= loop st . doB
      
    check :: (Stream s -> m (Iteratee s m a, Stream s))
             -> ((CBState, st), s)
             -> m (Iteratee s m a)
    check k ((HasMore,  st'), s) = k (Chunk s) >>= loop st' . fst
    check k ((Finished,_st'), s) = fst `liftM` k (Chunk s)
{-# INLINE enumFromCallbackCatch #-}
