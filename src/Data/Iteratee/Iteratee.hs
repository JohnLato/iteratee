{-# LANGUAGE KindSignatures
            ,RankNTypes
            ,FlexibleContexts
            ,ScopedTypeVariables
            ,BangPatterns
            ,DeriveDataTypeable #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Iteratee (
  -- * Types
  EnumerateeHandler
  -- ** Error handling
  ,throwErr
  ,throwRecoverableErr
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
  ,unfoldConvStreamCheck
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
  ,eneeCheckIfDoneIgnore
  ,eneeCheckIfDonePass
  ,mergeEnums
  -- ** Enumeratee Combinators
  ,($=)
  ,(=$)
  ,(><>)
  ,(<><)
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

import Control.Exception
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
throwErr e = icont (const (throwErr e)) (Just e)

-- |Report and propagate a recoverable error.  This error can be handled by
-- both 'enumFromCallbackCatch' and 'checkErr'.
throwRecoverableErr ::
  SomeException
  -> (Stream s -> Iteratee s m a)
  -> Iteratee s m a
throwRecoverableErr e i = icont i (Just e)


-- |Check if an iteratee produces an error.
-- Returns @Right a@ if it completes without errors, otherwise
-- @Left SomeException@. 'checkErr' is useful for iteratees that may not
-- terminate, such as @Data.Iteratee.head@ with an empty stream.
checkErr ::
 (NullPoint s) =>
  Iteratee s m a
  -> Iteratee s m (Either SomeException a)
checkErr iter = Iteratee $ \onDone onCont ->
  let od            = onDone . Right
      oc k Nothing  = onCont (checkErr . k) Nothing
      oc _ (Just e) = onDone (Left e) (Chunk empty)
  in runIter iter od oc

-- ------------------------------------------------------------------------
-- Parser combinators

-- |The identity iteratee.  Doesn't do any processing of input.
identity :: (NullPoint s) => Iteratee s m ()
identity = idone () (Chunk empty)

-- |Get the stream status of an iteratee.
isStreamFinished :: (Nullable s) => Iteratee s m (Maybe SomeException)
isStreamFinished = liftI check
  where
    check s@(Chunk xs)
      | nullC xs  = isStreamFinished
      | otherwise = idone Nothing s
    check s@(EOF e) = idone (Just $ fromMaybe (toException EofException) e) s
{-# INLINE isStreamFinished #-}


-- |Skip the rest of the stream
skipToEof :: Iteratee s m ()
skipToEof = icont check Nothing
  where
    check (Chunk _) = skipToEof
    check s         = idone () s


-- |Seek to a position in the stream
seek :: (NullPoint s) => FileOffset -> Iteratee s m ()
seek o = throwRecoverableErr (toException $ SeekException o) (const identity)

-- | Map a monadic function over the chunks of the stream and ignore the
-- result.  Useful for creating efficient monadic iteratee consumers, e.g.
-- 
-- >  logger = mapChunksM_ (liftIO . putStrLn)
-- 
-- these can be efficiently run in parallel with other iteratees via
-- @Data.Iteratee.ListLike.zip@.
mapChunksM_ :: (Monad m, Nullable s) => (s -> m b) -> Iteratee s m ()
mapChunksM_ f = liftI step
  where
    step (Chunk xs)
      | nullC xs   = liftI step
      | otherwise  = lift (f xs) >> liftI step
    step s@(EOF _) = idone () s
{-# INLINE mapChunksM_ #-}

-- | A fold over chunks
foldChunksM :: (Monad m, Nullable s) => (a -> s -> m a) -> a -> Iteratee s m a
foldChunksM f = liftI . go
  where
    go a (Chunk c) = lift (f a c) >>= liftI . go
    go a e = idone a e
{-# INLINE foldChunksM #-}

-- | Get the current chunk from the stream.
getChunk :: (Nullable s, NullPoint s) => Iteratee s m s
getChunk = liftI step
 where
  step (Chunk xs)
    | nullC xs  = liftI step
    | otherwise = idone xs $ Chunk empty
  step (EOF Nothing)  = throwErr $ toException EofException
  step (EOF (Just e)) = throwErr e
{-# INLINE getChunk #-}

-- | Get a list of all chunks from the stream.
getChunks :: (Nullable s) => Iteratee s m [s]
getChunks = liftI (step id)
 where
  step acc (Chunk xs)
    | nullC xs    = liftI (step acc)
    | otherwise   = liftI (step $ acc . (xs:))
  step acc stream = idone (acc []) stream
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
-- > breakE cpred = eneeCheckIfDone (liftI . step)
-- >  where
-- >   step k (Chunk s)
-- >       | LL.null s  = liftI (step k)
-- >       | otherwise  = case LL.break cpred s of
-- >         (str', tail')
-- >           | LL.null tail' -> eneeCheckIfDone (liftI . step) . k $ Chunk str'
-- >           | otherwise     -> idone (k $ Chunk str') (Chunk tail')
-- >   step k stream           =  idone (k stream) stream
-- 
eneeCheckIfDone ::
 (Monad m, NullPoint elo) =>
  ((Stream eli -> Iteratee eli m a) -> Iteratee elo m (Iteratee eli m a))
  -> Enumeratee elo eli m a
eneeCheckIfDone f = eneeCheckIfDonePass f'
 where
  f' k Nothing  = f k
  f' k (Just e) = throwRecoverableErr e (\s -> joinIM $ enumChunk s $ eneeCheckIfDone f (liftI k))

type EnumerateeHandler eli elo m a =
  (Stream eli -> Iteratee eli m a)
  -> SomeException
  -> Iteratee elo m (Iteratee eli m a)

-- | The same as eneeCheckIfDonePass, with one extra argument:
-- a handler which is used
-- to process any exceptions in a separate method.
eneeCheckIfDoneHandle
  :: (NullPoint elo)
  => EnumerateeHandler eli elo m a
  -> ((Stream eli -> Iteratee eli m a)
      -> Maybe SomeException
      -> Iteratee elo m (Iteratee eli m a)
     )
  -> Enumeratee elo eli m a
eneeCheckIfDoneHandle h f inner = Iteratee $ \od oc ->
  let onDone x s = od (idone x s) (Chunk empty)
      onCont k Nothing  = runIter (f k Nothing) od oc
      onCont k (Just e) = runIter (h k e)       od oc
  in runIter inner onDone onCont
{-# INLINABLE eneeCheckIfDoneHandle #-}

eneeCheckIfDonePass
  :: (NullPoint elo)
  => ((Stream eli -> Iteratee eli m a)
      -> Maybe SomeException
      -> Iteratee elo m (Iteratee eli m a)
     )
  -> Enumeratee elo eli m a
eneeCheckIfDonePass f = eneeCheckIfDoneHandle (\k e -> f k (Just e)) f
{-# INLINABLE eneeCheckIfDonePass #-}

eneeCheckIfDoneIgnore
  :: (NullPoint elo)
  => ((Stream eli -> Iteratee eli m a)
      -> Maybe SomeException
      -> Iteratee elo m (Iteratee eli m a)
     )
  -> Enumeratee elo eli m a
eneeCheckIfDoneIgnore f = eneeCheckIfDoneHandle (\k _ -> f k Nothing) f

-- | Convert one stream into another with the supplied mapping function.
-- This function operates on whole chunks at a time, contrasting to
-- @mapStream@ which operates on single elements.
-- 
-- > unpacker :: Enumeratee B.ByteString [Word8] m a
-- > unpacker = mapChunks B.unpack
-- 
mapChunks :: (NullPoint s) => (s -> s') -> Enumeratee s s' m a
mapChunks f = eneeCheckIfDonePass (icont . step)
 where
  step k (Chunk xs)     = eneeCheckIfDonePass (icont . step) . k . Chunk $ f xs
  step k str@(EOF mErr) = idone (k $ EOF mErr) str
{-# INLINE mapChunks #-}

-- | Convert a stream of @s@ to a stream of @s'@ using the supplied function.
mapChunksM
  :: (Monad m, NullPoint s, Nullable s)
  => (s -> m s')
  -> Enumeratee s s' m a
mapChunksM f = eneeCheckIfDonePass (icont . step)
 where
  step k (Chunk xs)     = lift (f xs) >>=
                          eneeCheckIfDonePass (icont . step) . k . Chunk
  step k str@(EOF mErr) = idone (k $ EOF mErr) str
{-# INLINE mapChunksM #-}

-- |Convert one stream into another, not necessarily in lockstep.
-- 
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- Iteratee s m s'.
convStream ::
 (Monad m, Nullable s) =>
  Iteratee s m s'
  -> Enumeratee s s' m a
convStream fi = eneeCheckIfDonePass check
  where
    check k (Just e) = throwRecoverableErr e (const identity) >> check k Nothing
    check k _ = isStreamFinished >>= maybe (step k) (idone (liftI k) . EOF . Just)
    step k = fi >>= eneeCheckIfDonePass check . k . Chunk
{-# INLINABLE convStream #-}

-- |The most general stream converter.  Given a function to produce iteratee
-- transformers and an initial state, convert the stream using iteratees
-- generated by the function while continually updating the internal state.
unfoldConvStream ::
 (Monad m, Nullable s) =>
  (acc -> Iteratee s m (acc, s'))
  -> acc
  -> Enumeratee s s' m a
unfoldConvStream f acc0 = eneeCheckIfDonePass (check acc0)
  where
    check acc k (Just e) = throwRecoverableErr e (const identity) >> check acc k Nothing
    check acc k _ = isStreamFinished >>=
                    maybe (step acc k) (idone (liftI k) . EOF . Just)
    step acc k = f acc >>= \(acc', s') ->
                    eneeCheckIfDonePass (check acc') . k . Chunk $ s'
{-# INLINABLE unfoldConvStream #-}

unfoldConvStreamCheck
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
unfoldConvStreamCheck checkDone f acc0 = checkDone (check acc0)
  where
    check acc k mX = isStreamFinished >>=
                   maybe (step acc k mX) (idone (icont k mX) . EOF . Just)
    step acc k Nothing = f acc >>= \(acc', s') ->
                  (checkDone (check acc') . k $ Chunk s')
    step acc k (Just ex) = throwRecoverableErr ex $ \str' ->
      let i = f acc >>= \(acc', s') ->
                           (checkDone (check acc') . k $ Chunk s')
      in joinIM $ enumChunk str' i
{-# INLINABLE unfoldConvStreamCheck #-}

-- | Collapse a nested iteratee.  The inner iteratee is terminated by @EOF@.
--   Errors are propagated through the result.
-- 
--  The stream resumes from the point of the outer iteratee; any remaining
--  input in the inner iteratee will be lost.
--  Differs from 'Control.Monad.join' in that the inner iteratee is terminated,
--  and may have a different stream type than the result.
joinI ::
 (Monad m, Nullable s) =>
  Iteratee s m (Iteratee s' m a)
  -> Iteratee s m a
joinI = (>>=
  \inner -> Iteratee $ \od oc ->
  let onDone  x _        = od x (Chunk empty)
      onCont  k Nothing  = runIter (k (EOF Nothing)) onDone onCont'
      onCont  _ (Just e) = runIter (throwErr e) od oc
      onCont' _ e        = runIter (throwErr (fromMaybe excDivergent e)) od oc
  in runIter inner onDone onCont)
{-# INLINE joinI #-}

-- | Lift an iteratee inside a monad to an iteratee.
joinIM :: (Monad m) => m (Iteratee s m a) -> Iteratee s m a
joinIM mIter = Iteratee $ \od oc -> mIter >>= \iter -> runIter iter od oc


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
enumEof iter = runIter iter onDone onCont
  where
    onDone  x _str    = return $ idone x (EOF Nothing)
    onCont  k Nothing = runIter (k (EOF Nothing)) onDone onCont'
    onCont  k e       = return $ icont k e
    onCont' _ Nothing = return $ throwErr excDivergent
    onCont' k e       = return $ icont k e

-- |Another primitive enumerator: tell the Iteratee the stream terminated
-- with an error.
enumErr :: (Exception e, Monad m) => e -> Enumerator s m a
enumErr e iter = runIter iter onDone onCont
  where
    onDone  x _       = return $ idone x (EOF . Just $ toException e)
    onCont  k Nothing = runIter (k (EOF (Just (toException e)))) onDone onCont'
    onCont  k e'      = return $ icont k e'
    onCont' _ Nothing = return $ throwErr excDivergent
    onCont' k e'      = return $ icont k e'


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
enumPure1Chunk str iter = runIter iter idoneM onCont
  where
    onCont k Nothing = return $ k $ Chunk str
    onCont k e       = return $ icont k e

-- | Enumerate chunks from a list
-- 
enumList :: (Monad m) => [s] -> Enumerator s m a
enumList chunks = go chunks
 where
  go [] i = return i
  go xs' i = runIter i idoneM (onCont xs')
   where
    onCont (x:xs) k Nothing = go xs . k $ Chunk x
    onCont _ _ (Just e) = return $ throwErr e
    onCont _ k Nothing  = return $ icont k Nothing
{-# INLINABLE enumList #-}

-- | Checks if an iteratee has finished.
-- 
-- This enumerator runs the iteratee, performing any monadic actions.
-- If the result is True, the returned iteratee is done.
enumCheckIfDone :: (Monad m) => Iteratee s m a -> m (Bool, Iteratee s m a)
enumCheckIfDone iter = runIter iter onDone onCont
  where
    onDone x str = return (True, idone x str)
    onCont k e   = return (False, icont k e)
{-# INLINE enumCheckIfDone #-}


-- |Create an enumerator from a callback function
enumFromCallback ::
 (Monad m, NullPoint s) =>
  (st -> m (Either SomeException ((Bool, st), s)))
  -> st
  -> Enumerator s m a
enumFromCallback c st =
  enumFromCallbackCatch c (\NotAnException -> return Nothing) st

-- Dummy exception to catch in enumFromCallback
-- This never gets thrown, but it lets us
-- share plumbing
data NotAnException = NotAnException
 deriving (Show, Typeable)

instance Exception NotAnException where
instance IException NotAnException where

-- |Create an enumerator from a callback function with an exception handler.
-- The exception handler is called if an iteratee reports an exception.
enumFromCallbackCatch
  :: (IException e, Monad m, NullPoint s)
  => (st -> m (Either SomeException ((Bool, st), s)))
  -> (e -> m (Maybe EnumException))
  -> st
  -> Enumerator s m a
enumFromCallbackCatch c handler = loop
  where
    loop st iter = runIter iter idoneM (onCont st)
    check k (True,  st') = loop st' . k . Chunk
    check k (False,_st') = return . k . Chunk
    onCont st k Nothing  = c st >>=
        either (return . k . EOF . Just) (uncurry (check k))
    onCont st k j@(Just e) = case fromException e of
      Just e' -> handler e' >>=
                   maybe (loop st . k $ Chunk empty)
                         (return . icont k . Just) . fmap toException
      Nothing -> return (icont k j)
{-# INLINE enumFromCallbackCatch #-}


