-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base (
  -- * Types
  StreamG (..),
  IterateeG (..),
  IterateeGM (..),
  EnumeratorN,
  EnumeratorGM,
  EnumeratorGMM,
  -- * Iteratees
  -- ** Iteratee Combinators
  liftI,
  (>>==),
  (==<<),
  joinI,
  stream2list,
  -- ** Error handling
  iterErr,
  iterReportError,
  -- ** Basic Iteratees
  break,
  dropWhile,
  drop,
  head,
  peek,
  skipToEof,
  seek,
  -- ** Advanced iteratee combinators
  take,
  takeR,
  mapStream,
  convStream,
  -- * Enumerators
  enumEof,
  enumErr,
  (>.),
  enumPure1Chunk,
  enumPureNChunk,
  -- * Misc.
  FileOffset,
  bindm
)
where

import Prelude hiding (head, drop, dropWhile, take, break)
import qualified Prelude as P

import qualified Data.Iteratee.Base.StreamChunk as SC
import Data.Iteratee.IO.Base
import Control.Monad.Trans
import Control.Monad.Identity
import System.IO

-- |A useful combinator.
bindm :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
bindm m f = m >>= maybe (return Nothing) f

-- |A stream is a (continuing) sequence of elements bundled in Chunks.
-- The first two variants indicate termination of the stream.
-- Chunk a gives the currently available part of the stream.
-- The stream is not terminated yet.
-- The case (null Chunk) signifies a stream with no currently available
-- data but which is still continuing. A stream processor should,
-- informally speaking, ``suspend itself'' and wait for more data
-- to arrive.
data (SC.StreamChunk c el) => StreamG c el = EOF | Error String | Chunk (c el)

-- |Iteratee -- a generic stream processor, what is being folded over
-- a stream
-- When Iteratee is in the 'done' state, it contains the computed
-- result and the remaining part of the stream.
-- In the 'cont' state, the iteratee has not finished the computation
-- and needs more input.
-- We assume that all iteratees are `good' -- given bounded input,
-- they do the bounded amount of computation and take the bounded amount
-- of resources. The monad m describes the sort of computations done
-- by the iteratee as it processes the stream. The monad m could be
-- the identity monad (for pure computations) or the IO monad
-- (to let the iteratee store the stream processing results as they
-- are computed).
-- We also assume that given a terminated stream, an iteratee
-- moves to the done state, so the results computed so far could be returned.

data IterateeG s el m a
  = Done a (StreamG s el)
  | Cont (StreamG s el -> IterateeGM s el m a)
  | Seek FileOffset (StreamG s el -> IterateeGM s el m a)

instance (Show a) => Show (IterateeG s el m a) where
  show (Done a _)  = "Iteratee done: " ++ P.show a
  show (Cont   _k) = "Iteratee: incomplete"
  show (Seek f _k) = "Iteratee: seek to " ++ (P.show f) ++ "requested"

newtype IterateeGM s el m a = IM {unIM :: m (IterateeG s el m a)}


-- Useful combinators for implementing iteratees and enumerators

-- |Lift an 'IterateeG' into an 'IterateeGM'.
liftI :: Monad m => IterateeG s el m a -> IterateeGM s el m a
liftI = IM . return

{-# INLINE liftI #-}

-- |Just like bind (at run-time, this is indeed exactly bind)
infixl 1 >>==
(>>==) :: Monad m =>
          IterateeGM s el m a ->
          (IterateeG s el m a -> IterateeGM s' el' m b) ->
          IterateeGM s' el' m b
m >>== f = IM (unIM m >>= unIM . f)

{-# INLINE (>>==) #-}

-- |Just like an application -- a call-by-value-like application
infixr 1 ==<<
(==<<) :: Monad m =>
          (IterateeG s el m a -> IterateeGM s' el' m b)
          -> IterateeGM s el m a
          -> IterateeGM s' el' m b
(==<<) = flip (>>==)


-- |The following is a `variant' of join in the IterateeGM s el m monad
-- When el' is the same as el, the type of joinI is indeed that of
-- true monadic join.  However, joinI is subtly different: since
-- generally el' is different from el, it makes no sense to
-- continue using the internal, IterateeG el' m a: we no longer
-- have elements of the type el' to feed to that iteratee.
-- We thus send EOF to the internal Iteratee and propagate its result.
-- This join function is useful when dealing with `derived iteratees'
-- for embedded/nested streams.  In particular, joinI is useful to
-- process the result of take, mapStream, or convStream below.
joinI :: (SC.StreamChunk s el, SC.StreamChunk s' el', Monad m) =>
         IterateeGM s el m (IterateeG s' el' m a) ->
         IterateeGM s el m a
joinI m = m >>= (\iter -> enumEof iter >>== check)
  where
  check (Done x (Error str)) = liftI $ Done x (Error str)
  check (Done x _) = liftI $ Done x EOF
  check (Cont _)   = error "joinI: can't happen: EOF didn't terminate"
  check (Seek _ _) = error "joinI: can't happen: EOF didn't terminate"

-- It turns out, IterateeGM form a monad. We can use the familiar do
-- notation for composing Iteratees

instance (SC.StreamChunk s el, Monad m) => Monad (IterateeGM s el m) where
  return x = liftI  $ Done  x (Chunk SC.empty)
  m >>= f = iter_bind m f

iter_bind :: (SC.StreamChunk s el, Monad m ) =>
             IterateeGM s el m a ->
             (a -> IterateeGM s el m b) ->
             IterateeGM s el m b
iter_bind m f = m >>== docase
  where
  docase (Done a (Chunk vec))
    | SC.null vec        = f a
  docase (Done a stream) = f a >>== (\r -> case r of
    Done x _             -> liftI $ Done x stream
    Cont k               -> k stream
    iter                 -> liftI iter)
  docase (Cont k)        = liftI $ Cont ((>>= f) . k)
  docase (Seek off k)    = liftI $ Seek off ((>>= f) . k)

{-# SPECIALIZE iter_bind :: SC.StreamChunk s el => IterateeGM s el IO a -> (a -> IterateeGM s el IO b) -> IterateeGM s el IO b #-}

instance (Monad m, Functor m) => Functor (IterateeGM s el m) where
  fmap f m = m >>== docase
    where
    docase (Done a stream) = liftI $ Done (f a) stream
    docase (Cont k)        = liftI $ Cont (fmap f . k)
    docase (Seek off k)    = liftI $ Seek off (fmap f . k)

instance (SC.StreamChunk s el) => MonadTrans (IterateeGM s el) where
  lift m = IM (m >>= unIM . return)

instance (SC.StreamChunk s el, MonadIO m) => MonadIO (IterateeGM s el m) where
  liftIO = lift . liftIO

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list
stream2list :: (SC.StreamChunk s el, Monad m) => IterateeGM s el m [el]
stream2list = liftI $ Cont (step SC.empty)
 where
 step acc (Chunk ls) | SC.null ls   = liftI $ Cont (step acc)
                     | otherwise    = liftI $ Cont (step $ acc `SC.append` ls)
 step acc stream                    = liftI $ Done (SC.toList acc) stream

-- |Report and propagate an error.  Disregard the input first and then
-- propagate the error.
iterErr :: (SC.StreamChunk s el, Monad m) => String -> IterateeGM s el m ()
iterErr err = liftI $ Cont step
  where
  step _ = liftI $ Done () (Error err)

-- |Check to see if the stream is in error
iterReportError :: (SC.StreamChunk s el, Monad m) =>
                   IterateeGM s el m (Maybe String)
iterReportError = liftI $ Cont step
  where
  step s@(Error str) = liftI $ Done (Just str) s
  step s             = liftI $ Done Nothing s

-- ------------------------------------------------------------------------
-- Parser combinators

-- |The analogue of List.break
-- It takes an element predicate and returns a pair:
--  (str, Just c) -- the element 'c' is the first element of the stream
--                   satisfying the break predicate;
--                   The chunk str is the prefix of the stream up
--                   to but including 'c'
--  (str,Nothing) -- The stream is terminated with EOF or error before
--                   any element satisfying the break predicate was found.
--                   str is the scanned part of the stream.
-- None of the element in str satisfy the break predicate.

break :: (SC.StreamChunk s el, Monad m) =>
          (el -> Bool) ->
          IterateeGM s el m (s el, Maybe el)
break cpred = liftI $ Cont (liftI . step SC.empty)
  where
  step before (Chunk str)
    | SC.null str = Cont (liftI . step before)
    | otherwise = case SC.findIndex cpred str of
        Nothing -> Cont (liftI . step (before `SC.append` str))
        Just ix -> let (str', tail') = SC.splitAt ix str
                   in
                   done (before `SC.append` str') (Just $ SC.head tail') (Chunk $ SC.tail tail')
  step before stream = done before Nothing stream
  done line' char = Done (line', char)


-- |A particular optimized case of 'drop': skip all elements of the stream
-- satisfying the given predicate - until the first element
-- that does not satisfy the predicate, or the end of the stream.
-- This is the analogue of List.dropWhile
dropWhile :: (SC.StreamChunk s el, Monad m) =>
              (el -> Bool) ->
              IterateeGM s el m ()
dropWhile cpred = liftI $ Cont step
  where
  step (Chunk str)
    | SC.null str = dropWhile cpred
    | otherwise = let remm = SC.dropWhile cpred str
                  in
                  case SC.null remm of
                    True  -> dropWhile cpred
                    False -> liftI $ Done () (Chunk remm)
  step stream   = liftI $ Done () stream


-- |Attempt to read the next element of the stream
-- Return (Just c) if successful, return Nothing if the stream is
-- terminated (by EOF or an error)
head :: (SC.StreamChunk s el, Monad m) =>
         IterateeGM s el m (Maybe el)
head = liftI $ Cont step
  where
  step (Chunk vec)
    | SC.null vec  = head
    | otherwise      = liftI $ Done (Just $ SC.head vec) (Chunk $ SC.tail vec)
  step stream        = liftI $ Done Nothing stream


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return (Just c) if successful, return Nothing if the stream is
-- terminated (by EOF or an error)
peek :: (SC.StreamChunk s el, Monad m) =>
         IterateeGM s el m (Maybe el)
peek = liftI $ Cont step
  where
  step s@(Chunk vec)
    | SC.null vec = peek
    | otherwise = liftI $ Done (Just $ SC.head vec) s
  step stream   = liftI $ Done Nothing stream


-- |Skip the rest of the stream
skipToEof :: (SC.StreamChunk s el, Monad m) => IterateeGM s el m ()
skipToEof = liftI $ Cont step
  where
  step (Chunk _) = skipToEof
  step _         = return ()

-- |Skip n elements of the stream, if there are that many
-- This is the analogue of List.drop
drop :: (SC.StreamChunk s el, Monad m) => Int -> IterateeGM s el m ()
drop 0 = return ()
drop n = liftI $ Cont step
  where
  step (Chunk str) | SC.length str <= n = drop (n - SC.length str)
  step (Chunk str) = liftI $ Done () (Chunk s2)
   where
   (_s1,s2) = SC.splitAt n str
  step stream      = liftI $ Done () stream

-- |Create a request to seek within an input stream.  This will result in
-- an error if the enumerator is not capable of responding to a seek request.
seek :: (SC.StreamChunk s el, Monad m) => FileOffset -> IterateeGM s el m ()
seek off = liftI (Seek off step)
  where
  step = liftI . Done ()

-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- |The type of the converter from the stream with elements el_outer
-- to the stream with element el_inner.  The result is the iteratee
-- for the outer stream that uses an `IterateeG el_inner m a'
-- to process the embedded, inner stream as it reads the outer stream.
type EnumeratorN s_outer el_outer s_inner el_inner m a =
  IterateeG s_inner el_inner m a ->
  IterateeGM s_outer el_outer m (IterateeG s_inner el_inner m a)

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements (even if the iteratee has accepted fewer).
take :: (SC.StreamChunk s el, Monad m) =>
   Int -> EnumeratorN s el s el m a
take 0 iter = return iter
take n iter@Done{} = drop n >> return iter
take n (Seek _off k) = liftI $ Cont step
  where
  step chunk@(Chunk str)
    | SC.null str        = liftI $ Cont step
    | SC.length str <= n = take (n - SC.length str) ==<< k chunk
  step (Chunk str)       = done (Chunk s1) (Chunk s2)
    where (s1,s2) = SC.splitAt n str
  step stream            = done stream stream
  done s1 s2             = k s1 >>== \r -> liftI $ Done r s2
take n (Cont k) = liftI $ Cont step
  where
  step chunk@(Chunk str)
    | SC.null str        = liftI $ Cont step
    | SC.length str <= n = take (n - SC.length str) ==<< k chunk
  step (Chunk str)       = done (Chunk s1) (Chunk s2)
    where (s1,s2) = SC.splitAt n str
  step stream            = done stream stream
  done s1 s2             = k s1 >>== \r -> liftI $ Done r s2


-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of `take' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early. This variation is particularly useful for randomIO,
-- where we do not have to care to `drain the input stream'.
takeR :: (SC.StreamChunk s el, Monad m) =>
          Int ->
          EnumeratorN s el s el m a
takeR 0 iter = return iter
takeR _n iter@Done{} = return iter
takeR _n iter@Seek{} = return iter
takeR n (Cont k)     = liftI $ Cont step
  where
  step chunk@(Chunk str)
    | SC.null str        = liftI $ Cont step
    | SC.length str <= n = takeR (n - SC.length str) ==<< k chunk
  step (Chunk str)       = done (Chunk s1) (Chunk s2)
    where (s1,s2) = SC.splitAt n str
  step stream            = done stream stream
  done s1 s2             = k s1 >>== \r -> liftI $ Done r s2

-- |Map the stream: yet another iteratee transformer
-- Given the stream of elements of the type el and the function el->el',
-- build a nested stream of elements of the type el' and apply the
-- given iteratee to it.
-- Note the contravariance

mapStream :: (SC.StreamChunk s el, SC.StreamChunk s el', Monad m) =>
              (el -> el') ->
              EnumeratorN s el s el' m a
mapStream _f iter@Done{} = return iter
mapStream f (Cont k) = liftI $ Cont step
  where
  step (Chunk str)
    | SC.null str  = liftI $ Cont step
  step (Chunk str) = k (Chunk (SC.cMap f str)) >>== mapStream f
  step EOF         = k EOF                     >>== \r -> liftI $ Done r EOF
  step (Error err) = k (Error err)             >>== \r ->
                       liftI $ Done r (Error err)
mapStream f (Seek off k) = liftI $ Seek off step
  where
  step (Chunk str)
    | SC.null str  = liftI $ Cont step
  step (Chunk str) = k (Chunk (SC.cMap f str)) >>== mapStream f
  step EOF         = k EOF                     >>== \r -> liftI $ Done r EOF
  step (Error err) = k (Error err)             >>== \r ->
                       liftI $ Done r (Error err)

-- |Convert one stream into another, not necessarily in `lockstep'
-- The transformer mapStream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- IterateeGM s el m (Maybe [el']).  The `Maybe' type reflects the
-- possibility of the conversion error.
convStream :: (SC.StreamChunk s el, SC.StreamChunk s' el', Monad m) =>
  IterateeGM s el m (Maybe (s' el')) -> EnumeratorN s el s' el' m a
convStream _fi iter@Done{} = return iter
convStream fi (Cont k) = fi >>=
  (convStream fi ==<<) . k . maybe (Error "conv: stream error") Chunk
convStream fi (Seek _off k) = fi >>=
  (convStream fi ==<<) . k . maybe (Error "conv: stream error") Chunk

{-# SPECIALIZE convStream :: (SC.StreamChunk s el, SC.StreamChunk s' el') => IterateeGM s el IO (Maybe (s' el')) -> EnumeratorN s el s' el' IO a #-}


-- ------------------------------------------------------------------------
-- Enumerators
-- |Each enumerator takes an iteratee and returns an iteratee
-- an Enumerator is an iteratee transformer.
-- The enumerator normally stops when the stream is terminated
-- or when the iteratee moves to the done state, whichever comes first.
-- When to stop is of course up to the enumerator...

-- We have two choices of composition: compose iteratees or compose
-- enumerators. The latter is useful when one iteratee
-- reads from the concatenation of two data sources.

type EnumeratorGM s el m a = IterateeG s el m a -> IterateeGM s el m a

-- |More general enumerator type: enumerator that maps
-- streams (not necessarily in lock-step).  This is
-- a flattened (`joinI-ed') EnumeratorN sfrom elfrom sto elto m a
type EnumeratorGMM sfrom elfrom sto elto m a =
  IterateeG sto elto m a -> IterateeGM sfrom elfrom m a

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee usually in the done state.
enumEof :: (SC.StreamChunk s el, Monad m) => EnumeratorGM s el m a
enumEof (Done x _)    = liftI $ Done x EOF
enumEof (Cont k)      = k EOF
enumEof (Seek _off k) = k EOF

-- |Another primitive enumerator: report an error
enumErr :: (SC.StreamChunk s el, Monad m) => String -> EnumeratorGM s el m a
enumErr str (Done x _)    = liftI $ Done x (Error str)
enumErr str (Cont k)      = k (Error str)
enumErr str (Seek _off k) = k (Error str)

-- |The composition of two enumerators: essentially the functional composition
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >. e2, e1 is executed first

(>.):: (SC.StreamChunk s el, Monad m) =>
       EnumeratorGM s el m a -> EnumeratorGM s el m a -> EnumeratorGM s el m a
e1 >. e2 = (e2 ==<<) . e1

-- |The pure 1-chunk enumerator
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enumPure1Chunk :: (SC.StreamChunk s el, Monad m) =>
                    s el ->
                    EnumeratorGM s el m a
enumPure1Chunk _str iter@Done{} = liftI iter
enumPure1Chunk str (Cont k) = k (Chunk str)
enumPure1Chunk _str (Seek _off _k) = fail "enumPure1Chunk cannot handle random IO"

-- |The pure n-chunk enumerator
-- It passes a given lift of elements to the iteratee in n chunks
-- This enumerator does no IO and is useful for testing of base parsing
-- and handling of chunk boundaries
enumPureNChunk :: (SC.StreamChunk s el, Monad m) =>
                    s el ->
                    Int ->
                    EnumeratorGM s el m a
enumPureNChunk _str _n iter@Done{}   = liftI iter
enumPureNChunk str  _n iter | SC.null str = liftI iter
enumPureNChunk str n (Cont k)        = enumPureNChunk s2 n ==<< k (Chunk s1)
  where (s1,s2) = SC.splitAt n str
enumPureNChunk _str _n (Seek _off _k) = fail "enumPureNChunk cannot handle ranom IO"

