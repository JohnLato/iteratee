{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-- |Monadic and General Iteratees:
-- incremental input parsers, processors and transformers

module Data.Iteratee.Base (
  -- * Types
  StreamChunk (..),
  ReadableChunk (..),
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
  iter_err,
  iter_report_err,
  -- ** Basic Iteratees
  sbreak,
  sdropWhile,
  snext,
  speek,
  skip_till_eof,
  sdrop,
  sseek,
  -- ** Advanced iteratee combinators
  stake,
  stakeR,
  map_stream,
  conv_stream,
  -- * Enumerators
  enum_eof,
  enum_err,
  (>.),
  enum_pure_1chunk,
  enum_pure_nchunk,
  -- * Misc.
  FileOffset,
  bindm
)

where

import Data.Iteratee.IO.Base
import Data.List (splitAt, findIndex)
import Control.Monad.Trans
import Control.Monad.Identity
import Foreign.Ptr
import System.IO

-- for the StorableVector instance
import qualified Data.StorableVector as Vec
import qualified Data.StorableVector.Base as VB
import Foreign.ForeignPtr
import Foreign.Marshal.Array
import Foreign.Storable


-- |A useful combinator.
bindm :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
bindm m f = m >>= maybe (return Nothing) f

-- |Class of types that can be used to hold chunks of data within Iteratee
-- streams.
class StreamChunk c el where
  -- |Length of currently available data.
  cLength :: c el -> Int 
  -- |Create an empty chunk of data.
  cEmpty :: c el 
  -- |Test if the current stream is null.
  cNull :: c el -> Bool 
  -- |Prepend an element to the front of the data.
  cCons :: el -> c el -> c el
  -- |Return the first element of the stream.
  cHead :: c el -> el
  -- |Return the tail of the stream.
  cTail :: c el -> c el
  -- |First index matching the predicate.
  cFindIndex :: (el -> Bool) -> c el -> Maybe Int
  -- |Split the data at the specified index.
  cSplitAt :: Int -> c el -> (c el, c el)
  -- |Drop data matching the predicate.
  cDropWhile :: (el -> Bool) -> c el -> c el
  -- |Append to chunks of data into one.
  cAppend :: c el -> c el -> c el
  -- |Create a stream from a list.
  fromList :: [el] -> c el
  -- |Create a list from the stream.
  toList :: c el -> [el]
  -- |Map a computation over the stream.
  cMap :: (StreamChunk c' el') => (el -> el') -> c el -> c' el'

instance StreamChunk [] el where
  cLength    = length
  cEmpty     = []
  cNull []   = True
  cNull _    = False
  cCons      = (:)
  cHead      = head
  cTail      = tail
  cFindIndex = findIndex
  cSplitAt   = splitAt
  cDropWhile = dropWhile
  cAppend    = (++)
  fromList   = id
  toList     = id
  cMap       = listmap

instance (Storable el) => StreamChunk Vec.Vector el where
  cLength    = Vec.length
  cEmpty     = Vec.empty
  cNull      = Vec.null
  cCons      = Vec.cons
  cHead      = Vec.head
  cTail      = Vec.tail
  cFindIndex = Vec.findIndex
  cSplitAt   = Vec.splitAt
  cDropWhile = Vec.dropWhile
  cAppend    = Vec.append
  fromList   = Vec.pack
  toList     = Vec.unpack
  cMap       = vecmap

listmap :: (StreamChunk s' el') => (el -> el') -> [el] -> s' el'
listmap f = foldr (cCons . f) cEmpty

{-# RULES "listmap/map" listmap = map #-}

vecmap :: (Storable el, StreamChunk s' el') =>
          (el -> el') ->
          Vec.Vector el ->
          s' el'
vecmap f xs = step xs
  where
  step vec
    | Vec.null vec = cEmpty
    | True         = f (Vec.head vec) `cCons` step (Vec.tail vec)

{-# RULES "vecmap/map" forall s (f :: forall el el'.(Storable el') => el -> el'). vecmap f s = Vec.map f s #-}

-- |Class of streams which can be filled from a 'Ptr'.  Typically these
-- are streams which can be read from a file.
class StreamChunk s el => ReadableChunk s el where
  readFromPtr :: Ptr (el) -> Int -> IO (s el)

instance (Storable el) => ReadableChunk [] el where
  readFromPtr = flip peekArray

instance Storable el => ReadableChunk Vec.Vector el where
  readFromPtr p l = do
    fptr <- newForeignPtr_ p
    return $ VB.fromForeignPtr fptr (fromIntegral l)

-- |A stream is a (continuing) sequence of elements bundled in Chunks.
-- The first two variants indicate termination of the stream.
-- Chunk a gives the currently available part of the stream.
-- The stream is not terminated yet.
-- The case (null Chunk) signifies a stream with no currently available
-- data but which is still continuing. A stream processor should,
-- informally speaking, ``suspend itself'' and wait for more data
-- to arrive.
data (StreamChunk c el) => StreamG c el = EOF | Err String | Chunk (c el)

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

data IterateeG s el m a = IE_done a (StreamG s el)
		      | IE_cont (StreamG s el -> IterateeGM s el m a)
		      | IE_jmp FileOffset (StreamG s el -> IterateeGM s el m a)
newtype IterateeGM s el m a = IM{unIM:: m (IterateeG s el m a)}


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
-- process the result of stake, map_stream, or conv_stream below.
joinI :: (StreamChunk s el, StreamChunk s' el', Monad m) =>
         IterateeGM s el m (IterateeG s' el' m a) ->
         IterateeGM s el m a
joinI m = m >>= (\iter -> enum_eof iter >>== check)
  where
  check (IE_done x (Err str)) = liftI $ IE_done x (Err str)
  check (IE_done x _) = liftI $ IE_done x EOF
  check (IE_cont _) = error "joinI: can't happen: EOF didn't terminate"
  check (IE_jmp _ _) = error "joinI: can't happen: EOF didn't terminate"

-- It turns out, IterateeGM form a monad. We can use the familiar do
-- notation for composing Iteratees

instance (StreamChunk s el, Monad m) => Monad (IterateeGM s el m) where
    return x = liftI  $ IE_done  x (Chunk cEmpty)
    m >>= f = iter_bind m f

iter_bind :: (StreamChunk s el, Monad m ) =>
                        IterateeGM s el m a ->
                        (a -> IterateeGM s el m b) ->
                        IterateeGM s el m b
iter_bind m f = m >>== docase
     where
     docase (IE_done a (Chunk vec)) | cNull vec = f a
     docase (IE_done a stream) = f a >>== (\r -> case r of
				IE_done x _  -> liftI $ IE_done x stream
				IE_cont k    -> k stream
                                iter         -> liftI iter)
     docase (IE_cont k) = liftI $ IE_cont ((>>= f) . k)
     docase (IE_jmp off k) = liftI $ IE_jmp off ((>>= f) . k)

{-# SPECIALIZE iter_bind :: StreamChunk s el => IterateeGM s el IO a -> (a -> IterateeGM s el IO b) -> IterateeGM s el IO b #-}

instance (Monad m, Functor m) => Functor (IterateeGM s el m) where
    fmap f m = m >>== docase
      where
      docase (IE_done a stream) = liftI $ IE_done (f a) stream
      docase (IE_cont k) = liftI $ IE_cont (fmap f . k)
      docase (IE_jmp off k) = liftI $ IE_jmp off (fmap f . k)

instance (StreamChunk s el) => MonadTrans (IterateeGM s el) where
    lift m = IM (m >>= unIM . return)

instance (StreamChunk s el, MonadIO m) => MonadIO (IterateeGM s el m) where
    liftIO = lift . liftIO

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list
stream2list :: (StreamChunk s el, Monad m) => IterateeGM s el m [el]
stream2list = liftI $ IE_cont (step cEmpty)
 where
 step acc (Chunk ls) | cNull ls = liftI $ IE_cont (step acc)
                     | otherwise    = liftI $ IE_cont (step $ acc `cAppend` ls)
 step acc stream                    = liftI $ IE_done (toList acc) stream

-- |Report and propagate an error.  Disregard the input first and then
-- propagate the error.
iter_err :: (StreamChunk s el, Monad m) => String -> IterateeGM s el m ()
iter_err err = liftI $ IE_cont step
  where
  step _ = liftI $ IE_done () (Err err)

-- |Check to see if the stream is in error
iter_report_err :: (StreamChunk s el, Monad m) =>
                   IterateeGM s el m (Maybe String)
iter_report_err = liftI $ IE_cont step
  where
  step s@(Err str) = liftI $ IE_done (Just str) s
  step s           = liftI $ IE_done Nothing s

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

sbreak :: (StreamChunk s el, Monad m) =>
          (el -> Bool) ->
          IterateeGM s el m (s el, Maybe el)
sbreak cpred = liftI $ IE_cont (liftI . step cEmpty)
 where
 step before (Chunk str)
   | cNull str = IE_cont (liftI . step before)
   | otherwise = case cFindIndex cpred str of
       Nothing      -> IE_cont (liftI . step (before `cAppend` str))
       Just ix      -> let (str', tail') = cSplitAt ix str
                       in
                       done (before `cAppend` str') (Just $ cHead tail') (Chunk $ cTail tail')
 step before stream = done before Nothing stream
 done line' char = IE_done (line', char)


-- |A particular optimized case of 'sdrop': skip all elements of the stream
-- satisfying the given predicate - until the first element
-- that does not satisfy the predicate, or the end of the stream.
-- This is the analogue of List.dropWhile
sdropWhile :: (StreamChunk s el, Monad m) =>
              (el -> Bool) ->
              IterateeGM s el m ()
sdropWhile cpred = liftI $ IE_cont step
 where
 step (Chunk str)
   | cNull str = sdropWhile cpred
   | otherwise = let remm = cDropWhile cpred str
                 in
                 case cNull remm of
                   True -> sdropWhile cpred
                   False -> liftI $ IE_done () (Chunk remm)
 step stream   = liftI $ IE_done () stream


-- |Attempt to read the next element of the stream
-- Return (Just c) if successful, return Nothing if the stream is
-- terminated (by EOF or an error)
snext :: (StreamChunk s el, Monad m) =>
         IterateeGM s el m (Maybe el)
snext = liftI $ IE_cont step
 where
 step (Chunk vec)
   | cNull vec  = snext
   | otherwise      = liftI $ IE_done (Just $ cHead vec)
                      (Chunk $ cTail vec)
 step stream        = liftI $ IE_done Nothing stream


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return (Just c) if successful, return Nothing if the stream is
-- terminated (by EOF or an error)
speek :: (StreamChunk s el, Monad m) =>
         IterateeGM s el m (Maybe el)
speek = liftI $ IE_cont step
 where
 step s@(Chunk vec)
   | cNull vec    = speek
   | otherwise        = liftI $ IE_done (Just $ cHead vec) s
 step stream          = liftI $ IE_done Nothing stream


-- |Skip the rest of the stream
skip_till_eof :: (StreamChunk s el, Monad m) => IterateeGM s el m ()
skip_till_eof = liftI $ IE_cont step
 where
 step (Chunk _) = skip_till_eof
 step _         = return ()

-- |Skip n elements of the stream, if there are that many
-- This is the analogue of List.drop
sdrop :: (StreamChunk s el, Monad m) => Int -> IterateeGM s el m ()
sdrop 0 = return ()
sdrop n = liftI $ IE_cont step
 where
 step (Chunk str) | cLength str <= n = sdrop (n - cLength str)
 step (Chunk str) = liftI $ IE_done () (Chunk s2)
  where (_s1,s2) = cSplitAt n str
 step stream = liftI $ IE_done () stream

-- |Create a request to seek within an input stream.  This will result in
-- an error if the enumerator is not capable of responding to a seek request.
sseek :: (StreamChunk s el, Monad m) => FileOffset -> IterateeGM s el m ()
sseek off = liftI (IE_jmp off step)
 where
 step = liftI . IE_done ()

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
stake :: (StreamChunk s el, Monad m) =>
	 Int -> EnumeratorN s el s el m a
stake 0 iter = return iter
stake n iter@IE_done{} = sdrop n >> return iter
stake n (IE_jmp _off k) = liftI $ IE_cont step
 where
 step chunk@(Chunk str)
   | cNull str   = liftI $ IE_cont step
   | cLength str <= n = stake (n - cLength str) ==<< k chunk
 step (Chunk str) = done (Chunk s1) (Chunk s2)
   where (s1,s2) = cSplitAt n str
 step stream = done stream stream
 done s1 s2 = k s1 >>== \r -> liftI $ IE_done r s2
stake n (IE_cont k) = liftI $ IE_cont step
 where
 step chunk@(Chunk str)
   | cNull str   = liftI $ IE_cont step
   | cLength str <= n = stake (n - cLength str) ==<< k chunk
 step (Chunk str) = done (Chunk s1) (Chunk s2)
   where (s1,s2) = cSplitAt n str
 step stream = done stream stream
 done s1 s2 = k s1 >>== \r -> liftI $ IE_done r s2


-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of `stake' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early. This variation is particularly useful for randomIO,
-- where we do not have to care to `drain the input stream'.
stakeR :: (StreamChunk s el, Monad m) =>
          Int ->
          EnumeratorN s el s el m a
stakeR 0 iter = return iter
stakeR _n iter@IE_done{} = return iter
stakeR _n iter@IE_jmp{} = return iter
stakeR n (IE_cont k) = liftI $ IE_cont step
 where
 step chunk@(Chunk str)
   | cNull str        = liftI $ IE_cont step
   | cLength str <= n = stakeR (n - cLength str) ==<< k chunk
 step (Chunk str) = done (Chunk s1) (Chunk s2)
   where (s1,s2) = cSplitAt n str
 step stream = done stream stream
 done s1 s2 = k s1 >>== \r -> liftI $ IE_done r s2

-- |Map the stream: yet another iteratee transformer
-- Given the stream of elements of the type el and the function el->el',
-- build a nested stream of elements of the type el' and apply the
-- given iteratee to it.
-- Note the contravariance

map_stream :: (StreamChunk s el, StreamChunk s el', Monad m) =>
   (el -> el') ->
   EnumeratorN s el s el' m a
map_stream _f iter@IE_done{} = return iter
map_stream f (IE_cont k) = liftI $ IE_cont step
 where
 step (Chunk str) | cNull str = liftI $ IE_cont step
 step (Chunk str) = k (Chunk (cMap f str)) >>== map_stream f
 step EOF         = k EOF       >>== \r -> liftI $ IE_done r EOF
 step (Err err)   = k (Err err) >>== \r -> liftI $ IE_done r (Err err)
map_stream f (IE_jmp off k) = liftI $ IE_jmp off step
  where
 step (Chunk str) | cNull str = liftI $ IE_cont step
 step (Chunk str) = k (Chunk (cMap f str)) >>== map_stream f
 step EOF         = k EOF       >>== \r -> liftI $ IE_done r EOF
 step (Err err)   = k (Err err) >>== \r -> liftI $ IE_done r (Err err)

-- |Convert one stream into another, not necessarily in `lockstep'
-- The transformer map_stream maps one element of the outer stream
-- to one element of the nested stream.  The transformer below is more
-- general: it may take several elements of the outer stream to produce
-- one element of the inner stream, or the other way around.
-- The transformation from one stream to the other is specified as
-- IterateeGM s el m (Maybe [el']).  The `Maybe' type reflects the
-- possibility of the conversion error.
conv_stream :: (StreamChunk s el, StreamChunk s' el', Monad m) =>
  IterateeGM s el m (Maybe (s' el')) -> EnumeratorN s el s' el' m a
conv_stream _fi iter@IE_done{} = return iter
conv_stream fi (IE_cont k) = fi >>=
  (conv_stream fi ==<<) . k . maybe (Err "conv: stream error") Chunk
conv_stream fi (IE_jmp _off k) = fi >>=
  (conv_stream fi ==<<) . k . maybe (Err "conv: stream error") Chunk

{-# SPECIALIZE conv_stream :: (StreamChunk s el, StreamChunk s' el') => IterateeGM s el IO (Maybe (s' el')) -> EnumeratorN s el s' el' IO a #-}


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

-- |more general enumerator type: enumerator that maps
-- streams (not necessarily in lock-step).  This is 
-- a flattened (`joinI-ed') EnumeratorN sfrom elfrom sto elto m a
type EnumeratorGMM sfrom elfrom sto elto m a =
  IterateeG sto elto m a -> IterateeGM sfrom elfrom m a

-- |The most primitive enumerator: applies the iteratee to the terminated
-- stream. The result is the iteratee usually in the done state.
enum_eof :: (StreamChunk s el, Monad m) => EnumeratorGM s el m a
enum_eof (IE_done x _)   = liftI $ IE_done x EOF
enum_eof (IE_cont k)     = k EOF
enum_eof (IE_jmp _off k) = k EOF

-- |Another primitive enumerator: report an error
enum_err :: (StreamChunk s el, Monad m) => String -> EnumeratorGM s el m a
enum_err str (IE_done x _) = liftI $ IE_done x (Err str)
enum_err str (IE_cont k)   = k (Err str)
enum_err str (IE_jmp _off k) = k (Err str)

-- |The composition of two enumerators: essentially the functional composition
-- It is convenient to flip the order of the arguments of the composition
-- though: in e1 >. e2, e1 is executed first

(>.):: (StreamChunk s el, Monad m) =>
       EnumeratorGM s el m a -> EnumeratorGM s el m a -> EnumeratorGM s el m a
e1 >. e2 = (e2 ==<<) . e1

-- |The pure 1-chunk enumerator
-- It passes a given list of elements to the iteratee in one chunk
-- This enumerator does no IO and is useful for testing of base parsing
enum_pure_1chunk :: (StreamChunk s el, Monad m) =>
                    s el ->
                    EnumeratorGM s el m a
enum_pure_1chunk _str iter@IE_done{} = liftI iter
enum_pure_1chunk str (IE_cont k) = k (Chunk str)
enum_pure_1chunk _str (IE_jmp _off _k) = fail "enum_pure_1chunk cannot handle random IO"

-- |The pure n-chunk enumerator
-- It passes a given lift of elements to the iteratee in n chunks
-- This enumerator does no IO and is useful for testing of base parsing
-- and handling of chunk boundaries
enum_pure_nchunk :: (StreamChunk s el, Monad m) =>
                    s el ->
                    Int ->
                    EnumeratorGM s el m a
enum_pure_nchunk _str _n iter@IE_done{}   = liftI iter
enum_pure_nchunk str  _n iter | cNull str = liftI iter
enum_pure_nchunk str n (IE_cont k)        = enum_pure_nchunk s2 n ==<< k (Chunk s1)
 where (s1,s2) = cSplitAt n str
enum_pure_nchunk _str _n (IE_jmp _off _k) = fail "enum_pure_nchunk cannot handle ranom IO"

