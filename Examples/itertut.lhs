> {-# LANGUAGE RankNTypes #-}
>
> module IterTut where
>
> import Prelude hiding (drop, take)
> import Data.Iteratee
> import qualified Data.ListLike as LL
> import Control.Monad.Identity

Reference material on Iteratees :

http://okmij.org/ftp/Streams.html

http://ianen.org/articles/understanding-iteratees/

This tutorial is based on
http://okmij.org/ftp/Haskell/Iteratee/IterateeIO-talk-notes.pdf
amongst other sources.  Hopefully you will find my additions positive.

[This tutorial is still incomplete, however I am including it in this release
as I hope the section on the CPS transform will be useful to users
switching to the new library.  JL]

Exercises : given a text file of Ints separated by newlines, write a
function which returns the first Int greater than a given k, or
Nothing.  Do this once using explicit handle operations (hGetLine) and
again using lazy IO (hGetContents.)

Problems : Handle IO is inconvenient : imperative, not composeable.
LazyIO (e.g. hGetContents) has unreliable semantics : when do the
handles get closed?  What is the resouce usage?  This example is a
toy -- imagine parsing HTTP requests in a high-performance server.
Imperative parsers are ugly, but we cannot sacrifice performance and
use lazy IO.

Oleg's solution : realize IO as left folds over streams.  Recall the
left fold

foldl :: (a -> b -> a) -> a -> [b] -> a

This is broken up into three parts : the input list [b], the worker
function (a -> b -> a) and initial state a, and the folding
function itself ("fold").

The input list is generalized into the Stream type :

> type ErrMsg = String
>
> data BasicStream a = B_Chunk [a] | B_EOF (Maybe ErrMsg)

We support chunked reads and non-blocking IO.  B_Chunk [] means the
handle is open but there isn't data available yet.

The worker function (plus state) is generalized into the Iteratee type :

> data BasicIteratee a b = Done b (BasicStream a)
>                        | Cont (BasicStream a -> BasicIteratee a b) (Maybe ErrMsg)

An iteratee is either done, returning a value and the remaining input,
or ready for more input.  The first argument to B_Cont is a function
that accepts more input, and advances the iteratee's state -- a
"continuation."  An iteratee can possibly be in an error state
(e.g. if parsing invalid data), as indicated by the second argument.

Simple examples :

> peekB :: BasicIteratee a (Maybe a)
> peekB = Cont step Nothing
>     where step (B_Chunk []) = peekB
>           step c@(B_Chunk (x:xs)) = Done (Just x) c
>           step stream = Done Nothing stream
>
> headBI :: BasicIteratee a a
> headBI = Cont step Nothing
>     where step (B_Chunk []) = headBI
>           step c@(B_Chunk (x:xs)) = Done x $ B_Chunk xs
>           step iter = Cont step $ Just "EOF"
>
> throwErrB :: ErrMsg -> BasicIteratee a b
> throwErrB e = Cont (\_ -> throwErrB e) (Just e)
>
> dropB :: Int -> BasicIteratee a ()
> dropB n = Cont (step n) Nothing
>     where step 0 st = Done () st


peek returns the next element, or Nothing if the stream is EOF.
headBI is like head.

The folding function is generalized to the Enumerator type.  It's job
is to feed an iteratee the contents of some resource, until it is
exhausted or the iteratee is done.

> type BasicEnumerator a b = BasicIteratee a b -> BasicIteratee a b

The simplest enumerator just feeds EOF :

> sendEOF :: BasicEnumerator a b
> sendEOF (Cont k Nothing) =
>     case k $ B_EOF Nothing of
>       iter@(Cont _ Nothing) -> throwErrB "Divergent iteratee"
>       iter -> iter
> sendEOF (Done x _) = Done x $ B_EOF Nothing
> sendEOF i = i

We can feed the contents of a list to an iteratee :

> enumListB :: [a] -> BasicEnumerator a b
> enumListB lst (Cont k Nothing) = k $ B_Chunk lst
> enumListB _ i = i
>
> enumListNChunkB :: [a] -> Int -> BasicEnumerator a b
> enumListNChunkB ls n it
>     | n <= 0 = error "Invalid n"
>     | Prelude.null ls = it
>     | otherwise =
>         case it of
>           Cont k Nothing -> enumListNChunkB t n $ k (B_Chunk h)
>               where (h,t) = splitAt n ls
>           _ -> it

The first sends the list in one big chunk; the second in chunks of
size no larger than n.

Advantages :

One perspective : lazy IO does not couple the resource (the handle)
with the demand tightly enough -- the list interface is too abstract.
The iteratee / enumerator protocol makes the demand explicit, and the
continuation passing style makes resource lifetime understandable.

------------------------
Composition : Horizontal
------------------------

Iteratees, unlike Handle IO, are compositional in many ways.  First is
"horizontal" :

> instance Monad (BasicIteratee a) where

Monadic composition is chaining iteratees : "horizontal."  In the
simplest case, if the first iteratee is done without any remaining
input, we pass the value it returns to the function f.

>     Done x (B_Chunk []) >>= f = f x

If it is done but has more input or an EOF, we pass that to the next
iteratee.

>     Done x st >>= f = case f x of

If the next iteratee is also done, it is safe to ignore the "rest" of
its "stream", since it was not actually fed any input.  Otherwise we
pass the stream (or error state) along.

>                         Done y _ -> Done y st
>                         Cont k Nothing -> k st
>                         i -> i

If the first iteratee wants to continue, the composition continues.
If f has type b -> BasicIteratee a c, then (>>= f) has type
BasicIteratee a b -> BasicIteratee a c.

>     Cont k e >>= f = Cont ((>>= f) . k) e

Meanwhile a monadic value is a done iteratee returning the value.

>     return x = Done x (B_Chunk [])


functors, applicative
enumerator composition

----------------------
Composition : Vertical
----------------------

joinI, enumeratees

> type BasicEnumeratee outer inner out =
>     BasicIteratee inner out -> BasicIteratee outer (BasicIteratee inner out)

'takeB' sends only the first n elements of the stream to the inner iteratee; even if more are available.

> takeB :: Int -> BasicEnumeratee a a b
> takeB 0 iter = return iter
> takeB n it@(Done x _) = dropB n >> return (return x)
> takeB n it@(Cont _ (Just e)) = dropB n >> throwErrB e
> takeB n it@(Cont k Nothing) = Cont (step n k) Nothing
>     where step n k (B_Chunk []) = Cont (step n k) Nothing
>           step n k c@(B_Chunk l)
>                    | Prelude.length l < n = takeB (n - Prelude.length l) $ k c
>                    | otherwise = Done (k (B_Chunk h)) (B_Chunk t)
>                    where (h,t) = splitAt n l
>           step n k st = Done (k st) st

---------------
Generalizations
---------------

StreamG, ListLike, Nullable / NullPoint : turn pattern-matching on
lists into guards

---------------
Monadic actions
---------------

> type BasicEnumeratorM m a b = BasicIteratee a b -> m (BasicIteratee a b)

BasicIterateeM, BasicEnumeratorM, BasicEnumerateeM


---------
CPS-style
---------

The actual iteratee library is "CPS transformed."  (See Oleg's
IterateeMCPS.hs.)  It uses CPS on two levels : the first is in the
continuation for the Cont state, and the second is to eliminate
constructors.

newtype Iteratee s m a = Iteratee {
      runIter :: forall r.
                 (a -> StreamG s -> m r) ->
                     ((StreamG s -> Iteratee s m a) -> Maybe SomeException -> m r) ->
                     m r }

The two arguments are continuations which return a value of type m r
(for some Monad m); the iteratee will call one of these two
continuations and return the value.  The first argument is the
continuation to call if the iteratee is in the "Done" state, the
second if in the "Cont" state.

Basic rule : replace separate constructors with calls to the
appropriate arguments, and pattern matching with continuations passed
into the appropriate argument.

Streams stay the same.

Iteratees :

an iteratee in state X ==> a function that calls continuation X
B_Done x s ==> Iteratee $ \onDone _ -> onDone x s
B_Cont k e ==> Iteratee $ \_ onCont -> onCont k' e

where k' s is the transformation of the BasicIteratee k s.

Some synonyms :

idone x s = Iteratee $ \od _ -> od x s
return x = idone x (Chunk empty)
icont k e = Iteratee $ \_ oc -> oc k e
liftI k = icont k Nothing

so B_Cont k Nothing = liftI k'.

Example :

headBI :: BasicIteratee a a
headBI = Cont step Nothing                                 -- turns into liftI step'
    where step (B_Chunk []) = headBI                       -- ListLike guard
          step c@(B_Chunk (x:xs)) = Done x $ B_Chunk xs    -- Done ==> idone
          step iter = Cont step $ Just $ ErrMsg "EOF"      -- Cont ==> icont

==>

> headI :: (Monad m, LL.ListLike s a) => Iteratee s m a
> headI = liftI step'
>     where step' (Chunk c)
>                 | LL.null c = headI
>                 | otherwise = idone (LL.head c) (Chunk $ LL.tail c)
>           step' st = icont step' (Just (setEOF st))

If the state of the iteratee depends some other parameter, the result
of the continuation will be an argument of both state arguments (and
the parameter.)

myit x = Iteratee step
     where step od oc = ...

Enumerators :

pattern-match on an iteratee in state X => pass continuation into
iteratee argument X

case iter of
  B_Done x s -> f x s
  B_cont k e -> g k e

==>

runIter iter onDone onCont
    where onDone x s = f' x s
          onCont k e = g' k e

where f' x s is the transformation of the (monadic) iteratee f x s,
and likewise for g' k e.

Example : the identity (monadic) enumerator

> idIB :: (Monad m) => BasicEnumeratorM m a b
> idIB (Done x s) = return $ Done x s
> idIB (Cont k e) = return $ Cont k e

is transformed into

> idI iter = runIter iter onDone onCont
>     where onDone x s = return $ idone x s
>           onCont k e = return $ icont k e

With the synonyms

idoneM = return . idone
icontM = return . icont

this simplifies to

> idI' iter = runIter iter idoneM icontM

Example :

enumListNChunkB :: [a] -> Int -> BasicEnumerator a b
enumListNChunkB ls n it
    | n <= 0 = error "Invalid n"
    | Prelude.null ls = it
    | otherwise =
        case it of
          Cont k Nothing -> enumListNChunkB t n $ k (B_Chunk h)
              where (h,t) = splitAt n ls
          _ -> it

==>

> enumListNChunks :: (Monad m, LL.ListLike s el) =>
>                    s -> Int -> Enumerator s m b
> enumListNChunks ls n it
>     | n <= 0 = error "Invalid n"
>     | LL.null ls = return it
>     | otherwise = runIter it idoneM onCont -- idoneM is the identity in the Done state
>     where onCont k Nothing = enumListNChunks t n $ k (Chunk h)
>               where (h, t) = LL.splitAt n ls
>           onCont k e = icontM k e -- icontM is the identity in the Cont state

Enumeratees :

("iteratees and enumerators at the same time.")  An example to keep in mind.

mapB :: (el -> el') -> BasicEnumeratee el el' a
mapB f it@(Done _ _) = Done it (B_Chunk [])
mapB _ it@(Cont k (Just e)) = throwErrB e
mapB f it@(Cont k Nothing) = Cont step Nothing
    where step (B_Chunk s) = mapB f $ k (B_Chunk $ map f s)
          step (B_EOF e) = mapB f $ k (B_EOF e)

Let's try our hand at a translation :

mapI f inner = ...

An idiom : the return value is a nested iteratee, with an outer
("from") part and an inner part ("to").  According to our iteratee
translation this is

mapI f inner = Iteratee $ \onDoneF onContF -> ...

The result of this outer iteratee typically depends on the state of
the inner iteratee.  Hence like with Enumerators we do a "pattern match"

mapI f inner = Iteratee $ \onDoneF onContF ->
               let onDoneT x s = ...
                   onContT k e = ...
               in runIter inner onDoneT onContT

I've prefered using let instead of a where because it keeps the outer
continuations onDoneF and onContF in scope.  One of onDoneT or onContT
will get called, depending on what state the "To" iteratee is in.
Remember though we want to eventually call either onDoneF or onContF
to signal what state the outer "From" iteratee is in.  In the simplest
cases we will simply directly call them, e.g.

mapB f it@(Done _ _) = Done it (B_Chunk [])
===>
let onDoneT x s = onDoneF it (Chunk empty)

If we however build up our desired iteratee value via combinators, we
need to remember to pass them the outer continuation messages :

mapB _ it@(Cont k (Just e)) = throwErrB e -- this is a Cont iteratee
===>
let onContT k (Just e) = runIter (throwErr e) onDoneF onContF

Note only onContF will get called, since throwErr delivers a
continuing iteratee.

The complete translation (we've truncated onDoneF to odf, etc.) :

> mapI :: (Monad m, LL.ListLike (s el) el, LL.ListLike (s el') el',
>          NullPoint (s el), NullPoint (s el') ) =>
>         (el -> el') -> Enumeratee (s el) (s el') m a
> mapI f it = Iteratee $ \odf ocf ->
>     let odt x s = odf it (Chunk empty)
>         oct _ (Just e) = runIter (throwErr e) odf ocf
>         oct k Nothing = ocf step Nothing
>               where
>                 step (Chunk xs)
>                     | LL.null xs = icont step Nothing
>                     | otherwise = mapI f $ k (Chunk $ LL.map f xs)
>                 step (EOF e) = mapI f $ k (EOF e)
>     in runIter it odt oct

Another example :

takeB :: Int -> BasicEnumeratee a a b
takeB 0 iter = return iter
takeB n it@(Done x _) = dropB n >> return (return x)
takeB n it@(Cont _ (Just e)) = dropB n >> throwErrB e
takeB n it@(Cont k Nothing) = Cont (step n k) Nothing
    where step n k (B_Chunk []) = Cont (step n k) Nothing
          step n k c@(B_Chunk l)
                   | Prelude.length l < n = takeB (n - Prelude.length l) $ k c
                   | otherwise = Done (k (B_Chunk h)) (B_Chunk t)
                   where (h,t) = splitAt n l
          step n k st = Done (k st) st

==>

> takeI :: (Monad m, Nullable a, LL.ListLike a el) => Int -> Enumeratee a a m b
> takeI 0 iter = return iter
> takeI n it =
>     Iteratee $ \odf ocf ->
>         let odt x _ = runIter (drop n >> return (return x)) odf ocf
>             oct _ (Just e) = runIter (drop n >> throwErr e) odf ocf
>             oct k Nothing = ocf (step n k) Nothing
>                 where step n k c@(Chunk xs)
>                            | LL.length xs < n = takeI (n - LL.length xs) $ k c
>                            | otherwise = idone (k (Chunk h)) (Chunk t)
>                            where (h,t) = LL.splitAt n xs
>                       step n k st = idone (k st) st
>         in runIter it odt oct

Exercise : why the calls to idone instead of odf?
