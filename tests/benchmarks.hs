{-# LANGUAGE RankNTypes, KindSignatures, NoMonomorphismRestriction #-}

-- some basic benchmarking of iteratee

module Main where
import qualified Data.Iteratee as I
import Data.Iteratee (enumPure1Chunk, enumPureNChunk, stream2list, stream2stream)
import Data.Iteratee.Base.StreamChunk
import Control.Monad.Identity
import Control.Monad
import qualified Data.ListLike as LL
import Control.DeepSeq

import Criterion.Main

main = defaultMain [allListBenches, allByteStringBenches]

-- -------------------------------------------------------------
-- helper functions and data

instance DeepSeq (WrappedByteString a) where
  deepseq (WrapBS bs) = ()

-- |Hold information about a benchmark.  This allows each
-- benchmark (and baseline) to be created independently of the stream types,
-- for easy comparison of different streams.
-- BDList is for creating baseline comparison functions.  Although the name
-- is BDList, it will work for any stream type (e.g. bytestrings).
data BD a b s el m = BDIter1 String (a -> b) (I.IterateeG s el m a) 
  | BDIterN String Int (a -> b) (I.IterateeG s el m a)
  | BDList String ((s el) -> b) (s el)

id1 name i = BDIter1 name id i
idN name i = BDIterN name 5 id i

makeList name f = BDList name f [1..10000]

makeBench (BDIter1 n eval i) = bench n $
  proc eval runIdentity (enumPure1Chunk [1..10000]) i
makeBench (BDIterN n csize eval i) = bench n $
  proc eval runIdentity (enumPureNChunk [1..10000] csize) i
makeBench (BDList n f l) = bench n $ B f l

proc :: (StreamChunk s el, Functor m, Monad m)
  => (a -> b) --function to force evaluation of result
  -> (m a -> a)
  -> I.Enumerator s m a
  -> I.Iteratee s m a
  -> B (I.Iteratee s m a) b
proc eval runner enum iter = B (eval . runner . I.run . enum) iter

defaultProc = proc id runIdentity (enumPure1Chunk [1..10000])
defaultNProc = proc id runIdentity (enumPureNChunk [1..10000] 5)

-- -------------------------------------------------------------
-- benchmark groups
makeGroup n bs = bgroup n $ map makeBench bs

listbench = makeGroup "Stream2List benchmarks" slistBenches
streambench = makeGroup "stream benchmarks" streamBenches
breakbench = makeGroup "Break benchmarks" breakBenches
headsbench = makeGroup "Heads benchmarks" headsBenches
dropbench = makeGroup "Drop benchmarks" dropBenches
lengthbench = makeGroup "Length benchmarks" listBenches
takebench = makeGroup "Take benchmarks" takeBenches
takeRbench = makeGroup "TakeR benchmarks" takeRBenches
mapbench = makeGroup "Map benchmarks" mapBenches
miscbench = makeGroup "Other iteratee benchmarks" miscBenches

listbenchbs =  makeGroupBS "stream2List" slistBenches
streambenchbs = makeGroupBS "stream" streamBenches
breakbenchbs = makeGroupBS "break" breakBenches
headsbenchbs = makeGroupBS "heads" headsBenches
dropbenchbs = makeGroupBS "drop" dropBenches
lengthbenchbs = makeGroupBS "length" listBenches
takebenchbs = makeGroupBS "take" takeBenches
takeRbenchbs = makeGroupBS "takeR" takeRBenches
mapbenchbs = makeGroupBS "map" mapBenches
convbenchbs = makeGroupBS "convStream" convBenches
miscbenchbs = makeGroupBS "other" miscBenches

allListBenches = bgroup "list" [listbench, streambench, breakbench, headsbench, dropbench, lengthbench, takebench, takeRbench, mapbench, convbench, miscbench]

allByteStringBenches = bgroup "bytestring" [listbenchbs, streambenchbs, breakbenchbs, headsbenchbs, dropbenchbs, lengthbenchbs, takebenchbs, takeRbenchbs, mapbenchbs, convbenchbs, miscbenchbs]

list0 = makeList "list one go" deepseq
list1 = BDIter1 "stream2list one go" deepseq stream2list
list2 = BDIterN "stream2list chunk by 4" 4 deepseq stream2list
list3 = BDIterN "stream2list chunk by 1024" 1024 deepseq stream2list
slistBenches = [list1, list2, list3]

stream1 = BDIter1 "stream2stream one go" deepseq stream2stream
stream2 = BDIterN "stream2stream chunk by 4" 4 deepseq stream2stream
stream3 = BDIterN "stream2stream chunk by 1024" 1024 deepseq stream2stream
streamBenches = [stream1, stream2, stream3]

break0 = makeList "break early list" (fst . Prelude.break (>5))
break0' = makeList "break never list" (fst . Prelude.break (<0))
break1 = id1 "break early one go" (I.break (>5))
break2 = id1 "break never" (I.break (<0)) -- not ever true.
break3 = idN "break early chunked" (I.break (>500))
break4 = idN "break never chunked" (I.break (<0)) -- not ever true
break5 = idN "break late chunked" (I.break (>8000))
breakBenches = [break1, break2, break3, break4, break5]

heads1 = id1 "heads null" (I.heads [])
heads2 = id1 "heads 1" (I.heads [1])
heads3 = id1 "heads 100" (I.heads [1.100])
heads4 = idN "heads 100 over chunks" (I.heads [1.100])
headsBenches = [heads1, heads2, heads3, heads4]

benchpeek = id1 "peek" I.peek
benchskip = id1 "skipToEof" (I.skipToEof >> return Nothing)
miscBenches = [benchpeek, benchskip]

drop0 = makeList "drop plain (list only)"
  ( flip seq () . Prelude.drop 100)
drop1 = id1 "drop null" (I.drop 0)
drop2 = id1 "drop plain" (I.drop 100)
drop3 = idN "drop over chunks" (I.drop 100)

dropw0 = makeList "dropWhile all (list only)" (Prelude.dropWhile (const True))
dropw1 = id1 "dropWhile all" (I.dropWhile (const True))
dropw2 = idN "dropWhile all chunked" (I.dropWhile (const True))
dropw3 = id1 "dropWhile small" (I.dropWhile ( < 100))
dropw4 = id1 "dropWhile large" (I.dropWhile ( < 6000))
dropBenches = [drop1, drop2, drop3, dropw1, dropw2, dropw3, dropw4]


l1 = makeList "length of list" Prelude.length
l2 = id1 "length single iteratee" I.length
l3 = idN "length chunked" I.length
listBenches = [l2, l3]

take0 = makeList "take length of list long" (Prelude.length . Prelude.take 1000)
take1 = id1 "take head short one go" (I.joinI $ I.take 20 I.head)
take2 = id1 "take head long one go" (I.joinI $ I.take 1000 I.head)
take3 = idN "take head short chunked" (I.joinI $ I.take 20 I.head)
take4 = idN "take head long chunked" (I.joinI $ I.take 1000 I.head)
take5 = id1 "take length long one go" (I.joinI $ I.take 1000 I.length)
take6 = idN "take length long chunked" (I.joinI $ I.take 1000 I.length)
takeBenches = [take1, take2, take3, take4, take5, take6]

takeR0 = makeList "take length of list long" (Prelude.length . Prelude.take 1000)
takeR1 = id1 "takeR head short one go" (I.joinI $ I.take 20 I.head)
takeR2 = id1 "takeR head long one go" (I.joinI $ I.takeR 1000 I.head)
takeR3 = idN "takeR head short chunked" (I.joinI $ I.takeR 20 I.head)
takeR4 = idN "takeR head long chunked" (I.joinI $ I.takeR 1000 I.head)
takeR5 = id1 "takeR length long one go" (I.joinI $ I.takeR 1000 I.length)
takeR6 = idN "takeR length long chunked" (I.joinI $ I.takeR 1000 I.length)
takeRBenches = [takeR1, takeR2, takeR3, takeR4, takeR5, takeR6]

map0 = makeList "map of list" (Prelude.length . Prelude.map id)
map1 = id1 "map length one go" (I.joinI $ I.mapStream id I.length)
map2 = idN "map length chunked" (I.joinI $ I.mapStream id I.length)
map3 = id1 "map head one go" (I.joinI $ I.mapStream id I.head)
map4 = idN "map head chunked" (I.joinI $ I.mapStream id I.head)
mapBenches = [map1, map2, map3, map4]

conv1 = idN "convStream id head chunked" (I.joinI . I.convStream idChunk $ I.head)
conv2 = idN "convStream id length chunked" (I.joinI . I.convStream idChunk $ I.length)
idChunk = Iteratee . return $ Cont step Nothing
  where
    step (I.Chunk xs)
      | LL.null xs      = idChunk
      | True            = Iteratee . return $ Done (Just xs) (I.Chunk mempty)
    step s@(I.EOF mErr) = Iteratee . return $ Done Nothing s
convBenches = [conv1, conv2]
