-- some basic benchmarking of iteratee

module Main where
import qualified Data.Iteratee as I
import Data.Iteratee (enumPure1Chunk, enumPureNChunk, stream2list, stream2stream)
import Data.Iteratee.Base.StreamChunk
import Control.Monad.Identity
import Control.Monad

import Criterion.Main

main = defaultMain [listbench, streambench, breakbench]

proc :: (StreamChunk s el, Functor m, Monad m)
  => (a -> b) --function to force evaluation of result
  -> (m a -> a)
  -> I.EnumeratorGM s el m a
  -> I.IterateeG s el m a
  -> Int --needed by 'bench'.  Not sure why.
  -> b
proc eval runner enum iter x = eval $ runner . join . fmap I.run . enum $ iter

listbench = bgroup "List benchmarks" [benchList1, benchList2, benchList2]
streambench = bgroup "stream benchmarks" [benchStream1, benchStream2, benchStream3]
breakbench = bgroup "Break benchmarks" [benchBreak1, benchBreak2, benchBreak3, benchBreak4, benchBreak5]

benchList1 = bench "stream2list one go" $
  proc sum runIdentity (enumPure1Chunk [1..10000::Int]) stream2list
benchList2 = bench "stream2list chunk by 4" $
  proc sum runIdentity (enumPureNChunk [1..10000::Int] 4) stream2list
benchList3 = bench "stream2list chunk by 1024" $
  proc sum runIdentity (enumPureNChunk [1..10000::Int] 1024) stream2list

benchStream1 = bench "stream2stream one go" $
  proc sum runIdentity (enumPure1Chunk [1..10000::Int]) stream2stream
benchStream2 = bench "stream2stream chunk by 4" $
  proc sum runIdentity (enumPureNChunk [1..10000::Int] 4) stream2stream
benchStream3 = bench "stream2stream chunk by 1024" $
  proc sum runIdentity (enumPureNChunk [1..10000::Int] 1024) stream2stream

benchBreak1  = bench "break early one go" $
  proc id runIdentity (enumPure1Chunk [1..10000]) (I.break (>5))
benchBreak2 = bench "break never" $
  proc id runIdentity (enumPure1Chunk [1..10000]) (I.break (<0)) -- not true
benchBreak3 = bench "break early chunked" $
  proc id runIdentity (enumPureNChunk [1..10000] 20) (I.break (>500))
benchBreak4 = bench "break never chunked" $
  proc id runIdentity (enumPureNChunk [1..10000] 20) (I.break (<0)) -- not true
benchBreak5 = bench "break late chunked" $
  proc id runIdentity (enumPureNChunk [1..10000] 20) (I.break (>8000))
