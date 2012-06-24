module Main where

import Criterion.Main
import BenchBase (allByteStringBenches)
import BenchIO   (allIOBenches)

main = defaultMain [allByteStringBenches, bgroup "IO" allIOBenches]
