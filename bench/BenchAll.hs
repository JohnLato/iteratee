module Main where

import Criterion.Main
import BenchBase (allByteStringBenches)
import BenchIO   (ioBenches)

main = defaultMain (allByteStringBenches: ioBenches)
