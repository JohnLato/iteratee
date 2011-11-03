#!/bin/bash

ghc --make -i -i../src -DUSE_POSIX -hide-package iteratee -hide-package mtl -odir . -hidir . testIteratee.hs QCUtils.hs -fhpc
./testIteratee

ghc --make -i -i../src -DUSE_POSIX -hide-package iteratee -hide-package mtl -odir . -hidir . benchmarkHandle.hs -fhpc
./benchmarkHandle

hpc sum testIteratee.tix benchmarkHandle.tix --exclude Main > total.tix
hpc markup total.tix
open hpc_index.html
