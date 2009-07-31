#!/bin/bash

ghc --make -i -i../src -hide-package iteratee -hide-package mtl -odir . -hidir . testIteratee.hs QCUtils.hs -fhpc
