#!/bin/bash

ghc --make -i -i../src -hide-package iteratee -hide-package mtl -hide-package MonadCatchIO-mtl -odir . -hidir . testIteratee.hs QCUtils.hs -fhpc
