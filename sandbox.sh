#!/bin/bash
rm -rf .cabal-sandbox/
rm cabal.sandbox.config
cabal sandbox init
# note: fix unix at 2.6.0.1 to resolve ghc dependency bug
cabal install "unix==2.6.0.1" hakyll -j8
