#!/bin/bash

rm -f site
rm -rf _site _cache
cabal exec ghc -- --make site.hs
./site build
