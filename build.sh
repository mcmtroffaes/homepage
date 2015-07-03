#!/bin/bash

rm -f site
rm -rf _site _cache
ghc --make site.hs
./site build
