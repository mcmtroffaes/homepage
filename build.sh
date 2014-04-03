#!/bin/bash

rm -f site
ghc --make site.hs
./site rebuild
