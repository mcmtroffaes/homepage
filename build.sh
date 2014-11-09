#!/bin/bash

rm -f site
cabal exec ghc -- --make site.hs
./site rebuild
rm -rf ../blowyourmindwithhaskell-gh-pages/*
cp -r _site/* ../blowyourmindwithhaskell-gh-pages/
