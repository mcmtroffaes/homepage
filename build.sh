#!/bin/bash

rm -f site
ghc --make site.hs
./site rebuild
rm -rf ../blowyourmindwithhaskell-gh-pages/*
cp -r _site/* ../blowyourmindwithhaskell-gh-pages/
