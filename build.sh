#!/bin/bash

rm -rf build
rm -rf ../blowyourmindwithhaskell-gh-pages/*
rm -rf ../blowyourmindwithhaskell-gh-pages/.buildinfo
sphinx-build -d build/doctrees . ../blowyourmindwithhaskell-gh-pages/
