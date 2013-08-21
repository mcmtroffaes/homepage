#!/bin/bash

rm -rf build
rm -rf ../bymwhaskell-gh-pages/*
rm -rf ../bymwhaskell-gh-pages/.buildinfo
sphinx-build -d build/doctrees . ../bymwhaskell-gh-pages/
