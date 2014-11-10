---
title: Agda Now Works Out of The Box in Hakyll
author: Matthias C. M. Troffaes
tags: hakyll, agda
---

Yesterday's
[pull request](https://github.com/jgm/highlighting-kate/pull/52)
to get the Agda syntax file into highlighting-kate has already been
merged and released! This means that Agda syntax highlighting now
works out of the box with hakyll:

``` {.sourceCode .bash}
cabal update
cabal sandbox init
cabal install -j hakyll
```

You can then run hakyll from your sandbox with

``` {.sourceCode .bash}
cabal exec ghc -- --make site.hs
```
