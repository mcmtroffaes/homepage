---
title: Agda Syntax Patches Merged into KDE
author: Matthias C. M. Troffaes
tags: hakyll, agda
---

The Agda syntax highlighting file has been merged into kate and is now
[maintained upstream as part of ktexteditor](https://projects.kde.org/projects/frameworks/ktexteditor/repository/revisions/master/entry/src/syntax/data/agda.xml).

A [pull request](https://github.com/jgm/highlighting-kate/pull/52) has
been submitted to get this syntax file into highlighting-kate.

Updated instructions, now using the new `cabal sandbox` command
instead of `hsenv`:

``` {.sourceCode .bash}
cabal update
git clone git@github.com:mcmtroffaes/highlighting-kate.git
cd highlighting-kate
git checkout feature/agda-syntax
cabal sandbox init
cabal install -j hxt regex-posix utf8-string
make prep
cd ..
mkdir hakyll
cd hakyll
cabal sandbox init
cabal install -j hakyll ../highlighting-kate
```

You can then run hakyll from your sandbox with

``` {.sourceCode .bash}
cabal exec ghc -- --make site.hs
```
