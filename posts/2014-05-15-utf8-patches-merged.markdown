---
title: Unicode Patches Now Merged
author: Matthias C. M. Troffaes
tags: hakyll, agda
---

Thanks to the speedy response of the pcre-regex-builtin and
highlighting-kate developers, enabling Agda syntax highlighting in
Hakyll just became a little bit simpler:

``` {.sourceCode .bash}
cabal update
git clone git@github.com:mcmtroffaes/highlighting-kate.git
cd highlighting-kate
git checkout feature/agda-syntax
hsenv
source .hsenv/bin/activate
cabal install -j hxt regex-posix utf8-string
make prep
deactivate_hsenv
cd ..
mkdir hakyll
cd hakyll
hsenv
source .hsenv/bin/activate
cabal install -j hakyll ../highlighting-kate
```
