---
title: Moving to Hakyll and Agda Syntax Highlighting in Pandoc
author: Matthias C. M. Troffaes
tags: hakyll, kate, agda
---

I have just finished moving this blog from
[tinkerer](http://tinkerer.me/) to
[Hakyll](http://jaspervdj.be/hakyll/).

Agda Syntax Highlighting in Pandoc
----------------------------------

The first issue one runs into is that
Hakyll does not support Agda syntax highlighting.
It turns out that Hakyll uses pandoc, which uses highlighting-kate,
which uses KDE's kate.
The solution to this is to write an Agda syntax highlighting file for kate.
The syntax file is currently up for
[review on the bug tracker](https://git.reviewboard.kde.org/r/117167/).

The second issue one runs into is that
highlighting-kate does not support unicode.
In fact, it turns out that the regular expression engine regex-pcre-builtin,
which drives highlighting-kate, does not handle unicode.
The patch that fixes this, at least for the `ByteString`{.haskell} backend,
[is already merged](https://github.com/audreyt/regex-pcre-builtin/pull/4).

Finally, the `ByteString`{.haskell} backend should be used
instead of the `String`{.haskell} backend
in highlighting-kate.
This is also up for
[review on the bug tracker](https://github.com/jgm/highlighting-kate/pull/42).

Note that there are some other solutions on the web,
notably
[hakyll-agda](http://hackage.haskell.org/package/hakyll-agda) and
[PandocAgda](http://hackage.haskell.org/package/PandocAgda),
to enable Agda
syntax highlighting with Hakyll,
however these solutions will only highlight complete Agda modules,
and they will go through the Agda compiler itself
thereby requiring the full Agda compiler to be installed.
Unfortunately, it seems impossible (or at least highly non-trivial)
to install Agda and Hakyll simultaneously:
cabal keeps telling me their dependencies cannot be simultaneously satisfied.
Also, only being able to highlight full modules seems overly restrictive.

Setting Up a Hakyll Sandbox
---------------------------

Anyway, until all patches are merged,
for now, here is how to enable Agda syntax highlighting for Hakyll,
using a sandbox (you probably do not want your own build of
highlighting-kate to interfere with the rest of your Haskell installation):

``` {.sourceCode .bash}
git clone git@github.com:audreyt/regex-pcre-builtin.git
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
cabal install -j ../highlighting-kate ../regex-pcre-builtin hakyll
```

Now you have an Agda syntax enabled Hakyll sandbox
that can be activated with

``` {.sourceCode .bash}
source hakyll/.hsenv/bin/activate
```

Hopefully all patches get merged soon.
Enjoy!
