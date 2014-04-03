---
title: Moving to Hakyll and Agda Syntax Highlighting in Pandoc
author: Matthias C. M. Troffaes
tags: hakyll, kate, agda
---

Hakyll
------

I've just finished moving this blog from
[tinkerer](http://tinkerer.me/) to
[Hakyll](http://jaspervdj.be/hakyll/),
in order to make it easier to integrate it with other parts of my website.

Agda Syntax Highlighting in Pandoc
----------------------------------

One issue I ran into is that Hakyll does not support Agda syntax highlighting.
It turns out that Hakyll uses pandoc, which uses highlighting-kate,
which uses KDE's kate.
So I ended up writing an Agda syntax highlighting file for kate.
The code is currently up for
[review on the bug tracker](https://git.reviewboard.kde.org/r/117167/).

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

Here is how to enable Agda syntax highlighting for Hakyll,
using a sandbox (you probably do not want your own build of
highlighting-kate to interfere with the rest of your Haskell installation):

``` {.sourceCode .bash}
hsenv
source .hsenv/bin/activate
git clone git://github.com/mcmtroffaes/highlighting-kate.git
git checkout agda-syntax
cd highlighting-kate
cabal install -j hxt regex-posix utf8-string
make prep
cabal install -j
cabal install -j hakyll
```

Now you have an Agda syntax enabled Hakyll sandbox.

Hopefully the patch gets merged soon into kate,
and then into highlighting-kate. Enjoy!