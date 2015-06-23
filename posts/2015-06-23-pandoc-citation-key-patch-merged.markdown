---
title: Citation Key Patch Merged Into Pandoc
author: Matthias C. M. Troffaes
tags: pandoc
---

One of the last hurdles preventing me to move my homepage to Hakyll was that
pandoc did not support citation keys which start with a digit.
Unfortunately, [my bibtex file](https://github.com/mcmtroffaes/bibliography)
has all citation keys in this format.
I [sent a patch to pandoc](https://github.com/jgm/pandoc/pull/1954)
back in February,
which was eventually merged in April.

All that's left now is writing up the
Haskell code to extract a categorised publication list from a bibtex file.
