---
title: Citations in Hakyll using Bibtex
author: Matthias C. M. Troffaes
tags: hakyll
---

Trying to move my homepage to Hakyll, to my pleasant surprise I found
out that pandoc, which drives Hakyll, supports citations. The
documentation on how to use this feature from within Hakyll is rather
sparse. After some searching, I came across the source of Julien
Tanguy's website, and in particular, his [site script](https://github.com/jtanguy/julien.jhome.fr/blob/41da0d2acd887ade65d3b4771b76a37ed30bbd1b/bin/blog.hs).
In a nutshell, you need to do the following.

First, download a csl file which encodes the referencing style. Zotero has
an excellent [index](https://www.zotero.org/styles).
I chose [Elsevier (numeric, with titles, sorted alphabetically)](https://github.com/citation-style-language/styles/blob/master/elsevier-with-titles-alphabetical.csl).

Then, you need to tell Hakyll to do a number of things.
It needs to compile your csl and bibtex file(s).
If you put all your bibtex files in the `bib/` folder,
and all your csl files in the `csl/` folder,
then you need, in your `main = hakyll $ do`{.haskell} block:

``` {.sourceCode .haskell}
match "bib/*" $ compile biblioCompiler
match "csl/*" $ compile cslCompiler
```

Next, write a function that loads the compiled csl and bib files, and
runs the pandoc compiler using those files:

``` {.sourceCode .haskell}
bibtexCompiler :: String -> String -> Compiler (Item String)
bibtexCompiler cslFileName bibFileName = do
    csl <- load $ fromFilePath cslFileName
    bib <- load $ fromFilePath bibFileName
    liftM writePandoc
        (getResourceBody >>= readPandocBiblio def csl bib)
```

This is the least trivial bit. It would be nice if Hakyll provided
this function by default;
in good open source spirit, I
[submitted a patch](https://github.com/jaspervdj/hakyll/pull/327).

Finally, compile any document that uses a bibliography with
`bibtexCompiler`{.haskell} instead of `pandocCompiler`{.haskell},
and specify the relevant bib and csl files.
For example, in your `main = hakyll $ do`{.haskell} block:

``` {.sourceCode .haskell}
match "research.md" $ do
    route   $ setExtension "html"
    compile $ bibtexCompiler
              "csl/elsevier-with-titles-alphabetical.csl" "bib/research.bib"
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
        >>= relativizeUrls
```

That's it!