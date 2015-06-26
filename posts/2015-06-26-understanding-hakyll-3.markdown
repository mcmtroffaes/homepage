---
title: Understanding Hakyll (Part 3)
author: Matthias C. M. Troffaes
tags: hakyll, pandoc
---

In the previous posts, we discussed Hakyll's basic compilers in some
detail.  We also saw how to use do notation to keep our code tidy.  In
this post, we will build a new compiler from scratch.  Our aim is to
produce an html file that contains a list of references generated from
a bibtex file.

Designing a Hakyll Bibtex to Html Compiler
==========================================

What functions do we need for our compiler?
We take a similar approach as the pandoc compiler.
First, we need a function to read the bibtex file into
an appropriate data structure, and then we need a function
to turn that data structure into an html string.

``` {.sourceCode .haskell}
readBiblio :: Item String -> Compiler (Item Biblio)
writeBiblio :: Item Biblio -> Item String
```

The implementation is then simply:

``` {.sourceCode .haskell}
compileBiblio :: Compiler (Item String)
compileBiblio = do
    src <- getResourceBody
    bib <- readBiblio src
    return (writeBiblio bib)
```

In fact, Hakyll already exposes a few functions to work with bibliographies
(see [Hakyll.Web.Pandoc.Biblio](http://jaspervdj.be/hakyll/reference/Hakyll-Web-Pandoc-Biblio.html)):

``` {.sourceCode .haskell}
cslCompiler :: Compiler (Item CSL)
biblioCompiler :: Compiler (Item Biblio)
readPandocBiblio :: ReaderOptions -> Item CSL -> Item Biblio -> Item String -> Compiler (Item Pandoc)
pandocBiblioCompiler :: String -> String -> Compiler (Item String)
```

The only really new thing here is the `CSL`{.haskell} type:
this type is used to store styles for bibliographies.
Pandoc (or rather, Pandoc's citeproc extension) can use
csl style files to format bibtex entries for us;
obviously we will want to make use of that.

Note the type signature of `cslCompiler`{.haskell}
and `biblioCompiler`{.haskell}: they do not take an `Item String`{.haskell}
argument to specify the source code, unlike say `readPandoc`{.haskell}.
I am not sure what is behind this design decision, but it means
that we cannot use these functions to, say, process the outcome of another
compile step as a bibtex file or as a csl file.
However, you probably don't want to do this anyway in normal use cases,
so it does not really bother us.

Another important thing to note is that the `CSL`{.haskell} class does
not actually expose the csl `Style`{.haskell} data; instead it is
only used as a placeholder to refer to the raw unparsed csl file itself.
The reason behind this design decision
is apparently that `Style`{.haskell} is not an instance of `Binary`{.haskell},
which is required for the outcomes of hakyll's `compile`{.haskell} function.
We can write a simple compiler to extract the csl `Style`{.haskell}
from a `CSL`{.haskell} item on the fly:

```
styleCompiler :: Item CSL -> Compiler (Item Style)
styleCompiler csl = do
    style <- unsafeCompiler
             $ readCSLFile Nothing . toFilePath . itemIdentifier $ csl
    makeItem style
```

This of course means that we will parse the csl file
every time the style is required.
A better solution would be to make
`Style`{.haskell} an instance of `Binary`{.haskell}.
Looking at the source code, `Style`{.haskell}
is a rather complex data structure, so we will not pursue this further here.

So, at this point,
what do we still need to implement? We can use `cslCompiler`{.haskell},
`styleCompiler`{.haskell}, and `biblioCompiler`{.haskell}
to compile the csl and bibtex files.
We then need a function that takes these data structures and
produces a `Pandoc`{.haskell} structure, which we can turn into html
with `writePandoc`{.haskell}.

So, we need to implement this function:

``` {.sourceCode .haskell}
readPandocBiblioList :: Item CSL -> Item Biblio -> Compiler (Item Pandoc)
```

which is really almost identical to Hakyll's
`readPandocBiblio`{.haskell}, the only difference being that it does
not take a source file but instead will spit out some code for all
references in the bibtex file, rendering them as a simple Pandoc list.

With that function, we can then implement our bibliography writer as:

``` {.sourceCode .haskell}
renderPandocBiblioList :: Item CSL -> Item Biblio -> Compiler (Item String)
renderPandocBiblioList csl bib = do
    pd <- readPandocBiblioList csl bib
    return (writePandoc pd)
```

A very simple usage of this function would then be:

``` {.sourceCode .haskell}
compile $ do
    csl <- load $ fromFilePath "style.csl"
    bib <- load $ fromFilePath "refs.bib"
    html <- renderPandocBiblioList csl bib
    return html
```

The new functions used above are

```
load :: (Binary a, Typeable a) => Identifier -> Compiler (Item a)
fromFilePath :: String -> Identifier
```

which both do the obvious thing: `load`{.haskell} loads a previously
compiled item, and `fromFilePath`{.haskell} allows us to
specify the compiled item through its file path.
A word of warning here:
this caching mechanism does not go through the usual type checking.
For example, if we would write

``` {.sourceCode .haskell}
csl <- load $ fromFilePath "refs.bib"
```

and then use `csl`{.haskell} as an `Item CSL`{.haskell} object,
then the Haskell compiler will not tell us that we made an error.
There will be a reasonably informative error at runtime;
sadly the error will not point you to the precise point in the code
where the error was made.

First Implementation
====================

So all that is left is implementation of `readPandocBiblioList`{.haskell}.
Without further ado:

``` {.sourceCode .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Text.CSL
import qualified Text.CSL.Output.Pandoc as CSL.Pandoc
import Text.Pandoc

styleCompiler :: Item CSL -> Compiler (Item Style)
styleCompiler csl = do
    style <- unsafeCompiler
             $ readCSLFile Nothing . toFilePath . itemIdentifier $ csl
    makeItem style

readPandocBiblioList :: Item CSL -> Item Biblio -> Compiler (Item Pandoc)
readPandocBiblioList icsl ibiblio = do
    istyle <- styleCompiler icsl
    let style = itemBody istyle
        Biblio refs = itemBody ibiblio
        formatted = processBibliography procOpts style refs
        blocks = map (return . Plain . CSL.Pandoc.renderPandoc style)
                 $ formatted
        pandoc = Pandoc nullMeta [BulletList blocks]
    makeItem pandoc

renderPandocBiblioList :: Item CSL -> Item Biblio -> Compiler (Item String)
renderPandocBiblioList csl bib = do
    pd <- readPandocBiblioList csl bib
    return (writePandoc pd)

main = hakyll $ do
    match "style.csl" $ compile cslCompiler
    match "refs.bib" $ compile biblioCompiler
    create ["index.html"] $ do
         route idRoute
         compile $ do
             csl <- load $ fromFilePath "style.csl"
             bib <- load $ fromFilePath "refs.bib"
             html <- renderPandocBiblioList csl bib
             return html
```

The hard work is done by `readPandocBiblioList`{.haskell},
which extracts the csl style and bibtex references,
and then formats these into `Pandoc`{.haskell} structure.
The implementation is in essence
a simple application of Pandoc's citeproc extension.

It is imperative that the style file does not print any labels.
Most, if not all, styles will print a label in front of each
bibliographic entry.
So, you can use your favourite style, but you will likely need to
remove the citation label text from the formatting instructions.
A [full example is on github here](https://github.com/mcmtroffaes/homepage/tree/master/posts/2015-06-26/implementation-1).
