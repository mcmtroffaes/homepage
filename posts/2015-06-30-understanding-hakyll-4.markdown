---
title: Understanding Hakyll (Part 4)
author: Matthias C. M. Troffaes
tags: hakyll, pandoc
---

In the previous posts, we wrote a simple bibtex to html compiler for Hakyll.
In his post, we will extend this compiler to support filtering and embedding
into other documents.

Filtering
=========

Let us start with the filtering job first. We can make use of the standard
`filter`{.haskell} function to select a particular subset of references:

``` {.sourceCode .haskell}
filter :: (a -> Bool) -> [a] -> [a]
```

This function takes another function which returns `True`{.haskell} on
all elements of a list that need to be retained. So, for example, to
get the list of all journal and conference papers by particular authors, say
whose last name is "Troffaes", we write the following functions:

``` {.sourceCode .haskell}
isArticleJournal :: Reference -> Bool
isArticleJournal ref = refType ref == ArticleJournal

isPaperConference :: Reference -> Bool
isPaperConference ref = refType ref == PaperConference

isTroffaes :: Agent -> Bool
isTroffaes ag = familyName ag == "Troffaes"

article :: Reference -> Bool
article ref = (any isTroffaes (author ref)) && isArticleJournal ref

conf :: Reference -> Bool
conf ref = (any isTroffaes (author ref)) && isPaperConference ref
```

In the previous post, the function which processed the bibtex file was:

``` {.sourceCode .haskell}
readPandocBiblioList :: Item CSL -> Item Biblio -> Compiler (Item Pandoc)
```

We must somehow include a filter function,
and update the implementation to use the filter.
Instead of doing all the work in `readPandocBiblioList`{.haskell},
we will decompose the task of filtering into a separate function:
we will compile the bibliography into a list of references,
and then compile the list of references into Pandoc.
This allows us to customize the list of references
in any way we want.

``` {.sourceCode .haskell}
readBiblio :: Item Biblio -> Compiler (Item [Reference])
readBiblio ibiblio = makeItem refs where Biblio refs = itemBody ibiblio

readPandocReferences :: Item CSL -> Item [Reference] -> Compiler (Item Pandoc)
readPandocReferences icsl irefs = do
    istyle <- styleCompiler icsl
    let style = itemBody istyle
        refs = itemBody irefs
        formatted = processBibliography procOpts style refs
        blocks = map (return . Plain . CSL.Pandoc.renderPandoc style)
                 $ formatted
        pandoc = Pandoc nullMeta [BulletList blocks]
    makeItem pandoc

renderPandocReferences :: Item CSL -> Item [Reference] -> Compiler (Item String)
renderPandocReferences csl refs = do
    pd <- readPandocReferences csl refs
    return (writePandoc pd)
```

A function

``` {.sourceCode .haskell}
compileReferences :: Compiler (Item [Reference])
```

might make more sense than our `readBiblio`{.haskell}, but this would
require `Reference`{.haskell} to be an instance of
`Binary`{.haskell}. Also, other functions may require the
`Biblio`{.haskell} object directly, rather than just the list of
references.

A very simple usage of our new functions would then be:

``` {.sourceCode .haskell}
compile $ do
    csl <- load $ fromFilePath "style.csl"
    bib <- load $ fromFilePath "refs.bib"
    refs <- readBiblio bib
    refs2 <- makeItem $ filter conf $ itemBody refs
    html <- renderPandocReferences csl refs2
    return html
```

This will render all references that satisfy the `conf`{.haskell}
condition (which we wrote earlier) as html.

Embedding
=========

To embed the generated html into another html file,
we can make use of Hakyll's template system,
which we already touched upon earlier.
To create new template variables, Hakyll requires us to write
a new `Context`{.haskell} type:

``` {.sourceCode .haskell}
referencesFilterContext :: String -> (Reference -> Bool) -> Context a
referencesFilterContext name condition = field name $ \item -> do
    csl <- load $ fromFilePath "style.csl"
    bib <- load $ fromFilePath "refs.bib"
    refs <- readBiblio bib
    refs2 <- makeItem $ filter condition $ itemBody refs
    html <- renderPandocReferences csl refs2
    return (itemBody html)
```

Now we can use this as follows:

``` {.sourceCode .haskell}
referencesContext =
    referencesFilterContext "conferencepapers" conf `mappend`
    referencesFilterContext "journalarticles" article `mappend`
    defaultContext

main = hakyll $ do
    match "style.csl" $ compile cslCompiler
    match "refs.bib" $ compile biblioCompiler
    match "refs.html" $ compile templateCompiler
    create ["index.html"] $ do
         route idRoute
         compile $ do
             html1 <- makeItem ""
             html2 <- loadAndApplyTemplate "refs.html" referencesContext html1
             return html2
```

where `refs.html` could be as follows:

```
<h2>Journal Articles</h2>

$journalarticles

<h2>Conference Papers</h2>

$conferencepapers
```

The [full source can be found on github](https://github.com/mcmtroffaes/homepage/tree/master/posts/2015-06-30/).

That's it for now!

All that remains is to see if upstream
is interested in merging some of these functions.
