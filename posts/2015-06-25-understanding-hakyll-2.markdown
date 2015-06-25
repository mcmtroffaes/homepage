---
title: Understanding Hakyll (Part 2)
author: Matthias C. M. Troffaes
tags: hakyll, pandoc
---

In the previous post, we discussed the very basic building blocks of
Hakyll, namely the `hakyll`{.haskell} function, and the rules for
identifying source files, and for specifying the name and content of
their corresponding destination files. The example we ended with was not
particularly exciting (it merely copied a file).
In this post, we cover some further compiler functions that allow us to
do more interesting things.

In doing so, we have to get into monads and functors, so whilst at it
we will be doing some interesting mathematics as well!

Pandoc Compilers
================

Writing your web content directly as html is tedious.
Hakyll allows you to write your content in practically any format,
through the absolutely awesome [pandoc](http://pandoc.org/).
The relevant functions that glue pandoc and hakyll together are
in
[Hakyll.Web.Pandoc](http://jaspervdj.be/hakyll/reference/Hakyll-Web-Pandoc.html).
The main function you will want to use is

``` {.sourceCode .haskell}
pandocCompiler :: Compiler (Item String)
```

This compiler will simply process the source file with pandoc, and
turn it into an html string, which can then be further processed
should this be desired.
As we wish to write our own compiler,
it is instructive to understand how this compiler is built from more
basic pieces.

A first basic pandoc compiler function exposed by Hakyll is:

``` {.sourceCode .haskell}
readPandoc :: Item String -> Compiler (Item Pandoc)
```

This will take a string and compile it with pandoc into the native
`Pandoc`{.haskell} format,
for example using Pandoc's `readMarkdown`{.haskell} or a similar function,
ready for further processing.
As reading might fail due to parsing errors or other errors,
the function returns a `Compiler (Item Pandoc)`{.haskell} object rather than
just an `Item Pandoc`{.haskell} object.
Also note that usually you will not have a raw `Item String`{.haskell} at your
disposal, but instead you will have a `Compiler (Item
String)`{.haskell}.  So, usually, we have to lift
`readPandoc`{.haskell} into the compiler monad using:

``` {.sourceCode .haskell}
(>>=) :: m a -> (a -> m b) -> m b
```

will take care of this for us. For instance, we could use `readPandoc`{.haskell}
together with

``` {.sourceCode .haskell}
getResourceBody :: Compiler (Item String)
```

in the following way:

``` {.sourceCode .haskell}
getResourceBody >>= readPandoc
```

The type of this expression is `Compiler (Item Pandoc)`{.haskell}, so
we cannot use this yet to create a compilation rule with

``` {.sourceCode .haskell}
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules ()
```

because `Pandoc`{.haskell} is not a writable type.  We have to convert it to
an html string first, and obviously that will be writable.
The function which converts `Pandoc`{.haskell} into html is exposed by
Hakyll as:

``` {.sourceCode .haskell}
writePandoc :: Item Pandoc -> Item String
```

which is a simple wrapper around Pandoc's `writeHtmlString`{.haskell}.
A naive implementation of `pandocCompiler`{.haskell}
could simply consist of

``` {.sourceCode .haskell}
pandocCompiler = fmap writePandoc (getResourceBody >>= readPandoc)
```

where we write `fmap writePandoc`{.haskell} to lift
`writePandoc`{.haskell} to the `Compiler`{.haskell} monad.
If you have forgotten about `fmap`{.haskell},
then now is a good moment to brush up your knowledge of
[functors](http://learnyouahaskell.com/functors-applicative-functors-and-monoids).

Hakyll also exposes the function:

``` {.sourceCode .haskell}
renderPandoc :: Item String -> Compiler (Item String)
```

which is basically `readPandoc`{.haskell} followed by `writePandoc`{.haskell},
so a naive implementation would be

``` {.sourceCode .haskell}
renderPandoc src = fmap writePandoc (readPandoc src)
```

With this function, we could also implement `pandocCompiler`{.haskell} as

``` {.sourceCode .haskell}
pandocCompiler = getResourceBody >>= renderPandoc
```

The actual implementations of these functions is quite a bit more complicated
to allow for more abstractions, but the basic ideas remain the same.

Simple Example
==============

We can now write a hakyll script that will convert every markdown file
in the current directory to an html file:

``` {.sourceCode .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll $ do
    match "*.md" $ do
         route (setExtension "html")
         compile pandocCompiler
```

Templates
=========

The script works, but it has one obvious limitation: the pandoc
compiler produces html from source, but this html is not properly embedded,
and also does not refer to any css style file. Ideally we would like to put some
extra html code before and after the generated html code to make for a proper
html file, with reference to the appropriate css file(s).

First, we create a template, say `default.html`,
which contains the surrounding html code:

``` {.sourceCode .html}
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
        <title>$title$</title>
        <link rel="stylesheet" type="text/css" href="/default.css" />
    </head>
    <body>
        <div id="header">
        <div id="navigation">
            <a href="/">Home</a>
            <a href="/about.html">About</a>
        </div>
        </div>

        <div id="content">
            <h1>$title$</h1>

            $body$
        </div>
        <div id="footer">
            Site generated by
            <a href="http://jaspervdj.be/hakyll">Hakyll</a>
        </div>
    </body>
</html>
```

We want to tell Hakyll to compile the documents, but then to use the
compiled result in the above template, that is, to replace `$title$`
and `$body$` with the the title and body of the compiled source.
The functions we need to achieve this are:

``` {.sourceCode .haskell}
templateCompiler :: Compiler (Item Template) 
loadAndApplyTemplate :: Identifier -> Context a -> Item a -> Compiler (Item String)
```

The first function simply compiles a template.  The second function
loads a compiled template through its identifier (in practice, this is
its filename), and applies it to an item (remember that an item in
Hakyll is simply a file path plus some content).
That item can be the source file directly, but it can also be a string that
results from a Pandoc compiler, or some other string compiler.

The most important and least trivial bit is the `Context`{.haskell}.
This object determines how template variables are substituted.
We will use the `defaultContext`{.haskell}, which makes available
`$title$` and `$body$` (among a few more).
More about contexts will be discussed later.

Simple Example Revisited
========================

``` {.sourceCode .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll $ do
    match "default.html" $ compile templateCompiler
    match "default.css" $ do
        route idRoute
        compile copyFileCompiler
    match "*.md" $ do
         route (setExtension "html")
         compile $ pandocCompiler
             >>= loadAndApplyTemplate "default.html" defaultContext
             >>= relativizeUrls
```

Note that the rule for `default.html` does not have a route
associated with it, because it does not need to be copied to the
website.

All generated files will now have
an appropriate title, header, footer, and stylesheet.
Whilst at it, we have not only applied `loadAndApplyTemplate`{.haskell},
but also `relativizeUrls`{.haskell}, which will replace all absolute
urls with relative ones, which makes it easier to relocate your website.

It is good to understand the type signatures of all expressions in the
markdown compile step, and how this fits in with the monad operator
`>>=`{.haskell}:

* `pandocCompiler`{.haskell} has type `Compiler (Item String)`{.haskell} and
  creates a compiler that turns the source file into html.

* `loadAndApplyTemplate "default.html" defaultContext`{.haskell} has
  type `Item String -> Compiler (Item String)`{.haskell} and creates a
  compiler that applies a template onto a string (in this case, the
  html string resulting from `pandocCompiler`{.haskell}).

* `relativizeUrls`{.haskell} also has type `Item String -> Compiler
  (Item String)`{.haskell} and relativizes the urls in the given
  string (in this case, the html resulting from the previous two
  steps).

This is a very common and useful pattern in Hakyll: to specify the
compile steps, start with a function that creates a `Compiler (Item
String)`{.haskell}, followed by a series of functions of type `Item
String -> Compiler (Item String)`{.haskell}, all joined with the monad
operator `>>=`{.haskell}. Note that we can also write these compile
steps with do notation:


``` {.sourceCode .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll $ do
    match "default.html" $ compile templateCompiler
    match "default.css" $ do
        route idRoute
        compile copyFileCompiler
    match "*.md" $ do
         route (setExtension "html")
         compile $ do
             html1 <- pandocCompiler
             html2 <- loadAndApplyTemplate "default.html" defaultContext html1
             html3 <- relativizeUrls html2
             return html3
```

This is obviously a lot more verbose, but it is perhaps easier to
understand for novice users. For more complex operations, you would
use do notation anyway.

That's it for now. Next, we will try to write our own Hakyll compiler
with a custom context, to handle things like publication lists.
