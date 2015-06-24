---
title: Understanding Hakyll
author: Matthias C. M. Troffaes
tags: hakyll
---

As I was playing around to get Hakyll to produce a page containing my
publications, I soon realized that a more thourough understanding of
the internals of Hakyll would be required. This post aims to go
through the basic building blocks to get to building a custom
publication page. First I strongly recommend reading
[Jasper Van der Jeught's tutorials](http://jaspervdj.be/hakyll/tutorials.html),
as well as
[Brent Yorgey's guide to the Hakyll module zoo](http://jaspervdj.be/hakyll/tutorials/a-guide-to-the-hakyll-module-zoo.html).

Rules
=====

Hakyll's starting point is the function `hakyll`:

``` {.sourceCode .haskell}
hakyll :: Rules a -> IO ()
```

Hopefully you know what the `IO`{.haskell} monad is, and that the
`hakyll`{.haskell} function is called from `main`{.haskell} (the main
entry point of your site's build script), so we are left to understand
the `Rules` class.

The `Rules`{.haskell} type is a monad, which we can think of as an
abstract object storing all the rules by which a website is built from
source. I am not sure why this class has a type variable; all commonly
used functions return `Rules ()`{.haskell}
as can be seen from the
[Hakyll.Core.Rules](http://jaspervdj.be/hakyll/reference/Hakyll-Core-Rules.html)
documentation.

Anyway, let us start with a simple rule, say one which looks up a source file
and then does something with it. The function for this rule is

``` {.sourceCode .haskell}
match :: Pattern -> Rules () -> Rules ()
```

So, to produce a rule with this function to be fed into our
`hakyll`{.haskell} function, we need to provide a `Pattern`{.haskell}
which identifies the source files, and another rule which will tell
Hakyll what to do with these source files.

What sort of rules operate on source files? Well, there are two sort
of rules for source files: route rules, and compile rules. If the
source file has a corresponding destination file, then you need a
route rule to specify what that destination file is.  You always need
a compilation rule to tell Hakyll how to process the source.
The relevant functions for creating these rules are:

``` {.sourceCode .haskell}
route :: Routes -> Rules ()
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules () 
```

Typically, we compose these rules monadically using `>>`{.haskell},
and then feed the resulting combined rule into `match`{.haskell}.
Let us first focus on each of these rules separately.

Routes
======

The `route`{.haskell} function is quite easy.
As can be seen from its function signature above,
we simply feed it a `Routes`{.haskell} object to create the rule.
Looking at the
[Hakyll.Core.Routes](http://jaspervdj.be/hakyll/reference/Hakyll-Core-Routes.html)
documentation, we can use for example:

``` {.sourceCode .haskell}
idRoute :: Routes
```

which basically says that the destination file has the same name as
the source file. Another commonly used route is:

``` {.sourceCode .haskell}
setExtension :: String -> Routes
```

which says that the destination file has the same name as the source file,
but with a different extension.

What is a route? Well, looking at the
[implementation](http://jaspervdj.be/hakyll/reference/src/Hakyll-Core-Routes.html#Routes),
we see that it essentially corresponds to a function with the
following signature:

``` {.sourceCode .haskell}
unRoutes :: RoutesRead -> Identifier -> IO (Maybe FilePath, UsedMetadata)
```

The `RoutesRead`{.haskell} argument is a bit mysterious for now, but
the `Identifier`{.haskell} essentially corresponds to the source file
path (see
[Hakyll.Core.Identifier](http://jaspervdj.be/hakyll/reference/Hakyll-Core-Identifier.html)).
We see that a route is a function which turns source paths (presumably
provided by the rule in which it is embedded) into a `Maybe
FilePath`{.haskell}, which is probably the destination path (embedded
into a `Maybe`{.haskell} to allow the rule to fail), and a boolean
value `UsedMetadata`{.haskell} which is also a bit mysterious so we
will not try to understand this now. In any case, omitting some
details, routes turn source paths into destination paths.

Finally, note that `Routes`{.haskell} is a `Monoid`{.haskell}.  That
means that there is an empty route (created with `mempty`), and that
we can compose routes together (with `mappend`).  To understand this,
you have to dig into the [source
code](http://jaspervdj.be/hakyll/reference/src/Hakyll-Core-Routes.html#instance%20Monoid%20Routes).
It seems that composing routes with `mappend`{.haskell} results in
trying each of the routes, and stopping as soon as a route succeeds.
The `mempty`{.haskell} rule is likely used
when no route is specified.

Note that neither `idRoute`{.haskell} nor `setExtension`{.haskell} can
ever fail. To leave this stone not unturned,
let us look at a rule that can fail:

``` {.sourceCode .haskell}
matchRoute :: Pattern -> Routes -> Routes
```

We will not use this immediately, but the meaning from the function
signature should be obvious: you can use this route
to filter source files by a specific pattern, and then to say what the route
should be for files that have this pattern. The route will fail
on files that do not match the pattern. We can use `mappend`{.haskell}
to chain `matchRoute` to another route that will be used for files that
do not match the pattern, for example:

``` {.sourceCode .haskell}
matchRoute "*hello*" (setExtension "html") `mappend` idRoute
```

which will change the extension of all file paths that contain the
string `hello` to `html`, but leave all other file paths unchanged.

Compilers
=========

We now look at the function which produces a rule for compiling a source file:

``` {.sourceCode .haskell}
compile :: (Binary a, Typeable a, Writable a) => Compiler (Item a) -> Rules () 
```

This is a bit more complicated.
We see that we need to feed it a `Compiler (Item a)`{.haskell} object
in order to produce the rule. Let us analyse each of the parts.
First of all, the type variable `a` is something of a binary writable type.
Most likely, this must correspond to the contents of the destination file.
What is an `Item a`{.haskell}?
Looking at
[Hakyll.Core.Item](http://jaspervdj.be/hakyll/reference/Hakyll-Core-Item.html),
it basically a container class that contains something of type `a`{.haskell}
along with an `Identifier`{.haskell}.
We already know that identifiers in Hakyll are file paths,
so our `Item a`{.haskell} simply stores a file path along with its contents.

Now, let us analyze `Compiler (Item a)`{.haskell}. The source code for
the `Compiler`{.haskell} class is quite complicated, so instead of
understanding the full implementation, let us merely try to understand
the functions that produce a `Compiler`{.haskell}.  Later, we will also try
to understand the monadic nature of the `Compiler`{.haskell} class.

The simplest function to produce a compiler is undoubtedly the
following one:

``` {.sourceCode .haskell}
getResourceBody :: Compiler (Item String)
```

This function simply takes the source file, and stores it into a string.
Note that there is also the function:

``` {.sourceCode .haskell}
getResourceString :: Compiler (Item String)
```

The documentation is sadly lacking at this point to explain the
difference, but
[Hakyll issue #228](https://github.com/jaspervdj/hakyll/issues/228)
provides an explanation: `getResourceString`{.haskell} will return the
full contents of the matched source file inside a string, whilst
`getResourceBody`{.haskell} will do the the same thing as
getResourceString, but without metadata preamble, if there was one.
In good open source spirit, I've [submitted a
patch](https://github.com/jaspervdj/hakyll/pull/354) to clear this up
in the documentation.

A First Simple Working Example
==============================

Say we have an `index.html` file containing:

``` {.sourceCode .html}
<h1>Hello World</h1>
```

Then, the following `site.hs` file will simply copy this source file
to the `_site` folder:

``` {.sourceCode .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll (match "index.html" (route idRoute >> compile getResourceString))
```

The only new things here are
the `OverloadedStrings` directive, and
the use of the `>>` operator to join the rules together.
The `OverloadedStrings` directive allows us to specify patters simply
via strings. Remember that we said that rules are monads? Essentially,
each rule stores some information which can be used by the next rule.
If you have forgotten what monads are, you can think of `>>` as an
operation which causes different rules to be executed in order,
but not necessarily in the order that the rules are specified.
Hakyll does dependency tracking and will figure out the correct order for you
through monad magic.
How that magic actually works is hard to decipher from the source code,
but fortunately an understanding of it is not required
even for moderately advanced usage of Hakyll.

We can clean this up a bit:

``` {.sourceCode .haskell}
{-# LANGUAGE OverloadedStrings #-}
import Hakyll
main = hakyll $ do
    match "index.html" $ do
         route idRoute
         compile copyFileCompiler
```

Using do notation allows us to chain many more rules together in a neat way.
Note that the order of the arguments of the match function (and of
similar functions) are always such that the non-rule arguments come
first, which helps to keep the code clean.
Also note that we did not use `getResourceString`{.haskell} just to copy
the contents over. Here, the file is small so it will not matter much,
however for plain copying you are better of using

``` {.sourceCode .haskell}
copyFileCompiler :: Compiler (Item CopyFile) 
```

from [Hakyll.Core.File](http://jaspervdj.be/hakyll/reference/Hakyll-Core-File.html).
This avoids having to read the entire file into a string.

We will stop here for now. Next on the menu is doing some more
advanced compile actions, and trying to write our own Hakyll compiler
to do some really interesting things, such as producing publication lists.
