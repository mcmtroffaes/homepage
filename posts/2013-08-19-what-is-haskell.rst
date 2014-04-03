---
title: What is Haskell?
author: Matthias C. M. Troffaes
tags: haskell
---

Did you see `John Carmack's keynote at QuakeCon 2013
<http://www.youtube.com/watch?v=Uooh0Y9fC_M>`_?
I was deeply intrigued by his comments about Haskell,
and by his statement that every programmer should learn Haskell
to write more so-called pure functional code,
thereby reducing bugs and increasing productivity.

Apparently, **Haskell is beautiful**.
Everybody says.
More than enough reason for me!
So, last week, I set myself the task to learn Haskell.
I worked through Chapters 1--7 of
`Real World Haskell <http://book.realworldhaskell.org/read/>`_
in reasonable depth,
and started scratching the surface of Chapters 8--15.
This post is a brief summary of my experience so far.

Then, first, what is Haskell?
Haskell is a programming language with a particular set of restrictions:

#. All values are *immutable* (i.e. ``const`` if you come from C++).
   This may seem weird at first, but in fact, it is not as bad as it sounds.

#. All values are either
   *data*
   (integers, characters, but also lists, records, unions, and so on),
   or *functions*.
   These values can be composed together to make *expressions*
   (as in any other programming language).
   Classes as you may know them from the Object Oriented world
   do not exist in Haskell.

#. The compiler assumes that functions are
   `pure <http://en.wikipedia.org/wiki/Pure_function>`_:
   if you call the same pure function with the same arguments,
   then you will always get the same result back.
   In other words, pure functions are functions in a mathematical sense.
   For example, ``getch`` is an impure function in C:
   its result is not deterministic.

#. All values are *statically typed*, that is,
   their type is fixed at compile time.

#. At runtime, a Haskell program simply evaluates the expression
   called ``main``.

#. Evaluation is *lazy*, that is, expressions are only evaluated when
   their result is required for the evaluation of ``main``.

#. There is no specified order in which subexpressions
   are evaluated, i.e. the compiler may resolve the evaluation in any
   way deemed appropriate (potentially, even in parallel). [1]_

For someone coming from, say, Python or C++,
some of these restrictions are highly peculiar, specifically,
immutability, laziness,
and execution through evaluation of a single expression.
In more traditional languages,
you essentially tell the computer what to do, step by step.
In Haskell, you specify an expression you want to be evaluated,
and the compiler figures out the steps to arrive at the result.

All in all, the Haskell approach
has a number of important practical implications:

#. Due to delegating more work to the compiler,
   Haskell programs
   are typically much shorter than,
   say, their equivalent Python/C++/<insert-your-language-here> implementations.

#. Haskell encourages you to write beautiful code:
   due to its emphasis on purity,
   there is a natural focus
   on hierarchically decomposing the
   problem into simple self-contained expressions,
   often leading to highly modularized
   self-explanatory code.
   Surely, that is a good thing!

#. We cannot completely kick the habit of impurity,
   because I/O is an essential aspect of nearly every program!
   Nevertheless, at first sight,
   non-trivial I/O
   with a compiler that expects only pure functions
   and immutable data, appears to be difficult, if not impossible.

Concerning the latter,
it is wildly surprising that Haskell can work
with impure functions in a sane way.
To do so, we have to rely on a generic but very useful
system called a *monad*.
I hope to write a bit more about monads in the next few posts.


.. [1] This is not entirely true.
       Some functions guarantee that certain of their arguments
       are evaluated first,
       even if the evaluation of those arguments is not needed.
       The only purpose this serves is
       to help the compiler to produce more efficient machine code.
       In principle,
       a really clever compiler would not need such hints.
       Also see:
       http://www.haskell.org/haskellwiki/Performance/Strictness
