---
title: The List Monad
author: Matthias C. M. Troffaes
tags: haskell
---

In the last post, we briefly looked at functions.
Today, we will use our learnings
to investigate a first simple example of monads,
namely, lists.

Lists are as fundamental to Haskell as functions.
Indeed, because everything is a function in Haskell,
you may have wondered how one writes loops in such language.
In a purely functional language,
loops are naturally translated to operations on lists.

List Syntax
-----------

First, some syntax. The empty list is denoted as

.. code:: haskell

  []

Lists can, obviously, contain as many elements as we want.
In fact, in Haskell, a list can even take any countable number of elements.
This is possible because Haskell is lazy.
To compare with Python, lists are like Python generators,
which can also go on to countable infinity.
Finite lists are denoted as follows:

.. code:: haskell

  [2,3,5,7,11,13]

In Haskell, a list consisting of characters is a *string*:

.. code:: haskell

  ['h','e','l','l','o']

Because this notation is rather heavy,
we can write the above list also as

.. code:: haskell

  "hello"

which is simply syntactic sugar.

The arithmetic progression, say, starting at 5, with step size 2, and
ending at 21, is denoted as

.. code:: haskell

  [5,7..21]

We can also denote infinite arithmetic progressions:

.. code:: haskell

  [5,7..]

In many (or perhaps most?) languages, the fundamental operation to
extend lists is appending, that is, extending at the end of the
list---for example, in Python, it is very natural to use the ``+=``
operator.
In Haskell however, *prepending* is the fundamental operation to
extend lists. The reason for this is straightforward: if you allow infinite
lists, prepending is the only sensible extension operator.
The ``:`` (colon) operator denotes prepend:

.. code:: haskell

  1:[3,4,5] == [1,3,4,5]

How do we denote lists in type signatures? Here is an example:

.. code:: haskell

  func :: [Int] -> Int
  func xs = 3 * sum xs

So, ``[Int]`` denote a list of ``Int`` elements.
Observe that the argument of ``func`` is called ``xs``, rather than ``x``.
It is a useful convention in Haskell code to denote list variables by
``xs``, ``ys``, and so on,
and to denote their elements by ``x``, ``y``, and so on.

In Haskell, all of a list's elements must be of the same type.
For example, we **cannot** write

.. code:: haskell

  [2,'a',"xyz"]

Comprehension
-------------

There is one more notation for lists which is enormously useful:

.. code:: haskell

  [ [n,x,y,z] | n <- [2..15], x <- [1..15], y <- [1..15],
                z <- [1..15], x ^ n + y ^ n == z ^ n]

This will give you all numbers, less than 15,
satisfying the conditions of Fermat's equation :math:`x^n+y^n=z^n`;
for the curious, the answer is

.. code:: haskell

  [[2,3,4,5],[2,4,3,5],[2,5,12,13],[2,6,8,10],[2,8,6,10],
   [2,9,12,15],[2,12,5,13],[2,12,9,15]]

Note that :math:`n` is never larger than 2,
per `Fermat's famous last theorem
<http://en.wikipedia.org/wiki/Fermat%27s_Last_Theorem>`_.
Here is how you could ask Haskell to try to prove the theorem:

.. code:: haskell

  null [ [n,x,y,z] | n <- [3..], x <- [1..], y <- [1..],
                     z <- [1..], x ^ n + y ^ n == z ^ n]

The calculation is still running on my machine---in fact, it will never end,
because Haskell will simply use brute force,
which is of course problematic
when there are infinitely many cases to consider.
In the above, the function

.. code:: haskell

  null :: [a] -> Bool

returns ``True`` if its list argument is empty---remember
that ``a`` is a type variable,
so this function is polymorphic and will work for lists of any type.

Anyway, let us stop this brief digression and get back to topic: monads!

A Poor Man's Monad
------------------

One way to explain monads, is to try to implement
list comprehension by ourselves, using just functions,
aiming to get as close as possible to the list comprehension syntax.
For this purpose, let us investigate a very simple example,
and try to rewrite

.. code:: haskell

  [ x + y ^ 3 | x <- [1,2,3], y <- [-x,x] ]

which results in

.. code:: haskell

  [0,2,-6,10,-24,30]

First, let us tackle each of the parts separately,
namely ``x <- [1,2,3]``, ``y <- [-x,x]``, and ``x + y ^ 3``.
Can we rewrite these as functions?

.. code:: haskell

  funcx :: [Int]
  funcx = [1,2,3]

  funcy :: Int -> [Int]
  funcy x = [-x, x]

  funcfinal :: Int -> Int -> [Int]
  funcfinal x y = [x + y ^ 3]

Note that we do not actually need ``funcx``---we introduce it here
merely for the sake of symmetry. The important observation is
that all these functions produce lists.
If we may get slightly ahead of ourselves,
in light of general monad theory,
what matters here is that all these functions
produce *containers* of the same *form*.

Next, we need a function to combine ``funcx``, ``funcy``, and ``funcfinal``.
Specifically, we wish to *bind* the outcome of ``funcx``
to the input of the function ``funcy``, and then to *bind*
the outcome of both of these to ``funcfinal``.
Here is what you might write in a possible attempt:

.. code:: haskell

  bind :: [Int] -> (Int -> [Int]) -> [Int]
  bind zs f = concat . map f $ zs

In the above, ``map`` applies a function to every element of a list:

.. code:: haskell

  map funcy $ funcx

gives

.. code:: haskell

  [[-1,1],[-2,2],[-3,3]]

The function ``concat`` concatenates this list of lists. Consequently,

.. code:: haskell

  bind funcx funcy

gives

.. code:: haskell

  [-1,1,-2,2,-3,3]

This is not exactly the result we want,
but we are clearly getting close:
we already have a list with six elements.
The elements are ``y`` rather than ``x + y ^ 3``,
because we have not yet used ``funcfinal``.
Can we use ``bind`` to combine ``funcy`` and ``funcfinal``?
Of course! For instance,

.. code:: haskell

  bind (funcy 1) (funcfinal 1)

will give us

.. code:: haskell

  [0,2]

which is the desired result, for ``x = 1``.
The only remaining problem is to feed all values for ``x``
into this expression:

.. code:: haskell

  bind2 f1 f2 x = bind (f1 x) (f2 x)

(The type signature is rather complex, so we have omitted it here.)
To get the final result, we thus apply ``bind`` twice:

.. code:: haskell

  bind funcx $ bind2 funcy funcfinal

This is about as close as we can get to the original expression

.. code:: haskell

  [ x + y ^ 3 | x <- [1,2,3], y <- [-x,x] ]

where
``funcx`` represents ``x <- [1,2,3]``,
``funcy`` represents ``y <- [x,-x]``, and
``funcfinal`` represents ``x + y ^ 3``.
The ``bind`` and ``bind2`` functions are merely glue.

If you followed this far, congratulations!!
You may not realize it yet, but you now know in essence what a monad is.
A monad is a container, along with a higher order function
which binds functions that operate on these containers.
Everything else about monads in Haskell comes down to:

1. adding syntactic sugar to remove the boilerplate in the above code, and
2. generalizing from ``[Int]`` lists to arbitrary containers.

Yippikayee!

Syntactic Sugar
---------------

The aim of this section is
to simplify the structure of our monad code, step by step.

Infix Notation
~~~~~~~~~~~~~~

The first thing we can do is rewrite the glue in infix notation:

.. code:: haskell

  funcx `bind` (funcy `bind2` funcfinal)

For any function ``f`` in Haskell

.. code:: haskell

  x `f` y

is just a shorthand notation for

.. code:: haskell

  f x y

Backticked functions are left-associative.
In the above, we are using the operators in a right-associative way,
thus we need brackets to denote the order of operation.

Lambda Functions
~~~~~~~~~~~~~~~~

To get one more step closer to list comprehension notation,
we would like to get rid of the helper functions.
For this purpose, we can use so-called lambda functions,
which allow us to define anonymous functions directly into our expressions.
Note that the use of lambda functions is somewhat frowned upon,
and are generally only used for very simple functions:

.. code:: haskell

  [1,2,3] `bind` ((\x -> [-x,x]) `bind2` (\x y -> [x + y ^ 3]))

In fact, with lambda functions, we can also get rid of ``bind2``:

.. code:: haskell

  [1,2,3] `bind` (\x -> ([-x,x] `bind` (\y -> [x + y ^ 3])))

Oh dear, what has happened here?
Let us look at the unsugared version of this code:

.. code:: haskell

  bind funcx funcxy

where

.. code:: haskell

  funcxy x = bind [-x,x] (\y -> [x + y ^ 3])

or equivalently

.. code:: haskell

  funcxy' x = bind (funcy x) (funcfinal x)

It now becomes clear that this is entirely equivalent to the original code,
simply by observing that we could also have written

.. code:: haskell

  funcxy'' = bind2 funcy funcfinal

Note that our full code is now down to two lines: a definition of `bind`,
(which is highly generic: we can reuse it for any list comprehension),
and the comprehension itself:

.. code:: haskell

  bind zs f = concat . map f $ zs
  [1,2,3] `bind` (\x -> ([-x,x] `bind` (\y -> [x + y ^ 3])))

We can omit the brackets around lambda definitions, because
the body of the lambda extends as far to the right as possible without hitting
an unbalanced parenthesis [1]_:

.. code:: haskell

  [1,2,3] `bind` \x -> [-x,x] `bind` \y -> [x + y ^ 3]

We got rid of all brackets,
and this *almost* looks like our list comprehension.

The ``>>=`` Operator
~~~~~~~~~~~~~~~~~~~~

Because the ``bind`` operation is so generically useful
for arbitrary list comprehensions,
Haskell implements an ``>>=`` operator for us,
which behaves just like our ``bind``.
We get

.. code:: haskell

  [1,2,3] >>= \x -> [-x,x] >>= \y -> [x + y ^ 3]

We note that, in this example,
the infix notation, along with the lambda notation,
is absolutely indispensible to make for readable code.
To convince yourself, compare with the prefix notation,

.. code:: haskell

  bind [1,2,3] (\x -> (bind [-x,x] (\y -> [x + y ^ 3])))

or with fewer brackets,

.. code:: haskell

  bind [1,2,3] (\x -> bind [-x,x] (\y -> [x + y ^ 3]))

which, although perhaps more explicit, may feel less natural.

Do Notation and the ``<-`` Operator
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For large list comprehensions, keeping everything on a single line
becomes tedious. Instead, we can write

.. code:: haskell

  [1,2,3] >>=
  \x -> [-x,x] >>=
  \y -> [x + y ^ 3]

where it is **very important to remember that
the body of ``->`` extends to the right as far as logically possible**,
i.e. with brackets, our code is equivalent to

.. code:: haskell

  [1,2,3] >>=
      (\x -> [-x,x] >>=
          (\y -> [x + y ^ 3]))

Perhaps, you will find that this is already obscure enough.
Nevertheless, Haskell allows you to take this yet one step further,
with a so-called do block.
A do block allows us to replace ``>>=`` operators with
newlines and some sort of 'reverse lambda notation':

.. code:: haskell

  do x <- [1,2,3]
     y <- [-x,x]
     [x + y ^ 3]

The only remaining touch we can give this code is to use Haskell's
``return`` function:

.. code:: haskell

  do x <- [1,2,3]
     y <- [-x,x]
     return (x + y ^ 3)

The ``return`` function transforms a value into a container
(or, a monad, if you like), and for lists, it is defined as

.. code:: haskell

  return :: a -> [a]
  return x = [x]

This now looks suspiciously similar to code from an imperative language,
for instance the following in Python:

.. code:: python

  def example():
      for x in [1, 2, 3]:
          for y in [-x, x]:
              yield x + y ** 3

It is tempting, yet flawed,
to think of do blocks as a sequence of imperative statements.
Indeed, Haskell may evaluate expressions in any order it wants,
and is only constrained by data flow. For example, in

.. code:: haskell

  do x <- [1,2]
     y <- [9,10]
     [x + y, x - y]

there is no guarantee whatsoever that Haskell will evaluate ``[1, 2]``
before ``[9,10]``. For all we know,
Haskell might even evaluate them in parallel.

The Monad Typeclass
-------------------

The do notation does not only exist for lists, but applies to any monad.
It is crucial to realize that
**the ``>>=`` operator determines how a do block is evaluated**,
as do blocks are simply a fancy way of rewriting
a ``>>=``-separated chain of expressions.
In fact, any container type
which implements ``>>=`` and ``return`` is a monad.
We have not yet seen
how Haskell can overload functions to take arbitrary types.
This is done through Haskell's typeclasses.

We will cover typeclasses in a next post,
along with more monad examples.

Something to Blow Your Mind
---------------------------

1. Our attempt at proving Fermat's theorem using Haskell
   leads to a never ending evaluation,
   quite logically so.

   Explain why

   .. code:: haskell

     null [ [x,y,z] | x <- [1..], y <- [1..],
                      z <- [1..], x ^ 2 + y ^ 2 == z ^ 2]

   might not end either (depending on the details of the compiler)
   although the list is non-empty, but

   .. code:: haskell

     null [ [x,y,z] | x <- [3..], y <- [4..],
                      z <- [1..], x ^ 2 + y ^ 2 == z ^ 2]

   might be evaluated in finite time
   (again depending on the details of the compiler).

2. Fermat's problem involved filtering,
   but our poor man's implementation did not implement filtering.
   What extra operations do we need for our list monad
   to gain filtering ability?

   How could you abstract this to apply to general monads?

   Hint. Analyze the following code:

   .. code:: haskell

     filt :: Bool -> Int -> [Int]
     filt cond x = if cond then [x] else []

     [1,2,3] >>=
     \x -> [-x..x] >>=
     \y -> [x + y ^ 3] >>=
     filt (odd y)

.. [1] http://stackoverflow.com/questions/11237076/haskell-precedence-lambda-and-operator
