Everything is a Function
========================



.. author:: default
.. categories:: none
.. tags:: none
.. comments::

.. highlight:: haskell

Monads and The Magic Blob
-------------------------

Everyone who starts learning Haskell eventually hits monads.
In fact, it ought to be the first thing to start with in Haskell:
remember how any Haskell program essentially does one thing, namely
evaluating the ``main`` function?
Guess what ``main`` returns: yes, indeed, a monad.

Many (though certainly not all) tutorials I came across
start with treating monads---specifically, the IO monad---as
some kind of a magical blob.
In retrospect, now that I *think* to have an at least somewhat
sufficiently accurate understanding of
what monads do, and what role they play functional languages,
it makes sense to me to try to understand monads first,
before diving into the IO monad magic.

So, my aim is to try to explain in the next few posts how monads work
in a pure functional language,
and how they allow you to refactor your code to be more beautiful---because
that is what Haskell is all about, beautiful code!
Apparently, monads are *also* useful for functions that have side effects,
i.e. functions whose result
does not depend only on the value of their arguments.
Once we have a good understanding of what monads are,
our plan is to look at the implementation of the IO monad,
to finally understand the magic.

Functions
---------

Before we look at monads,
it is useful to reflect on how Haskell works with functions,
and how it provides really convenient notation
for combining simple functions together to make up really complex functions.
That is the main purpose of the current post.

First, how do we denote functions?
In Haskell, functions have a name, and one argument.
Yes, all functions have a single argument---we will see in a bit how we
can fake functions with multiple arguments.
It is a good habit, although not necessary, to annotate your functions
with a so-called type signature::

  increment :: Int -> Int

In the above, ``Int`` is simply the name of the type for integers in Haskell.
The code declares the fact that the function, named ``increment``,
takes an integer, and returns an integer.
Once we have this, we can define the function itself::

  increment x = x + 1

The left hand side denotes the function name (``increment``)
and its argument (``x``).
Note that we do not need to use brackets for the function argument:
a space denotes function application.
This may seem a bit weird at first,
but one gets used it quite quickly:
it makes for neat code.

The right hand side denotes the expression used to evaluate the function,
namely ``x + 1``, which does what you expect.
We already have something weird here: surely, addition is a function too.
How can we write ``x + 1`` if every function takes just a single argument?
And why is the function, ``+``, denoted in between of its arguments?
Well, there are two things going on:

* ``x + 1`` is just an alternative notation for ``((+) x) 1``.

* As the notation in the previous point already suggests,
  ``(+)`` is *a function which returns another function*::
  [1]_

    (+) :: Int -> (Int -> Int)

  The brackets around the plus symbol
  distinguish the *infix* notation
  ``x + 1`` from the *prefix* notation ``((+) x) 1``.

So, ``x + 1`` first evaluates ``(+) x``,
which is a function with type signature ``Int -> Int``.
Consequently, we apply this function to the argument ``1``,
to get an integer back.
To make the confusion complete,
observe that we can also denote ``(+) x`` as ``(x+)``.
Cool.

Here is the full code,
which you can save as ``test.hs`` and run with ``runghc test.hs``::

  increment :: Int -> Int
  increment x = x + 1
  main :: IO ()
  main = print (increment 5)

The type signature of ``main`` is a bit strange: main takes no arguments,
and returns something that has type ``IO ()``.
In fact, ``IO ()`` is a monad.
For now, suffice it to say that
to get an IO monad out of some result, we can use the ``print`` function.
Coincidently, ``print`` will also print its argument to the screen,
which is rather convenient.

A few conventions help us with reducing bracket bloat.

1. The mapping operator ``->`` in type signatures is right-associative,
   so we can write::

     (+) :: Int -> Int -> Int

   instead of::

     (+) :: Int -> (Int -> Int)

2. Space (for function application) is left-associative,
   so we can write::

     (+) x 1

   instead of::

     ((+) x) 1

3. Space (for function application)
   has higher precedence than any other operator.

Note that, earlier, we put brackets around ``increment 5``
to apply its outcome to the ``print`` function. Had we omitted those brackets,
as in::

  main = print increment 5

then the compiler would have interpreted this as::

  main = (print increment) 5

due to the left-associativity of the space operator
(as function application),
which is obviously wrong.
In fact, Haskell will give you a compile error on such code,
because the expression fails the type checks.
Indeed, type checks do prevent a rather frequent cause
of sometimes hard to track bugs;
that is why those type signatures are especially important.

Anyway, with this knowledge, we can now for instance define::

  affine :: Double -> Double -> Double -> Double
  affine a b x = a + b * x
  main :: IO ()
  main = print (affine 1 2 3)

There are two more infix operators which help us with readability.

First, the ``$`` operator denotes function application,
so it is identical to the space operator,
with the only difference that ``$`` has very low precedence
and is right-associative,
whereas space has very high precedence
and is left-associative.
Thus, we can simplify the last line and write::

  main = print $ affine 1 2 3

Finally, the ``.`` operator denotes function composition.
Here is its definition::

  (.) :: (b -> c) -> (a -> b) -> (a -> c)
  (f . g) x = f $ g x

In the above, ``a``, ``b``, and ``c``, are generic placeholders
for any type our heart desires; we say that ``.`` is polymorphic,
and ``a``, ``b``, and ``c`` are called *type variables*.
They are similar to template arguments in C++.

``.`` has higher precedence than ``$``, but lower precedence
than space.
Function composition is associative, so if we chain functions together
through composition, there is no need to write brackets to denote
the order of composition.

Something to Blow Your Mind
---------------------------

Explain why::

  main = print . affine 1 2 $ 3

is the same as::

  main = print $ affine 1 2 3

Lessons Learned
---------------

* A function that takes multiple arguments can be modelled as a
  function which returns another function.

* Space is an operator: it applies functions to arguments, and it
  is left-associative, which saves us brackets when working with functions
  that take multiple arguments.

* A dollar ``$`` is like space, but with very low precedence, and it is
  right-associative.

* A dot ``.`` denotes function composition. It is associative,
  and has medium precedence (higher than ``$``, and actually also
  higher than all the usual binary operators, but lower than space).

* Mapping operators ``->`` in type signatures are right-associative, which
  saves us brackets, again, when working with functions that take
  multiple arguments.

* The standard binary infix operators (``+``, ``*``, ``-``, ``/``,
  and so on) can be used
  in prefix notation---i.e. as normal functions---by
  surrounding them with brackets.
  It is now not clear why this is useful---just take it on faith that
  there are plenty of situations where
  it is useful to pass these operators as arguments of other functions,
  which is made possible through the prefix notation.

* A function can be polymorphic through type variables in their type signature.

* For now, ``main`` returns magic blob.
  For the time being, we will use ``print`` and be happy in our ignorance.

.. [1] Actually, the type signature is ``(+) :: Num a => a -> a -> a``
       but let us not get ahead of ourselves.
