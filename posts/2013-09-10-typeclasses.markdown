---
title: Typeclasses
author: Matthias C. M. Troffaes
tags: haskell
---

Polymorphism
============

We already saw one way to achieve polymorphism in Haskell:
type variables. For example,
we defined function composition as

``` {.sourceCode .haskell}
(.) :: (b -> c) -> (a -> b) -> (a -> c)
(f . g) x = f $ g x
```

Above, `a`{.haskell}, `b`{.haskell}, and `c`{.haskell} are generic placeholders
to denote arbitrary types. These are called type variables.

If we think about generalising the monad pattern to general types,
we will quickly find that type variables cannot help us doing so.
Why is that? Well, to use the monad pattern, we need, in essence,
to define a function that binds our monadic structures.
Looking at further examples,
we will very quickly find that we cannot implement this binding operation
in a fully generic way using type variables, because the implementation
of the binding operation is highly tied to the monad structure itself.
In terms of, say, C++, we would like to overload the binding function
for different types.

This is exactly what a *typeclass* does: it allows us to specify that
a certain type implements certain functions.
The monad typeclass is defined as follows:

``` {.sourceCode .haskell}
class Monad m where
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a
```

Note that we are also required to implement a return function
to embed a type into our monad;
actually, for may examples, this is not really required,
but it can be useful in case a type's constructors are hidden.
Note that the bind operator `>>=`{.haskell} and the `return`{.haskell} function
are required to be polymorphic, through type variables.

So, in essence,
typeclasses are used to declare that
a particular type implements a particular interface.
For example, the list monad would be implemented as

``` {.sourceCode .haskell}
instance Monad [] where
    (>>=) xs f = concat . map f $ xs
    return x = [x]
```

Of course, we do not have to type the above ourselves;
Haskell has already done that for us.

A Simple Container Monad
========================

Let us proceed to a second example of a monad.
Suppose we are carrying out a computation on some value,
but each step of the computation might fail for some reason.
In case of failure, we simply wish to return a dummy value
to signal this failure.

For the time being, let us use a list for this purpose:
an empty list denotes a failed computation, and a non-empty list
of length one denotes successful computation,
with the value it stores being the value computed so far.

As an example of computation, we will parse three digits into an
integer.
A first challenge is to convert a single character into a numerical value.
For this we use the `ord`{.haskell} function from the `Data.Char`{.haskell} module:

``` {.sourceCode .haskell}
import Data.Char
```

We will construct our parser by chaining. A single step of the algorithm
consists of taking a character from the front of a string
(we could also take it from the back, but Haskell encourages
it the other way around), trying to convert it to an integer,
and returning that integer along with the remainder of the characters
still to convert. So, ideally, we would like our function signature to be

``` {.sourceCode .haskell}
getdigit :: [Char] -> (Int, [Char])
```

Two problems still to solve are:
(i) how to deal with error conditions, and
(ii) how to allow this function to be recursively "eat" characters.
We change the function signature somewhat to accommodate both issues:

``` {.sourceCode .haskell}
getdigit :: (Int, [Char]) -> [(Int, [Char])]
```

Two things have happened: we have written the function
to take just a single argument, namely, a tuple of `Int`{.haskell} and `[Char]`{.haskell}.
This will simplify chaining: suppose we have eaten a character
and are left with a remaining string, then we can simply call
the function again to get the next digit with its remaining string, and so on.
The second change is that the function now returns a list,
so an empty list can signal an error condition,
and a single element list can signal successful parsing.

Here is the full implementation.

``` {.sourceCode .haskell}
getdigit :: (Int, [Char]) -> [(Int, [Char])]
getdigit (n, []) = []
getdigit (n, x:xs)
  | m >= 0 && m <= 9 = [(10 * n + m, xs)]
  | otherwise        = []
  where m = ord x - ord '0'
```

In case of invalid input (empty string, or non-numerical character),
the function simply returns an empty string.
If the input is valid, then the function multiplies the result so far by 10,
and adds the parsed digit to that result;
it also returns the remaining characters.

How can we now parse, say, a three-digit integer?
The list monad helps us out.

``` {.sourceCode .haskell}
getint :: [Char] -> [(Int, [Char])]
getint xs = getdigit (0, xs) >>= getdigit >>= getdigit
```

That looks rather elegant, but what is going on here?

Remember what the list monad does: it takes a list, applies a function
to all elements of that list, and then concatenates the resulting
elements of that list. Let us analyse this process in the above code.
First, we start with `getdigit (0, xs)`{.haskell}.
If, on the one hand,
the input `xs`{.haskell} is empty, or has an invalid first character,
then we end up with an empty list.
If, on the other hand,
the input `xs`{.haskell} starts with a valid character,
then we get a list containing the value of that character,
along with the tail of `xs`{.haskell}, i.e. all characters still to process.

The monad operation `>>=`{.haskell} will then apply `getdigit`{.haskell}
to all elements of the list we just obtained---remember that this
list is either empty, or contains exactly one element.
If that list was empty, `>>=`{.haskell} will just return an empty list again
without even calling `getdigit`{.haskell}.
If that list contained one element,
it will multiply the original result by 10, add the newly processed digit
to the result, and return a list containing one pair,
namely the result and the remaining characters.
If at this stage, parsing fails, an empty list is produced.

Rinse and repeat.

Maybe
=====

Using a list to keep track of a failure mode is somewhat contorted:
we are using a cannon to shoot a fly.
Haskell provides a simpler data structure just for the purpose
of storing so-called *optional* values.

``` {.sourceCode .haskell}
data Maybe a = Nothing | Just a
```

Semantically, a `Maybe`{.haskell} is just like a list with at most one element.
Its monad implementation is somewhat simpler than that of lists.

``` {.sourceCode .haskell}
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) Nothing f = Nothing
(>>=) (Just a) f = f a
```

The full implementation becomes

``` {.sourceCode .haskell}
getdigit2 :: (Int, [Char]) -> Maybe (Int, [Char])
getdigit2 (n, []) = Nothing
getdigit2 (n, x:xs)
  | m >= 0 && m <= 9 = Just (10 * n + m, xs)
  | otherwise        = Nothing
  where m = ord x - ord '0'

getint2 :: [Char] -> Maybe (Int, [Char])
getint2 xs = getdigit2 (0, xs) >>= getdigit2 >>= getdigit2
```

This is obviously very similar to our list implementation.
The main difference is that the intent of the code has become clearer
due to the explicit use of `Maybe`{.haskell}, `Nothing`{.haskell}, and `Just`{.haskell}.
