---
title: Agda, Dependent Types, Theorem Proving
author: Matthias C. M. Troffaes
tags: agda
---

Agda
====

A few days ago, I stumbled on
[David Sankel's talk at BoostCon 2013](http://youtu.be/vy5C-mlUQ1w)
on Agda.
Whilst the focus of the talk is, eventually, software design,
computer assisted theorem proving was repeatedly mentioned.
Some years ago I tried
the proof assistent
[Isabelle](http://www.cl.cam.ac.uk/research/hvg/Isabelle/)
but I never really played around enough in it to a point
where I could really use it.

Now, what David Sankel's talk made me realise,
is that functional programming languages are a stepping stone to
proof assistents.
The only bit missing from Haskell to make this happen
are so-called *dependent types*.
Take Haskell, add dependent types, and you get Agda!
So, for the next few months,
I have set myself the task of understanding how exactly this works.

Types as Instances
==================

The first important feature which enables assisted theorem proving
in Agda is the unification of types and instances.
In many languages,
including C++ and Haskell,
types and instances are completely different concepts.
In Agda, types are instances as well.
This is certainly not unique to Agda.
For example, in Python, types are called classes,
and it turns out that an instance of a class can be another class:
simply derive your class from `type`{.python}.
Such class is called a *metaclass* in Python,
to distinguish it from "standard" classes, which derive from `object`{.python}.
In Agda, by convention,
standard types are instances of a builtin type called `Set`{.agda}.
So, one could think of Agda's `Set`{.agda} as Python's `type`{.python}.

There is however one crucial difference:
in Agda, `Set`{.agda} is not an instance of `Set`{.agda},
whereas `type`{.python} is an instance of `type`{.python} in Python:

``` {.sourceCode .agda}
x : Set
x = Set
```

will fail to load in Agda, but

``` {.sourceCode .python}
>>> isinstance(type, type)
True
```

runs just fine in Python.
Instead, in Agda, `Set`{.agda} is an instance of `Set1`{.agda}:

``` {.sourceCode .agda}
x : Set1
x = Set
```

loads just fine in Agda.
There are deep reasons for `Set`{.agda} not being an instance of itself:
if `Set`{.agda} were an instance of `Set`{.agda}, then
[Russell's paradox](https://en.wikipedia.org/wiki/Russell%27s_paradox)
would hold true.
Coming from a standard set-theoretic background,
it can be helpful to think of types as sets,
and of the colon operator `:`{.agda}
as an "is an element of" operator `∈`{.agda}.

Dependent Types
===============

What are they?
Informally, they are types which depend on a value,
like templates in C++. Here is an example:

``` {.sourceCode .agda}
id : (A : Set) -> A -> A
id _ a = a
```

The type signature declares a function, which takes
as arguments an arbitrary type and a value of that type,
and returns a value of that type.
The definition is simply the identity map.
Something to blow your mind:
could you define any other function with the same function signature?

In the above example,
the second argument and the return value have dependent types,
because their type depends on the first argument.

Proving Theorems
================

With just these ingredients---static typing with unified types and instances,
and dependent types---we can do all of constructive mathematics.
How does this work? The key thing to remember is that
**a theorem is a type**,
and **a proof of that theorem is an instance of that type.**
That sounds weird at first, so let us first explore some simple examples.

Let us start with propositional logic.
Propositional logic deals with statements.
In set theory, statements
are usually identified with subsets of some possibility space.
We have seen that we can think of sets as types:
by convention, let us take statements to be instances of Agda's `Set`{.agda} type.

How do we specify that a statement is true, or false?
Well, theorems are statements, and theorems are true
if they can be proven, that is, if the corresponding type has an instance.
So, we specify that a statement is true simply by stating that it has a value.
For example, we can declare the following algebraic data types: [^1]

``` {.sourceCode .agda}
data False : Set where

data True : Set where
  tt : True
```

Here, `False`{.agda} is a proposition which has no instances,
or, no proofs. We can identify it with the impossible event.

Conversely, `True`{.agda} is a proposition which is always true,
i.e. we can identify it with the true event:
it has exactly one instance, or proof, namely `tt`{.agda}
(we could declare more constructors, this would make no practical difference).

So, what can we do with propositions?
Well, one proposition can imply another.
How do we formalize that?
We need to distinguish carefully between two things:
the statement that A implies B,
which is a theorem and hence an instance of `Set`{.agda},
and an actual proof of A implying B,
which is an instance of the theorem's type.

How do we produce a new statement from two existing statements?
We use a function:

``` {.sourceCode .agda}
_implies_ : Set -> Set -> Set
```

(The underscores make this an infix function, which will read better.)
So, the function `implies`{.agda} takes as arguments two statements or theorems,
and uses them to produce a new statement. What statement?

``` {.sourceCode .agda}
A implies B = A -> B
```

The set of functions from A to B? What does this mean?
It means that `A implies B`{.agda} is considered true
if there is an instance of `A -> B`{.agda},
that is if
**there is a function that transforms any proof of A into a proof of B**.

We can now prove a first theorem:

``` {.sourceCode .agda}
thmimpliesisreflexive : (A : Set) -> (A implies A)
thmimpliesisreflexive A = \a -> a
```

The definition of the proof reads as follows.
The function has one argument:
`A`{.agda} is simply the theorem or statement under consideration.
The result is an implication, which we represent as a function.
For our purpose, the identity map `\a -> a`{.agda} does the job.
`a`{.agda} is a proof of the left hand side of the implication,
namely of `A`{.agda}.
The function needs to produce
a proof for the right hand side of the implication, which is also `A`{.agda}.
Obviously, we can simply return the same proof `a`{.agda} again.

A more tricky case:

``` {.sourceCode .agda}
thmfimpt : (False implies True)
thmfimpt ()
```

The type `False implies True`{.agda} simply maps proofs of `False`{.agda}
to proofs of `True`{.agda}.
But, there are no proofs of `False`{.agda}.
Consequently, `thmfimpt`{.agda} is simply the empty function,
mapping nothing to nothing.
In Agda, we say that a function is empty by writing empty brackets.

Can we do negation? Easily, in terms of implication:

``` {.sourceCode .agda}
not : Set -> Set
not A = (A implies False)
```

So, `not A`{.agda} is true if we can map every proof of `A`{.agda}
to a proof of `False`{.agda}.
But, `False`{.agda} has no proofs. So we can only do this if `A`{.agda} has no proofs.
Similarly, `not A`{.agda} is false
if there is no map from a proof of `A`{.agda} to a proof of `False`{.agda}.
This happens as soon as `A`{.agda} has a proof
(otherwise, if there would be such a map, `False`{.agda} would be true).

So, to show that `not A`{.agda} is true,
effectively, we must establish that there is no proof of `A`{.agda}.
To do this, we rely on contradiction: assuming we had a proof
for `A`{.agda}, we find a map that gives us a proof for `False`{.agda}.
Agda's typechecker will figure out from this that `A`{.agda} has no proofs:
if it had, it would have a proof of `False`{.agda},
which would contradict the type declaration of `False`{.agda}.

Here is an example of proving a negation:

``` {.sourceCode .agda}
thmntimpf : not (True implies False)
thmntimpf timpf = timpf tt
```

How do we interpret this proof?
The function signature reduces to `True implies False -> False`{.agda},
so `timpf`{.agda} is a proof of `True implies False`{.agda}.
Applying this function to our proof of truth, `tt`{.agda},
we get a proof of `False`{.agda},
establishing the contradiction.

Let us finish with proving something slightly less trivial:

``` {.sourceCode .agda}
thmcontraposition : (A B : Set) -> ((A implies B) implies ((not B) implies (not A)))
thmcontraposition A B aimpb nb a = nb (aimpb a)
```

How do we arrive at this proof? In emacs, write the proof definition as

``` {.sourceCode .agda}
thmcontraposition A B = {! !}
```

and use C-c C-l to load the file. This will tell you that
the normalised type signature of the expression between curly brackets is
`A implies B -> not B -> A -> False`{.agda}
(this is also easy to figure this out without the help of Agda,
but using Agda to provide hints like this saves some time).
In other words,
`aimpb`{.agda} proves `A implies B`{.agda},
`nb`{.agda} proves `not B`{.agda},
and `a`{.agda} proves `A`{.agda}.
We must produce `False`{.agda}: a contradiction.
Indeed,
`aimpb a`{.agda} turns `a`{.agda} into a proof of `B`{.agda}.
And `nb`{.agda} turns this proof into a proof of `False`{.agda}. Done!

Note that `nb (aimpb a)`{.agda} translates directly into
the proof by contradiction:
Given A implies B, and not B,
we must show that A does not hold.
Assume, A were to hold (`a`{.agda}).
Then, because A implies B, B needs to hold as well (`aimpb a`{.agda}).
But, B does not hold, so we arrived at a contradiction (`nb (aimpb a)`{.agda}).

Finally,
note that the actual act of theorem proving 
does not involve compilation
of the code into a binary.
The theorems are proved as soon as it is verified
that the function definitions match the type signatures.
So, **theorem proving is just a special case of type checking**.

[^1]: In Haskell, this would be:

    ``` {.sourceCode .haskell}
    data False
    data True = True
    ```
