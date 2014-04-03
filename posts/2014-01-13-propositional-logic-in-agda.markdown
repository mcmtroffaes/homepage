---
title: Propositional Logic in Agda
author: Matthias C. M. Troffaes
tags: agda
---

Types are Theorems, Proofs are Instances, Implications are Functions
--------------------------------------------------------------------

In the last post, we looked at Agda,
a extension of Haskell which implements dependent types,
to assist us in proving theorems.
In Agda, theorems are types, and proofs are instances of types.
To verify the proof of a theorem,
we define a function whose type signature corresponds to the theorem,
and whose implementation is an instance of that type, namely, the proof.

Why functions? Because functions embody implication.
Specifically, A implies B
if every proof of A can be transformed into a proof of B.

``` {.sourceCode .agda}
_implies_ : Set -> Set -> Set
A implies B = A -> B
```

So, in Agda, we essentially reason by transforming proofs.
This corresponds nicely to how traditional mathematical reasoning works.
In this post, we will further explore propositional logic
to gain more confidence in proof transformation.

We will not use the above `implies`{.agda} operator any more,
and instead directly write `->`{.agda} for implication.

Conjunction and Disjunction
---------------------------

How do we model logical 'and' and logical 'or'?
Both operators take two theorems, and return a new theorem,
so the type signatures should be as follows:

``` {.sourceCode .agda}
_and_ : (A B : Set) -> Set
_or_ : (A B : Set) -> Set
```

The implementations are also reasonably obvious:
`A and B`{.agda} is proven
if we have an instance of `A`{.agda} and an instance of `B`{.agda},
for instance, it could return a pair, i.e. a Haskell tuple.
On the other hand, `A or B`{.agda} is proven
if we have an instance of `A`{.agda} or an instance of `B`{.agda}:
we could use Haskell's `Either`{.haskell} type.

As far as I know,
Agda has no builtin implementation for tuples,
and it also has no `Either`{.haskell} type, but it is easy to roll our own.
For example:

``` {.sourceCode .agda}
data Pair (A B : Set) : Set where
  _,_ : A -> B -> Pair A B

data Either (A B : Set) : Set where
  left : A -> Either A B
  right : B -> Either A B

_and_ : (A B : Set) -> Set
A and B = Pair A B

_or_ : (A B : Set) -> Set
A or B = Either A B
```

The syntax for declaring data types is slightly different from Haskell's,
so this deserves some explanation.
`Pair`{.agda} has constructor `_,_ : A -> B -> Pair A B`{.agda}.
The return type of a constructor is always the type it is defined for.
What this signature thus says is that `_,_`{.agda} constructs a
new instance of `Pair A B`{.agda}
by taking an instance of `A`{.agda} and an instance of `B`{.agda}.
Remember that, in Haskell and also in Agda,
constructors of algebraic data types do not declare their implementation
as in C++, Python, or Java.
In Haskell and Agda, constructors simply define expressions
that create instances of a particular type.

Expressing a Proof as a Function
--------------------------------

Let us prove that `A and B`{.agda} implies `A or B`{.agda}:
How does this work? Let us first do the proof in words,
and then translate it into Agda code.
Clearly, if `A and B`{.agda} holds, then `A`{.agda} holds.
But if `A`{.agda} holds, then `A or B`{.agda} holds.

``` {.sourceCode .agda}
lemma : (A B : Set) -> (A and B) -> A
lemma A B (a , b) = a

theorem : (A B : Set) -> (A and B) -> (A or B)
theorem A B ab = left (lemma A B ab)
```

Here, `lemma`{.agda} proves that if `A and B`{.agda} holds, then `A`{.agda} holds.
To do so, we take a proof of `A and B`{.agda}, which is `(a , b)`{.agda},
and transform it into a proof of `A`{.agda}, which is `a`{.agda}.
Next, `theorem`{.agda} proves the initial statement.
We take our proof of `A and B`{.agda}, which we denote by `ab`{.agda},
then apply `lemma`{.agda} to get a proof of `A`{.agda},
and then turn this into an instance of `A or B`{.agda}.

Something rather interesting is happening in the last step:
constructors can be used as theorems too!
Our type signature
`left : A -> Either A B`{.agda}
means that, from a proof of `A`{.agda}, we can prove `Either A B`{.agda},
which is the same as `A or B`{.agda}.
In a sense, **constructors are the axioms of our theory**.

To make this more formal, we could simply get rid of `Pair`{.agda} and `Either`{.agda},
and directly write:

``` {.sourceCode .agda}
data _and_ (A B : Set) : Set where
  _,_ : A -> B -> A and B

data _or_ (A B : Set) : Set where
  left : A -> A or B
  right : B -> A or B
```

Agda has some useful syntax
that saves us from having to write all arguments:
any arguments between curly braces can be omitted
if they can be unambiguously inferred.
With this in mind, the proof becomes a little bit more readable:

``` {.sourceCode .agda}
lemma : {A B : Set} -> (A and B) -> A
lemma (a , b) = a

theorem : {A B : Set} -> (A and B) -> (A or B)
theorem ab = left (lemma ab)
```

(So, `lemma`{.agda} is simply Haskell's `fst`{.haskell}!)

Finally, note that we could also have integrated
the lemma into the proof of the theorem:

``` {.sourceCode .agda}
theorem : {A B : Set} -> (A and B) -> (A or B)
theorem (a , b) = left a
```

Using Pattern Matching to Prove Distinct Cases
----------------------------------------------

Let us prove another theorem:

``` {.sourceCode .agda}
distributivity : {A B C : Set} -> (A and (B or C)) -> ((A and B) or (A and C))
distributivity (a , left b) = left (a , b)
distributivity (a , right c) = right (a , c)
```

Here, we exploited pattern matching to prove two cases separately.
An instance of `A and (B or C)`{.agda}
is a proof of `A`{.agda} (`a`{.agda}),
along with a proof of either `B`{.agda} (`left b`{.agda}) or `C`{.agda} (`right c`{.agda}).
We can convert `(a , left b)`{.agda}
into an instance of `((A and B) or (A and C))`{.agda}, namely `left (a , b)`{.agda}.
Similarly, we can convert `(a , left b)`{.agda}
into an instance of `((A and B) or (A and C))`{.agda}, namely `right (a , c)`{.agda}.
