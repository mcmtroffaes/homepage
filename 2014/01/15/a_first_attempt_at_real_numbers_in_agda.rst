A First Attempt at Real Numbers in Agda
=======================================

.. author:: default
.. categories:: none
.. tags:: none
.. comments::

Agda and Axioms
---------------

Verifying proofs that involve real numbers is apparently not obvious in Agda.
Real numbers are a mathematician's bread and butter,
so it comes somewhat as a surprise that there is, apparently, not even
a standard module for reals.
There is for natural numbers.
In fact, natural numbers are really simple, just three lines.
Defining a type for rational numbers
is also reasonably straightforward
(in essence, just pick two natural numbers).
However, defining a type for the real numbers is far less straightforward,
and there seems to be only little information about it.
Nevertheless, as it turns out, we have all the ingredients already.

First, some theory.
Roughly, there are two ways to go about real numbers.
Either, one defines the reals
in terms of sets or sequences of rational numbers.
Or, one defines the reals as some set
along with some operators (notably, addition and multiplication)
which satisfy a particular collection of axioms
(e.g. ``1 * x == x`` and so on).

How will we go about this?
When defining the reals in terms of rationals,
the "axioms" of the reals need to be proven from the properties of rationals.
On the other hand, when defining the reals through axioms,
we will have a lot less proving to do,
because axioms are precisely statements that are accepted without proof.
So, the axiomatic approach seems to be the approach of least effort.
Of course, in principle, we should to prove that the axioms are consistent.
One way of doing so, is by proving that there is a construction
in terms of rationals which satisfies the axioms.
Agda allows us to state axioms without proof of consistency.
So, we will take the axiomatic approach.

For simplicity, for now,
we will not attempt to define the full set of real numbers.
Let us focus on a set with just some properties of the reals.

Constructing Real Instances
---------------------------

First, we declare the constructors of our mock real type:

.. code-block:: haskell

  data ℝ : Set where
    r0 : ℝ
    r1 : ℝ
    _+_ : ℝ -> ℝ -> ℝ

(In emacs, you get ``ℝ`` by typing ``\br``.)

This says that we can construct reals in three ways:
we have two elements: zero and one, and we can also add reals together.
Of course, a full definition would also include
at least multiplication,
possibly along with substraction, division, negation, and so on.

Declaring Axioms
----------------

Now, we wish to specify some properties of addition.
How do we go about this?
Well, for instance, we might like to say that ``r + r0 == r``.
We need an equality operator: 

.. code-block:: haskell

  data _==_ : ℝ -> ℝ -> Set where
    AXrefl== : ∀ {r} -> r == r
    AXsymm== : ∀ {r s} -> r == s -> s == r
    AXtrans== : ∀ {r s t} -> r == s -> s == t -> r == t
    AX+0 : ∀ {r} -> (r + r0) == r
    AXsymm+ : ∀ {r s} -> (r + s) == (s + r)
    AX+== : ∀ {r s t} -> r == s -> (r + t) == (s + t)

So, the equality operator ``==`` takes two real numbers,
and returns a theorem, that is, an instance of ``Set``.
In an earlier post, we saw that instances of ``Set`` are types,
and essentially correspond to theorems.
So, ``r == s``, where ``r`` and ``s`` are instances of ``ℝ``,
i.e. real numbers, is a theorem.
The theorem ``r == s`` is true if there is an instance of this type.
So, constructors of ``==``
declare theorems which are necessarily true.
Therefore, they are axioms of our theory.
In the code above,
all of the constructors start with "AX" to emphasize that they are axioms.

There is one bit of new syntax here: the forall operator "∀".
The code is fully equivalent to the following more verbose code:

.. code-block:: haskell

  data _==_ : ℝ -> ℝ -> Set where
    AXrefl== : {r : ℝ} -> r == r
    AXsymm== : {r s : ℝ} -> r == s -> s == r
    AXtrans== : {r s t : ℝ} -> r == s -> s == t -> r == t
    AX+0 : {r : ℝ} -> (r + r0) == r
    AXsymm+ : {r s : ℝ} -> (r + s) == (s + r)
    AX+== : {r s t : ℝ} -> r == s -> (r + t) == (s + t)

So, ``∀ {r}`` does two things:
it declares ``r`` to be an optional argument
(as you might have guessed from the curly braces,
which we already discussed in an earlier post),
and it causes the type of ``r`` to be inferred
from the remainder of the function signature.
Agda will complain if it cannot infer the type.

For example, how can Agda know the type in ``∀ {r} -> r == r``?
Well, we know that ``_==_ : ℝ -> ℝ -> Set``,
so in the expression ``r == r``, it must be that ``r`` has type ``ℝ``.

A First Theorem
---------------

Let us prove a first theorem: ``r = 0 + r``.
This follows from  ``AXsymm+`` (``0 + r = r + 0``) and ``AX+0`` (``r + 0 = r``),
by transitivity (``AXtrans ==``), as well as symmetry (``AXsymm==``).

In Agda, this can be written as follows:

.. code-block:: haskell

  THM0+ : {r : ℝ} -> r == (r0 + r)
  THM0+ = AXsymm== (AXtrans== AXsymm+ AX+0)
  -- AXsymm+ AX+0   r0 + r == r + r0 and r + r0 == r
  -- AXtrans==      so r0 + r == r
  -- AXsymm==       so r == r0 + r

Agda resolved all hidden parameters for us.
This really simplified the notation.
Here is how the same proof looks like with all parameters specified:

.. code-block:: haskell

  THM0+ {r} = AXsymm== {r0 + r} {r} ((AXtrans== {r0 + r} {r + r0} {r}) (AXsymm+ {r0} {r}) (AX+0 {r}))

This kind of symplicity really helps readability,
although it probably also hurts understanding the code to some extent.
But it does not really hurt understanding how the proof work:
because we can omit all instances of reals,
all that is left are the axioms (and theorems, later) that have been applied,
and in what order.
This provides a nice summary of the proof.

What is amazing here is that it is *not* a summary.
**All variables in the proof can be inferred unambiguously from the axioms and the order in which they are applied.**
I am not sure whether this is possible in general (probably not),
but it surely is a nice feature of Agda.

Another Theorem
---------------

Here are some axioms for strict inequality:

.. code-block:: haskell

  data _<_ : ℝ -> ℝ -> Set where
    AXtrans<<< : ∀ {r s t} -> r < s -> s < t -> r < t
    AX<=< : ∀ {r s t} -> r < s -> s == t -> r < t
    AX=<< : ∀ {r s t} -> r == s -> s < t -> r < t
    AX0<1 : r0 < r1
    AX+<< : ∀ {r s t} -> r < s -> (r + t) < (s + t)

Let us prove that ``r < r + 1``.

.. code-block:: haskell

  THM<+1 : {r : ℝ} -> r < (r + r1)
  THM<+1 = AX<=< (AX=<< THM0+ (AX+<< AX0<1)) AXsymm+
  -- AX0<1              0 < 1
  -- AX<+ %             so 0 + r < 1 + r
  -- AX=<< lem0+ %      so r < 1 + r
  -- AX<=< % AXsymm+    so r < r + 1

Again, to interpret these proofs, it is useful
to first decipher the ordering in which the axioms and theorems
are applied.
The innermost expression is ``AX0<1``.
We start from there and work our way to the outer expression
to get to the full proof, as explained in the comments.
I used ``%`` as a symbol for the expression on the previous line
(as in Maple).

Is the sky the limit?
There seems nothing preventing us to do the entire real calculus.
Let us try this in a next post.
