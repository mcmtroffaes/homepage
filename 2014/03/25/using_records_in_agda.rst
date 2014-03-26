Using Records in Agda
=====================

.. author:: default
.. categories:: none
.. tags:: none
.. comments::

Organizing Code
---------------

In previous posts, we covered some of the basic principles behind Agda.
We discussed the use of data types to describe axioms of our theory,
and the use of functions to prove theorems from those axioms.
Using just data types and functions, however,
it is quite hard to recycle properties in a convenient way.
For this purpose, Agda provides a special bit of syntax: records.

Essentially, a record is a glorified data type.
Glorified in two ways:
(i) parameters can have names (called *fields*), and
(ii) we can embed declarations inside records.
In this post, we will only discuss the first point, fields.

For example, let us revisit the mock real type that we declared in our
earlier post:

.. code-block:: haskell

  data ℝ : Set where
    r0 : ℝ
    r1 : ℝ
    _+_ : ℝ -> ℝ -> ℝ

  data _==_ : ℝ -> ℝ -> Set where
    AXrefl== : ∀ {r} -> r == r
    AXsymm== : ∀ {r s} -> r == s -> s == r
    AXtrans== : ∀ {r s t} -> r == s -> s == t -> r == t
    AX+0 : ∀ {r} -> (r + r0) == r
    AXsymm+ : ∀ {r s} -> (r + s) == (s + r)
    AX+== : ∀ {r s t} -> r == s -> (r + t) == (s + t)

How can we make this nicer?
Well, it would be quite nice if we could
somehow seperate the first three axioms that turn ``_==_`` into an
equivalence relation.
Secondly, there might be many different ways
in which we can actually construct an equivalence relation on a type.
In fact, we might work with different equivalence relations at the same time.
How can we generically say that some arbitrary relation, say ``_≈_``,
is an equivalence relation?

Parametric Data Types in Overdrive
----------------------------------

Well, we could specify the relation itself as a parameter,
along with proofs of the properties it needs to satisfy.

Here is one way:

.. code-block:: haskell

  data IsEquivalence
    {M : Set}
    (_≈_ : M -> M -> Set)
    : Set where

    isEquivalence :
      (refl : ∀ {r} -> r ≈ r)
      -> (symm : ∀ {r s} -> r ≈ s -> s ≈ r)
      -> (trans : ∀ {r s t} -> r ≈ s -> s ≈ t -> r ≈ t)
      -> IsEquivalence _≈_

So, if we can create an instance of the type ``IsEquivalence``
for some relation ``_≈_``, then ``_≈_`` is an equivalence relation.
Here is an example of how we might use ``IsEquivalence``:

.. code-block:: haskell

  data ℕ : Set where
    zero : ℕ
    suc : ℕ -> ℕ

  data _==_ : ℕ -> ℕ -> Set where
    natrefl : ∀ {n} -> n == n

  theorem-==-symm : ∀ {n m} -> n == m -> m == n
  theorem-==-symm natrefl = natrefl

  theorem-==-trans : ∀ {n m k} -> n == m -> m == k -> n == k
  theorem-==-trans natrefl natrefl = natrefl

  theorem-==-is-equivalence : IsEquivalence _==_
  theorem-==-is-equivalence
    = isEquivalence natrefl theorem-==-symm theorem-==-trans

Note that in the above example,
only reflexivity had to be specified as an axiom of ``_==_``, and
the other properties could be proved from the definition of ``_==_``
(of course this will not be the case in general!).

This is very generic already.
Can we do better? What are the problems with the above approach?

An obvious problem occurs if we have to specify
many parameters. Whenever we need
the ``IsEquivalence`` type, or its ``isEquivalence`` constructor,
we also need to specify four parameters
(excluding the hidden parameter ``M``), namely the relation,
along with the three axioms.
This makes the code hard to read,
and even worse,
we might get the ordering of parameters wrong.
In the above example, we only have three axioms,
but in general, the number of axioms that we may want to work with
simultaneously can become very, very large.

Finally, and perhaps this is the strongest shortcoming of all,
showing already in this very simple example:
using ``IsEquivalence``
in theorems that require equivalence relations
does not lead to further abstraction and simplification of our code.
For example:

.. code-block:: haskell

  data ⊥ : Set where
  ¬_ : Set -> Set
  ¬ A = A -> ⊥
  theorem-equivalence-simple :
    {M : Set}
    -> {_≈_ : M -> M -> Set}
    -> IsEquivalence _≈_
    -> ∀ {r s t} -> r ≈ s -> ¬ (s ≈ t) -> ¬ (r ≈ t)
  theorem-equivalence-simple
    (isEquivalence refl symm trans)
    r≈s ¬s≈t r≈t = ¬s≈t (trans (symm r≈s) r≈t)

In order to specify an instance of ``IsEquivalence``
in the premises of the theorem,
we can now use our new data type
instead of having to specify proofs of all axioms.

Similarly, it provides with some simplification
when we have to apply the theorem,
say, to prove that it holds for natural numbers:

.. code-block:: haskell

  theorem-==-equivalence-simple :
    ∀ {r s t} -> r == s -> ¬ (s == t) -> ¬ (r == t)
  theorem-==-equivalence-simple
    = theorem-equivalence-simple theorem-==-is-equivalence

Record Syntax
-------------

One downside is that pattern matching will become a bit tedious
if we have many properties.
It can be very easy to get the ordering wrong.
A logical improvement would be to provide named parameters.
This leads us to record syntax:

.. code-block:: haskell

  record IsEquivalence2
    {M : Set}
    (_≈_ : M -> M -> Set)
    : Set where
    field
      refl : ∀ {r} -> r ≈ r
      symm : ∀ {r s} -> r ≈ s -> s ≈ r
      trans : ∀ {r s t} -> r ≈ s -> s ≈ t -> r ≈ t

Note the differences from our earlier data type definition:
(i) we write "record" instead of "data",
(ii) we have moved the constructor's arguments into so-called fields,
(iii) we no longer have to specify a constructor.
(In fact, we still could specify a specifically named constructor
if we wanted to, which would then work exactly as the constructor
of our earlier data type, i.e. it might be useful for patter matching.)

The theorem now becomes:

.. code-block:: haskell

  theorem-==-is-equivalence2 : IsEquivalence2 _==_
  theorem-==-is-equivalence2 = record {
    refl = natrefl;
    symm = theorem-==-symm;
    trans = theorem-==-trans
    }

So, records are constructed with the ``record {...}`` syntax.
Agda inferred its type from the theorem's type signature.
Parameters that are fields can be passed to this constructor
in a named fashion. In particular, the ordering does not matter,
and the intention of the code becomes much clearer.

There is also special syntax for accessing any particular field of a record:
``<RecordType>.<fieldname> <instance>``.
Our theorem thus becomes:

.. code-block:: haskell

  theorem-equivalence2-simple :
    {M : Set} -> {_≈_ : M -> M -> Set} -> IsEquivalence2 _≈_
    -> ∀ {r s t} -> r ≈ s -> ¬ (s ≈ t) -> ¬ (r ≈ t)
  theorem-equivalence2-simple equiv r≈s ¬s≈t r≈t
    = ¬s≈t ((IsEquivalence2.trans equiv) ((IsEquivalence2.symm equiv) r≈s) r≈t)

This is less error prone than our earlier theorem,
because we no longer rely on the particular ordering
of the constructor arguments: we no longer rely on pattern matching.
The syntax for field access is still somewhat verbose.
Agda provides us with another trick to simplify this:
for every instance of a record, ``<RecordType> <instance>``
corresponds to a module which provides direct access
to the fields of the instance.
We can thus simply write:

.. code-block:: haskell

  theorem-equivalence2-simple-alt :
    {M : Set} -> {_≈_ : M -> M -> Set} -> IsEquivalence2 _≈_
    -> ∀ {r s t} -> r ≈ s -> ¬ (s ≈ t) -> ¬ (r ≈ t)
  theorem-equivalence2-simple-alt equiv r≈s ¬s≈t r≈t
      = ¬s≈t (trans (symm r≈s) r≈t)
        where open IsEquivalence2 equiv

The ``open`` command opens a module, that is, brings its declarations
into the current namespace, so we can use ``trans`` and ``symm``
directly without having to specify the record type and the instance.

The record syntax that we discussed
is heavily used in Agda's standard library,
and it is probably time that we started to pay some more attention to it,
in a next post.

Reals Revisited
---------------

To finish this post, here is how our mock real type can be
reimplemented using record syntax. This is entirely equivalent to our
earlier simpler data type syntax, but it leads to code that is much
easier to reuse:

.. code-block:: haskell

  module Reals where

  record IsEquivalence
    {M : Set}
    (_==_ : M -> M -> Set)
    : Set where
    field
      refl : ∀ {r} -> r == r
      symm : ∀ {r s} -> r == s -> s == r
      trans : ∀ {r s t} -> r == s -> s == t -> r == t

  record IsStrictPartialOrder
    {M : Set}
    (_==_ : M -> M -> Set)
    (_<_ : M -> M -> Set)
    : Set where
    field
      trans<<< : ∀ {r s t} -> r < s -> s < t -> r < t
      trans<=< : ∀ {r s t} -> r < s -> s == t -> r < t
      trans=<< : ∀ {r s t} -> r == s -> s < t -> r < t

  record IsMockReals
    {ℝ : Set}
    (_==_ : ℝ -> ℝ -> Set)
    (_<_ : ℝ -> ℝ -> Set)
    (_+_ : ℝ -> ℝ -> ℝ)
    (r0 : ℝ)
    (r1 : ℝ)
    : Set where
    field
      isEquivalence : IsEquivalence _==_
      isStrictPartialOrder : IsStrictPartialOrder _==_ _<_
      r+r0 : ∀ {r} -> (r + r0) == r
      symm+ : ∀ {r s} -> (r + s) == (s + r)
      cong+= : ∀ {r s t} -> r == s -> (r + t) == (s + t)
      cong+< : ∀ {r s t} -> r < s -> (r + t) < (s + t)
      0<1 : r0 < r1

    open IsEquivalence isEquivalence public
      renaming (refl to refl==; symm to symm==; trans to trans==)

    open IsStrictPartialOrder isStrictPartialOrder public

    r0+r : {r : ℝ} -> r == (r0 + r)
    r0+r = symm== (trans== symm+ r+r0)

    thm<+1 : {r : ℝ} -> r < (r + r1)
    thm<+1 = trans<=< (trans=<< r0+r (cong+< 0<1)) symm+

The new bits are: ``public``, which re-exports all imported declarations,
``renaming`` which renames imported declarations,
and the use of declarations directly inside the record
We could have used ``where open ...`` syntax as well in case we did not
want the theorems to be included as members of the record.
The Agda standard library seems not to put theorems inside records generally;
it may also not always be obvious which record a theorem should belongs to.

An interesting question:
in an arbitrary record,
which parameters should be (unnamed) type parameters,
and which parameters should be (named) field parameters?
