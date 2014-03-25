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
along with the axioms it needs to satisfy.

Here is one way:

.. code-block:: haskell

  data IsEquivalence
    {M : Set}
    (_≈_ : M -> M -> Set)
    (refl : ∀ {r} -> r ≈ r)
    (symm : ∀ {r s} -> r ≈ s -> s ≈ r)
    (trans : ∀ {r s t} -> r ≈ s -> s ≈ t -> r ≈ t)
    : Set where

    isEquivalence : IsEquivalence _≈_ refl symm trans

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

  theorem-==-is-equivalence
    : IsEquivalence _==_ natrefl theorem-==-symm theorem-==-trans
    theorem-==-is-equivalence = isEquivalence

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
    -> {refl : ∀ {r} -> r ≈ r}
    -> {symm : ∀ {r s} -> r ≈ s -> s ≈ r}
    -> {trans : ∀ {r s t} -> r ≈ s -> s ≈ t -> r ≈ t}
    -> IsEquivalence _≈_ refl symm trans
    -> ∀ {r s t} -> r ≈ s -> ¬ (s ≈ t) -> ¬ (r ≈ t)
  theorem-equivalence-simple
    {_} {_≈_} {_} {symm} {trans} _
    r≈s ¬s≈t r≈t = ¬s≈t (trans (symm r≈s) r≈t)

In order to specify an instance of ``IsEquivalence``
in the premises of the theorem,
we need to specify all axioms first anyway.
In other words, we must replicate the type signature of all axioms
in every theorem that needs ``IsEquivalence``.

Nevertheless,
it does provide some simplification when we have to apply the theorem,
say, to prove that it holds for natural numbers:

.. code-block:: haskell

  theorem-==-equivalence-simple :
    ∀ {r s t} -> r == s -> ¬ (s == t) -> ¬ (r == t)
  theorem-==-equivalence-simple
    = theorem-equivalence-simple theorem-==-is-equivalence

Record Syntax
-------------

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
(ii) we have moved some of the type parameters into so-called *fields*
using the ``field`` keyword,
(iii) we no longer have to specify a constructor (in fact, we still
could specify a specifically named constructor if we wanted to).

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
in a named fashion. In particular, the ordering does not matter.

Now, the really good news:
**the type of fields becomes embedded into the type of the record**.
So, the type signature of our earlier simple theorem
no longer needs to specify the type of all fields:
they are no longer type parameters!
There is also special syntax for accessing any particular field of a record:
``<RecordType>.<fieldname> <instance>``.
Our theorem thus becomes:

.. code-block:: haskell

  theorem-equivalence2-simple :
    {M : Set} -> {_≈_ : M -> M -> Set} -> IsEquivalence2 _≈_
    -> ∀ {r s t} -> r ≈ s -> ¬ (s ≈ t) -> ¬ (r ≈ t)
  theorem-equivalence2-simple equiv r≈s ¬s≈t r≈t
    = ¬s≈t ((IsEquivalence2.trans equiv) ((IsEquivalence2.symm equiv) r≈s) r≈t)

This is much simpler, and much more readable, than our earlier theorem.
The syntax for field access are still somewhat annoying.
Agda provides us with another trick to simplify this:
for every instance of a record, ``<RecordType> <instance>``
corresponds to a module which provides direct access
to the fields of the instance.
We can thus simply write:

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
