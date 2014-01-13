Propositional Logic in Agda
===========================

.. author:: default
.. categories:: none
.. tags:: none
.. comments::

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

.. code-block:: haskell

  _implies_ : Set -> Set -> Set
  A implies B = A -> B

So, in Agda, we essentially reason by transforming proofs.
This corresponds nicely to how traditional mathematical reasoning works.
In this post, we will further explore propositional logic
to gain more confidence in proof transformation.

We will not use the above ``implies`` operator any more,
and instead directly write ``->`` for implication.

Conjunction and Disjunction
---------------------------

How do we model logical 'and' and logical 'or'?
Both operators take two theorems, and return a new theorem,
so the type signatures should be as follows:

.. code-block:: haskell

  _and_ : (A B : Set) -> Set
  _or_ : (A B : Set) -> Set

The implementations are also reasonably obvious:
``A and B`` is proven
if we have an instance of ``A`` and an instance of ``B``,
for instance, it could return a pair, i.e. a Haskell tuple.
On the other hand, ``A or B`` is proven
if we have an instance of ``A`` or an instance of ``B``:
we could use Haskell's ``Either`` type.

As far as I know,
Agda has no builtin implementation for tuples,
and it also has no ``Either`` type, but it is easy to roll our own.
For example:

.. code-block:: haskell

  data Pair (A B : Set) : Set where
    _,_ : A -> B -> Pair A B

  data Either (A B : Set) : Set where
    left : A -> Either A B
    right : B -> Either A B

  _and_ : (A B : Set) -> Set
  A and B = Pair A B

  _or_ : (A B : Set) -> Set
  A or B = Either A B

The syntax for declaring data types is slightly different from Haskell's,
so this deserves some explanation.
``Pair`` has constructor ``_,_ : A -> B -> Pair A B``.
The return type of a constructor is always the type it is defined for.
What this signature thus says is that ``_,_`` constructs a
new instance of ``Pair A B``
by taking an instance of ``A`` and an instance of ``B``.
Remember that, in Haskell and also in Agda,
constructors of algebraic data types do not declare their implementation
as in C++, Python, or Java.
In Haskell and Agda, constructors simply define expressions
that create instances of a particular type.

Expressing a Proof as a Function
--------------------------------

Let us prove that ``A and B`` implies ``A or B``:
How does this work? Let us first do the proof in words,
and then translate it into Agda code.
Clearly, if ``A and B`` holds, then ``A`` holds.
But if ``A`` holds, then ``A or B`` holds.

.. code-block:: haskell

  lemma : (A B : Set) -> (A and B) -> A
  lemma A B (a , b) = a

  theorem : (A B : Set) -> (A and B) -> (A or B)
  theorem A B ab = left (lemma A B ab)

Here, ``lemma`` proves that if ``A and B`` holds, then ``A`` holds.
To do so, we take a proof of ``A and B``, which is ``(a , b)``,
and transform it into a proof of ``A``, which is ``a``.
Next, ``theorem`` proves the initial statement.
We take our proof of ``A and B``, which we denote by ``ab``,
then apply ``lemma`` to get a proof of ``A``,
and then turn this into an instance of ``A or B``.

Something rather interesting is happening in the last step:
constructors can be used as theorems too!
Our type signature
``left : A -> Either A B``
means that, from a proof of ``A``, we can prove ``Either A B``,
which is the same as ``A or B``.
In a sense, **constructors are the axioms of our theory**.

To make this more formal, we could simply get rid of ``Pair`` and ``Either``,
and directly write:

.. code-block:: haskell

  data _and_ (A B : Set) : Set where
    _,_ : A -> B -> A and B

  data _or_ (A B : Set) : Set where
    left : A -> A or B
    right : B -> A or B

Agda has some useful syntax
that saves us from having to write all arguments:
any arguments between curly braces can be omitted
if they can be unambiguously inferred.
With this in mind, the proof becomes a little bit more readable:

.. code-block:: haskell

  lemma : {A B : Set} -> (A and B) -> A
  lemma (a , b) = a

  theorem : {A B : Set} -> (A and B) -> (A or B)
  theorem ab = left (lemma ab)

(So, ``lemma`` is simply Haskell's ``fst``!)

Finally, note that we could also have integrated
the lemma into the proof of the theorem:

.. code-block:: haskell

  theorem : {A B : Set} -> (A and B) -> (A or B)
  theorem (a , b) = left a

Using Pattern Matching to Prove Distinct Cases
----------------------------------------------

Let us prove another theorem:

.. code-block:: haskell

  distributivity : {A B C : Set} -> (A and (B or C)) -> ((A and B) or (A and C))
  distributivity (a , left b) = left (a , b)
  distributivity (a , right c) = right (a , c)

Here, we exploited pattern matching to prove two cases separately.
An instance of ``A and (B or C)``
is a proof of ``A`` (``a``),
along with a proof of either ``B`` (``left b``) or ``C`` (``right c``).
We can convert ``(a , left b)``
into an instance of ``((A and B) or (A and C))``, namely ``left (a , b)``.
Similarly, we can convert ``(a , left b)``
into an instance of ``((A and B) or (A and C))``, namely ``right (a , c)``.
