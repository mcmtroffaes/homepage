module Equivalence where

  data IsEquivalence
    {M : Set}
    (_≈_ : M -> M -> Set)
    (refl : ∀ {r} -> r ≈ r)
    (symm : ∀ {r s} -> r ≈ s -> s ≈ r)
    (trans : ∀ {r s t} -> r ≈ s -> s ≈ t -> r ≈ t)
    : Set where

    isEquivalence : IsEquivalence _≈_ refl symm trans

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

  theorem-==-equivalence-simple :
    ∀ {r s t} -> r == s -> ¬ (s == t) -> ¬ (r == t)
  theorem-==-equivalence-simple
    = theorem-equivalence-simple theorem-==-is-equivalence

  record IsEquivalence2
    {M : Set}
    (_≈_ : M -> M -> Set)
    : Set where
    field
      refl : ∀ {r} -> r ≈ r
      symm : ∀ {r s} -> r ≈ s -> s ≈ r
      trans : ∀ {r s t} -> r ≈ s -> s ≈ t -> r ≈ t

  theorem-==-is-equivalence2 : IsEquivalence2 _==_
  theorem-==-is-equivalence2 = record {
    refl = natrefl;
    trans = theorem-==-trans;
    symm = theorem-==-symm
    }

  theorem-equivalence2-simple :
    {M : Set} -> {_≈_ : M -> M -> Set} -> IsEquivalence2 _≈_
    -> ∀ {r s t} -> r ≈ s -> ¬ (s ≈ t) -> ¬ (r ≈ t)
  theorem-equivalence2-simple equiv r≈s ¬s≈t r≈t
    = ¬s≈t (IsEquivalence2.trans equiv (IsEquivalence2.symm equiv r≈s) r≈t)

  theorem-equivalence2-simple-alt :
    {M : Set} -> {_≈_ : M -> M -> Set} -> IsEquivalence2 _≈_
    -> ∀ {r s t} -> r ≈ s -> ¬ (s ≈ t) -> ¬ (r ≈ t)
  theorem-equivalence2-simple-alt equiv r≈s ¬s≈t r≈t
    = ¬s≈t (trans (symm r≈s) r≈t)
      where open IsEquivalence2 equiv
