module Propositions where

  module Version1 where

    data Pair (A B : Set) : Set where
      _,_ : A -> B -> Pair A B

    data Either (A B : Set) : Set where
      left : A -> Either A B
      right : B -> Either A B

    _and_ : (A B : Set) -> Set
    A and B = Pair A B

    _or_ : (A B : Set) -> Set
    A or B = Either A B

  module Version2 where

    data _and_ (A B : Set) : Set where
      _,_ : A -> B -> A and B

    data _or_ (A B : Set) : Set where
      left : A -> A or B
      right : B -> A or B

  module Imp1 where

    open Version2

    lemma : (A B : Set) -> (A and B) -> A
    lemma A B (a , b) = a

    theorem : (A B : Set) -> (A and B) -> (A or B)
    theorem A B ab = left (lemma A B ab)

  module Imp2 where

    open Version2

    lemma : {A B : Set} -> (A and B) -> A
    lemma (a , b) = a

    theorem : {A B : Set} -> (A and B) -> (A or B)
    theorem ab = left (lemma ab)

  module Imp3 where

    open Version2

    theorem : {A B : Set} -> (A and B) -> (A or B)
    theorem (a , b) = left a

  module DistExample1 where

    open Version2

    distributivity : {A B C : Set} -> (A and (B or C)) -> ((A and B) or (A and C))
    distributivity (a , left b) = left (a , b)
    distributivity (a , right c) = right (a , c)
