module Propositions where

data False : Set where

data True : Set where
  tt : True

_implies_ : Set -> Set -> Set
A implies B = A -> B

thmimpliesisreflexive : (A : Set) -> (A implies A)
thmimpliesisreflexive A = \a -> a

not : Set -> Set
not A = (A implies False)

thmfimpt : (False implies True)
thmfimpt ()

thmntimpf : not (True implies False)
thmntimpf timpf = timpf tt

thmcontraposition : (A B : Set) -> ((A implies B) implies ((not B) implies (not A)))
thmcontraposition A B aimpb nb a = nb (aimpb a)
