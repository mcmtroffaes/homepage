---
title: Research
---

My main research
interests concern the **foundations of statistics**
and **decision making under severe uncertainty**, including but not limited to:

* sequential decision processes: backward induction and dynamic programming
* uncertainty modelling: lower and upper previsions, set-valued choice functions, p-boxes, info-gap theory
* elicitation, representation, and aggregation of expert information
* optimal control
* bioinformatics, sequence alignment

Foundations of Statistics
=========================

In modelling a system, it often occurs that some of its aspects, or
some of the influences acting on it, are not well known. The
uncertainty this produces about the system's behaviour is usually
modelled by a probability distribution, and treated using techniques
from probability theory. Such a model will often not be adequate,
simply because not enough information is available in order to
identify a unique probability distribution. In that case, techniques
from the theory of *imprecise probabilities* can be applied in
order to represent and manipulate the really available knowledge about
the system. The term imprecise probability theory actually covers a
wide range of extensions of the classical theory of probability.

A behavioural approach to generalising probability theory, which is
termed *lower previsions* and which can be seen as a
generalisation of the work of De Finetti [@c1974:definetti] in the classical theory of
probability, has been extensively studied and developed during the
1980's by Peter Walley [@c1981:walley:lowuppprobs] [@c1991:walley], based on the work of Boole [@c1854:boole], Smith [@c1961:smith], Williams [@c1975:williams:condprev] [@c2007:williams:condprev] [@c1976:williams], and many others.
In Russia, Vladimir
Kuznetsov [@c1991:kuznetsovinterval] developed, simultaneously and independently from Peter
Walley, a theory which is mathematically very similar to Walley's
lower previsions.

There are many reasons to prefer lower previsions above other well-known
generalisations of probability theory (such as belief functions, possibility
measures, fuzzy measures, credal sets, risk measures, Choquet capacities,
comparative probability orderings, p-boxes, etc.). The most important
reasons are:

* Mathematically, the theory has a **unifying character**: it
  unifies a large number of other generalisations of probability
  theory. Interestingly, imprecise probability also unifies the
  classical theory of probability with the classical theory of logic:
  see Boole's "An investigation of the laws of thought" [@c1854:boole].

* The theory has a very clear **behavioural interpretation** in terms
    of buying (or, equivalently, selling) prices, similar to de
    Finetti's approach to probability theory. So the theory allows for
    both epistemic uncertainty and aleatory uncertainty, and handles
    expert elicitation naturally.

* And last but not least, it leads naturally to a **decision theory**,
  essentially dropping Savage's completeness axiom, and whence admitting
  set-valued choice functions, which reflect more accurately how the
  available information leads to optimal choices (see discussion below).

There are of course also reasons not to prefer lower previsions. The most
important drawbacks are:

* Their **computational complexity** can become prohibitive. Indeed, computing with lower previsions essentially involves solving linear programming
  problems. Even though very large linear programming problems can be solved quite efficiently by computer, yet in many
  applications, especially when involving many variables,
  linear programming problems can simply become
  too large to solve practically.

* Lower previsions also require more **complex mathematical tools**, such as non-linear
  functionals and non-additive measures.
  Many mature concepts of probability theory, such as
  independence, characteristic functions, Markov processes, etc. do not
  carry over to imprecise probability theory in a straightforward
  manner.
  This can be mitigated in part by considering particular models that
  yield simpler mathematical descriptions, possibly at the expense of
  generality, but gaining ease of use. Popular examples of such models are
  possibility measures, p-boxes, and belief functions.

* Lower previsions rely heavily on the
  concept of a (precise) **linear utility**, and whence, do not allow for
  arbitrary rewards.

Decision Making Under Severe Uncertainty
========================================

One of the most profound results that arise from imprecise probability
theory is found in its treating of *optimality*. It is
intuitively clear that if only a scarce amount of information is
available about a variable, then the optimal decision, whose gains and
losses depend on that variable, cannot be completely determined.

Imprecise probability theory, and the theory of lower previsions in particular,
grasps this aspect of the optimal decision problem in a rigorous manner,
resulting in a set of possibly optimal decisions,
that is, a **set-valued choice function**,
rather than giving us,
seemingly arbitrarily, only a single optimal decision from this set. From a
decision making point of view, imprecise probability theory drops the
*completeness axiom*. Imprecise probability theory is especially more
desirable than probability theory in *critical* decision problems, that
is, when gains and losses heavily depend on objects which are not completely
known. One could say that imprecise probability theory allows for a more
accurate description of our knowledge of reality. It
therefore allows for a more accurate criterion of optimality too.

# Bibliography
