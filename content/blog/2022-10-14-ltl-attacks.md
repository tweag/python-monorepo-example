---
title: "Testing stateful systems, part two: Linear Temporal Logic"
author: Carl Hammann
tags: [auditing, smart-contracts, haskell, blockchain, formal-methods]
---

On a [previous post][previous-post], we explained how to write tests for
stateful systems using _traces_ -- sequences of stateful actions -- that can be
combined and modified to write complex test cases easily and transparently. This
post elaborates on the combinators used to generate new traces from previously
existing traces.

Writing generators for property-based tests is an art, and there's often a
trade-off between how many test cases are generated and how meaningful each of
them is. When considering stateful systems, this problem is exacerbated, because
any action taken now may constrain the available actions for the future. This
means that the test generator will already need to track how the state evolves
from each step to the next in order to generate valid inputs. We think that
this is problematic because it means that the test case generator will likely
end up duplicating much of the work (and many of the mistakes) of the program
being tested.

So, ideally, we'd like to be able to use the program we're testing to keep
track of the state, but still apply state-dependent modifications at each time
step. Succinctly, we propose the following approach to make this possible:

- Generate test cases as variations of example traces. This guides the
  exploration of the space of possible tests in a very transparent way: There
  are many test cases being generated, but each one is a precisely understood
  variation of a known scenario.

- Use a language derived from linear temporal logic (LTL) to describe where
  single-step modifications should be applied. In order to allow for single-step
  modifications to depend on the state of the computation, we evaluate the
  modified traces while also interpreting the LTL formula describing the
  composite modification from step to step.

## An example to motivate LTL test cases

Here's a pattern we frequently encounter in our audits of smart contracts: Say
we're testing a protocol that involves two transactions, `txA` and `txB`. Also
assume that a potential vulnerability comes from malicious `txB`s, which are
only possible if preceded by a suitably modified `txA`.

Now, you will often have a collection of valid traces for the protocol, which
will feature `txA`s and `txB`s in various contexts. Wouldn't it be nice to use
all these different scenarios as a "backdrop" for your test, by modifying `txA`s
and `txB`s in them? That is, we want to apply a _coordinated modification_ of
two transactions: In order to witness the vulnerability, one has to modify a
pair of transactions, which occur at unknown positions in the trace although the
_order_ in which they appear is known.

The property we'd like to test here is "at some point in time, we can modify a
`txA` so that later on, there'll be a `txB` we can also modify". If that
property holds, the protocol is broken. But really, we do not merely want to
test a property, we want to execute the trace -- and modify it while we go -- to
witness the attack.

This is where linear temporal logic (LTL) comes to the scene.

## LTL primer

Linear Temporal Logic is a _temporal_ logic in the sense that, in addition to
propositional variables and the connectives of propositional logic, also
features some connectives pertaining to the order of events in time. It is
_linear_ in the sense that its temporal connectives can only specify properties
of a single time line.

Our idea is to think of propositional variables as _single-step modifications_
that apply to time steps within a timeline -- in the preceding example, the time
steps are transactions like `txA` and `txB`, the modifications described by
propositional variables apply to single transactions, and timelines are
transaction sequences. Our method aims to be applicable generally, and therefore
we define the type of LTL formulas with single-step modifications of any type
`a`:

```haskell
data Ltl a
  = LtlTruth
  | LtlFalsity
  | LtlAtom a
  | LtlOr (Ltl a) (Ltl a)
  | LtlAnd (Ltl a) (Ltl a)
  | LtlNext (Ltl a)
  | LtlUntil (Ltl a) (Ltl a)
  | LtlRelease (Ltl a) (Ltl a)
```

Now, we think of an element of type `Ltl a` not as a formula that describes a
property to be _checked_, but as a composite modification to be _applied to_ a
trace. From this perspective, we can give a meaning to the constructors
above. Let's start with the first three.

- `LtlTruth` is the "do nothing" modification that can be applied anytime and
  leaves everything unchanged.

- `LtlFalsity` is the modification that never applies and always leads to
  failure. In other words, `LtlFalsity` terminates the current timeline, which
  means that the whole modified computation is disregarded.

- The formula `LtlAtom x` means "at the current time step, apply `x`".

The next two constructors look innocuous enough, but still need some
explanation.

- The modification `` x `LtlOr` y `` should be understood as "timeline
  branching": It splits the trace we're modifying into two traces, one
  modified with `x` and the other modified with `y`.

- Conjunction is slightly more subtle: The modification `` x `LtlAnd` y ``
  applies both `x` and `y` to the same trace (no time branching here!). Our
  current implementation applies `y` first, so that `LtlAnd` is not necessarily
  commutative. It will however be commutative whenever the order in which
  _single-step_ modifications are applied does not matter.

The last three constructors are what really turns LTL into a temporal logic.

- The formula `LtlNext x` is the modification that applies `x` at the next time
  step, or fails if there is no such time step.

- The formula `` x `LtlUntil` y `` applies `x` at every time step, until `y`
  becomes applicable (and is applied) at some time step, which must happen
  eventually.

- The formula `` x `LtlRelease` y `` is dual to `` x `LtlUntil` y ``. It applies
  `y` at every time step, up to and including the first time step when `x`
  becomes applicable (and is applied); should `x` never become applicable, then
  `y` will be applied forever.

The absence of negation and implication from our presentation stems from the our
point of view that LTL formulas are composite modifications. For now, we have
not settled on one obviously correct meaning for the negation (if there is one):
Should it mean to check for applicability of the modification, failing if it is
applicable, and leaving everything unchanged otherwise? Should it somehow mean
to branch into the infinitude of all other possible modifications (using some
kind of mask)...? Likewise, implication is problematic: What should the meaning
of `` LtlNext x `LtlImplies` y `` be? -- Assume `x` is applicable at the next
time step, then we would have to apply `y` now. However, applying `y` now might
change the state, and that might make `x` non-applicable at the next time
step. In that sense, this formula would describe a modification that violates
causality. All of this is not intended to mean that negation and implication are
impossible for fundamental reasons, but that there is no clear path to handle
them at the moment.

## Applying LTL in the example scenario

Assume that we interact with our protocol through a monad `Protocol` that uses
two transactions `txA :: Protocol ()` and `txB :: Protocol ()`, and say that one
of our simple traces looks like this:

```haskell
aabab :: Protocol ()
aabab = txA >> txA >> txB >> txA >> txB
```

As a warm-up, let's generate all possibilities to modify exactly one `txA` with
some single-transaction modification `modifyA`, and let's denote the modified
transactions by `txA'`. Since there are three `txA`s, there should be three
modified traces (in pseudo-Haskell):

```haskell
{ txA' >> txA  >> txB >> txA  >> txB
, txA  >> txA' >> txB >> txA  >> txB
, txA  >> txA  >> txB >> txA' >> txB
}
```

Generalising the atomic modification, we can describe the "at some point in
time" part with the following LTL formula:

```haskell
eventually :: a -> Ltl a
eventually x = LtlTruth `LtlUntil` LtlAtom x
```

This means that `eventually x` is successfully applied either if we can apply
`x` right now, or if we can recursively apply `eventually x` from the next
transaction onward.

So, `eventually modifyA` is the modification we want to apply. To do so, we use
the type class

```haskell
class Monad m => MonadModal m where
  type Modification m :: Type
  modifyLtl :: Ltl (Modification m) -> m a -> m a
```

which allows us to apply the composite modification described by an LTL formula
to some monadic computation, returning a computation of the same type. In all of
the examples we have encountered so far, the monad under consideration will have
an obvious branching structure like `MonadPlus`, so that we can think of
`modifyLtl` as the function that returns all "timelines" that can be obtained by
applying the given modification.

So, `modifyLtl (eventually modifyA)`, applied to `aabab`, should describe the
three traces above.

Now for the grand finale: In the original discussion of the example, we wanted
to apply a coordinated modification to all pairs of a `txA` and a later `txB`.
The formula that interests us is therefore

```haskell
andLater :: a -> a -> Ltl a
x `andLater` y = eventually $ LtlAtom x `LtlAnd` LtlNext (eventually y)
```

It describes the composite modification that somewhere applies `x` and then at
some later step applies `y`. This should then yield, again in pseudo-Haskell, the
following five modified traces:

```haskell
modifyLtl (modifyA `andLater` modifyB) aabab ==
  { txA' >> txA  >> txB' >> txA  >> txB
  , txA  >> txA' >> txB' >> txA  >> txB
  , txA' >> txA  >> txB  >> txA  >> txB'
  , txA  >> txA' >> txB  >> txA  >> txB'
  , txA  >> txA  >> txB  >> txA' >> txB'
  }
```

That is, each pair of a `txA` followed at some point by a `txB` receives
modifications.

## A rough idea of the implementation

The main difficulty in interpreting LTL formulas as state-aware modifications
lies in the fact that the parameters of single-transaction modifications might
depend on parts of the state that are only known once we run the actual
trace. In the example, `modifyA` and `modifyB` might behave differently
depending on the state. For example, consider the first two of the modified
traces from above.

```haskell
modifyLtl (modifyA `andLater` modifyB) aabab ==
  { txA' >> txA  >> txB' >> txA  >> txB
  , txA  >> txA' >> txB' >> txA  >> txB
  , ...
```

The `txB'` from the first modified trace might be different from the `txB'` in
the second trace, because the state after the first two transactions was
different, and `modifyB` therefore produced a different `txB'`. However, since
the relevant state can only be known once the first two transaction have already
been modified and run, we have to run the modified trace while we apply
single-step modifications.

Another way to phrase this is that we can't first generate a list of traces and
then run them in a second step; we don't know all of the details of the
computation(s) we're running beforehand. It is this fact that makes the
implementation rather involved, but also what ultimately makes our idea
useful: We can modify in a state-aware way, but we don't need to track the state
ourselves.

Our mental model to get around this difficulty is the observation that every
formula `x :: Ltl a` corresponds to a list `nowLater x :: [(a, Ltl a)]` of pairs
of a single-step modification to apply right now and a composite modification to
apply from the next time step onward. You can think of this as a normal form:
Every formula is equivalent to a disjunction of formulas of the form `` a `LtlAnd` LtlNext x ``, where `a` is an atom, truth, or falsity.

For example, the formula `` x `LtlUntil` y `` corresponds to the list `` [(y, LtlTruth), (x, x `LtlUntil` y)] ``, because there are two ways to satisfy it:
Either `y` is already applicable at the current time step and then we need not
apply any further modifications, or `x` is applicable now, and in that case we
recursively have to apply `` x `LtlUntil` y `` from the next time step onward.

Now, our idea is to have an abstract syntax tree (AST) of the traces we're
trying to modify and then interpret that AST while also using the function
`nowLater` to pass the relevant modifications from each time step to the
next. This becomes possible with the freer monad ideas described in the
[previous post][previous-post]. Very briefly, in the setting of the example, the
idea is to have a type `Op` to reify the methods of the monad `Protocol`, such
that a method that returns an `a` is reified as an `Op a`. For example, `txA`
and `txB` would correspond to two constructors of `Op ()`. The AST is then
constructed as the freer monad on `Op`, together with some operations that are
hidden from the end user to allow us to thread LTL formulas through. Then, we
define a function

```haskell
interpretLtl ::
  (
    -- some constraints on m
  ) =>
  AST a -> StateT (Ltl modification) m a
```

to interpret the AST while also passing the modifications from one step of the
interpretation to the next. The conditions on `m` require that `m` has the
necessary structure to interpret the operations reified by `Op`.

In the end, we obtain a convenient method to define instances of the "magical"
type class `MonadModal` from the last section. (If you want to understand how
it works, I recommend reading the previous post and then starting your
exploration of the code with [this instance declaration][monadmodal-instance]
for `MonadModal`).

## Closing Remarks

The preceding discussion proposes a method to turn a relatively small number of
uninteresting traces into a big number of interesting tests. Also, since each of
the test cases is obtained as a precisely described composite modification of an
original trace, we're running many tests, but it's easy to keep track of what
we're actually testing. Especially in combination with a convenient way to
define single-step modifications, this method allows us to quickly explore many
test ideas. (For the blockchain use case, we have a [growing
collection][attack-implementation] of single-step modifications, which
correspond to common attacks on smart contracts.)

The objective of this post is mainly to share the technique to use "LTL to _modify_ a
sequence of stateful actions, not to _check_ some of its properties".
We imagine that this idea will prove useful in many applications, not
only for testing. Our method can be applied to every monad that has some
"builtin" operations that can be meaningfully modified. The idea of the
`MonadModal` type class -- and the idea to generate instances for it using a
freer monad -- is relevant whenever it makes sense to consider stateful
computations as step-by-step modifications of one original computation.

## Further work

There are now many interesting questions on the more theoretical side to be
investigate.

- What's the right set of logical connectives?

  - The problem with the "causality violating" formula `` LtlNext x `LtlImplies` y `` seems not to be implication per se, but that we can't have `LtlNext`
    before an implication, if we want to use our "`nowLater` normal form"
    approach.

  - Likewise, the fact that it's not obvious what negation should be doesn't
    mean that we should not consider it. What are some conditions on an
    operation that would make it worthy of being called "negation" in this
    context?

  - We're working with a set of connectives that implicitly assumes time to be
    infinite, but the computations we consider all have a finite number of time
    steps. Is there a sensible finite-time fragment of LTL we can use? We're
    investigating this question with an Agda formalisation of the ideas in this
    post, and the goal of this effort is to reach a complete specification and
    verification.

- What is the formula that describes the modification we get by first applying
  the `x` and then `y` to the same trace? Our implementation just relies on the
  assumption that, in the relevant cases, we can use `LtlAnd`, but as we
  discussed, there are some problems around commutativity. Phrased differently,
  atomic modifications form a (not necessarily commutative) monoid, and our LTL
  modifications inherit monoid structure from them. Is there a sensible logical
  junctor corresponding to that monoid operation?

- What about branching-time logic? -- We already heavily use the "time
  branching" metaphor, so maybe it's useful to conceptualise computations not as
  linear sequences of instructions, but to use the "nondeterministic
  computation" perspective throughout.

We expect that all of these questions can (and should) only be answered on the
basis of a sound denotational semantics for the function `modifyLtl`. So the one
question to rule them all is: What's `modifyLtl`, really?

[previous-post]: https://www.tweag.io/blog/2022-01-26-property-based-testing-of-monadic-code/
[cooked-validators]: https://github.com/tweag/plutus-libs/tree/main/cooked-validators
[ltl-implementation]: https://github.com/tweag/plutus-libs/blob/main/cooked-validators/src/Cooked/Ltl.hs
[attack-implementation]: https://github.com/tweag/plutus-libs/blob/main/cooked-validators/src/Cooked/Attack.hs
[wiki-ltl]: https://en.wikipedia.org/wiki/Linear_temporal_logic
[monadmodal-instance]: https://github.com/tweag/plutus-libs/blob/48ab77cf4a02695b3fb0d9e333dc342e557b76f9/cooked-validators/src/Cooked/Ltl.hs#L293
