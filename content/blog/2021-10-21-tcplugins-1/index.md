---
title: "Type-checking plugins, Part I: Why write a type-checking plugin?"
shortTitle: "Why write a type-checking plugin?"
author: Sam Derbyshire
tags: [haskell, ghc]
description: "Why would you want to write a type-checking plugin? Many limitations of the GHC type-checker are exposed here."
---

Type-checking plugins for GHC are a powerful tool which allows users to
inject domain-specific knowledge into GHC's type-checker. In this
series of posts, we will explore why you might want to write your own plugin, and how to do so.

- **I: Why write a type-checking plugin?**
- [II: GHC's constraint solver](https://www.tweag.io/blog/2021-12-09-tcplugins-2/)
- [III: Writing a type-checking plugin](https://www.tweag.io/blog/2022-02-17-tcplugins-3/)

In this first blog post of the series, I'll be outlining a few
examples that showcase some limitations in GHC's instance resolution
and type family reduction mechanisms. With a type-checker plugin, we
are no longer restricted by these limitations, and can decide ourselves
how to solve constraints and reduce type families.

### Instance resolution

Recall how GHC goes about instance resolution, as per the [user's guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html).
A typeclass instance has two components:

```haskell
instance ctxt => Cls arg_1 ... arg_n
```

To the left of `=>` is the instance **context**, and to the right the instance **head**.
To solve a constraint like `Cls x_1 ... x_n`, GHC goes through all the class instance declarations for `Cls`,
trying to match the arguments `x_1, ..., x_n` against the arguments `arg_1, ..., arg_n` appearing in the instance head.
Once it finds such an instance (which should be unique, unless one is using [overlapping instances](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#instance-overlap)), GHC commits to it, picking up the context as a Wanted constraint (we will cover Wanted and Given constraints in depth in [Part II: § Constraint solving](https://www.tweag.io/blog/2021-12-09-tcplugins-2.html#constraint-solving)).

However, one might be interested in using a different method to resolve instances. Let's look at two simple examples.

#### State machines

Suppose we want to implement a state machine: each state corresponds to a type, and we can transition values between types according to certain rules.
This can be implemented as a typeclass: arrows in the state diagram are typeclass instances.

```haskell
type Transition :: Type -> Type -> Constraint
class Transition a b where { transition :: a -> b }
```

We could start by defining some basic instances:

```haskell
instance Transition A B where {..}
instance Transition B C where {..}
instance Transition C D where {..}
instance Transition A E where {..}
instance Transition B F where {..}
instance Transition E F where {..}
```

<p align="center">
<img src=instance_graph.svg alt="Instance graph for Transition typeclass." />
</p>

Then, we might want the compiler to use composition to solve other instances.
For example, when trying to solve `Transition A D`, we notice that there is a unique path

`A -> B -> C -> D`

which would allow us to compose the transition functions to obtain a transition function of type `A -> D`.
On the other hand, if we want a transition from `A` to `F`, we notice that there are two different paths, namely
`A -> B -> F`, and `A -> E -> F`. We could either reject this, or perhaps choose one of the two paths arbitrarily.

This goes beyond GHC's instance resolution mechanisms, but we could implement a graph reachability test in a type-checking plugin
to solve general `Transition X Y` instances.

#### Constraint disjunction

The central feature of typeclasses is that they are **open**: one can define a typeclass and keep adding instances to it.
This is in contrast to something like a datatype

```haskell
data ABC
  = A
  | B Int
  | C Float Bool
```

which is **closed**: users can't add new constructors to `ABC` in their own modules.

This property of typeclasses comes with a fundamental limitation: we can't know in advance whether a typeclass constraint is satisfied.
A typeclass constraint that was insoluble at some point might become solvable in another context
(e.g. a user could define a `Show (Bool -> Int)` instance).

As a result, GHC does not offer any mechanism for determining whether a constraint is satisfied, as this could result in incoherent behaviour.[^1]
This is unfortunate, as this can be quite useful: for example, one might want to implement an arithmetic computation differently for integral vs floating-point numbers, to ensure numerical stability:

```haskell
stableAlgorithm = select @(Floating a) @(Num a) fp_algo int_algo
  where
    fp_algo  :: Floating a => [a] -> a -- floating-point algorithm
    int_algo ::      Num a => [a] -> a -- integral algorithm
```

Here, the `select` function dispatches on whether the first constraint
(in this case `Floating a`) is satisfied;
when it is, it uses the first (visible) argument; otherwise, the second.
This behaviour can be implemented in a type-checker plugin (see for instance my [`if-instance` plugin](https://hackage.haskell.org/package/if-instance)):
when attempting to solve a constraint disjunction `ct1 || ct2`, we can simply look up whether `ct1` is currently satisfiable, disregarding the fact
that new instances might be defined later (which can lead to incoherence as mentioned above). The satisfiability of `ct1` at the point of solving
the disjunction `ct1 || ct2` will then determine which implementation is selected.

### Stuck type families

Consider the type family

```haskell
type (+) :: Nat -> Nat -> Nat
type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)
```

GHC can only reduce `a + b` when it knows what `a` is: is it `Zero` or is it `Succ i` for some `i`?
This causes a problem when we don't have concrete natural numbers, but still want to reason about `(+)`:

```haskell
infixr 5 :<
type Vec :: Nat -> Type -> Type
data Vec n a where
  Nil  :: Vec Zero a
  (:<) :: a -> Vec n a -> Vec (Succ n) a

-- | Interweave the elements of two vectors.
weave :: Vec n a -> Vec n a -> Vec (n + n) a
weave Nil       Nil       = Nil
weave (a :< as) (b :< bs) = a :< b :< weave as bs
```

To typecheck `weave`, we need to know that

```
Succ n + Succ n ~ Succ (Succ (n + n))
```

Using the second type family equation on the LHS, this reduces to:

```
Succ (n + Succ n) ~ Succ (Succ (n + n))
```

Peeling off `Succ`:

```
n + Succ n ~ Succ (n + n)
```

Now we can't make any more progress: we don't know which equation of `(+)` to use,
as the first argument is a bare type variable `n`. We say the type family application
is **stuck**; it doesn't reduce.

By contrast, in a type-checking plugin, we can rewrite type-family applications involving
variables, and thus implement a solver for natural number arithmetic.

### Type family arguments

Suppose we are interested in solving a theory of **row types**, e.g. to implement
a framework of extensible records.

A **row** is an unordered association map, field name ⇝ type, e.g.

```haskell
myRow = ( "intField" :: Int, "boolField" :: Bool, "anotherInt" :: Int )
```

Crucially, order doesn't matter in a row. To communicate this fact to the type-checker,
we would want to be able to prove a fact such as:

```haskell
Insert k v (Insert l w r) ~ Insert l w (Insert k v r)
```

when `k` and `l` are distinct field names that don't appear in the row `r`. However,
we can't write a type family equation of the sort:

```haskell
type Insert :: Symbol -> Type -> Row -> Row
type family Insert k v r where
  Insert k v (Insert l w r) = ...
```

```
• Illegal type synonym family application ‘Insert l w row’ in instance:
    Insert k v (Insert l w row)
```

GHC doesn't allow type families to appear inside the LHS of type family equations.
Doing so would risk **non-confluence**: the result of type-family reduction
might depend on the order in which we rewrite arguments. For example:

```haskell
type F :: Type -> Type
type family F a where
  F (F a) = a       -- F[0]
  F Bool  = Float   -- F[1]
  F a     = Maybe a -- F[2]
```

Given a type such as `F (F Bool)`, we can proceed in two ways:

1. Reduce the outer type family application first, using the first equation (written `F[0]`).
   This yields the reduction `F (F Bool) ~~> Bool`.
2. Reduce the argument first, using the second equation, `F[1]`, and following up with `F[2]`:
   `F (F Bool) ~~> F Float ~~> Maybe Float`.

We obtained different results depending on the order in which reductions were performed.
To avoid this problem, GHC simply disallows type family applications
from appearing on the LHS of type family equations.

In a type-checking plugin, we can inspect the arguments of a type family,
and use that information in deciding how to reduce the type family application.

### Performance of type-family reduction

The current implementation of type families in GHC suffers from one significant problem:
they can be [painfully slow](https://gitlab.haskell.org/ghc/ghc/-/issues/8095).

This is because, when GHC reduces a type family application, it also creates a proof
that keeps track of which type family equations were used. Such proofs can be large,
in particular when using recursive type families. Returning to the example of natural number addition:

```haskell
type (+) :: Nat -> Nat -> Nat
type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)
```

The proof that `5 + 0` reduces to `5`, in the coercion language that will be explained
in [Part II: § Constraint solving](https://www.tweag.io/posts/2021-12-09-tcplugins-2.html#constraint-solving),
is as follows:

```haskell
+[1] <Succ (Succ (Succ (Succ Zero)))> <Zero>
; (Succ (+[1] <Succ (Succ (Succ Zero))> <Zero>
        ; (Succ (+[1] <Succ (Succ Zero)> <Zero>
                ; (Succ (+[1] <Succ Zero> <Zero>
                        ; (Succ (+[1] <Zero> <Zero>
                                ; (Succ (+[0] <Zero>))))))))))
```

Here `+[0]` refers to the first type-family equation of `+`, and `+[1]` to the second. The difficulty is that,
in this proof language, we store the types of the arguments. For example, in the first reduction step,
in which we reduce `Succ (Succ (Succ (Succ (Succ Zero)))) + Zero` to `Succ (Succ (Succ (Succ (Succ Zero))) + 0)`,
the proof records the two arguments to `+`, namely `Succ (Succ (Succ (Succ Zero)))` and `Zero`.
As a result, the size of the proof that `n + 0` reduces to `n` is quadratic in `n`.

This can be a problem, for example, in libraries that implement large sum or product types using type families
and type-level lists (like many anonymous record libraries do), causing slow compile-times for even moderately-sized
records.

In a type-checking plugin, we can instead perform type-family reduction in a single step,
returning a single proof term which omits the intermediate steps. In this way, type-checking plugins
allow us to sidestep many of the performance issues that surround type families.

### Practical examples

- Solving with arithmetic expressions of natural numbers with Christiaan Baaij's [`ghc-typelits-natnormalise`](https://hackage.haskell.org/package/ghc-typelits-natnormalise),
- units of measure and dimensional analysis with Adam Gundry's [`uom-plugin`](https://hackage.haskell.org/package/uom-plugin),
- regular expressions with Oleg Grenrus's [`kleene-type`](https://github.com/phadej/kleene-type),
- row types with Divesh Otwani and Richard Eisenberg's [`thoralf`](https://github.com/bgamari/the-thoralf-plugin),
- intrinsically typed System F with a solver for a lambda calculus of explicit substitutions, bundled with the [`ghc-tcplugin-api`](https://github.com/sheaf/ghc-tcplugin-api) library.

### Conclusion

We've seen that type-checking plugins can be useful in many different circumstances:

- custom constraint-solving logic,
- added flexibility for type-family reduction,
- performance considerations.

The next question is then, hopefully: how does one actually write a type-checking plugin?
Because type-checking plugins operate directly on constraints, it's important to be somewhat familiar
with GHC's constraint solver, and how type-checking plugins interact with it.

This will be the topic of Part II of this series, before we dive into
the practical aspects of actually writing and debugging a type-checking plugin in Part III.

[^1]:
  In this context, coherence is the property that the runtime behaviour of
  programs does not depend on the specific way in which they are typechecked.
