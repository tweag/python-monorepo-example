---
title: "Type-checking plugins, Part II: GHC's constraint solver"
shortTitle: "GHC's constraint solver"
author: Sam Derbyshire
tags: [haskell, ghc]
description: "In part two of this series on type-checking plugins, we will see how GHC's constraint solver works."
---

Type-checking plugins interface directly with GHC's constraint solver. This means we need to have
a certain level of familiarity with how GHC generates, manipulates and solves constraints.

In this post, we will attempt to demystify this constraint solving process, as well as review
a few aspects of type-family reduction, to facilitate the development of type-checking plugins.

This post will serve as a reference, when we start writing our own type-checking plugins in Part III.
Don't worry: you don't need to know everything before you start writing your own plugins!
It's only when debugging a type-checking plugin that the rubber hits the road:
a few skills become rather important, such as the ability to understand the different types of constraints,
to read **Core** (and, in particular, **coercions**), etc.

- [I: Why write a type-checking plugin?](https://www.tweag.io/blog/2021-10-21-tcplugins-1/)
- **II: GHC's constraint solver**
- [III: Writing a type-checking plugin](https://www.tweag.io/blog/2022-02-17-tcplugins-3/)

### Table of contents

- [Constraints and constraint solving](#constraints-and-constraint-solving)

  - [Constraints](#constraints)

    - [Dictionary constraints](#dictionary-constraints)
    - [Equality constraints](#equality-constraints)
    - [Coercions: a reading guide](#coercions-a-reading-guide)
    - [Single-method dictionaries](#single-method-dictionaries)

  - [Constraint canonicalisation](#constraint-canonicalisation)
  - [Constraint solving](#constraint-solving)

        - [A simple example](#a-simple-example)
        - [The interaction pipeline](#the-interaction-pipeline)
        - [The constraint solving loop](#the-constraint-solving-loop)
        - [Manually discharging single-method constraints](#manually-discharging-single-method-constraints)

- [Type families](#type-families)

  - [A primer on type family reduction](#a-primer-on-type-family-reduction)
  - [Type family coercion axioms](#type-family-coercion-axioms)

## Constraints and constraint solving

### Constraints

In Haskell, a constraint is a type whose values are determined implicitly through constraint solving,
rather than passed explicitly as arguments.

#### Typeclass constraints

There are several different sorts of constraints.
Let us consider first **typeclass constraints**, with the `Eq` typeclass as a first example.

```haskell
type Eq :: Type -> Constraint
class Eq a where
  (==), (/=) :: a -> a -> Bool

elem1 :: Eq a => a -> [a] -> Bool
elem1 _ []     = False
elem1 x (y:ys) = x == y || elem1 x ys
```

```haskell
type EqDict :: Type -> Type
data EqDict a =
  MkEqDict { eq, neq :: a -> a -> Bool }

elem2 :: EqDict a -> a -> [a] -> Bool
elem2 _      _ []     = False
elem2 eqDict x (y:ys) = eq eqDict x y || elem2 eqDict x ys
```

The difference between `elem1` and `elem2` is that in the latter, we pass the equality-checking function
explicitly rather than implicitly. This distinction goes away in **Core**[^1], where we get:

```haskell
elem1 :: forall a. Eq a => a -> [a] -> Bool
elem1 = \ @a ($dEq_a :: Eq a) (x :: a) (xs :: [a]) ->
  case xs of
    []     -> False
    (y:ys) -> (==) @a $dEq_a x y || elem1 @a $dEq_a x ys
```

We are passing a record `$dEq_a` of type `Eq a`, and then using the field accessor

```haskell
(==) :: forall a. Eq a -> (a -> a -> Bool)
```

to retrieve the method. We say that `$dEq_a` is a **dictionary** for the typeclass constraint `Eq a`;
it provides **evidence** that the typeclass constraint is satisfied. This evidence is passed at
runtime, as in the explicit dictionary-passing example.[^2]

This means that defining a typeclass is tantamount to defining a record type, whose values will be obtained
from constraint solving and passed implicitly (we will cover constraint solving in more detail later, see [§ Constraint solving](#constraint-solving)).  
For instance, when calling `elem1 (3 :: Int) [4,5]`, GHC will look up the instance `Eq Int`
to obtain the dictionary, whereas the user would have to manually pass a record
of type `EqDict Int` to use `elem2` instead.

#### Equality constraints

GHC defines several different equality constraints that users can manipulate. These are:

- **Nominal** equality, which can be _homogeneous_ `~` or _heterogeneous_ `~~`,[^3]
- **Representational** equality, `Coercible` (which is homogeneous).

These all behave like ordinary typeclasses: evidence for such a constraint consists of
a typeclass dictionary, as in the previous section.
However, typeclass dictionaries in GHC are always _boxed_ and _lifted_.
In this case, this means that evidence for an equality is represented by a pointer, which might be an unevaluated thunk. This is rather unsatisfactory: we would prefer to only allow genuine equality proofs, as opposed to
also allowing ⊥. In other words, we would like to rule out evidence of the form `undefined` or an `error` (e.g. a deferred type error, when `-fdefer-type-errors` is enabled).
To solve this problem, GHC defines its own primitive equality types:

- `~#`, primitive nominal equality, and
- `~R#`, primitive representational equality.

One can imagine that GHC internally defines:

```haskell
type (~) :: forall k. k -> k -> Constraint
class    a ~# b => a ~ b
instance a ~# b => a ~ b

type (~~) :: forall k l. k -> l -> Constraint
class    a ~# b => a ~~ b
instance a ~# b => a ~~ b

type Coercible :: forall k. k -> k -> Constraint
class    a ~R# b => Coercible a b
instance a ~R# b => Coercible a b
```

However, as `~#` and `~R#` are unlifted (and completely erased at runtime),
we can't define them as normal Haskell typeclasses as we attempted above.
Instead, GHC defines these constraints internally, by specifying that the appropriate
notion of evidence for an equality constraint is a _coercion_, a special part of the Core syntax.

A value of type `a ~# b` is a genuine proof that the types `a` and `b` are equal –
a nominal coercion. It can't be `undefined` or an `error`.
Similarly, a value of type `a ~R# b` is a genuine proof that the types `a` and `b` have
the same runtime representation – a representational coercion.

#### Coercions: a reading guide

To support its type level features such as GADTs and type-families, GHC uses coercions
to reason about equalities in types. This is most apparent with how Martin–Löf equality
is encoded in a GADT:

```haskell
type (:~:) :: forall k. k -> k -> Type
data a :~: b where
  Refl :: a :~: a
```

Let's see how GHC typechecks the following program:

```haskell
subst :: (a :~: b) -> a -> b
subst Refl x = x
```

We have `x :: a`, but we must produce a result of type `b`. We achieve this by **casting** `x`
with the **cast** operator `|>`, using the evidence that `a ~ b` obtained by matching on `Refl`.

```haskell
subst =
  \ @a @b (eq :: a :~: b) (x :: a) ->
    case eq of
      Refl (co :: b ~# a) -> x |> ( Sub ( Sym co ) :: a ~#R b )
```

Notice how the `Refl` constructor has an extra argument in Core, here `co :: b ~# a`. We can think of `Refl` as simply boxing
up a unboxed coercion. We can then cast `x` using the coercion `co2 = Sub ( Sym co ) :: a ~#R b` (to be explained below), obtaining `( a |> co2 ) :: b`.

GHC has a vast collection of coercions, which serves as its type-level proof language.
It is important to be able to recognise some common coercions,
as type-checking plugins will often manipulate them, and debugging a type-checker
plugin often involves inspecting coercions.  
We provide here a short and non-exhaustive inventory of coercions one is liable to encounter
in the depths of Core (refer to the [GHC Core specification](https://gitlab.haskell.org/ghc/ghc/-/blob/master/docs/core-spec/core-spec.pdf) for a complete list).  
In the following, `r` denotes the **role** of the coercion:
either `N` (Nominal) or `R` (Representational), with `~r` denoting `~#` or `~R#`, respectively.

- Reflexivity: `<a>_r :: a ~r a`.
- Symmetry: we can reverse the orientation of `co :: a ~r b`
  to obtain `Sym co :: b ~r a`.
- Transitivity: given `co1 :: a ~r b` and `co2 :: b ~r c`,
  we can compose them to get `co1 ; co2 :: a ~r c`.
- Type constructor applications: we can apply a type constructor to coercions.
  For instance, given `left_co :: a1 ~r a2` and `right_co :: b1 ~r b2`
  we obtain `(Either left_co right_co)_r :: Either a1 b1 ~r Either a2 b2`.
- Coercion applications: we can also apply one coercion to another.
  Given `co_f :: f1 ~r f2` and `co_arg :: arg1 ~# arg2` we get
  `co_f co_arg :: f1 arg1 ~r f2 arg2`.
- Type constructor decompositions: we can decompose type constructor applications.
  For instance, given `co :: Either a1 b1 ~# Either a2 b2`, we can obtain
  `Nth:0 co :: a1 ~# a2` and `Nth:1 co :: b1 ~# b2`.
- Downgrading: given `co :: a ~# b`, we can downgrade its role to `Representational`,
  with `Sub co :: a ~R# b`.
- Unsafe coercions: `Univ r prov :: ty1 ~r ty2`, where `prov` describes
  where such an universal coercion came from (its provenance),
  e.g. "a plugin unsafely claimed this equality".
- Coercion axioms, such as those derived from newtypes or type family equations;
  these are written `AxiomName[i]` for some natural number `i`; see below.

Let us give a simple example of a coercion axiom; we will return to this subject later
(see [§ Type family coercion axioms](#type-family-coercion-axioms)).
When we define a newtype such as

```haskell
newtype Sum a = Sum { getSum :: a }
```

we also obtain a way to coerce between `Sum a` and `a` **for any type `a`**.
So we don't have a single coercion, but rather a coercion constructor,
or **coercion axiom** in GHC parlance.
In this case, the coercion axiom is written `Sum[0]`;
it takes in a representational coercion `co :: a ~#R b`
and returns a representational coercion `Sum[0] co :: Sum a ~#R b`.

#### Single-method dictionaries

The dictionary associated with a typeclass with a single method is somewhat special,
as it is defined as a newtype, not as a datatype.
Thus, instead of retrieving dictionary fields, GHC coerces from the dictionary to the method using a **cast**.

To illustrate, if we remove the `(/=)` method of `Eq` (so that `Eq` has a single method `(==)`), we get the following Core:

```haskell
elem1b :: forall a. Eq a => a -> [a] -> Bool
elem1b = \ @a ($dEq_a :: Eq a) (x :: a) (xs :: [a]) ->
  case xs of
    []     -> False
    (y:ys) -> ( $dEq_a |> Eq[0] <a>_N ) x y || elem1b @a $dEq_a x ys
```

This cast `$dEq_a |> Eq[0] <a>_N` requires some explanation:
we are casting the dictionary `$dEq_a` using the coercion `Eq[0] <a>_N`;
this is akin to using `coerce` in source Haskell.
We can consider that GHC has internally defined:

```haskell
newtype Eq a = MkEq { (==) :: a -> a -> Bool }
```

To go from `$dEq_a :: Eq a` to `(==) :: a -> a -> Bool`, we `coerce`. This means using a representational type equality.
As we saw above, whenever we define a newtype, `GHC` creates a corresponding coercion axiom, in this case `Eq[0]`, which takes a nominal coercion `co :: a ~# b` and returns
a representational coercion `Eq[0] co :: Eq a ~R# (b -> b -> Bool)`.
To retrieve the method `(==) :: a -> a -> Bool`, we apply the coercion axiom to the reflexive nominal coercion, written `<a>_N`.
This results in the coercion above:

```haskell
Eq[0] <a>_N :: Eq a ~R# (a -> a -> Bool)
```

which we can then use to cast, obtaining the method:

```haskell
( $dEq_a |> Eq[0] <a> ) :: a -> a -> Bool
```

### Constraint canonicalisation

Before reviewing how GHC goes about solving constraints,
let's first look at how GHC itself rewrites constraints and classifies them into different categories.

User-written constraints are born **non-canonical**: as GHC typechecks a type signature,
it adds the constraints it comes across to its **work list**, to be processed later.  
Initially, GHC hasn't analysed these constraints to determine their nature.
For instance, one might have a type family

```haskell
type TyFamCt :: Type -> Constraint
type family TyFamCt a where
  TyFamCt Bool  = ( () :: Constraint )
  TyFamCt (a,b) = a ~ b
  TyFamCt c     = Integral c
```

In general, we don't know what kind of constraint `TyFamCt a` is. If we can't reduce the type family application, we're stuck,
so we say this is an **irreducible** constraint. However, we might be able to rewrite the type family application, e.g.
`TyFamCt Int` will be canonicalised to `Integral Int`, a typeclass constraint; on the other hand, something like
`TyFamCt (x, y)` will be canonicalised to `x ~# y`, an equality constraint.

| Predicate   | Examples                              | Evidence      |
| ----------- | ------------------------------------- | ------------- |
| Typeclass   | `Ord a`, `Num a`, `(c1, c2)`, `a ~ b` | Dictionary    |
| Equality    | `a ~# b`, `a ~R# b`                   | Coercion      |
| Quantified  | `forall a. Eq a => Eq (f a)`          | Function      |
| Irreducible | `c a`, `F x y`                        | Not yet known |

The job of the canonicaliser is to rewrite the constraint as much as possible,
e.g. reducing all type-family applications contained within the constraint.
Note that, once a constraint is canonicalised, it is not necessarily frozen.
For instance, `TyFamCt x` is an irreducible constraint, but we might later instantiate `x` to `Int`,
in which case the constraint will be re-canonicalised into the typeclass constraint `Integral Int`.

### Constraint solving

#### A simple example

Let's now briefly review how GHC goes about solving constraints.
Consider the following simple example:

```haskell
palindrome :: Eq a => [a] -> Bool
palindrome ds = ds == reverse ds
```

GHC compiles this program to the following Core:

```haskell
palindrome :: Eq a => [a] -> Bool
palindrome = \ @a ($dEq_a :: Eq a) ->
  let
    $dEq_List_a :: Eq [a]
    $dEq_List_a = $fEq_List @a $dEq_a
  in \ (ds :: [a]) -> (==) @[a] $dEq_List_a ds (reverse @a ds)
```

What has happened here is that the `palindrome` function was provided with the constraint `Eq a`,
but in its body it calls `(==)` at type `[a]`, which requires `Eq [a]`.

In GHC parlance, we are solving an **implication constraint**, we have a **Given** constraint

```
[G] $dEq_a :: Eq a
```

This means we have evidence for the constraint `Eq a` (which will be provided by the caller
of the `palindrome` function). We must use it to synthesise evidence for the **Wanted** constraint

```
[W] $dEq_List_a :: Eq [a]
```

GHC begins by canonicalising `[G] $dEq_a :: Eq a`; it's obviously a dictionary constraint.
This constraint then gets added to the **inert set**, which is the collection of constraints that GHC
considers fully processed. As noted previously, addition of new information might allow rewriting
to take place, in which case GHC could **kick out** a constraint (removing it from the inert set),
to continue working on it later.

Next, we canonicalise `[W] $dEq_List_a :: Eq [a]`, which is also clearly a dictionary constraint.  
We then notice that the constraint `Eq [a]` matches with the class instance head

```haskell
instance forall x. Eq x => Eq [x] where { .. }
```

(Recall that GHC only looks at instance heads when determining which instances to use; it never looks at
the instance context before committing to an instance.)

Associated to the above instance is the **dictionary function** (or `DFun`)

```haskell
$fEq_List :: forall x. Eq x -> Eq [x]
```

which takes the dictionary evidence for `Eq x` and builds the corresponding dictionary
evidence for `Eq [x]`. GHC thus solves the Wanted constraint:

```haskell
$dEq_List_a = $fEq_List @a $dEq_a
```

discharging it from the work list.

#### The interaction pipeline

To recapitulate, when processing a work list of constraints, GHC will pick a work item from the work list
and take it through the interaction pipeline:

<p align="center">
<img src=interaction_pipeline.svg alt="Flowchart of the constraint interaction pipeline: canonicalisation, inert reactions, top-level reactions." />
</p>

In the "inert reactions" stage, the work item interacts with the constraints in the inert set.
For instance, if the work item is a Wanted constraint, we might want to know whether we can solve it
using Givens in the inert set (or perhaps just simplify it).  
In the "top-level reactions" stage, we use top-level instances. This is what happened when we used
the top-level instance `instance forall x. Eq x => Eq [x]` to solve `[W] Eq [a]` using `[G] Eq a`.

Each step of this pipeline can change the work list or the inert set. After each step, we either

- go back to the start, e.g. because we want to change work item, or
- continue, with a possibly rewritten constraint.

Only once a work item makes it through the entire pipeline does GHC decide to then add it to the inert set.

#### The constraint solving loop

Now that we know how GHC processes its work list, we want to see how items get added to the work list in
the first place. The most important aspect to understand is GHC's **constraint solving loop**. Typically,
when typechecking something, one can encounter two types of work:

- simple constraints, i.e. type-checking `f (x :: a) = x + 1`
  will incur a simple Wanted `Num a` constraint,
- implication constraints, like we saw with the `palindrome` example,
  which gave rise to an implication a Given `Eq a` and a Wanted `Eq [a]`.

The first step, that of simplifying simple Given constraints/solving simple Wanted constraints,
is where type-checking plugins get to have their say:

<p align="center">
<img src=solver_plugins.svg alt="Solver plugin stage interaction with the typechecker" />
</p>

After processing simple constraints, GHC will proceed to go under implications.
These can be nested: for instance, a successful pattern match on a GADT
might introduce new information, which might need to be used when type-checking
the RHS of the pattern match:

```haskell
class C a where {..}
class D a where {..}
class E a where { methE :: a -> Int }

instance (C a, D a) => E a where {..}

data G a where
  MkG1 :: C a => a -> G a
  MkG2 :: Integral a => a -> a -> G a

foo :: D a => G a -> Int
foo x = case x of
  MkG1 a -> methE a
  MkG2 i j -> fromIntegral (i + j)
```

When type-checking `foo`, GHC processes the constraints, canonicalising them and
eventually generating the following nested implications:

```
[G] D a
  ===>
    [ [G] C a ===> [W] E a
    , [G] Integral a ===> [W] Integral a, [W] Num a
    ]
```

To solve a collection of constraints, containing both simple Wanteds as well as implications, we proceed as follows:

- Run a constraint solving loop on all the simple Wanted constraints.
  Note that this step can create new implication constraints.
- Process the wanted implication constraints.
  For each implication, we will:
  - Process its Givens (e.g. adding them to the inert set, when this makes sense).
  - Solve the inner simple Wanted constraints.
  - Recur into the nested implications.
  - Reset the inert set to what it was before entering the implication,
    so that we can continue processing the other implications.

<p align="center">
<img src=solving_implications.svg alt="Flowchart for simplifying/solving implication constraints" />
</p>

#### Manually discharging single-method constraints

Note that GHC provides a mechanism to manually discharge single-method constraints,
in the form of `withDict` (GHC 9.4 and above):

```haskell
withDict :: c_dict -> (c => r) -> r
```

That is, one can discharge the `c` constraint by explicitly passing its dictionary, of type `c_dict`. For example:

```haskell
class MyCt a where
  myMeth :: a -> a -> a
-- NB: no instances.

myFun :: [Int] -> [Int] -> Int
myFun xs ys = withDict @(Int -> Int -> Int) @(MyCt Int) (+) (sum $ zipWith myMeth xs ys)
```

Here we have `sum $ zipWith myMeth xs ys :: MyCt Int => Int`, and we discharge the `MyCt Int` constraint
by passing the dictionary `(+) :: Int -> Int -> Int`.

## Type families

Type families are one of the most powerful tools we have when type-level programming in Haskell,
as they can be used to perform complex type-level reasoning.
Let's quickly review a few salient aspects, with a focus on the more powerful _closed_ type families.

### A primer on type family reduction

For the most part, type family reduction follows the same general principles as regular pattern matching:

```haskell
lookup :: forall k v. Eq k => k -> [(k,v)] -> Maybe v
lookup _ []
  = Nothing
lookup k ( (l,v) : kvs )
  | k == l
  = Just v
  | otherwise
  = lookup k kvs

type Lookup :: forall k v. k -> [(k,v)] -> Maybe v
type family Lookup k kvs where
  Lookup _ '[]
    = Nothing
  Lookup k ( '(k,v) : _ )
    = Just v
  Lookup k ( _ : kvs )
    = Lookup k kvs
```

Already one difference can be spotted: type families support **non-linear** patterns, in which a variable
occurs more than once. In this case, the second equation only matches if the first visible argument to `Lookup`,
`k`, is equal to the first key in the list passed as second visible argument.

When reducing a type family application such as

```haskell
Lookup "name" '[ '("occupation", "mathematician"), '("name", "晴三") ]
```

we step through to check which branch to take. In this case:

- the first branch doesn't match: the second (visible) argument is not an empty list,
- the second branch doesn't match: `"name"` is definitely not equal to `"occupation"`,
- the third branch does match, so we reduce:

```haskell
Lookup "name" '[ '("occupation", "mathematician"), '("name", "晴三") ]
  ~
Lookup "name" '[ '("name", "晴三") ]
```

We can keep going: this time the second equation matches, and we reduce further:

```haskell
Lookup "name" '[ '("name", "晴三") ] ~ "晴三"
```

Another peculiar aspect of type family reduction is that the `forall` quantifier in the kind signature
is actually a misnomer: in reality, type families behave as if we had a **relevant** (instead of _irrelevant_)
dependent quantifier. This simply means that we are allowed to perform case analysis on the quantified
kind variables, for instance:

```haskell
type Weird :: forall k. k
type family Weird where
  Weird = Int
  Weird = Maybe

weird :: Weird -> Weird Int
weird x = Just (x+1)
```

What is happening here is that the type family is implicitly matching on the invisible argument:

```haskell
type family Weird @k where
  Weird @Type           = Int
  Weird @(Type -> Type) = Maybe

weird :: Weird @Type -> Weird @(Type -> Type) Int
```

That is, we should really be writing:

```haskell
type Weird :: foreach k. k
```

Here `foreach` denotes relevant dependent quantification (i.e. a dependent product type),
and is one of the [Dependent Haskell quantifiers](https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell#4-quantifiers).
The use of this quantifier corresponds to the fact that implicitly quantified kinds behave as normal arguments.
Indeed, when writing a type-checking plugin, we will see that they are put on the same footing as visible
arguments.

### Type family coercion axioms

We've just seen some basic aspects of type-family reduction in Haskell.  
When writing a type-checking plugin, it's also important to know about the implementation
of type families in GHC. As type families come from equations between types, it should come
as no surprise that they are closely tied with coercions.

It's never a bad idea to look at Core to see how GHC handles things.
Let's take the following simple example:

```haskell
type F :: Type -> Type
type family F a where
  F Word      = Int
  F (Maybe a) = Maybe (F a)
  F a         = a

g :: Maybe (F Word) -> F (Maybe Int)
g mb_x = fmap negate mb_x
```

We obtain the following Core (omitting role information and downgrades):

```haskell
g1 :: [F Word] -> [Int]
g1 = \ ( mb_x :: Maybe (F Word) ) ->
  fmap @Maybe $fFunctorMaybe @Int @Int
    ( negate @Int $fNumInt )
    ( mb_x |> Maybe F[0] )

g :: [F Word] -> F [Int]
g = g1 |> ( <Maybe (F Word)> -> Maybe ( Sym ( F[2] <Int> ) ) ; Sym ( F[1] <Int> ) )
```

Recalling the syntax of coercions from [§ Coercions, a reading guide](#coercions-a-reading-guide),
we can make sense of this. The basic principle is that GHC associates to `F` a **coercion axiom**
with three branches:

- `F[0] :: F Word ~# Int`,
- `F[1]`, which takes in `co :: a ~# b` and returns `F[1] co :: F (Maybe a) ~# Maybe (F b)`,
- `F[2]`, which takes in `co :: a ~# b` and returns `F[2] co :: F a ~# b`.

Now, looking at `g1` first:

- In `g1`, we use the `0`-th equation of `F` to obtain the coercion `F[0] :: F Word ~# Int`.  
  We then apply the `Maybe` type constructor to `F[0]` to obtain
  `Maybe F[0] :: Maybe (F Word) ~# Maybe Int`.  
  This allows us to typecheck `\ mb_x -> fmap negate ( mb_x |> Maybe F[0] )`
  at the type `Maybe (F Word) -> Maybe Int`.

- The second piece of the puzzle comes from the following two coercions:

  - `F[1] <Int> :: F (Maybe Int) ~# Maybe (F Int)`,
  - `F[2] <Int> :: F Int ~# Int`.

  As we need to rewrite from `Maybe Int` to `F (Maybe Int)`, we reverse these using `Sym`,
  and apply the `Maybe` type constructor to the second coercion so that we can compose them:

  - `Maybe ( Sym ( F[2] <Int> ) ) ; Sym ( F[1] <Int> ) :: Maybe Int ~# F (Maybe Int)`.

  As we are casting between function types, we need to handle the argument type as well as
  the result type. We use the coercion we just obtained for the result type;
  for the argument type, the reflexive coercion `<Maybe (F Word)>` will do.

We thus recover the coercion that we saw in the `Core`:

```haskell
<Maybe (F Word)> -> Maybe ( Sym ( F[2] <Int> ) ) ; Sym ( F[1] <Int> )
  :: ( Maybe (F Word) -> Maybe Int ) ~# ( Maybe (F Word) -> F (Maybe Int) )
```

### Conclusion

We've now seen:

- how GHC represents constraints, which will be useful when we want to manipulate
  them in a plugin,
- how GHC solves constraints, and at which point plugins get invoked during constraint solving,
- some aspects of type family reduction and their relation with coercions, which will be
  important to know when rewriting type family applications in a plugin, and in debugging
  Core Lint issues.

In the next part of this series, we will see how to write a type-checking plugin.
You certainly don't need to know everything outlined here to get started, but you can
refer back to this document in case something goes wrong (e.g. constraint solving not behaving as expected,
or a Core Lint error).

[^1]:
  Core is a small functional language, based on [System F](https://en.wikipedia.org/wiki/System_F)
  with [added coercions](https://www.microsoft.com/en-us/research/publication/system-f-with-type-equality-coercions),
  which Haskell is desugared into after typechecking. Many of GHC's optimisations happen on the level of Core.
  To see the Core that GHC generates when compiling a Haskell program, use the `-ddump-simpl` GHC option;
  additional flags such as `-dsuppress-uniques -dsuppress-idinfo -dsuppress-ticks -dsuppress-module-prefixes`
  can make the Core easier to read.

[^2]:
  This evidence must be passed at runtime: the types are erased, so we must use
  another mechanism to record which instance was selected (i.e. which implementation of the typeclass methods to use).

[^3]:
  A homogeneous coercion is one that relates types of the same kind, while a heterogeneous
  coercion can relate two types of different kinds.
