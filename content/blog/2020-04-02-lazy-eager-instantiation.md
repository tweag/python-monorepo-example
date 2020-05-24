---
title: "Eager vs. Lazy Instantiation:  Making an Informed Decision"
shortTitle: "Eager vs. Lazy Instantiation"
author: Gert-Jan Bottu
tags: [haskell, internship]
description: "This blog post describes the tradeoffs of the choice between eager and lazy instantiation of type variables in GHC."
---

During my internship at Tweag, I've been given the opportunity to work on GHC
alongside Simon Peyton Jones at Microsoft Research Cambridge (MSRC) and my
Tweag supervisor Richard Eisenberg. During a visit at MSRC, I got caught up in
a [discussion][discussion] regarding the lazy or eager instantiation of type
variables in GHC.

This discussion serves as a great showcase for how language design works in
practice: it is a hard and involved process where not everyone will agree on
the same answers. In this blog post I will show both sides of the discussion in
order to:

1. Showcase the kind of tradeoffs that are made in language design.
2. Clarify this discussion and its relevance for the Haskell language and its
   community.
3. Make sure that you, as a Haskell developer, are sufficiently informed when
   you encounter the problems described here in your day to day life.

I thus wholeheartedly encourage you to visit the [GitHub thread][discussion]
after reading this post!

You can find all the code examples from this blog post [here][code].

[proposal]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0287-simplify-subsumption.rst
[discussion]: https://github.com/ghc-proposals/ghc-proposals/pull/287#issuecomment-579219615
[code]: https://raw.githubusercontent.com/tweag/blog-resources/blogpost-lazy-eager-inst/2020-02-05-lazy-eager-instantiation/Instantiation.hs

## What's the Problem?

Over the past few months, we encountered the following three issues:

### Synonyms

Consider the following example, where we define `myConst` to be a synonym
for `const`, whose type is `forall a b. a -> b -> a`:

```hs
myConst = const
```

When inferring the type for `myConst`, the compiler _instantiates_ the type of
`const`, meaning that the type variables `a` and `b` get replaced by
(potentially yet unknown) types. Afterwards, the compiler _generalises_ this
type, meaning that all remaining unknown types get bound using new `forall`
binders.

But should the resulting type for `myConst` be `forall a b. a -> b -> a` or
`forall b a. a -> b -> a`? There is no way for the compiler to know the
intended order of the generalised variables. While these types look equivalent,
they are most certainly not in combination with type applications. Should the
type of `myConst @Int` be `forall b. Int -> b -> Int` or `forall a. a -> Int -> a`?

For this reason GHC only allows type application for user defined type
variables, called "specified" variables. Compiler generated variables, called
"inferred" variables, can never be manually instantiated, and GHC marks them
using braces, resulting in `forall {a} {b}. a -> b -> a`. If you want to know
more about this distinction, you can read my last [blog post][blogpost], which
goes into much greater depth on the topic.

This means that `myConst` and `const` do not have the same type, which
contradicts our intuition that they should behave identically.

[blogpost]: https://www.tweag.io/posts/2020-03-12-expl-spec.html

### Type Abstraction

While discussing type inference for type abstraction in lambda binders, another
issue popped up, this time related to type abstraction, as discussed in this
accepted (but not yet implemented) [GHC proposal][proptyabs]. Just as you would
write a lambda binder `\ x -> e` to introduce a term variable `x` and bring
it into scope in `e`, this proposal allows `\ @a -> e` to bind a type
variable `a` and bring it into scope in `e`.

At the moment, the proposal only discusses this feature under type-_checking_,
meaning that a type signature needs to be present. However, in order to extend
this feature with type inference, things got a bit more hairy. Consider the
following example:

```hs
foo = \ @a (x :: a) -> x
```

We would expect the inferred type for `foo` to
be `forall a. a -> a`, since we explicitly abstract over `a`: `a`
is, literally, specified.
However, under eager type instantiation, the type for `foo` would actually be
`forall {a}. a -> a`.

[proptyabs]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0155-type-lambda.rst

### Nested Foralls

A third issue appears when instantiating types with nested `forall` binders.
Consider the following example:

```hs
{-# LANGUAGE RankNTypes #-}

f :: forall a. a -> forall b. b -> b
f x = id

g = f
```

We ask GHC to infer the type of `g` for us, and as you might expect from the
`myConst` example above, the types of `f` and `g` are not identical:

```hs
*Main> :set -fprint-explicit-foralls
*Main> :info f
f :: forall a. a -> forall b. b -> b
*Main> :info g
g :: forall {a}. a -> forall b. b -> b
```

While typing `g`, GHC instantiates and generalises the binder for `a`. As a
consequence, the type of `g` now contains a mix of type variables which can
and can not be manually instantiated. This hard-to-predict handling of type
variables is quite confusing.

This behaviour was introduced in a recent [GHC proposal][proposal] and is
expected to be released in GHC 8.12. If you want to try this example
for yourself, you can download our prebuilt GHC compiler using
`docker pull gertjanb/simplified-subsumption-ghc`, and run it using
`docker run -it gertjanb/simplified-subsumption-ghc`.

## Eager vs. Lazy Instantiation

All three issues explained above involve type variable instantiation, so let us
explore this in a bit more detail. When type inference encounters the variable
`f` in the last example, a choice emerges when assigning it a type:

- GHC 8.10 instantiates types eagerly, resulting in the following type:
  `alpha -> forall b. b -> b` (note that we use greek letters to denote types
  yet to be determined). Finally, generalisation produces for `g` the type
  `forall {a}. a -> forall b. b -> b`.

- Another choice is to instantiate lazily, that is, returning the type of `f`
  as is, and only instantiating it when needed. The function `g` thus gets
  assigned the type of `f`: `forall a. a -> forall b. b -> b`.

## A Silver Bullet!

Up to GHC 8.10, type variables have always been instantiated eagerly. However,
lazy instantiation might solve our issues above! Let's investigate:

### Synonyms

When inferring a type for `myConst` under lazy instantiation, the type
variables of `const` would not get instantiated, resulting in the inferred
type `forall a. a -> a`. This is the type we would expect, and
indeed GHC no longer treats `const` and `myConst` differently.

### Type Abstraction

When inferring a type for `foo` under lazy instantiation, the user-specified
type variable `a` does not get abstracted over, and the inferred type becomes
`forall a. a -> a`. This is again the type we would expect for `foo`, and
using this type as a signature works like a charm.

### Simplified Subsumption

A similar story holds for our `g` example above. Since type variables do not
get instantiated unless absolutely necessary, its type becomes identical to the
type of `f`: `forall a. a -> forall b. b -> b` with no inferred variables.

## ... But Comes at a Cost.

Unfortunately, as pointed out by Simon Peyton Jones, lazy instantiation might
not be the amazing solution we hoped it would be. While figuring out the details
of how this should work in practice, a number of new issues popped up:

### Case expressions

Type inference for case expressions requires the compiler to assign a
monomorphic type to each of the branches. This means that the type can not
contain top-level `forall` binders or binders on the right of function
arrows. In order to illustrate this restriction, consider the following
example:

```hs
bar1 True  = \ x -> id

bar2 True  = \ x -> id
bar2 False = error "Impossible case for reasons"
```

Under eager instantiation, the inferred types are as you would expect:
when encountering `id`, its forall type is instantiated eagerly.
This results in the type
`forall {a} {b}. Bool -> a -> b -> b` for both `bar1` and `bar2`.

The case of lazy instantiation is more interesting. The function `bar1` gets
the type `forall {a}. Bool -> a -> forall b. b -> b`. But by adding the
catch-all sanity check in `bar2`, we are actually introducing a case
expression, thus forcing the compiler to return a monomorphic type. The
`forall`s get instantiated, resulting in the same type we get from eager
instantiation.

### Evaluation

So far the impact of our discussion has mainly been limited to type level
differences. However, these type level choices do have an impact on the actual
evaluation of the program. Consider the following example:

```hs
{-# LANGUAGE BangPatterns #-}

diverge = let !x = undefined in ()
```

Note that the bang forces GHC to evaluate `x` eagerly. We would thus expect
this function to throw an exception. However, while this is certainly the case
under eager instantiation, this does not hold when variables are instantiated
lazily. This happens because the type of `undefined` is `forall a. HasCallStack => a`, and eager instantiation will instantiate the type `a`
and the `HasCallStack` argument. Evaluating this instantiated `undefined`
throws an exception, as we would expect. However, under lazy instantiation, the
type of `undefined` remains effectively a function type (from
`HasCallStack` evidence to type `a`), and functions do not diverge.

### Implicit Arguments

The story becomes even more involved when we include GHC's implicit arguments.
While recognizing that this extension is not widely used, it remains important
to take all the compiler features into account when considering changes. Take
for instance the following code example:

```hs
{-# LANGUAGE ImplicitParams #-}

x :: (?i :: Int) => Int
x = ?i

y :: (?i :: Int) => Int
y = let ?i = 5 in x

z :: Int
z = let ?i = 6 in y
```

Again, the choice of either eager or lazy instantiation determines the
evaluation outcome. Similarly to before, under eager instantiation, while typing
`y`, the type of `x` gets instantiated right away with `?i = 5`. On the
other hand, under lazy instantiation, this is postponed as far as possible.
Concretely, at the very end when typing `z`, the implicit variable `?i` has
to be instantiated, in this case with `?i = 6`.
This means that `z` evaluates to `5` under eager instantiation (as most
people would expect), but evaluates to `6` under lazy instantiation.

## Compromises Have to be Made

I hope this blog post illustrates that these decisions are just plain hard, and
ultimately quite subjective. Both eager and lazy instantiation are reasonable
approaches and lead to type-safe languages. In the end the choice comes down
to taste and desire for the best user experience.

After going back and forth a couple of times, we finally concluded that eager
instantiation seems most sensible: while lazy instantiation would certainly
solve the three issues described above, it unfortunately comes at too heavy a cost.
Instead, we will just have to accept the strangeness of synonyms not behaving
like we expect and shallow instantiation making some variables inferred while
keeping others specified. Regarding type abstraction in lambda binders, to avoid
the issues described above, we propose limiting this feature to type checking
only.

## Conclusion

GHC is made by the Haskell community, so it's important that you're informed
about this discussion. I thus wholeheartedly encourage you to visit the [Github
page][discussion] and continue reading about the topic.
