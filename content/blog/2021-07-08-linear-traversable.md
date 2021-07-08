---
title: "Exploring linear Traversable using generics"
shortTitle: "Exploring linear Traversable"
author: Sjoerd Visscher
tags: [haskell, linear-types]
description: "What should the type of linear traverse be exactly?"
---

The design of a library like [`linear-base`][linear-base], Tweag's standard library for Haskell's
[linear types][linear-types], requires many choices.
One of these choices for `linear-base` is what a linear `Traversable`
type class should look like.

One way I use to approach such problems
is to implement instances for the types in [`GHC.Generics`][ghc-generics].
`GHC.Generics` provides a way to implement datatype-generic functions by translating a datatype to and from
a limited set of primitive datatypes representing sums, products, constants, etc.
Therefore, a type class implemented for this set can be given an
instance at a large class of algebraic datatypes.
So this is a nice way to apply structural reasoning to a problem.

## The problem

Here is the signature of `fmap` in `linear-base`, as it applies, for instance, to lists:

```haskell
-- The syntax for linear functions is `%1->`.
-- Linear functions consume their input exactly once.
fmap :: (a %1-> b) -> t a %1-> t b
```

Correspondingly, it seems pretty clear that the type of linear `traverse` should be:

```haskell
traverse :: Applicative f => (a %1-> f b) -> t a %1-> f (t b)
```

But which `Applicative` type class should we use? In the context of linear types, there are three different ones! The one from the prelude:

```haskell
Prelude.pure :: a -> f b
Prelude.fmap :: (a -> b) -> (f a -> f b)
Prelude.liftA2 :: (a -> b -> c) -> (f a -> f b -> f c)
```

And [two linear ones][data-vs-control]: a linear _data_ `Applicative`:

```haskell
Data.pure :: a -> f b
Data.fmap :: (a %1-> b) -> (f a %1-> f b)
Data.liftA2 :: (a %1-> b %1-> c) -> (f a %1-> f b %1-> f c)
```

and a linear _control_ `Applicative`:

```haskell
Control.pure :: a %1-> f b
Control.fmap :: (a %1-> b) %1-> (f a %1-> f b)
Control.liftA2 :: (a %1-> b %1-> c) %1-> (f a %1-> f b %1-> f c)
```

Note that I'm leaving out `<*>`. Its type signature hides a nice symmetry, and can be defined as `liftA2 ($)` in all cases.

Another thing to note is that any linear function is also a regular function, which means that any linear _control_ `Applicative`
is also a linear _data_ `Applicative`, so the latter is a superclass
of the former. On the other hand, neither linear `Applicative` is a subclass (or superclass)
of the prelude `Applicative`, since they can be given linear functions as input, while prelude `Applicative`
requires regular functions as input (subtyping goes the other way
for inputs).

## Instances of `GHC.Generics` datatypes

### `V1`

Let's start by writing an instance for `V1`, which represents empty data types. Such instances are usually either easy or impossible
to implement. In this case we're lucky since callers of `traverse` are supposed to provide a value of type `V1 a`,
which doesn't exist, so pattern matching on it (using the extensions `LambdaCase` and `EmptyCase`) is all we need to do.
There's also no need for any `Applicative` method, so we don't learn anything either.

```haskell
-- data V1 a
instance Traversable V1 where
  traverse _ = \case
```

### `U1`

Next is `U1`, which represents constructors with no fields. We get a value of type `U1 a` and need to produce a value of type `f (U1 b)`.
We can produce values of type `U1 b` for any `b` out of thin air by applying `pure`, linear or not.
So this works for all 3 `Applicative`s. But note that the linearity forces us to pattern match on the input
to prove that we completely consume it. Just `traverse _ _ = pure U1` does not work.

```haskell
-- data U1 a = U1
instance Traversable U1 where
  traverse _ U1 = pure U1
```

### `Par1`

The constructor `Par1` represents uses of the type parameter. It is declared as

```haskell
newtype Par1 a = Par1 a
```

and `traverse` at this type is:

```haskell
traverse :: (a %1-> f b) -> (Par1 a %1-> f (Par1 b))
```

We're going to implement this in roughly the following way:

```haskell
traverse g (Par1 a) = let fb = g a in fmap Par1 fb
```

Since we're promising to use `Par1 a` linearly, every step needs to be linear.
We pattern match to get an `a`, apply the input function of type `a %1-> f b` and get `fb :: f b` linearly.
But we need `f (Par1 b)`, so we apply `fmap Par1`. But `fmap` from the Prelude isn't linear,
it does not promise to use `fb` exactly once, so we can't use it here for linear `Traversable`!
That means that the prelude `Applicative` is also out.

We're down to either the linear _data_ `Applicative` or the linear _control_ `Applicative`.
Thanks to the superclass relation between the two we can use the `Data` one here in both cases:

```haskell
instance Traversable Par1 where
  traverse g (Par1 a) = Data.fmap Par1 (g a)
```

### `:+:`, `:.:`, `M1`, `Rec1`

The instances for the datatypes representing multiple constructors (sums) `:+:`, composition of type constructors `:.:`,
meta information `M1` and reference to other type constructors `R1` just use `fmap`
and don't provide any new information.

```haskell
-- data (l :+: r) a = L1 (l a) | R1 (r a)
instance (Traversable l, Traversable r) => Traversable (l :+: r) where
  traverse f (L1 la) = Data.fmap L1 (traverse f la)
  traverse f (R1 ra) = Data.fmap R1 (traverse f ra)
-- newtype (s :.: t) a = Comp1 (s (t a))
instance (Traversable s, Traversable t) => Traversable (s :.: t) where
  traverse f (Comp1 sta) = Data.fmap Comp1 (traverse (traverse f) sta)
-- newtype M1 i c t a = M1 (t a)
instance Traversable t => Traversable (M1 i c t) where
  traverse f (M1 ta) = Data.fmap M1 (traverse f ta)
-- newtype Rec1 t a = Rec1 (t a)
instance Traversable t => Traversable (Rec1 t) where
  traverse f (Rec1 ta) = Data.fmap Rec1 (traverse f ta)
```

### `:*:`

In the case of the product `(:*:)`, we need to combine two `Applicative` values using `liftA2`.
Again we have to do this linearly so the prelude `Applicative` does not work, but the two linear ones do.

```haskell
-- data (l :*: r) a = l a :*: r a
instance (Traversable l, Traversable r) => Traversable (l :*: r) where
  traverse f (la :*: ra) = Data.liftA2 (:*:) (traverse f la) (traverse f ra)
```

### `K1`

We have one more datatype left to do, `K1` for constants. We have an input value of type `K1 i c a` and need
to produce a value of type `f (K1 i c b)`. We can pattern match on the input to get the constant of type `c` out,
and apply the `K1` constructor again to produce a value of type `K1 i c b`. Next we can apply `pure` to get the
desired result, but we need to do so linearly and `Data.pure` is not linear! So we're left with just the linear
_control_ `Applicative`, and this is indeed what `linear-base` [provides][traversable].

```haskell
-- newtype K1 i c a = K1 c
instance Traversable (K1 i c) where
  traverse _ (K1 c) = Control.pure (K1 c)
```

## An alternative

We couldn't use `Data.pure` because it doesn't consume its input linearly, and we can use the constant just once.
But what if we could use the constant more than once? This is exactly what [`Movable`][movable] from `linear-base` provides!

```haskell
class Movable a where
  move :: a %1-> Ur a
```

Here `Ur a` (`Ur` stands for unrestricted) gives unlimited access to values of type `a`, even in linear context.
So if we have an instance of `Movable` for the constant we _can_ use `Data.pure`, and therefore it seems useful
to also have a version of `Traversable` that uses the linear _data_ `Applicative`.

```haskell
instance Moveable c => Traversable (K1 i c) where
  traverse _ (K1 c) = move c & \case (Ur c') -> Data.pure (K1 c')
```

This would lead to instances like:

```haskell
instance Movable a => Traversable (Either a)
instance Movable a => Traversable ((,) a)
```

Is having two different `Traversable` classes worth it? It seems so, for at least one particular reason:
`Foldable` can be shown to be derived from `Traversable` using the `Const` `Applicative`. But `Const` is
not a linear _control_ `Applicative`! It is only a linear _data_ `Applicative`,
because its `pure` implementation throws away the argument, so it is not linear. There is an [issue][issue190]
open to add these classes to `linear-base`.

## Conclusion

One final thought (and you might have been wondering about this) now that we have instances of `Traversable` for all
`GHC.Generics` datatypes: can we generically derive instances of linear `Traversable`? Well, there is one catch. The
`to1` and `from1` methods from the `Generic1` class that are needed are not linear! They should be in principle, since
instances are not supposed to throw stuff out nor duplicate anything either. But we can't be sure, so for now we can
only provide generic instances using [`Unsafe.toLinear`][unsafe-linear] with a big fat warning sign.
(We're still [working out][linear-base-pr] the best way to do so.)

I hope you enjoyed this exploration of linear traversals over the zoo of `GHC.Generics` datatypes as much as I did.
I encourage you to play with linear types a bit, it can be really surprising what works and what doesn't work linearly!

[linear-base]: https://tweag.io/blog/2021-02-10-linear-base/
[data-vs-control]: https://tweag.io/blog/2020-01-16-data-vs-control/
[ghc-generics]: https://hackage.haskell.org/package/base-4.14.1.0/docs/GHC-Generics.html
[traversable]: https://hackage.haskell.org/package/linear-base-0.1.0/docs/Data-Functor-Linear.html#t:Traversable
[movable]: https://hackage.haskell.org/package/linear-base-0.1.0/docs/Data-Unrestricted-Linear.html#t:Movable
[issue190]: https://github.com/tweag/linear-base/issues/190
[unsafe-linear]: https://hackage.haskell.org/package/linear-base-0.1.0/docs/Unsafe-Linear.html
[linear-types]: https://tweag.io/blog/tags/linear-types
[linear-base-pr]: https://github.com/tweag/linear-base/pull/316
