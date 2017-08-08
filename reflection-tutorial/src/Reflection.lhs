---
title: Type Class reflection: a tutorial
author: Arnaud Spiwack
featured: yes
---

Among our tools, at Tweag I/O, is [type-class
reflection][reflection-package]. We don't reach for it often, but it can be very
useful.  Reflection is rather little known, and when it is, it is often spoken
of with an hint of fear.

In this post, I want to convince you that reflection is not hard and that you
ought to know about it. To that end, let me invite you to join me on a journey
to sort a list:
```haskell
sortBy :: (a->a->Ordering) -> [a] -> [a]
```

But first:

What is reflection?
===================

[Type-Class reflection][reflection-package] is a mechanism to use a value as a
type-class instance. The types are a little scary:

```haskell
class Reifies s a | s -> a where
  reflect :: proxy s -> a
reify :: forall a r. a -> (forall s. Reifies s a => Proxy s -> r) -> r
```

We will come back to these types later. The important thing, for the moment, is
that `Reifies` and `reify` are the entirety of the interface for type-class
reflection: we only have to understand three lines.

Literate Haskell
================

This being a literate Haskell file, so that you can easily copy paste the code
and start playing, there is a bit of boilerplate to get out of the way first.

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE UndecidableInstances #-}
>
> module Reflection where
>
> import Data.Proxy
> import Data.Reflection

Yes, I know:
[`UndecidableInstances`](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html?highlight=rebindablesyntax#ghc-flag--XUndecidableInstances). It
is unfortunately required. Our usage will be safe, of course, but it is never
pleasant to have this extension around.


Sorted lists
============

As I said at the beginning: my goal, today is to sort a list. In order to make
the exercise a tiny bit interesting, I will use types to enforce invariants. Let
me start by introducing a type of _sorted_ lists.

> newtype SortedList a = Sorted [a]

Obviously, a `SortedList` is a list: we can just forget about its sortedness.

> forget :: SortedList a -> [a]
> forget (Sorted l) = l

But how does one construct a sorted list? Well, at the very least, the empty
lists and the lists of size 1 are always sorted.

> nil :: SortedList a
> nil = Sorted []
>
> singleton :: a -> SortedList a
> singleton a = Sorted [a]

What about longer lists though? We could go about it in several ways. Let's
decide to take the union of two sorted list:

> merge :: Ord a => SortedList a -> SortedList a -> SortedList a
> merge left0 right0 = doMerge left0 (smallest left0) right0 (smallest right0)
>   where
>     doMerge _ Empty right _ = right
>     doMerge left _ _ Empty = left
>     doMerge left (Smallest a l) right (Smallest b r) =
>         case a <= b of
>           True-> unsafeCons a (merge l right)
>           False -> unsafeCons b (merge left r)
>
> -- * Find helper functions below
>
> -- | 'unsafeCons' adds an element in front of the list. It is the burden of
> -- | the programmer to ensure that this element is smaller than (or equal to)
> -- | all the elements of the list.
> unsafeCons :: a -> SortedList a -> SortedList a
> unsafeCons a (Sorted l) = Sorted (a:l)
>
> -- | 'smallest' and 'ViewSmallest' help pattern-matching on a sorted list.
> data ViewSmallest a
>   = Smallest a (SortedList a)
>   | Empty
>
> smallest :: SortedList a -> ViewSmallest a
> smallest (Sorted (a:l)) = Smallest a (Sorted l)
> smallest (Sorted []) = Empty

Note that we _need_ `Ord a`. If we passed an order as an argument, we could
change the order between two calls to `merge`. There wouldn't be a good sense in
which `SortedList a` actually represents sorted lists. With a type class, on the
other hand, it is _guaranteed_ that we _always_ use the same comparison function
for the same type. Therefore ensuring that `SortedList` are indeed sorted. No
exception!


If it weren't for you meddling type classes
===========================================

That's it! we are done writing unsafe code. We can sort lists with the
`SortedList` interface: we simply need to split the list in two parts sort said
parts, then merge them (you will have recognised [merge
sort](https://en.wikipedia.org/wiki/Merge_sort)).

> fromList :: Ord a => [a] -> SortedList a
> fromList [] = nil
> fromList [a] = singleton a
> fromList l = merge orderedLeft orderedRight
>   where
>     orderedLeft = fromList left
>     orderedRight = fromList right
>     (left,right) = splitAt (div (length l) 2) l

Composing with `forget`, this gives us a sorting function

> sort :: Ord a => [a] -> [a]
> sort l = forget (fromList l)

Though that's not quite what we had setup to write. We wanted
```haskell
sortBy :: (a->a->Ordering) -> [a] -> [a]
```

It is easy to define `sort` from `sortBy` (`sort = sortBy compare`). But we
needed the typeclass for type safety of the `SortedList` interface. What to do?
We would need to use a regular value as a type-class instance. Ooh! What may
have sounded excentric when I first brought it up above is now exactly what we
need!

Let's go back to type-class reflection and unravel these curious types we have
glimpsed earlier. As I said when I discussed the type of `merge`: one property
of type classes is that they are _canonically_ attached to a type. It may seem
to render `sortBy` impossible: if I use `sortBy myOrd :: [a]->[a]` and `sortBy
myOtherOrd :: [a]->[a]` on the same type, then I am creating two different
instances of `Ord a`. This is certainly not canonical.

So what, if, instead, we created an _entirely new_ type each time we create an
instance for `a`. Something like
```haskell
newtype ReflectedOrd a = ReflectOrd a
```
Except that we can't do a `newtype` every time we call `sortBy`. So let's make
one `newtype` once and for all, with an additional parameter.

> newtype ReflectedOrd s a = ReflectOrd a

We now how only to create a new parameter `s` at each `sortBy` call. And if you
are familiar with the [`ST` monad][ST], you already know the solution: rank 2
quantification. That is, we need a function of the form:
```haskell
reify :: (forall s. …) -> …
```
In order to have a single `reify` function, rather than one for every
type-class, the `reflection` package introduces a generic type class so that you
have:
```haskell
reify :: forall e r. e -> (forall s. Reifies s e => Proxy s -> r) -> r
```
Think of `e` as an _evidence_ for `Ord`, and `Reifies s e` as a way to retrieve
that evidence. The `Proxy s` is only there to satisfy the type-checker, which
would otherwise complain that `s` does not appear anywhere. To reiterate: we can
read `s` as a unique, dynamically generated type which is valid only in the
scope of the `reify` function. For completeness, here is the the `Reifies` type
class, which just gives us back our `e`:
```haskell
class Reifies s a | s -> a where
  reflect :: proxy s -> e
```

Sorting with reflection
=======================

All that's left to do is to use reflection to give an `Ord` instance to
`ReflectedOrd`. We need an evidence for `Ord`: in order to build an `Ord`
instance, we need an equality function for the `Eq` subclass, and a comparison
function for the instance proper:

> data ReifiedOrd a = ReifiedOrd {
>   reifiedEq :: a -> a -> Bool,
>   reifiedCompare :: a -> a -> Ordering }

Given an evidence of type `ReifiedOrd`, we can define instances for `Eq` and
`Ord` of `ReflectedOrd`. But since type-class instances only take type-class
instances as an argument, we need to provide the evidence as a type class. That
is, using `Reifies`.

> instance Reifies s (ReifiedOrd a) => Eq (ReflectedOrd s a) where
>   (==) (ReflectOrd x) (ReflectOrd y) =
>     reifiedEq (reflect (Proxy :: Proxy s)) x y
>
> instance Reifies s (ReifiedOrd a) => Ord (ReflectedOrd s a) where
>   compare (ReflectOrd x) (ReflectOrd y) =
>     reifiedCompare (reflect (Proxy :: Proxy s)) x y

Notice that because of the `Reifies` on the left of the instances GHC does not
know that it will for sure terminate during typeclass resolution (hence the use
of `UndecidableInstances`). However canonicity is ensured: these typeclass are,
by definition, the only way to have an `Ord` instances on the `ReflectedOrd`
type! Otherwise GHC would complain!

And just like that, we are about done: if we `reify` a `ReifiedOrd a`, we have a
scoped instance of `Ord (ReflectedOrd s a)` (for some dynamically generated
`s`). To sort our list, we simply need to convert between `[a]` and
`ReflectedOrd s a`.

> sortBy :: (a->a->Ordering) -> [a] -> [a]
> sortBy ord l =
>   reify (fromCompare ord) $ \ p ->
>     fmap unreflectOrd . sort . fmap (reflectOrd p) $ l
>
> -- * Helper functions below
>
> -- | Like `ReflectOrd` but takes a `Proxy` argument to help GHC with unification
> reflectOrd :: Proxy s -> a -> ReflectedOrd s a
> reflectOrd _ a = ReflectOrd a
>
> unreflectOrd :: ReflectedOrd s a -> a
> unreflectOrd (ReflectOrd a) = a
>
> -- | Creates a `ReifiedOrd` with an comparison function. The equality function
> -- | is deduced from the comparison.
> fromCompare :: (a -> a -> Ordering) -> ReifiedOrd a
> fromCompare ord = ReifiedOrd {
>   reifiedEq = \x y -> ord x y == EQ,
>   reifiedCompare = ord }

Wrap up & further reading
=========================

We've reached the end of our journey. And we've seen along the way that we can
enjoy the safety of type class, which makes it safe to write function like
`merge` in Haskell, while still having the flexibility to instantiate the type
class from a dynamic arguments, such as options from the command line. Since
type class instances are canonically attached to types, such dynamic instances
must come with dynamically generated types. This is what type class reflection
is all about.

If you want to delve deeper into the subject of type-class reflection, let me,
as I'm wrapping up this tutorial, leave you with a few pointers to further
material:

- A [talk by Edward Kmett][reflection-talk], the author of the reflection
  package, on the importance of the global coherence of type classes and about
  reflection
- There is no built-in support for reflection in GHC, this [tutorial by Austin
  Seipp][reflection-impl-tutorial] goes over the _very unsafe_, internal compiler
  representation dependent, implementation of the library
- John Wiegley [discusses an application of
  reflection][reflection-wiegly-use-case] in relation with QuickCheck.
- You may have noticed, in the definition of `sortBy`, that we `fmap` the
  `reflectOrd` and `unreflectOrd` in order to convert between `a` and
  `ReflectedOrd s a`. However, while, `reflectOrd` and `unreflectOrd`, have no
  computational cost, using them in combination with `fmap` will traverse the
  list. If you are dissatified with this situation, you will have to learn about
  the
  [Coercible](https://www.stackage.org/haddock/lts-9.0/base-4.9.1.0/Data-Coerce.html)
  type class. I would start with this [video from Simon Peyton
  Jones][coercible-talk]

[reflection-package]: https://www.stackage.org/haddock/lts-9.0/reflection-2.1.2/Data-Reflection.html
[reflection-talk]: https://www.youtube.com/watch?v=hIZxTQP1ifo
[reflection-impl-tutorial]: https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection
[reflection-wiegley-use-case]: http://newartisans.com/2017/02/a-case-of-reflection/
[coercible-talk]: https://skillsmatter.com/skillscasts/5296-safe-zero-cost-coercions-in-haskell
[ST]: https://www.stackage.org/haddock/lts-9.0/base-4.9.1.0/Control-Monad-ST.html
