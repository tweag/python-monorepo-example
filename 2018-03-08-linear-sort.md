---
title: Implementing a safer sort<br/> with linear types
author: Alexander Vershilov, Arnaud Spiwack
tags: haskell, linear-types
---

In the ["all about reflection"][blog-reflection] post, we introduced
the `SortedList` data type. Any sorting function `f` should have the
type `[a] -> SortedList a`. However, this doesn't actually guarantee
that `f` is indeed a sorting function: it only says that `f l` is
a sorted list, but not that it is actually a sort of `l`. For example
`f _ = [1,2,3]` is a perfectly good solution on `Int`s, as the result
list is sorted indeed. What we need is an additional guarantee that
the returned list is a permutation of the elements of the incoming
list. In this post we want to show how such a static guarantee could
be achieved, using [linear types][blog-linear-types].

[blog-reflection]: https://www.tweag.io/posts/2017-12-21-reflection-tutorial.html
[blog-linear-types]: https://www.tweag.io/posts/2017-03-13-linear-types.html

## Parametricity

By looking at the type of any polymorphic function, we can derive
properties that hold for this function due to parametricity, such as
the fact that the function must behave in the same way no matter the
type of the argument. One interesting example for us is functions with
type:

```haskell
[a] -> [a]
```

Parametricity, for such a function, *if we are to assume that the function is total*, entails that the elements of the
result are a subset of the elements of the argument (up to possible
duplications of elements). This property is guaranteed by the type of
the function. Intuitively, this is true because the function can
neither inspect values nor produce new one because their concrete type
is unknown at function definition time. This class of properties is
usually called parametricity. However, it is sometimes referred to as
"theorems for free" after
Wadler's
[paper](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.9875).

We are not aware of a theory of parametricity with linear types.
However, in practice, linear functions offer stronger parametricity
guarantees. In our case, a function with the type `[a] ->. [a]` is
necessarily a permutation. Intuitively, this is true because the
function can neither forget nor duplicate any value in its argument
due to linearity. In order to get more guarantees, we need to use more
sophisticated tools, and we would need better support for dependent
types or use Liquid Haskell. But parametricity is a lighter-weight
tool that you can leverage to get a lot of mileage.

With this knowledge we can actually construct a type of sorting function.
This post is a literate Haskell file and can be compiled by GHCi with
the [linear-types](https://arxiv.org/abs/1710.09756) extension
enabled.

```haskell
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-} -- for merge
{-# LANGUAGE UndecidableInstances #-} -- For OrdL
{-# LANGUAGE FlexibleInstances #-} -- For OrdL
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE BangPatterns      #-}

import Prelude.Linear
import Unsafe.Linear as Unsafe
import Data.Ord
```

We will reuse the types from the previous post:

```haskell
newtype SortedList a = Sorted [a]

forget :: SortedList a ->. [a]
forget (Sorted l) = l

nil :: SortedList a
nil = Sorted []

singleton :: a ->. SortedList a
singleton a = Sorted [a]
```

The first question is: how can we actually compare the elements?
Recall the types of the usual `compare` function:

```haskell
compare :: Ord a => a -> a -> Ordering
```

We can't simply lift this method to a linearly typed context, because
it would consume its argument and we can't use them further to build
the output list. We can solve that problem if we can return the
original arguments alongside the `Ordering`. To that effect, let's
introduce a new linearized `Ord` type class:

```haskell
class OrdL a where
 -- | Compares the elements and their 'Ordering' and values
 -- that are by convention in the sorted order.
 compareL :: a ->. a ->. (Ordering, a, a)
 default compareL :: (Ord a, Movable a) => a ->. a ->. (Ordering, a, a)
 compareL a b = go (move a) (move b) where
    go :: Unrestricted a ->. Unrestricted a ->. (Ordering, a, a)
    go (Unrestricted x) (Unrestricted y) = (compare x y, x, y)
```

This is the first piece of the code that actually uses linear types.
Ideally we could have dispensed with introducing a `go` helper
function, but our linear-types enabled GHC prototype doesn't support
the `case-of` construct yet.

We'll then introduce yet another piece of kit: the `Movable` type
class from
the [linear-base library](https://github.com/tweag/linear-base/). It
describes which linear values can be converted to unrestricted
(non-linear) values:

```haskell
class Movable a where -- simplified
  move :: a ->. Unrestricted a
```

Any first-order Haskell type should be an instance this class. For
types that are movable we may have a simple default implementation for
`compareL`. For types that are not, users will have to write their own
definitions.

Now we are ready to implement merge sort. Merge sort has two steps:

1. split the list into two sublists and
2. merge sorted sublists.

Let's implement split first:

```haskell
split :: [a] ->. ([a], [a])
split []      = ([], [])
split [x]     = ([x], [])
split (x:y:z) = go (x,y) (split z) where
  go :: (a,a) ->. ([a], [a]) ->. ([a], [a])
  go (a,b) (c,d) = (a:c, b:d)
```

We split the list into two parts by moving all elements with even positions into one
sublist and those with odd positions into the other. Almost no magic and discussions
here; but, `split` being linear, the type itself makes sure that elements are neither
lost nor duplicated.

Our actual merge function:

```haskell
merge :: forall a. OrdL a  => SortedList a ->. SortedList a ->. SortedList a
merge (Sorted []) bs = bs
merge as (Sorted []) = as
merge (Sorted (a:as)) (Sorted (b:bs)) = go (compareL a b) as bs where
  go :: (Ordering, a, a) ->. SortedList a ->. SortedList a ->. SortedList a
  go (EQ,k,l) ks ls = Sorted (k: l : forget (merge ks ls))
  go (LT,k,l) ks ls = Sorted (k: forget (merge ks (Sorted (l: forget ls))))
  go (GT,l,k) ks ls = Sorted (l: forget (merge (Sorted (k: forget ks)) ls))
```

Recall what we had in the non-linear case:

```Haskell
 merge :: Ord a => SortedList a -> SortedList a -> SortedList a
 merge (Sorted left0) (Sorted right0) = Sorted $ mergeList left0 right0 where
   mergeList :: Ord a => [a] -> [a] -> [a]
   mergeList [] right = right
   mergeList left []  = left
   mergeList left@(a:l) right@(b:r) =
     if a <= b
     then a: mergeList l right
     else b: mergeList left r
```

Up to minor changes, it's exactly the same code.

```haskell
fromList :: forall a. OrdL a => [a] ->. SortedList a
fromList [] = Sorted []
fromList [a] = singleton a
fromList xs = go1 (split xs) where
 go1 :: ([a], [a]) ->. SortedList a
 go1 (left, right) = merge (fromList left) (fromList right)
```

Just by changing the arrow types in our functions to linear arrows, we
are able to get additional guarantees that are enough to prove that
the returned result is a permutation of the input. In addition, *we
were able to preserve the proof that the returned list is ascending
without any changes to existing data types*. This is a central
property that the authors of the linear types extension worked very
hard to achieve: that we are able to introduce additional guarantees
*post facto* without having to refactor all the callers across the
entire codebase. Sometimes we want to prove more facts about the
sorting function, for example to prove that it has the desired
complexity. Such facts cannot be proven in a linear type framework.
Still, linear types provide a lightweight framework that is sufficient to
make our sorting functions safer, so that we need to put less faith into the
codebase.
