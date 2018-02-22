---
title: Implementing safer sort with linear-types
shortTitle: some subtitle
author: Alexander Vershilov and Arnaud Spiwak
---

In [all about reflection](https://www.tweag.io/posts/2017-12-21-reflection-tutorial.html)
post we introduced `SortedList` data type. So, a sorting function f will have type
`[a] -> SortedList a`. However, this doesn't actually guarantee that `f` is indeed
a sorting function: it only says that `f l` is sorted, but not that it is a sort `l`.
For example `f _ = [1,2,3]` is a perfect solution on `Int`s, as result list is sorted,
indeed. We need an additional guarantee that returned list is a permutation of the
elements of incomming list. In this post we want to show a way how such a guarantee
could be introduced in linear types framework.

By looking at type of any parametrically polymorphic function we can derive properties
that holds for this function due to parametricity. A property that function must work
for any type that was passed to the function. One interesting example for us is function
with type:

```haskell
[a] -> [a]
```

Parametricity of this function entails  that the elements of the result of this function
is a subset of the argument (up to the possible with duplications of the elements). And
this property is guaranteed by the type of the function. Intuitively this happens because
function can neither inspect values nor produce new one because their type is unknown to
the function.  This technique is usually called parametricity. However, it is sometimes
referred to as « theorems for free » after Wadler's [paper](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.9875).

I am not aware of a theory of parametricity with linear types, however, in practice,
linear function offer stronger parametricity guaranteesIn our case function with the 
type `[a] ->. [a]` is necessarily a permutations. Intuitively this happens because function
can neither forget nor duplicate any value
in it's argument due to linearity. In order to get more guarantees we need to use more
sophisticated tools, and we would need better support for dependent types or use Liquid
Haskell. But parametricity is a lighterweight tool that you can leverage to get a lot of
mileage.

With this knowledge we can actually implement sorting function.
This is literate haskell file and can be compiled by ghci with [linear-types](https://arxiv.org/abs/1710.09756)
extension enabled.

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

We will reuse the types from the previous post.

```haskell
newtype SortedList a = Sorted [a]

forget :: SortedList a ->. [a]
forget (Sorted l) = l

nil :: SortedList a
nil = Sorted []

singleton :: a ->. SortedList a
singleton a = Sorted [a]
```

The first quesion is how can we actually compare the elements.
Recall the types of the usual `compare` function, it is:

```haskell
compare :: Ord a => a -> a -> Ordering
```

We can't just lift this method to a linear types, because it
will consume values and we can't use them in further code.
We can solve that problem if we either return values in sorted
order, or return `Ordering` alongside with values. So now we need
to introduce new type class that will be a linear variant of the
ordinary `Ord` class.

```haskell
class OrdL a where
 -- | Compares the elements and their 'Ordering' and values
 -- that are by convention in the sorted order.
 compareL :: a ->. a ->. (Ordering, a, a)
 default compareL :: (Ord a, Movable a) => a ->. a ->. (Ordering, a, a)
 ompareL a b = go (move a) (move b) where
    go :: Unrestricted a ->. Unrestricted a ->. (Ordering, a, a)
    go (Unrestricted x) (Unrestricted y) = (compare x y, x, y)
```

This is a first piece of the code that actually uses linear types.
It looks more complex than it needs to be but it happens because
at current stage compiler can't work with linear `case`. So we need
to introduce helper functions and keep linear arrows explicitly there.

Here we meed one new type class from the linear-base - `Movable`.
It is a type class  that allows to convert linear values to unrestricted ones.

```haskell
class Movable a where -- simplified
  move :: a ->. Unrestricted a
```

Any first-order Haskell type of should have this class. For values that are
movable we may have a simple default implementation, for values that are
not user will have to write his own definition.

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

We split list into 2 parts by moving all elements with even position in one
sublist and ones with odd into the other. This way we need not count the size
of the list and can do split in streaming fasion. Almost no magic and discussions
here, but split is linear and type itself makes sure that elements are neither
lost nor duplicated.

And actually our merge function:

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

Recall what we had in non-linear case:

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

up to minor changes it's exactly the same code.

```haskell
fromList :: forall a. OrdL a => [a] ->. SortedList a
fromList [] = Sorted []
fromList [a] = singleton a
fromList xs = go1 (split xs) where
 go1 :: ([a], [a]) ->. SortedList a
 go1 (left, right) = merge (fromList left) (fromList right)
```

Just by changing the arrow types in our functions to linear we are able to get
additional guarantees that are enough to prove that the returned result is a
permutation of the input. In addition, we were able to preserve proof that
returned list is ascending without any changes in existing data types. And this
is a good properly of the linear types that we are able to provide additional
guarantees without large changes in existing codebase. Sometimes we want to prove
more facts about the sort, for example to proove that sorting function has
desired complexity; such facts could not be proven a linear type framework.
Still linear types provide a lightweight framework, that is enough to make our
sorting functions safer, so that we need to trust less of the codebase.

