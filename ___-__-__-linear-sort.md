---
title: Implementing safer sort with linear-types
shortTitle: some subtitle
author: Alexander Vershilov and 
---

In [ALL ABOUT REFLECTION](https://www.tweag.io/posts/2017-12-21-reflection-tutorial.html)
post we introduced `SortedList` structure. But even if we used this special structure sorting
is not guaranteed to be safe as theoretically implementation can lose elements or duplicate them.

Ideally we want an implementation that is guaranteed to be a permutation
of the incoming elements. There are a few ways how to do that:

  * just trust the code
  * use liquid haskell and prove the code
  * use linear types and parametricity

By writing the definition of the polymorphic function we can find the
theorems that function satisfies. This technique is called theorems
for free and can be found in [theorems for free](http://citeseer.ist.psu.edu/viewdoc/summary?doi=10.1.1.38.9875)
paper by Philip Wadler. In case of lists, crusially, functions of type `[a] ->. [a]`
they are nessesarily permutations. So if we provide a functions that typechecks we will need
to prove that elements in the result have right order.

This is literate haskell file and can be compiled by ghci with
[linear-types](https://www.microsoft.com/en-us/research/wp-content/uploads/2017/03/haskell-linear-submitted.pdf)
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
```

I'm not sure that this is an ideal type (thus it's not in linear-base)
but at least it will fit our needs.

```haskell
  default compareL :: (Ord a, Movable a) => a ->. a ->. (Ordering, a, a)
  compareL a b = go (move a) (move b) where
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

Any first-order Haskell value of should have this class. For values that are
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

Now lets introduce small helper:

```haskell
view1 :: SortedList a ->. (a, SortedList a)
view1 (Sorted (a:as)) = (a, Sorted as)
```

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

modulo minor changes it's exactly the same code.

```haskell
fromList :: forall a. OrdL a => [a] ->. SortedList a
fromList [] = Sorted []
fromList [a] = singleton a
fromList xs = go1 (split xs) where
 go1 :: ([a], [a]) ->. SortedList a
 go1 (left, right) = merge (fromList left) (fromList right)
```

Some conclusion, possibly reference to priority queue


