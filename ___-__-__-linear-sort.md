---
title: Implementing safer sort with linear-types
shortTitle: some subtitle
author: Alexander Vershilov and 
---

In last post we have checked how we can use reflection framework in
order to sort list, and used 'SortedList' structure inside. But even
we used this special structure sorting is not guaranteed to be safe
as theoretically implmentation can lose elements or duplicate them.

Ideally we want an implementation that is guaranteed to be a reordering
of the incoming elemnts. There are some ways to do that:
  
  * just trust the code
  * use liquid haskell and prove the code
  * use linear types together with code that returns AscList.

The key idea here is that if we have linear parametric structure, then
and return the structure of the same type it's guaranteed that it will
be reordering. In this case we want to use linear types extension to ghc.
And it may be an interesting experiment about how to get additional guarantees
from the extention that didn't have this feature in mind (??).

An interesting use of the linear types here is that we will not
consume values at all instead we want the guarantee that they
are not consumed in our code (if they are properly consumed somewhere
later).

This is literate haskell file and can be compiled by ghci with
linear-types extension enabled.

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

We will reuse types from the previous post.

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

we can't just lift this method to a linear types, because it
will consume values and we can't use them in further code.
In order to solve this problem properly we need to have a notion
of `borrow`ing. I.e. ability to pass value to method but without
passing ownership, so that method can't free it. Currently
compiler and proposal does not have notion of borrowing, but
we can encode it using linear types, for the method with signature
`foo :: a -> b` we can introduce method `fooL :: a -> (a, b)` that
will return back value that was passed, so it's guaranteed that
it was not totally consumed. It's possible that in final extension
we will have helpers or utilities that would allow to help us with
removing boilerplate code.

So now we need to introduce new typeclass that will be a linear
variant of the ordinary `Ord` class. (In future releases that will
not be needed and we would be able to just use Ord ?)


```haskell
class OrdL a where
 -- | Compares the elements and their 'Ordering' and values
 -- that are by convention in the sorted order.
 compareL :: a ->. a ->. (Ordering, a, a)
```

I'm not sure that this is an ideal type (thus it's not in linear-base)
but at least it would fit our needs.

```haskell
  default compareL :: (Ord a, Movable a) => a ->. a ->. (Ordering, a, a)
  compareL a b = go (dup a) (dup b) where
    go :: (Movable a, Ord a) => (a, a) ->. (a, a) ->. (Ordering, a, a)
    go (a', as) (b', bs) = go' a' b' (move as) (move bs)
    go' :: Ord a => a ->. a ->. Unrestricted a ->. Unrestricted a ->. (Ordering, a, a)
    go' a' b' (Unrestricted as) (Unrestricted bs) = 
      go'' a' b' (compare as bs)
    go'' :: a ->. a ->. Ordering ->. (Ordering, a, a)
    go'' a' b' EQ = (EQ, a', b')
    go'' a' b' GT = (GT, a', b')
    go'' a' b' LT = (LT, a', b')
```

This is a first piece of the code that actually users linear types
and it looks quite scarry. So we have a lot to discuss here.
The first question is why to we have so many helper functions. It's
pretty simple at current stage compiler can't work with linear `case`.
All variables in case are treated as unrestricted, so if we want to
pattern match on the linear variables we have to use helper functions,
this style is a bit annoying but it will go away once compiler will
be in up to the date state.
Then we have some interesting `dup`, `move`, thingies. Those are methods
from the typeclasses that makes a bridge between linear and non-linear
world. Lets discuss them, first one is `Consumable`:

```haskell
class Consumable a where
  consume :: a ->. ()
```

This is the class for normal haskell values living on a haskell heap,
that can be forgotten at any time without breaking any internal invariant.
Some examples of this type class are `Int`, `()`, `Double`, and other
primitive types.

Then we introduce a `Dupable` type class:

```haskell
class Dupable a where
  dup :: a ->. (a, a)
```

This class allows you do duplicate your linear value, so basically it
allows you do use you data as many times as you wish, but you have to
consume each value. One example of the values that are `Dupable` but not 
`Consumable` is reference counted variables. We can "copy" in as many
times as we like, but we should explicitly mark each copy as being
processed (consumed).

If we have both `Consumable` and `Dupable` class, then we can convert
our linear value to unrestricted one, and class for that is called
movable:

```haskell
class (Consumable a, Dupable a) => Movable a where
  move :: a ->. Unrestricted a
```

The reference counted value is again an example of value that is 
Consumable and Dupable but not Movable.

Any normal haskell value of should have this class. If we have both
dupable and consumable values then we can implement a nice default
implementation of the ordering function. At first we `duplicate` our
value, because at the end we need to return back our linear values.
Then we call `move` on one of the duplicates, making it Unrestricted,
this is useful in order to reuse standart functions from base, though
if they are weight polymorphic then we can implement the function without
using Movable, but require values to be Consumable only.

The rest part if purely technical we just pattern match on the `Ordering`
and return relevant values.

Now we are ready to implement merge sort. Merge sort has two steps:

  1. split list into two sublists and
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

We split list into 2 parts by moving all elements with even position
in one sublist and ones with odd into the other. This way we should
not count the size of the list and can do split in streaming fasion.
Almost no magic and discussions here.

But ... eye can see that this function is not iterative and is using
`O(N)` stack. The problem here is quite interesting, remind the non-linear
code that doesn't have the problem:

```haskell
   let ~(xs,ys) = split zs
   in split (x:xs,y:ys)
```

With irrefutable patterns this one will not need to be run to the end
and we can return result of the iteration immediately (I'll note that
without irrefutable pattern we still need to run the code to the end
in order to match constructor being returned). This code looks perfectly
fine, but if we would look into the code we would see (simplified):

```haskell
  let t0 = split zs
  in split (x:fst t0, y: snd t0)
```

That is no longer linear! We use `t0` two times. So that code is not
actually linear and once linearity checks will be in code this code
will fail to be compiled. (Side effect you need to write `let !(...)`
for the linear values in order to avoid this problem. I personally do
not like that and hope that there exist some way to make this code look
haskell like, be iterative and compatible with linear types, but I don't
know the recipe yet.

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
merge (view1 -> (a, as)) (view1 -> (b, bs)) = go (compareL a b) as bs where
  go :: (Ordering, a, a) ->. SortedList a ->. SortedList a ->. SortedList a
  go (EQ,k,l) ks ls = Sorted (k: l : forget (merge ks ls))
  go (LT,k,l) ks ls = Sorted (k: forget (merge ks (Sorted (l: forget ls))))
  go (GT,l,k) ks ls = Sorted (l: forget (merge (Sorted (k: forget ks)) ls))
```

Recall wahat we had in non-linear case:

```
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
