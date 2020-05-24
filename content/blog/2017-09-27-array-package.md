---
redirect_from: [/posts/2017-09-27-array-package.html]
title: Immutability and unboxing in array programming
author: Manuel M T Chakravarty
featured: yes
tags: [haskell]
---

_This is the third post in a series about array programming in Haskell — you might be interested in the [first](http://www.tweag.io/posts/2017-08-09-array-programming-in-haskell.html) and [second](http://www.tweag.io/posts/2017-08-31-hmatrix.html), too._

In the [previous post](http://www.tweag.io/posts/2017-08-31-hmatrix.html) of this series, we discussed commonly used vector and matrix routines, which are available in highly-optimised implementations in most programming languages. However, often we need to implement our own custom array algorithms. To this end, the [Haskell standard (Haskell 2010)](https://www.haskell.org/onlinereport/haskell2010/) already comes with a simple [array API](https://www.haskell.org/onlinereport/haskell2010/haskellch14.html#x22-20100014) in the form of the `Data.Array` standard module. These arrays are _immutable_, _boxed_, and _non-strict_. This allows for the elegant, high-level description of many array algorithms, but it is suboptimal for compute-intensive applications as boxing and non-strictness, especially in combination with the reliance on association lists for array construction, lead to code that is [very difficult for the Haskell compiler to optimise](https://link.springer.com/chapter/10.1007/978-3-540-44833-4_2), resulting in rather limited performance.

## Strictness

The extra expressiveness granted by non-strictness is nicely displayed by the following example, courtesy of the classic [A Gentle Introduction to Haskell](https://www.haskell.org/tutorial/). The `wavefront` function defines an *n*x*n* array whose leftmost column and topmost row are populated with 1s (by the first two list comprehensions below). All other array elements are defined as the sum of the three elements to their left, top, and top-left (by the third list comprehension in the definition).

```
wavefront :: Int -> Array (Int,Int) Int
wavefront n = arr
  where
    arr = array ((1,1),(n,n))
                ([((1,j), 1) | j <- [1..n]] ++
                 [((i,1), 1) | i <- [2..n]] ++
                 [((i,j), arr!(i,j-1) + arr!(i-1,j-1) + arr!(i-1,j))
                             | i <- [2..n], j <- [2..n]])
```

The `!` infix operator implements array indexing:

```
(!) :: Ix i => Array i e -> i -> e
```

Moreover, the function `array` constructs an array from an association list mapping indexes to values:

```
array :: Ix i => (i, i) -> [(i, e)] -> Array i e
```

The elegance of `wavefront` is in the recursive definition of the array `arr`. In the expression `arr!(i,j-1) + arr!(i-1,j-1) + arr!(i-1,j)`, we access the elements to the left, top, and top-left of the current one by appropriate indexing of the very array that we are currently in the process of defining. Such a recursive dependency is only valid for a non-strict data structure.

## Boxing

Unfortunately, the expressiveness of non-strict arrays comes at a price, especially if the array elements are simple numbers. Instead of being able to store those numeric elements in-place in the array, non-strict arrays require a _boxed_ representation, where the elements are pointers to heap objects containing the numeric values. This additional indirection requires extra memory and drastically reduces the efficiency of array access, especially in tight loops. The layout difference between an unboxed (left) and a boxed (right) representation is illustrated below.

<img title="Boxed versus unboxed array representation" alt="Boxed versus unboxed array representation" src="../img/posts/unboxed-vs-boxed.png" style="max-width: 100%;max-height: 100%;"></img>

While both strict and non-strict data structures admit boxed representations, non-strict structures typically require boxing. To provide an alternative to the standard non-strict arrays, the [`array` package](https://hackage.haskell.org/package/array) provides strict, unboxed arrays of type `Data.Array.Unboxed.UArray i e`. By way of overloading via the type class [`Data.Array.IArray.IArray`](https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-IArray.html#t:IArray), they provide the same API as the standard non-strict, boxed arrays. However, the element type is restricted to basic types that can be stored unboxed, such as integral and floating-point numeric types.

Unfortunately, array construction based on association lists, such as the [`array`](https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-IArray.html#v:array) function, still severely limits the performance of immutable `UArray`s. Nevertheless, at least, array access by way of [`(!)`](https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-IArray.html#v:-33-) is efficient for unboxed arrays.

## Immutability

While immutable arrays —i.e., arrays that cannot directly be in-place updated— are semantically simpler, it turns out that indexed-based array construction is drastically more efficient for mutable arrays. Hence, computationally demanding Haskell array code typically adopts a two-phase array life cycle: (1) arrays are allocated as mutable arrays and initialised using in-place array update; once initialised, (2) they are _frozen_ by making them immutable.

This usage pattern is supported by the interface provided by the [`Data.Array.MArray.MArray`](https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-MArray.html#t:MArray) class and we use it in the following example function `generate` to initialise an array of `Double`s with an index-based generator function `gen`:

```
generate :: Ix i => (i, i) -> (i -> Double) -> UArray i Double
generate bnds gen
  = runSTUArray $ do
      arr <- newArray_ bnds
      mapM_ (\i -> writeArray arr i (gen i)) (range bnds)
      return arr
```

Mutable arrays come in various flavours that are, in particularly, distinguished by the monad in which the array operations take place. Usually, this is either `IO` or `ST`, and the `array` package provides both boxed and unboxed variants for both monads. We have the boxed `IOArray` and the unboxed `IOUArray` as well as the boxed `STArray` and the unboxed `STUArray`. The above definition of `generate` uses `STUArray` to initialise the array, and then, freezes it into a `UArray`, which is returned. The choice of `STUArray` is implicit in the use of [`runSTUArray`](https://hackage.haskell.org/package/array-0.5.2.0/docs/Data-Array-ST.html#v:runSTUArray), which executes the code in the state transformer monad `ST` and freezes the `STUArray` into a `UArray`.

The function `newArray_` provides a fresh _uninitialised_ array:

```
newArray_ :: (MArray a e m, Ix i) => (i, i) -> m (a i e)
```

We can read and write a mutable array with

```
readArray  :: (MArray a e m, Ix i) => a i e -> i      -> m e
writeArray :: (MArray a e m, Ix i) => a i e -> i -> e -> m ()
```

In the definition of `generate`, we use `writeArray` once on each index in the range `range bnds` of the mutable array to initialise the value at index `i` with the value of the generator function at that index, `gen i`.

Generally, we can freeze a mutable array, obtaining an immutable array, with

```
freeze :: (Ix i, MArray a e m, IArray b e) => a i e -> m (b i e)
```

There is also the unsafe variant `unsafeFreeze` that avoids copying the array during freezing, but puts the onus on the programmer to ensure that the mutable argument is subsequently not updated anymore. In the code for `generate` above, we indirectly use `unsafeFreeze` by way of `runSTUArray`. As `runSTUArray` makes it impossible to use the mutable array after freezing, this encapsulated use of `unsafeFreeze` is always safe.

An expression, such as, `generate (1,100) ((^2) . fromIntegral)` produces an unboxed array with the first one hundred square numbers. Internally, it is based on the initialisation of a mutable array, but this is completely abstracted over by the definition of `generate`. While there is no inbuilt need to ever freeze a mutable array, good functional programming style requires to keep mutable code as localised as possible and to avoid passing mutable data structures around.

## Summary

Well written code based on unboxed arrays and using the discussed pattern to create arrays by initialising a mutable version, which is subsequently frozen, can achieve performance comparable to low-level C code. In fact, the collection-oriented high-performance array frameworks that we will discuss in subsequent blog posts work exactly in this manner.
