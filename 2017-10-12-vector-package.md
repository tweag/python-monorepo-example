---
title: Array fusion with vector
author: Manuel M T Chakravarty
featured: yes
---

*This is the fourth post in a series about array programming in Haskell — you might be interested in the [first](http://www.tweag.io/posts/2017-08-09-array-programming-in-haskell.html), [second](http://www.tweag.io/posts/2017-08-31-hmatrix.html), and [third](http://www.tweag.io/posts/2017-09-27-array-package.html), too.*

In the [previous post](http://www.tweag.io/posts/2017-09-27-array-package.html) of this series, we explored the basic, down-to-earth, index-based array interfaces that have their roots in the Haskell standard libraries. We also discussed the need for *strictness*, *unboxing*, and a *two-phase* initialisation strategy, freezing mutable structures after they have been populated. With those basics out of the way, we can now explore the design of higher-level, *collection-oriented* array interfaces whose efficient implementation critically relies on those basic techniques.

The [vector](https://hackage.haskell.org/package/vector) package is probably the most popular array library in Haskell, and it is a prime example of this approach. The package, implemented by Roman Leshchinskiy, is a spin off from the [Data Parallel Haskell](https://wiki.haskell.org/GHC/Data_Parallel_Haskell) project . Specifically, Data Parallel Haskell is organised as multiple layers of array libraries, and [vector](https://hackage.haskell.org/package/vector) is a generalisation of what used to be the lowest layer: sequential, int-indexed arrays combined with a powerful array-fusion framework that makes the composition of successive collective operations efficient.

## Collective
Collective operations that process all elements of an array in a single operation, such as `map`, `fold`, `zip`, `filter`, and so forth, fit more naturally into a functional style than the index-based loops that originated in FORTRAN. This is not just to favour combinator-based code, but also because index-based, one-element-at-a-time processing quickly leads to the use of mutable arrays. Indexed-based operations are sometimes inevitable, but they ought to be used sparingly; often we only need to extract individual or only a few elements of an array. In cases, where random data-driven access is required, collective index-based operations often suffice — such as, for example, `backpermute`:

```
backpermute (fromList ['a', 'b', 'c', 'd']) (fromList [0, 3, 2, 3, 1, 0])
  = fromList ['a', 'd', 'c', 'd', 'b', 'a']
```

The function `backpermute` returns a result of the same length as its second argument, where the integer values of the second argument are used to determine the index positions of the elements to pick from the first argument. (The function `fromList` turns a list into a `Vector`.)

## Array fusion
Consider the following pipeline of collective vector operations, where we assume `import Data.Vector as V` to disambiguate `vector` operations from the standard list operations of the same name:

```
  V.map fst
$ V.filter (\t -> snd t > 0)
$ V.zip points cs
```

First, we combine a vector of `points :: Vector (Double, Double)` with a vector `cs :: Vector Double` by way of `V.zip` to obtain a value of type `Vector ((Double, Double), Double)`. Then, we `V.filter` to only retain those `((Double, Double), Double)` triples whose last component is strictly positive. Finally, we project the points out of the filtered vector. Overall, this pipeline drops all `points` whose corresponding (index-wise) `cs` value is zero or negative.

While this is a compact functional specification, its naive implementation would be rather inefficient. Each of the three collective operations creates a new immutable vector. All this, only to finally get a vector containing a subset of the elements of `points`. The main problem with this is not that it is wasteful to allocate those intermediate arrays, but that initialising and then consuming those intermediates generates a lot of memory traffic — this is rather unfortunate as memory bandwidth is the most severe limiting factor of current hardware.

Hence, it is absolutely crucial that a library, such as `vector`, performs *array fusion*, a compiler optimisation that eliminates intermediate arrays and combines multiple successive array traversals into one. In fact, the `vector` package, compiles the above code consisting of `V.map`, `V.filter`, and `V.zip` into a single traversal without any intermediates. It achieves this using a fusion framework known as [stream fusion](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.421.8551).

## From generic to concrete
The `vector` package is architected in a flexible manner with generic type classes `Vector` (for immutable vectors) and `MVector` (for mutable vectors), defined in `Data.Vector.Generic`.  `Vector` is a subclass of `MVector` , as the latter provides mutating methods on top of the pure ones. 

The modules `Data.Vector` and `Data.Vector.Unboxed` instantiate the generic framework with concrete boxed and unboxed vector types — both again providing an immutable and a mutable flavour. In addition, there is variant of unboxed vectors, called *storable*, in `Data.Vector.Storable`, which ties in with the `Storable` class of the foreign function interface and is useful for interoperability with C code.

Due to this generic set up, array algorithms can be implemented generically and, then, used with all of the concrete instances. It is also perfectly possible to extend `vector` with your own flavour of arrays; for example, for interoperability with another array library.

## Convex hull
One of the algorithms included in the benchmarks that are part of `vector` is the [quickhull algorithm](https://en.wikipedia.org/wiki/Quickhull) for computing the convex hull of a set of points. Quickhull is —just like quicksort— a split-based divide-and-conquer algorithm. It begins by cutting the set of points into two by drawing a line between the leftmost and rightmost point in the set. Then, it recursively operates on these two sets of points. In each recursive invocation, the point furthest from the last dividing line gives rise to two new lines (using the two end points of that last dividing line). This is illustrated by the following animation from [Wikimedia Commons](https://commons.wikimedia.org/wiki/File%3AAnimation_depicting_the_quickhull_algorithm.gif).

<center>
<img style="max-width: 50%;max-height: 50%;" alt="Animation depicting the quickhull algorithm" src="https://upload.wikimedia.org/wikipedia/commons/4/42/Animation_depicting_the_quickhull_algorithm.gif"/>
</center>

Given a `Vector` of x/y-coordinates, we can implement the initial step of quickhull, namely determining the left and rightmost point, as follows:

```
import Data.Vector.Unboxed as V

quickhull :: Vector (Double, Double) -> Vector (Double, Double)
quickhull points = hsplit points pmin pmax V.++ hsplit points pmax pmin
  where
    xs   = V.map fst points
    pmin = points V.! V.minIndex xs
    pmax = points V.! V.maxIndex xs
```

The functions `V.minIndex` and `V.maxIndex` return the index position of the smallest and largest element of the argument vector, respectively (here, of the x-coordinates `xs` of all points). We use those to extract the leftmost (`pmin`) and rightmost (`pmax`) point. When invoking the recursive component of quickhull —here, implemented by `hsplit`— we pass the points and the dividing line in the form of its starting and end point. As the latter are swapped in the two recursive calls, one call processes the points above the dividing line and the other call processes the points below that line.

The function `hsplit`, first, computes the distance `cs` of all `points` to the line from `p1` to `p2`. Second, it uses the pipeline of collective operations that we discussed above to remove all `points` that are below that line. The remaining points, `packed`, are passed into the two recursive calls together with `pm`, the point that is furthest from the line.

```
hsplit :: (Ord b, Num b, Unbox b) 
       => Vector (b, b) -> (b, b) -> (b, b) -> Vector (b, b)
hsplit points p1 p2
  | V.length packed < 2 = p1 `V.cons` packed
  | otherwise = hsplit packed p1 pm V.++ hsplit packed pm p2
  where
    cs     = V.map (\p -> cross p p1 p2) points
    packed = V.map fst
           $ V.filter (\t -> snd t > 0)
           $ V.zip points cs

    pm     = points V.! V.maxIndex cs

-- cross p p1 p2 = distance of p from line between p1 and p2
cross :: Num a => (a, a) -> (a, a) -> (a, a) -> a
cross (x,y) (x1,y1) (x2,y2) = (x1-x)*(y2-y) - (y1-y)*(x2-x)
```

The function `cross` that we use to compute the distance of a point `p` from the line between points `p1` and `p2` returns a signed distance; i.e., a positive distance value is for a point above the line and a negative distance value for a point below the line. This is crucial for `V.filter (\t -> snd t > 0)` to remove all points located on or below the line.

The `vector` package optimises this code rather well and gets within about a factor of two of the performance of hand-optimised C code. However, the algorithmic structure of quickhull also highlights the limits of `vectors`’s array fusion framework stream fusion. This is explored in the paper [Data flow fusion with series expressions in Haskell](https://dl.acm.org/citation.cfm?doid=2503778.2503782) ([PDF](http://benl.ouroborus.net/papers/2013-series/flow-Haskell2013-rev1.pdf)), which also contains concrete benchmark results, including those for quickhull.

## Conclusion
The `vector` package is a tool that should be in every Haskell developers tool box. It is versatile, efficient, and widely used. Consequently, there is a range of add ons; in particular, [vector-algorithms](https://hackage.haskell.org/package/vector-algorithms), which implements search and sorting algorithms, is quite popular.

The `vector` package works hard to achieve excellent sequential performance. However, it, by design, ignores the option of improving performance by parallelisation. This will be the topic of the next post in this series.
