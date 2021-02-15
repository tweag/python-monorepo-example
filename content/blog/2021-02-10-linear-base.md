---
title: "linear-base makes writing Linear Haskell easy and fun"
shortTitle: "Announcing linear-base"
author: Divesh Otwani, Utku Demir
tags: [haskell, linear-types]
description: "Announcing linear-base, a standard library for linearly typed Haskell programs."
---

We're announcing [`linear-base`][linear-base-hackage],
a standard library for [Linear Haskell][learn-linear] programs.
Our release accompanies the release of [GHC
9.0][ghc-release] which supports `-XLinearTypes`. Linear base has been
written by Bhavik Mehta, a former Tweag intern, Arnaud Spiwack, and ourselves.

In the spirit of a standard library, `linear-base` is not a strict replica of
`base` with linearly-typed variants of all the facilities in `base`. Instead,
`linear-base` focuses on the abstractions one naturally _needs_ and _desires_
when programming with linear types.

It does include some linear variants from `base`, but the library
ranges from mutable data structures with a pure API to a resource-safe I/O
monad, to linear-streams. It's a "base" library, a helpful
prelude to programming using `-XLinearTypes`.

Of course this raises the question, what does a linear Haskell program look like?
And, how does `linear-base` provide what we _need_ and what we _want_ to write one?

## Envisioning Linear Haskell Programs

If you're unfamiliar with `-XLinearTypes`, you can learn more by
browsing our
[examples][linear-base-ex] and [README][learn-linear].
At a bare-bones level, linear types simply
provide some further properties that a function declared with a signature like `f :: A %1-> B` must satisfy. Namely, to type-check the body of `f`,
the `A` must be consumed exactly once. We are then able to leverage this
type-checking power to write functions that would have been unsafe to write
before (e.g., a pure interface to mutable data structures) or provide
alternative APIs to existing ones that place an obligation on the programmer to
ensure they avoid certain errors (such as reading from a closed file handle).
For more information about how `-XLinearTypes` works see our [earlier
posts][linear-posts].

There are two main families of motivations for this additional type-checking discussed
in the [Linear Haskell paper][lh-paper]. First, we can write pure
APIs for mutable data structures such as arrays. As discussed in the paper,
this approach is sometimes more convenient to work with than using the
`ST` monad, and even makes [`unsafeFreeze`](https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array-Unsafe.html#v:unsafeFreeze) safe.

Second, `-XLinearTypes` enables us to type-check usage protocols.
An example discussed in the paper is the protocol of file handles, namely, that
file handles must be closed and then cannot be written to or read from[^1].

Thus, a typical linear program would likely be one that

- is mostly pure, but has a piece of state to use throughout. Using
  linear types avoids that an `ST` monad invades
  the larger, pure, part of the program
- is resource-heavy and requires a non-trivial amount of reasoning on the part
  of the programmer to ensure resource protocols are upheld.

[^1]:
  Of course, these are not the only uses, though many use-cases of
  linear types can be seen as a combination of these two
  aspects. Notably, [computations on serialized data][packed-post]
  could be represented with an API similar to that of mutable arrays,
  with a usage protocol layered on top;
  the API ensures a legal and complete write to a memory block that
  has a serialized representation of some data type.

With this in mind, we can envision a standard library as one that has the
**linear gadgets we _want_**, like pure-interfaced mutable data structures and
resource-safe APIs, and the **utilities we _need_**, like a linear `($)`, to
write a typical linear program.

## What we want: linear abstractions or use-cases!

- `linear-base` comes equipped with a pure API for mutable [arrays][lb-array],
  [vectors][lb-vector], [hashmaps][lb-hmap] and [sets][lb-set].
  These enable us to write a mutable implementation of
  [quicksort][lb-qsort] in a few lines:

```hs
  quicksort :: Array Int %1-> Array Int
  quicksort arr = Array.size arr & \case
    (arr', Ur len) -> go 0 (len-1) arr'

  go :: Int -> Int -> Array Int %1-> Array Int
  go lo hi arr = case lo >= hi of
    True -> arr
    False -> Array.read arr lo & \case
      (arr0, Ur pivot) -> partition arr0 pivot lo hi & \case
        (arr1, Ur ix) -> swap arr1 lo ix & \case
          arr2 -> go lo ix arr2 & \case
            arr3 -> go (ix+1) hi arr3

  partition :: Array Int %1-> Int -> Int -> Int -> (Array Int, Ur Int)
  partition arr pivot lx rx
    | (rx < lx) = (arr, Ur rx)
    | otherwise = Array.read arr lx & \case
        (arr1, Ur lVal) -> Array.read arr1 rx & \case
          (arr2, Ur rVal) -> case (lVal <= pivot, pivot < rVal) of
            (True, True) -> partition arr2 pivot (lx+1) (rx-1)
            (True, False) -> partition arr2 pivot (lx+1) rx
            (False, True) -> partition arr2 pivot (lx-1) (rx-1)
            (False, False) -> swap arr2 lx rx & \case
              arr3 -> partition arr3 pivot (lx+1) (rx-1)
```

This code reads rather like an imperative
program, where `arr` is updated through `arr`, `arr1`, `arr2` and so on.
Compared to using the `ST` monad, we lose the ability to share the
effect of mutations, in exchange we gain a pure interface, which can
be less invasive.

- `linear-base` has a resource-safe [File I/O API][lb-rio] that ensures that file
  handles, for instance, are closed and then no longer used.
- `linear-base` includes a [linear variant][lb-streaming] of the
  [streaming][streaming] library, which is compatible with the
  resource-safe I/O monad, to ensure that resources accessed in the
  monadic layers of streams have their protocols obeyed and that the protocols
  of streams themselves are enforced. If the effects in streams can be repeated
  more than once, we could, say, repeat the effect of reading from a file handle
  after that handle is closed. Hence, we use control monads inside linear streams
  to ensure monadic effects are performed exactly once. This is based
  on
  [the work][stream-post] of Edvard Hübinette, for the Haskell Summer
  of Code of 2018.
- In order to define such resource-safe protocols, `linear-base` provides
  [control monads][lb-cmonad] and related monadic functions like `foldM`.

## What we need: linear utilities!

Of course, `linear-base` includes various abstractions that many
linear programs will end up using, namely:

- Variants of `base` abstractions like a linear `Num`, linear `Functor`s (in [two
  flavors][functors-post]), a linear `Data.List` API, and linear versions of
  standard `Prelude` functions like `($)`.

  We need these because functions as simple as

```hs
  f :: Int %1-> Int %1-> Int
  f a b = (a * 4) + b
```

will only type-check if `(*)` and `(+)` have linear arrows. Similarly, using
standard abstractions from `base`, like functors, `($)`, `map`, `foldl`, will
not work inside many functions that have linear arguments.

As a special note, we use a linear `(&)` for a workaround to the
current limitation that all case statements consume the scrutinee (the `x` in
`case x of`) unrestrictedly. See the [user guide][linear-base-guide] for more.

- Mechanisms to interface linear and non-linear code: `linear-base` comes with
  `Ur`, which stands for unrestricted and lets you manipulate a
  non-linear value in a linear context.
  Hence, a function `f :: Ur a %1-> b` can use the `a`
  unrestrictedly after pattern matching on `(Ur x)`. Alongside this,
  `linear-base` comes with the [classes][lb-ur] `Consumable`, `Dupable` and
  `Movable` which allow non-linear use of linearly-bound values; one can
  `consume`, duplicate (possibly with `dup2 :: a %1-> (a,a)`) or, `move` a
  linearly bound value.

```hs
  consume :: Consumable a => a %1-> ()
  dup2 :: Dupable a => a %1-> (a,a)
  move :: Movable a => a %1-> Ur a
```

These are used throughout `linear-base` itself and are likely to
show up in linearly typed programs. For instance,
the linearly-typed streaming function `chain` has
the following type

```hs
  chain :: forall a m r y . (Control.Monad m, Consumable y) =>
    (a -> m y) -> Stream (Of a) m r %1-> Stream (Of a) m r
```

because the `y` must be discarded as the monadic effect is chained
after each `a` in the stream.

## Advanced abstractions \& sneak peeks

Finally, `linear-base` has several features still in the works. Please note
that some of these features like non-GC memory allocation, destination arrays
and push-pull arrays are still very experimental and likely to change.

- Linear [optics][lb-optics] are optics re-designed to work in linear contexts.
- [Non-GC memory allocation][lb-marshall]. We can use linear types to enforce the protocols for
  using memory outside of GHC's garbage collector and on the system heap, i.e., that
  we allocate before use, necessarily free and do not use after that. Hence, the GC has
  less work to do and we as programmers have the control over some allocation which
  could impact the performance of our program.
- [Destination arrays][lb-dest]. Destination arrays, provide a way to write code in
  destination-passing-style. This lets us decide when we allocate space
  for a write-only array that's threaded throughout our program. If we instead
  had just used a vector, we might not have had all the fusion we'd like and might
  have had extra allocations which impact memory use and performance.
- [Push-pull arrays][lb-push-pull]. We can use linear-types to represent
  [polarized arrays][polarized-link]. This enables us to provide a `vector`-like
  API but explicitly control when allocation occurs. With the existing
  `vector` library, one is often relying on array fusion to avoid
  unnecessary allocations, but it can be hard to predict when array
  fusion occurs. Polarized arrays can be either
  output-friendly (a push array) or input-friendly (a pull
  array) and neither form allocates. Only when a regular array is needed does
  one _explicitly_ allocate. So, controlling allocation is done by
  the programmer, not the compiler.

## Community first

Our goal with `linear-base` is to make it easy and fun for the community to
write Linear Haskell programs. The first release is available on
[Hackage][linear-base-hackage] and comes with [examples][linear-base-ex] from
the repository, and, in addition to the haddock, a short [user
guide][linear-base-guide]. Please feel welcome to [point out bugs or request
features][linear-base-issues]. To check out the latest update, see [our GitHub
repository][linear-base-github].

[ghc-release]: https://gitlab.haskell.org/ghc/ghc/-/wikis/status/ghc-9.0.1
[linear-base-guide]: https://github.com/tweag/linear-base/blob/master/docs/USER_GUIDE.md
[learn-linear]: https://github.com/tweag/linear-base#learning-about--xlineartypes
[linear-posts]: https://www.tweag.io/blog/tags/linear-types/
[linear-types-journey]: https://www.tweag.io/blog/2020-06-19-linear-types-merged/
[linear-base-hackage]: http://hackage.haskell.org/package/linear-base
[linear-base-github]: https://github.com/tweag/linear-base
[linear-base-issues]: https://github.com/tweag/linear-base/issues
[lh-paper]: https://arxiv.org/abs/1710.09756
[linear-base-ex]: https://github.com/tweag/linear-base/tree/master/examples
[packed-post]: https://www.tweag.io/blog/2017-08-24-linear-types-packed-data/
[lb-qsort]: https://github.com/tweag/linear-base/blob/quicksort/examples/Simple/Quicksort.hs
[lb-vector]: https://github.com/tweag/linear-base/blob/master/src/Data/Vector/Mutable/Linear.hs
[lb-hmap]: https://github.com/tweag/linear-base/blob/master/src/Data/HashMap/Linear.hs
[lb-set]: https://github.com/tweag/linear-base/blob/master/src/Data/Set/Mutable/Linear.hs
[lb-array]: https://github.com/tweag/linear-base/blob/master/src/Data/Array/Mutable/Linear.hs
[lb-rio]: https://github.com/tweag/linear-base/blob/master/src/System/IO/Resource.hs
[streaming]: https://hackage.haskell.org/package/streaming
[lb-streaming]: https://github.com/tweag/linear-base/tree/master/src/Streaming
[stream-post]: https://www.tweag.io/blog/2018-06-21-linear-streams/
[functors-post]: https://www.tweag.io/blog/2020-01-16-data-vs-control/
[lb-ur]: https://github.com/tweag/linear-base/blob/master/src/Data/Unrestricted/Linear.hs
[lb-cmonad]: https://github.com/tweag/linear-base/blob/master/src/Control/Functor/Linear.hs
[lb-optics]: https://github.com/tweag/linear-base/blob/master/src/Control/Optics/Linear/Internal.hs
[lb-marshall]: https://github.com/tweag/linear-base/blob/master/src/Foreign/Marshal/Pure.hs
[lb-dest]: https://github.com/tweag/linear-base/blob/master/src/Data/Array/Destination.hs
[lb-push-pull]: https://github.com/tweag/linear-base/blob/master/src/Data/Array/Polarized.hs
[polarized-link]: http://jyp.github.io/posts/controlled-fusion.html
