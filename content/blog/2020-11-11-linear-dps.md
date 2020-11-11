---
title: "Pure destination-passing style in Linear Haskell"
author: Arnaud Spiwack
tags: [haskell, linear-types]
description: "Destinations give you control over your memory allocations, and linear types make them pure."
---

My goal today is to convince you that destination-passing style is
neat, actually. And that [linear types][linear-types] make
destination-passing purely functional. But first, I must answer a
question.

## What is destination-passing style?

If you've ever programmed in C, C++, or Fortran, you are sure to have
encountered the style of programming which sometimes goes by the name
_destination-passing style_. It is the practice of writing, _e.g._ an
array-producing functions as, instead, taking an empty array as an
extra argument and filling it. Consider, for example, the C `strcpy` function:

```c
char* strcpy ( char* destination, const char* source );
```

It copies the string in `source` to the array `destination` (it also
returns `destination` when it's done).

The name "destination-passing style" itself seems to be more common in
the functional programming language compilation literature, however. C
programmers don't appear to have a name for it. So it is likely that
you have never encountered it.

## But this is extremely imperative, why should I care?

Why, indeed, care about destination-passing? It does let you ask a new
question: "whose responsibility is it to allocate the array?". If I
were to write an array copy in Haskell, it would have type

```haskell
copyArray :: Array a -> Array a
```

And there is no way around `copyArray` allocating an array itself. The
question doesn't even exist. With `strcpy`, I can either choose to
allocate an array, and pass it immediately to `strcpy`, or, I can
delegate the allocation of the array to someone else.

But, once I can ask this question, what can I do with it? I can
compose it! Let's imagine that we have a function to split an array in
two

```haskell
splitArray :: Array a -> (Array a, Array a)
```

Now consider the following (admittedly not especially useful)
function:

```haskell
copyArray2 :: Array a -> Array a
copyArray2 a = case splitArray a of
  (al, ar) -> copyArray al <> copyArray ar
```

When the question doesn't exist, each call to `copyArray` has, no matter what,
to allocate an array, which is then copied into a new array. It means
that we are making a superfluous copy of our original array,
only to discard it immediately. This is quite wasteful.

## Won't fusion take care of that, though?

Often, you can, indeed, rely on array fusion to avoid too egregious a
behaviour. Array fusion, such as implemented in the excellent [vector][vector]
library will eliminate a ton of intermediate allocations.

However, fusion is unreliable. Sometimes, a simple refactoring will
push a function's size beyond what GHC is willing to inline, and it
will break an entire fusion pipeline. Most of the time, this is fine,
but not when you are dependent on fusion happening. And if you need
GHC to produce code without allocations, why not write your program directly as you want
it, rather than try and coax the compiler into hopefully eliminating
the allocations for you.

This has been a guiding principle in the development of the linear
types project: **compiler optimisations are great, as you don't need
to think about a lot of things; until you do, and you find yourself
second-guessing the optimiser**. When that happens, we want linear
types to empower you to write the code that you mean, without
sacrificing Haskell's type safety.

Besides, in the [article about F̃][dps-msr], a restricted array-based
functional language which compiles to very efficient code, the authors
find significant performance gains for using destination-passing on
top of an array fusion optimisation. They only use destination-passing
in the optimiser, though, not as a language feature.

Finally, fusion doesn't always work. Suppose I rewrite my `copyArray2`
function to use threads to better utilise my multicore architecture

```haskell
copyArray3 :: Array a -> IO (Array a)
copyArray3 = case splitArray a of
  (al, ar) -> do
    (bl, br) <- concurrently
      (evaluate $ copyArray al)
      (evaluate $ copyArray ar)
    return $ bl <> br
```

This is beyond a fusion framework ability to optimise. Or maybe I want
to copy my array into a memory mapped buffer. The point is: fusion
will do a lot for you, just not everything.

## Ok, but does that mean I have to use ST everywhere?

The obvious way to encode destination-passing style, in Haskell, is to
move all our computation to `ST`, so that `copyArray` would be

```haskell
copyArray :: STArray s a -> STArray s a -> ST ()
```

But it's not very congruent with how functional programmers write
their programs. It does lift all of the above limitations, at the
price of adding state everywhere, which is an entire error-inducing
surface that functional programming usually avoids.

It's a huge price to pay, and that's why the [vector][vector] library is not
structured like this. It does feature mutable arrays, but immutable
arrays are very much encouraged.

This is where [linear types][linear-types] help. Indeed, let's take a
step back and ask: what makes a destination impure to begin with?

- If I read out a cell, then write to it, then read it again: I'll see a
  different result the second time.
- If I write to the same cell twice, the writes need to be ordered,
  otherwise the result would be non-deterministic.
- Reading a cell which has not been initialised is non-deterministic
  (though in most case, we can salvage this by initialising every cell
  with `undefined`)

All of these behaviours are prohibited in pure code. But we could
avoid all the prohibited behaviours if we could make sure that each
cell is written to exactly once before being read. Aha! Exactly once,
this is the sort of thing that linear types are good at! Ok, so let's
try again:

```haskell
copyArray :: Array a -> DArray a ⊸ ()
```

This means that `copyArray` is a _pure_ function which uses its destination
(in its entirety) exactly once. We only need to make sure that there
is only ever a unique pointer to a destination array, which we do with
the `alloc` function:

```haskell
alloc :: Int -> (DArray a ⊸ ()) ⊸ Array a
```

A destination is allocated for the scope of the linear function. At
the end of the function, we know that the destination has been fully
filled, and so we get an array out. From this destination-passing
version of `copyArray`, by the way, it is easy to retrieve the
direct style variant:

```haskell
copyArray' :: Array a -> Array a
copyArray' a = alloc (length a) (\d -> copyArray a d)
```

The reverse, as I've been arguing throughout this post, is very much
not true. So the destination-passing function is the more fundamental
one.

Now, to be able to implement `copyArray2`, we need a function which
splits destinations

```haskell
splitDArray :: DArray a ⊸ (DArray a, DArray a)
```

Then, it is just a matter of following the types (the curious-looking `& \case` construction is due to a limitation of the current
implementation of linear types in GHC, see [here](https://github.com/tweag/linear-base/blob/8642e4209ffd663e1f1f35ddd977da0d073fa1af/docs/USER_GUIDE.md#case-statements-are-not-linear))

```haskell
copyArray2 :: Array a -> DArray a ⊸ ()
copyArray2 a d = case splitArray a of
  (al, ar) -> splitDArray d & \case
    (dl, dr) -> copyArray al dl `lseq` copyArray ar dr
```

Voilà! No superfluous allocation. Not because of the optimiser, but
because of the semantics of my program: it doesn't allocate an array
anywhere.

You'll find a more complete destination array interface in [the
`Data.Array.Destination` module of linear-base](https://github.com/tweag/linear-base/blob/191badef5c92aaa44a7f311b0c9978fc144622f1/src/Data/Array/Destination.hs).

## Closing thoughts

One of the features of linear types, is that they often allow to
expose as pure interfaces objects which appear to be intrinsically
impure. But I want to argue that, in the case of destinations, we've
actually done more than this: we've made the interface _better_ than
the impure interface. Not because pure interfaces are better than
impure interfaces (though it's a defensible position), but because the
linear destination interface is a more faithful representation of what
destinations mean.

There is no longer confusion about what is an input and what is an
output: inputs are `Array`, and outputs are `DArray`. Destinations are
there solely for output, they can't be used as a temporary store of
data. And the types ensure that they are fully filled, and that we
don't accidentally overwrite an output, by the time the destination is
read back as an array.

And this is pretty neat.

If you want to go a bit deeper into this particular brand of weed, let
me leave you with a handful of comments which you can take either as
closing this blog post, or opening new avenues.

- The `alloc` function takes a destination-consuming function as an
  argument, instead of returning a destination directly. This style is
  common in Linear Haskell, as a means to enforce uniqueness. It is
  sometimes seen as a limitation of Linear Haskell's design. However
  in this particular case, the function is necessary to _delimit the
  scope_ of the destination. In fact, the `alloc` function is
  virtually identical to that of the [F̃ article][dps-msr], where there
  is no linear typing whatsoever.
- Affine types (affine arguments are consumed _at most_ once,
  rather than _exactly_ once for linear arguments) are sometimes
  preferable to linear types. For instance affine types appear to
  [represent streaming
  computations better](https://www.tweag.io/blog/2018-06-21-linear-streams/). But
  in the case of destinations we really do want linear types: it
  wouldn't make as much sense to return from `alloc` with a
  partially-filled destination.
- When using linear types to make a pure interface to array functions
  which, in fact, mutate an array for efficiency (like in [this module
  of linear
  base](https://github.com/tweag/linear-base/blob/191badef5c92aaa44a7f311b0c9978fc144622f1/src/Data/Array/Mutable/Linear.hs)),
  we lose the ability to alias the mutable array in exchange for
  purity. Sometimes it's a perfectly acceptable trade-off, but some
  algorithms depend on sharing mutation for efficiency, these are not
  available with linear pure mutable arrays. We are not making such a
  trade-off for destinations: linear destinations, being pure output,
  are, arguably, a more faithful interface for destination-passing
  style than mutable array.
- Have you noticed how in the destination-passing `copyArray2`, the
  call to array concatenation from the direct-style implementation has
  been replaced by a call to `splitDArray`? And, if you have, have you
  also noticed the symmetry between these two functions?

  ```haskell
  uncurry (<>) :: (Array a, Array a) -> Array a
  splitDArray :: Darray a ⊸ (DArray a, DArray a)
  ```

  This is not a coincidence. There is a sort of duality between
  destinations and constructors. This duality happens when writing
  destinations for other types than array types as well. I [spoke of this
  phenomenon at Haskell Exchange 2017][hx-2017].

- If I define `type PushArray a = DArray a ⊸ ()`, then the type of
  `copyArray`
  ```haskell
  copyArray :: Array a -> DArray a ⊸ ()
  ```
  can be written
  ```haskell
  copyArray :: Array a -> PushArray a
  ```
  We can give `PushArray`s a (restricted) array interface, then we don't
  even need to abandon direct style to benefit from destinations. This
  is part of the
  [Data.Array.Polarized](https://github.com/tweag/linear-base/blob/8642e4209ffd663e1f1f35ddd977da0d073fa1af/src/Data/Array/Polarized.hs)
  framework in linear-base.
- In [a previous blog
  post](https://www.tweag.io/blog/2017-08-24-linear-types-packed-data/),
  I had written about how linear types made it possible to manipulate
  compact data representation directly. The `Need` type in that blog
  post is, in fact, a form of destination.

[linear-types]: https://www.tweag.io/blog/tags/linear-types
[dps-msr]: https://dl.acm.org/doi/abs/10.1145/3122948.3122949
[hx-2017]: https://skillsmatter.com/skillscasts/10637-distributed-programming-with-linear-types
[vector]: https://hackage.haskell.org/package/vector
