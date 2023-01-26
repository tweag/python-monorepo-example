---
title: "Linear Constraints: the problem with O(1) freeze"
author: Arnaud Spiwack
description: "How a linear constraint arrow relates to freezing arrays in Haskell."
tags: [haskell, linear-types]
---

This is the first of two companion blog posts to the paper [_Linearly
Qualified Types_][linear-constraints-paper], published at ICFP 2021
(there is also a [long version, with
appendices][linear-constraints-arxiv]). These blog posts will dive
into some subjects that were touched, but not elaborated on, in the
paper. For more introductory content, you may be interested in [my talk
at ICFP][linear-constraints-talk].

<!-- 1. _The problem with O(1) freeze_ -->
<!-- 2. TBA -->

---

In 2018, Simon Peyton Jones was giving a Haskell Exchange's [keynote on
linear types in Haskell][spj-haskellx-2018] (there is also a version
of the [talk on Youtube][spj-haskellx-2018-yt], but the audio desyncs
after a while). Roman Leshchinskiy, author of the remarkable
[vector][vector-hackage] package, was sitting next to me. Simon Peyton
Jones was describing how linear types allowed for mutable arrays with
a pure interface. Roman Leshchinskiy asked, “What about mutable arrays
of mutable arrays?” (timestamp: 22min) My answer was, “It's
harder.”

This blog post is me finally explaining this in more detail. In fact, this
mutable-array-of-mutable-array issue was what caused the inception of
the work on linear constraints.

## Mutable arrays with ST

The traditional API for mutable arrays, in Haskell, looks like this:

```haskell
new :: Int -> a -> ST s (MArray s a)
read :: MArray s a -> Int -> ST s a
write :: MArray s a -> Int -> a -> ST s ()
unsafeFreeze :: MArray s a -> ST s (Array a)
```

It uses the `ST` monad for sequencing to represent mutation. The
`unsafeFreeze` function is crucial: this is how we make immutable
arrays. To build an _immutable_ array, first make a _mutable_ array,
set the values in each cell to the desired value, then freeze (and
`runST`). Up to some low-level considerations, this is how Haskell's
[`array`][base-array-function] is implemented:

```haskell
array :: Int -> [(Int, a)] -> Array a
array n assocs = runST $ do
  buffer <- newArray n undefined
  forM_ assocs $ \(i,a) ->
    write buffer i a
  unsafeFreeze buffer
```

However, after using `unsafeFreeze` you must not
mutate the array ever again. If you keep a pointer to the `MArray` and
modify it, then you will, in fact, modify the “frozen” immutable array
as well.

```haskell
runST $ do
  -- A mutable array full of zeros
  marr <- newArray 42 0
  -- Freeze into a immutable array
  arr <- freeze marr
  let x = arr!0
  -- But write into the array again!
  writeArray marr 0 57
  -- False. Or sometimes True. Depending on whether x has been
  -- inlined. Haskell is very angry at you.
  return $ x == arr!0
```

This is why the
function is called _unsafe_. It would be quite possible to make a safe
`freeze` function: simply make a copy the array. Now there doesn't
exist an `MArray` pointer to the `Array` and we are safe, but the cost
is that freezing is no longer constant time. This is a cost we are typically
not willing to pay.

## Pure mutable arrays with linear types

Linear types offer a solution to this problem: a safe constant-time
freeze. In addition to making the whole interface pure, it gets rid of
the `ST` monad. It looks something like this:

```haskell
new :: Int -> a -> (MArray a %1 -> Ur b) %1 -> Ur b
read :: MArray a %1 -> Int -> (MArray a, Ur a)
write :: MArray a %1 -> Int -> a -> MArray a
freeze :: MArray a %1 -> Ur (Array a)
```

The idea is that there is always a single pointer to a given `MArray`,
this way we can safely mutate the array: nobody can look at past
versions of the array, so they can't observe that the array has, in
fact, changed. This is also why `freeze` is both safe and runs in
constant time: it simply returns a pointer to the same array,
consuming the (unique) pointer of type `MArray` in the process, so we
can't mutate the array anymore.

Notice that `freeze` returns an `Ur (Array a)`. `Ur` stands for
“unrestricted”, which means that the `Array a` doesn't have to be used
linearly (if you are familiar with linear logic, `Ur a` corresponds to
`!A`). In the case of `freeze`, it means that the returned `Array` is
not subject to the linearity discipline.

The way we arrange for there to always be a single pointer to the
`MArray` is that it is only ever consumed by linear functions. The
key is the `new` function. This is why it takes a continuation as an
argument. I call this argument a _scope function_ because it scopes
where the array can be used. The scope function returns an `Ur b` so
that the array doesn't escape its scope: an `MArray` is never
unrestricted, in particular there is no value of type `Ur (MArray a)`. Scope functions, and how to improve on them, are the subject of my
next blog post.

As `read` and `write` make apparent, an `MArray` cannot contain linear
data, only unrestricted data. The reason for this is `freeze`: we want
to simply return the same pointer with a different type, but by virtue
of the returned `Array` being unrestricted, we cannot guarantee that
the values stored in the cells will be used linearly. For instance, we
may decide not to use the `Array` at all, in particular not to use the
cells' content.

But an `MArray` is always a linear value, therefore, we can't make an
`MArray (MArray a)`. Another way to think about this is that if I'm
freezing an `MArray (MArray a)`, what we want to get is an `Array (Array a)`. That is, not only do we need to change the outer type, we
need to change the type of cells as well.
One way would be to map `freeze` over the cells, then `freeze` the
result. This process no longer runs in constant time. The same problem
exists in the `ST` implementation, but there it is at least possible
to make an `MArray s (MArray s a)`, while retaining a constant-time
`freeze` in the shallow case.

This API was what Simon Peyton Jones was presenting at
Haskell 2018. It's also the API from the [Linear Haskell
paper][linear-haskell-paper] ([long version][linear-haskell-arxiv]
with appendices), and, in fact, the [API currently in
linear-base][linear-base-mutable-array].

## It's harder

If we are to make mutable arrays of mutable arrays possible while
retaining a constant-time freeze, we will have to look for another
strategy.

Let's turn to [Rust] for a moment. A `freeze` function in Rust would
look like:

```rust
fn freeze<T>(v: Vec<T>) -> Rc<Vec<T>> {
  Rc::new(v)
}
```

Freezing is the most natural thing in an ownership system
like Rust's: freezing is relinquishing ownership. What matters to us
is that you can freeze a `Vec<Vec<T>>` or a
`Vec<Vec<Vec<T>>>` and everything will have become recursively
immutable.
A key reason why Rust can pull this off at all is that mutable
vectors and immutable vectors have the same type. Immutability can be
signified, instead, by adding an `Rc` prefix.

If we are to make `freeze` not change the type of arrays, we can't
have an invariant like “An `MArray` is only ever consumed by linear
functions”. We need a change of perspective. What if, instead of the
array being linear, we had a token that gives us permission to write to
and read from the array?

An API function would look like this:

```haskell
write :: RW %1 -> Array a -> Int -> a -> RW
```

The `Array` argument doesn't need to be linear anymore: only the `RW`
token is (and only the `RW` needs to be returned). This is not quite
right though: there is no connection between the `RW` token and the
`Array`. We could use the `RW` token for another, frozen, array on
which we only have read permission. To link the two we introduce an
extra argument `n`, serving as a type-level name of the array. We can
then have:

```haskell
write :: RW n %1 -> Array a n -> Int -> a -> RW n
```

Now, the `RW` token is what ensures proper sequencing of reads and
writes.
What have we gained? Well, we can make the `RW` token scope over the
entire `Array`. That is, the `Array a n` contains mutable data, whose
permissions to read from or write to is controlled by the `RW n` token
for the outer array. Pulling all this together we can make the
following API:

```haskell
type Array :: (Name -> Type) -> Name -> Type

type RO :: Name -> Type
type RW :: Name -> Type

-- `newArray` creates an array initialised with `undefined` values.
newArray :: Int -> (forall n. RW n %1 -> Array a n -> Ur b) -> Ur b
-- `borrow` gives unrestricted read-only permission to the entire
-- array. You can read subarrays with `read`.
borrow :: RW n %1 -> Array a n -> (forall m. RO m -> Array a m -> b) -> (RW n, b)
read :: RO n -> Array a n -> Int -> a n
-- `borrowWriteAt` gives linear read-write permission to an inner
-- array. You can write at the current array with `write`, or access
-- a more inner array with nested `borrowWriteAt`.
borrowWriteAt :: RW n %1 -> Array a n -> Int -> (forall m. RW m %1 -> a m -> (RW m, b)) -> (RW n, b)
write :: RW n %1 -> RW m %1 -> Array a n -> Int -> a m -> RW n
freeze :: RW n %1 -> Array a n -> Ur (RO n)

-- `Ref` wraps regular values into permissionned values.
type Ref :: Type -> Name -> Type

newRef :: a -> (forall n. RW n %1 -> Ref a n -> Ur b)
-- `readRef` doesn't require permission because there is no
-- `writeRef`.
readRef :: Ref a n -> a
```

The type of `Array` cells is changed to be of type `Name -> Type`. This is used in `write`, for instance, where the name `m` of
the array whose cell we write into vanishes, so that the outer `RW n` now scopes over the former `a m` as well. There is a new
function, `borrow`, to allow scoped read-only access to the array (and
its inner components). Now `freeze` simply consumes a `RW` token
and returns an unrestricted `RO` token. That is all it
does. Internally, a `RW` token and a `RO` token are trivial values, so
freeze is constant-time. After `freeze`, the array and all the
inner arrays are immutable, and reads are unrestricted, as we
intended.

Having to carry these token around, however, is quite cumbersome, to
say the least. I think we'd all take the absence of support for
mutable arrays of mutable arrays rather than manually carrying a token
around.

## Linear constraints

The _Linearly Qualified Types_ paper offers a solution to this
conundrum in the form of _linear constraints_. The manipulation of the
`RW` tokens is systematic enough, so our goal is to make the type
checker deal with them itself.

A constraint, in Haskell, is whatever is to the left of a fat arrow
`=>`. For instance, `Show a`, in `show :: Show a => a -> String`, is a
constraint. A constraint is precisely what
GHC's type checker deals with itself: we want `RW` to be a
constraint. But `RW` needs to be handled linearly, whereas constraints are
typically unrestricted.

Linear constraints, as their name implies, bridge this gap by
introducing a _linear fat arrow_ `%1 =>`. Linear constraints are
constraints which adhere to the linear type discipline. It does mean
that using a given linear constraint `C`, for a function that wants it,
makes `C` unavailable to other functions. So, just like we return
`RW` tokens in the API above, we will need to return constraints.

To return a constraint we will use the `⧀` type:

```haskell
⧀ :: Constraint -> Type -> Type
```

The `⧀` type can be implemented today as:

```haskell
data c ⧀ a where
  R :: c %1 => a %1 -> c ⧀ a
```

From an ergonomic standpoint, this is a bit of a pain, as we need to
explicitly construct and deconstruct `c ⧀ a` values with a data
constructor, rather than returning a plain `a`. Fortunately, this is
precisely the sort of issues that the [existential types
proposal][existential-types-proposal] will solve.

With linear constraints, we can turn the `RW` token into a constraint
and the array API becomes:

```haskell
type Array :: (Name -> Type) -> Name -> Type

type RO :: Name -> Constraint
type RW :: Name -> Constraint

newArray :: RW m %1 => Int -> (forall n. RW n %1 => Array a n -> Ur b) -> Ur b
borrow :: RW n %1 => Array a n -> (forall m. RO m => Array a m -> b) -> RW n ⧀ b
read :: RO n => Array a n -> Int -> a n
borrowWriteAt :: RW n %1 => Array a n -> Int -> (forall m. RW m %1 => a m -> RW m ⧀ b) -> RW n ⧀ b
write :: (RW n, RW m) %1 => Array a n -> Int -> a m -> RW n ⧀ ()
freeze :: RW n %1 => Array a n -> Ur (RO n) ⧀ ()
```

(Note that we've overloaded `Ur`, here, to also hold unrestricted constraints.)

## Conclusion

One of the reasons why I got interested in making linear constraints a
thing in the first place was this problem of $O(1)$ recursive
freeze. The token-passing API solves the issue in theory, but I don't
think that it's pleasant enough to use in practice.

While there is definitely room for mutable data structures which only
hold unrestricted data, the story wouldn't quite be complete without
nested mutable data structures. At the end of the day, all linear
constraints do for us is let the type-checker push tokens
around. This turns out to be a powerful idea.

[linear-constraints-paper]: https://dl.acm.org/doi/10.1145/3547626
[linear-constraints-arxiv]: https://arxiv.org/abs/2103.06127
[linear-constraints-talk]: https://www.youtube.com/watch?v=c8VZp-3eQU0
[linear-haskell-paper]: https://dl.acm.org/doi/10.1145/3158093
[linear-haskell-arxiv]: https://arxiv.org/abs/1710.09756
[linear-base-mutable-array]: https://hackage.haskell.org/package/linear-base-0.2.0/docs/Data-Array-Mutable-Linear.html
[linear-base-ur]: https://hackage.haskell.org/package/linear-base-0.2.0/docs/Data-Unrestricted-Linear.html#t:Ur
[spj-haskellx-2018]: https://skillsmatter.com/skillscasts/11067-keynote-linear-haskell-practical-linearity-in-a-higher-order-polymorphic-language
[spj-haskellx-2018-yt]: https://www.youtube.com/watch?v=5mxKEYzBAVk
[base-array-function]: https://hackage.haskell.org/package/array-0.5.4.0/docs/Data-Array.html#v:array
[rust]: https://www.rust-lang.org/
[existential-types-proposal]: https://github.com/ghc-proposals/ghc-proposals/pull/473
[vector-hackage]: https://hackage.haskell.org/package/vector
