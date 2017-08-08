---
title: Safe and efficient binary representation <br>with linear types
author: Arnaud Spiwack
featured: yes
---

We saw [last time][blog-post-sockets] that with linear types, we could
precisely capture the state of sockets _in their types_. In this post,
I want to use the same idea of tracking states in types, but applied
to a more unusual example from our [paper][paper]: optimized data type
representation.

Consider a simple tree type:

```haskell
data Tree = Branch Tree Tree | Leaf Int
```

We are used to seeing this represented as a pointer structure in the
heap. But what if, instead, we actually represent it as an array, in
preorder traversal. That is,
`Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Branch (Leaf 4)
(Leaf 5)))` would be represented as:

```haskell
+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+
| Branch | Branch |  Leaf  |   1    |  Leaf  |   2    | Branch |  Leaf  |   3    | Branch |  Leaf  |   4    |  Leaf  |   5    |
+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+
```

Why would anybody want to do such a thing? Because,

* There are no pointers _at all_ in this representation. This means
  that the garbage collector never needs to traverse this structure.
  Less work for the garbage collector means more predictable latencies
  and better throughput.
* It is a very cache friendly representation, because data commonly
  accessed together lives contiguously in memory.
* You may be sending or receiving this tree over the network, and
  working directly on an array-of-byte representation saves the cost
  of serializing and deserializing data, which is a common bottleneck
  in distributed applications. If you have heard
  of [compact normal forms][cnf], it is pretty much the same idea. (If
  you want to know more, Ryan Newton, one of the coauthors on
  the [linear-type paper][paper], has also been involved in an
  entire [article on such representations][gibbon].)

To program with such a data structure, I need a pattern matching
operation:

```haskell
type Packed (l :: [*])

caseTree
  :: Packed (Tree ': r)
  -> Either (Packed (Tree ': Tree ': r)) (Int, Packed r)
```

The type is indexed by a list of types. This is because
in the `Branch` case, we can't return two packed subtrees. Indeed, we
don't know the size of the left subtree. So we need to traverse it
before we can get to the right subtree. Instead we just return a
packed representation of _two trees_.

But how do we create a new `Packed Tree`? In the compact normal form
paper, we are given two functions:

```haskell
unpack :: Packed [a] -> a
pack :: a -> Packed [a]
```

However, relying on them to build new `Packed Tree`-s is
wasteful: one of the reasons for using such a representation was to avoid
(de)serialisation. Instead, we want to create new `Packed Tree`-s by
explicitly writing into a buffer.

Writing into a buffer calls for the `ST` monad. But there is the
business of the index. Just like with the sockets
of [my previous post][blog-post-sockets], we could go back in time to
disastrous effects: we could write two trees in two interleaved
histories and get out an inconsistent mix of these histories. So we
reach for linear types again. An added bonus is that linearly-used
write-only buffers are observationally pure, so no need for a monad at
all.

The type of write buffers is

```haskell
type Need (l :: [*]) (t :: *)
```

where `Need '[Tree, Tree, Int] Tree` should be understood as "write two
`Tree`-s and an `Int` and you will get a (packed) `Tree`", as illustrated
by the `finish` function:

```haskell
-- `Unrestricted` means that the returned value is not linear: you get
-- as many uses as you wish
finish :: Need '[] t ⊸ Unrestricted (Packed [t])
```

Buffers are filled with "constructors":

```haskell
leaf :: Int -> Need (Tree':r) t ⊸ Need r t
branch :: Need (Tree':r) ⊸ Need (Tree':Tree':r)
```

Finally (or initially!) we need to allocate a buffer. The following
pattern expresses that the buffer _must_ be used linearly:

```haskell
alloc :: (Need '[Tree] Tree ⊸ Unrestricted r) ⊸ Unrestricted r

-- When using `Unrestricted a` linearly, you have no restriction on the inner `a`!
data Unrestricted a where Unrestricted :: a -> Unrestricted a
```

What I find amazing about this API is that it changes a mind-bendingly
hard problem: programming directly with serialized forms of data
types, into a rather comfortable situation. The key ingredient is
linear types. I'll leave you with an exercise, if you like a challenge: implement `pack` and
`unpack` in terms of the other primitives. You may want to use
a [type checker][prototype].

[paper]: https://github.com/tweag/linear-types/releases/download/v2.0/hlt.pdf
[prototype]: https://github.com/tweag/ghc/tree/linear-types
[blog-post-one]: http://www.tweag.io/posts/2017-03-13-linear-types.html
[blog-post-sockets]: http://www.tweag.io/posts/2017-08-03-linear-typestates.html
[socket-library]: https://www.stackage.org/package/socket
[typestate-wikipedia]: https://en.wikipedia.org/wiki/Typestate_analysis
[gibbon]: http://dx.doi.org/10.4230/LIPIcs.ECOOP.2017.26
[cnf]: https://doi.org/10.1145/2858949.2784735
