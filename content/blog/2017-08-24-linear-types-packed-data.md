---
redirect_from: [/posts/2017-08-24-linear-types-packed-data.html]
title: Compact normal forms + linear types  = efficient network communication
shortTitle: Compact normal forms + linear types
author: Arnaud Spiwack
featured: yes
tags: [haskell, linear-types]
---

We saw [last time][blog-post-sockets] that with linear types, we could
precisely capture the state of sockets _in their types_. In this post,
I want to use the same idea of tracking states in types, but applied
to a more unusual example from our [paper][paper]: sending rich
structured data types across the network and back with as little
copying as possible.

Our approach to this problem works for values of any datatype, but for
the sake of simplicity, we'll consider here simple tree values of the
following datatype:

```haskell
data Tree = Branch Tree Tree | Leaf Int
```

## Network communication and serialization

Say I have one such `Tree`. And suppose that I want to use a service,
on a different machine across the network, that adds `1` to all the
leaves of the tree.

The process would look like this:

- I serialize my tree into a serialized form and send it across the
  network.
- The service deserializes the tree.
- The service adds `1` to the leaves.
- The service serializes the updated tree and sends it across the
  network.
- I deserialize this tree to retrieve the result.

This process involves copying the tree structure _5 times_, converting
back and forth between a pointer representation, which Haskell can
use, and a serialized representation, which can be sent over the
network, for a single remote procedure call.

This goes to show that it should be no surprise when overhead of
serialization and deserialization in a distributed application is
significant. It can even become the main bottleneck.

## Compact normal forms

To overcome this, [compact normal forms][cnf] were introduced in GHC
8.2. The idea is to dispense with the specialised serialized representation
and to send the pointer representation through the network.

Of course, this only works if the service is implemented in Haskell
too. Also, you can still only send bytestrings across the network.

To bridge the gap, data is copied into a _contiguous_ region of memory,
and the region itself can be seen as a bytestring. The interface is
(roughly) as follows:

```haskell
compact   :: Tree -> Compact Tree
unCompact :: Compact Tree -> Tree
```

The difference with serialization and deserialization is that while
`compact t` copies `t` into a form amenable to network communication,
`unCompact` is _free_ because, in the compact region, the `Tree` is
still a bunch of pointers. So our remote call would look like this:

- I `compact` my tree and send it across the network.
- The service retrieves the tree and adds `1` to the leaves.
- The service compacts the updated tree and sends it across the
  network.

When I receive the tree, it is already in a pointer representation
(remember, `unCompact` is free), so we are down to 3 copies! We also
get two additional benefits from compact normal forms:

- A `Tree` in compact normal form is close to its subtrees, so tree
  traversals will be cache friendly: it is likely that following a
  pointer will land into already prefetched cache.
- Compact regions have no _outgoing_ pointers. It means that the
  garbage collector never needs to traverse a compact region: a
  compact region does not keep other heap object alive. Less work for
  the garbage collector means both more predictable latencies and
  better throughput.

## Programming with serialized representations

Compact normal forms save a lot of copies. But they still _impose_ one
copy as `compact` is the only way to introduce a compact value. To
address that we could make an even more radical departure from the
traditional model. While compact normal forms do away with the
serialized representation and send the pointer representation instead,
let's go in the opposite direction, and compute directly with the
serialized representation! That is, `Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3))` would be represented as a single contiguous buffer in memory:

```haskell
+--------+--------+--------+--------+--------+--------+--------+--------+
| Branch | Branch |  Leaf  |   1    |  Leaf  |   2    |  Leaf  |   3    |
+--------+--------+--------+--------+--------+--------+--------+--------+
```

If you want to know more, Ryan Newton, one of my coauthors on
the [linear-type paper][paper], and also a co-author on
the [compact-normal-form paper][cnf] has also been involved in an
entire [article on such representations][gibbon].

In the meantime, let's return to our example. The remote call now has
a _single_ copy, which is due to our immutable programming model,
rather than due to networking:

- I send my tree, _the service adds `1` to the leaves of the tree_,
  and sends the result back.

Notice that when we are looking at the first cell of the
array-representation of the tree above, we know that we are looking at
a `Branch` node. The left subtree comes immediately after, so that one
is readily available. But there is a problem: we have no way to know
where the right subtree starts in the array, so we can't just jump to
it. The only way we will be able to access subtrees is by doing
left-to-right traversals, the pinnacle of cache-friendliness.

If this all starts to sound a little crazy. It's probably because it
kind of is. **It is very tricky to program with such a data
representation**. It is so error prone that, despite the performance
edge, our empirical observation is that even very dedicated
C programmers don't do it.

The hard part isn't so much directly consuming a representation of the
above form. It's constructing them in the first place. We'll get to
that in a minute, but first let's look at what consuming trees looks
like:

```haskell
data Packed (l :: [*])

caseTree
  :: Packed (Tree ': r)
  -> Either (Packed (Tree ': Tree ': r)) (Int, Packed r)
```

We have a datatype `Packed` of trees represented as above. We define
a one-step unfolding function, `caseTree`, which you can as well think
of as an elaborate `case-of` (pattern matching) construct. There is
a twist: `Packed` is indexed by a list of types. This is because of
the right-subtree issue that I mentioned above: once I've read
a `Branch` tag, I have a pointer to the left subtree, but not to the
right subtree. So all I can say is that I have a pointer which is
followed by the representation of two consecutive trees (in the
example above, this means a pointer to the second `Branch` tag).

The construction of trees is a much trickier business. Operationally
we want to write into an array, but we can't just use a mutable array
for this: it is too easy to get wrong. We need at least:

- To write each cell only once, otherwise we can get inconsistent, nonsensical
  trees such as
  ```haskell
  +--------+--------+--------+--------+--------+--------+--------+--------+
  | Branch | Branch |   0    |   1    |  Leaf  |   2    |  Leaf  |   3    |
  +--------+--------+--------+--------+--------+--------+--------+--------+
  ```
- To write complete trees otherwise we may get things like
  ```haskell
  +--------+--------+--------+--------+--------+--------+--------+--------+
  | Branch | Branch |  Leaf  |   1    |  Leaf  |   2    |        |        |
  +--------+--------+--------+--------+--------+--------+--------+--------+
  ```
  where the blank cells contain garbage

You will have noticed that these are precisely the invariants that
linear types afford. An added bonus is that linear types make our
arrays observationally pure, so no need for, say, the `ST` monad.

The type of write buffers is

```haskell
data Need (l :: [*]) (t :: *)
```

where `Need '[Tree, Tree, Int] Tree` should be understood as "write two
`Tree`-s and an `Int` and you will get a (packed) `Tree`", as illustrated
by the `finish` function:

```haskell
-- `Unrestricted` means that the returned value is not linear: you get
-- as many uses as you wish
finish :: Need '[] t ⊸ Unrestricted (Packed [t])
```

To construct a tree, we use constructor-like functions (though,
because we are constructing the tree top-down, the arrows are in the
opposite direction of regular constructors):

```haskell
leaf :: Int -> Need (Tree':r) t ⊸ Need r t
branch :: Need (Tree':r) t ⊸ Need (Tree':Tree':r) t
```

Finally (or initially!) we need to allocate an array. The following
idiom expresses that the array _must_ be used linearly:

```haskell
alloc :: (Need '[Tree] Tree ⊸ Unrestricted r) ⊸ Unrestricted r

-- When using `Unrestricted a` linearly, you have no restriction on the inner `a`!
data Unrestricted a where Unrestricted :: a -> Unrestricted a
```

Because the `Need` array is used linearly, both `leaf` and `branch`
make their argument unavailable. This ensures that we can only write, at
any time, in the left-most empty cell, saving us from inconsistent
trees. The type of `finish`, makes sure that we never construct a
partial tree. Mission accomplished!

The main routine of our service can now be implemented as follows:

```haskell
add1 :: Packed '[Tree] -> Packed '[Tree]
add1 tree = getUnrestricted finished
  where
    -- Allocates the destination array and run the main loop
    finished :: Unrestricted (Packed '[Tree])
    finished = alloc (\need -> finishNeed (mapNeed tree need))

    -- Main loop: given an packed array and a need array with
    -- corresponding types (starting with a tree), adds 1 to the
    -- leaves of the first tree and returns the rest of the
    -- arrays. Notice how `mapNeed` is chained twice in the `Branch`
    -- case.
    mapNeed
      :: Packed (Tree ': r) -> Need (Tree ': r) Tree
      ⊸ (Unrestricted (Packed r), Need r Tree)
    mapNeed trees need = case (caseTree trees) of
      Left subtrees -> mapNeed' (mapNeed subtrees (branch need))
      Right (n, otherTrees) -> (Unrestricted otherTrees, leaf (n+1) need)

    -- Uncurried variant of the main loop
    mapNeed'
      :: (Unrestricted (Packed (Tree ': r)), Need (Tree ': r) Tree)
      ⊸  (Unrestricted (Packed r), Need r Tree)
    mapNeed' (Unrestricted trees, need) = mapNeed h n

    -- The `finish` primitive with an extra unrestricted argument
    finishNeed
      :: (Unrestricted (Packed '[]), Need '[] Tree) ⊸ Unrestricted (Has '[Tree])
    finishNeed (Unrestricted _, need) = finish need
```

In the end, what we have is **a method to communicate and compute over
trees without having to perform any extra copies in Haskell because of
network communication**.

What I like about this API is that it turns a highly error-prone
endeavour, programming directly with serialized representation of data
types, into a rather comfortable situation. The key ingredient is
linear types. I'll leave you with an exercise, if you like
a challenge: implement `pack` and `unpack` analogs of compact region
primitives:

```haskell
unpack :: Packed [a] -> a
pack :: a -> Packed [a]
```

You may want to use a [type checker][prototype].

[paper]: https://github.com/tweag/linear-types/releases/download/v2.0/hlt.pdf
[prototype]: https://github.com/tweag/ghc/tree/linear-types
[blog-post-one]: http://www.tweag.io/posts/2017-03-13-linear-types.html
[blog-post-sockets]: http://www.tweag.io/posts/2017-08-03-linear-typestates.html
[socket-library]: https://www.stackage.org/package/socket
[typestate-wikipedia]: https://en.wikipedia.org/wiki/Typestate_analysis
[gibbon]: http://dx.doi.org/10.4230/LIPIcs.ECOOP.2017.26
[cnf]: https://doi.org/10.1145/2858949.2784735
