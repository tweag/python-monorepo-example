---
title: Typestates from linear types
author: Arnaud Spiwack
featured: yes
---

Last time [we wrote][blog-post-one] about linear types, our work
towards extending GHC with linear types was in its infancy. We were
fresh out of the design phase, we had an prototype implementation but
it was just a proof of concept: there was precious little you could do
with it.

A few months down the line, and we are in a very different place. We
have revamped our [paper][paper], and I trust you will find it much
more fun to read. Our [branch][prototype] of GHC is now working
fine. It's not anything close to bug-free, but it's a nice little
playground. There is a Docker image for easy use with Stack, and
everything. It is all in the `README`: check it out.

Today, I would like to work you through some of the new exciting
examples that we have worked into the paper.
<!--more-->

# IO protocols #

Say you want to speak something across a network. So, you use
the [socket][socket-library] library. You open the doc and see that to
use a TCP socket, on a server, you first need to `bind` the socket to an
address. At this point the socket isn't doing anything: you need to
`listen` for incoming traffic. Now the socket receives messages from
the network, specifically connection requests, that you must
`accept`. An `accept` call returns a new socket which can `receive` a
TCP stream.

That's a bit of a headache! And you've got no safety net: the
(simplified) type of `bind` is:
```haskell
bind ::  Socket -> SocketAddress -> IO ()
```
The type of `listen` is:
```haskell
listen :: Socket -> IO ()
```

It's really not all that helpful. In Haskell, we like our types to
tell us what we can do with a value. But when I have a `Socket`, I can
maybe `bind` it or `listen` with it, but certainly not both.

What we really need is that the type of a socket be indexed by the
kind of things I can do with it. Something along the lines of:
```haskell
data State = Unbound | Bound | Listening | Connected
data Socket (s :: State)

bind :: Socket Unbound -> IO ()
listen :: Socket Bound -> IO ()
…
```

Good! Now, what is the type of `s` in `bind s`? It must be `Socket
Unbound` so that I can apply `bind`. Wait! When the `bind` call
returns, `s` must not be `Unbound` anymore. It must be `Bound`. So…
the type of `s` seems to change over time. This is the idea
of [_typestate_][typestate-wikipedia].

To implement a typestate for socket, maybe I could simply return a
socket with its type changed like so:
```haskell
bind :: Socket Unbound -> IO (Socket Bound)

do { s' <- bind s; … }
```
I can then use `s'` as evidence that the socket has been
bound. But I also still have the old `s` hanging about which claims to
be unbound: we can go back in time. One has to be careful not to use
`s` ever again to avoid ill effects: types have failed us once again.

What we need is the ability to _consume_ the old `s`, to make it
inaccessible. Which, coincidentally, is exactly what linear types make
possible. We just need to make `IO` a little bit more general so that
it can return both linear and non-linear values:
```haskell
data IOL p a
type IO = IOL ω

-- `a ->_1 b = a ⊸ b` and `a ->_ω b = a -> b`
return :: a ->_p IOL p a
(>>=) :: IO p a ⊸ (a ->_p IO q b) ⊸ IO q b
```
and we can not have sockets with typestates:
```haskell
socket ::  IOL 1 (Socket Unbound)
bind ::  Socket Unbound ⊸ SocketAddress -> IOL 1 (Socket Bound)
listen :: Socket Bound ⊸ IOL 1 (Socket Listening)
…
```

# Packed representation of data types #

And now, for something completely different: data type representation.

Let us consider a simple tree type:
```haskell
data Tree = Branch Tree Tree | Leaf Int
```
We are used to see this represented as a pointer structure in the
heap. But what if we actually represented as an array, in preorder traversal. So that
`Branch (Branch (Leaf 1) (Leaf 2)) (Branch (Leaf 3) (Branch (Leaf 4)
(Leaf 5)))` is represented as:
```haskell
+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+
| Branch | Branch |  Leaf  |   1    |  Leaf  |   2    | Branch |  Leaf  |   3    | Branch |  Leaf  |   4    |  Leaf  |   5    |
+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+--------+
```
Why would anybody want to do such a thing? There are several
reasons. One is that there are no pointers _at all_ in this
representation. This means that the garbage collector never needs to
traverse this structure. A second reason is that it is a very cache
friendly representation. Yet another reason is that you may be sending
or receiving this tree over the network, and working directly on an
array-of-byte representation saves the cost of serialising and
deserialising data, which is a common bottleneck of distributed
applications. If you have heard of [compact normal forms][cnf], it is
pretty much the same idea. If you want to know more Ryan Newton, one
of my co-author on the linear type paper, has co-authored an
entire [article on such representations][gibbon].

To program with such a data structure I need a pattern-matching
operation:
```haskell
type Packed (l :: [*])

caseTree :: Packed (Tree ': r) -> Either (Packed (Tree ': Tree ': r)) (Int, Packed r)
```
The type is a bit weird, indexed with a list of types: this is because
in the `Branch` case, we can't return two sub-packed-trees. Indeed, we
don't know the size of the left sub-tree, so we need to traverse it
before we can get to the right sub-tree. Instead we just return a
packed representation of _two trees_.

But how do we create a new `Packed Tree`? In the compact normal form
paper, we are given two functions:
```haskell
unpack :: Packed [a] -> a
pack :: a -> Packed [a]
```
However, relying on them to build new `Packed Tree`-s is extremely
wasteful: one of the reasons for using such a representation was to avoid
(de)serialisation. Instead, we want to create new `Packed Tree` by
explicitly writing into a buffer.

Writing into a buffer calls for the `ST` monad. But there is the
business of the index. Just like with the socket above, we could go back in time to
disastrous effects. So we reach for linear types again. An added bonus
is that linearly-used write-only buffers are actually pure, so no need
for a monad at all.

The type of write-buffers is:
```haskell
type Need (l :: [*]) (t :: *)
```
Where `Need '[Tree, Tree, Int] Tree` should be understood as "write two
`Tree`-s and an `Int` and you will get a `Tree`". As illustrated
by the `finish` function:
```haskell
finish :: Need '[] t ⊸ t
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

What I find amazing about this API is that it changes an mind-bendingly
hard problem: programming directly with serialised forms of data
types; into a rather comfortable situation. Thanks to linear types!
I'll leave the bravest reader with an exercise: implement `pack` and
`unpack` in terms of the other primitives. You may want to use
a [type checker][prototype].

[paper]: https://github.com/tweag/linear-types/releases/download/v2.0/hlt.pdf
[prototype]: https://github.com/tweag/ghc/tree/linear-types
[blog-post-one]: http://www.tweag.io/posts/2017-03-13-linear-types.html
[socket-library]: https://www.stackage.org/package/socket
[typestate-wikipedia]: https://en.wikipedia.org/wiki/Typestate_analysis
[gibbon]: http://dx.doi.org/10.4230/LIPIcs.ECOOP.2017.26
[cnf]: https://doi.org/10.1145/2858949.2784735

<!--  LocalWords:  GHC README TCP typestates typestate deserialising
 -->
<!--  LocalWords:  preorder deserialisation
 -->
