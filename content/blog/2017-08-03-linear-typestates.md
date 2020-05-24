---
redirect_from: [/posts/2017-08-03-linear-typestates.html]
title: Encode state transitions in types  using linear types
author: Arnaud Spiwack
featured: yes
tags: [haskell, linear-types]
---

At the time of [our first post on linear types][blog-post-one], we
were fresh out of the design phase to extend GHC with linear types. We
had a prototype implementation, but it was just a proof of concept:
there was precious little you could do with it.

A few months down the line, we are now in a very different place. We
have pushed out a new [paper][paper], rewritten top to bottom. This
one is chock-full of example applications of linear types. Today and
in my next post in this series on linear types, I'd like to touch on
two nifty uses of linear types from the paper.

But before I get started - I want to point out some recent progress on
the `linear-types` branch in GHC. This [branch][prototype] is now
a usable playground to experiment with linear types. Most of the basic
features are there, but do expect some very rough edges and keep in
mind that error messages still need to be improved. We provide
a ready-made Docker image, so you don't have to compile GHC yourself
just to play with linear types. It's all in the `README`: check it
out!

# I/O states, in your types

Say you want to communicate across a network. You use
the [socket][socket-library] library. You open the documentation and find that to
use a TCP socket, on a server, you first need to `bind` the socket to an
address. At this point the socket isn't doing anything: you need to
`listen` for incoming traffic. Now the socket receives messages from
the network, specifically connection requests, which you must
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

These types are really not that helpful. In Haskell, we like our types to
tell us what we can do with a value. But when I have a `Socket`, I can
maybe `bind` it or `listen` to it, but certainly not both.

What we really need is that the type of a socket be indexed by the
kind of things we can do with it. Something along the lines of:

```haskell
data State = Unbound | Bound | Listening | …
data Socket (s :: State)

bind :: Socket Unbound -> IO ()
listen :: Socket Bound -> IO ()
…
```

Good! Now, what is the type of `s` in `bind s`? It must be `Socket Unbound` so that I can apply `bind`. Wait! When the `bind` call
returns, `s` must not be `Unbound` anymore. It must be `Bound`. So…
the type of `s` seems to change over time. This is the idea
of [_typestates_][typestate-wikipedia].

To implement a typestate for socket, maybe I could simply return a
socket with its type changed like so:

```haskell
bind :: Socket Unbound -> IO (Socket Bound)

do { s' <- bind s; … }
```

I can then use `s'` as evidence that the socket has been
bound. But I also have the old `s` still hanging about. And `s` claims to
be unbound: we can go back in time. One has to be careful not to use
`s` ever again to avoid ill effects: types have failed us once again.

What we need is the ability to _consume_ the old `s`, to make it
inaccessible. Which, coincidentally, is exactly what linear types
enable. We just need to make `IO` a little bit more general, so that
it can return both linear and non-linear values:

```haskell
data IOL p a

-- An old friend, redefined.
type IO = IOL ω

-- `a ->_1 b = a ⊸ b` and `a ->_ω b = a -> b`
return :: a ->_p IOL p a
(>>=) :: IO p a ⊸ (a ->_p IO q b) ⊸ IO q b
```

and we can now have sockets with typestates:

```haskell
socket :: IOL 1 (Socket Unbound)
bind   :: Socket Unbound ⊸ SocketAddress -> IOL 1 (Socket Bound)
listen :: Socket Bound ⊸ IOL 1 (Socket Listening)
…
```

What we've done here is precisely and safely captured the state of the
socket at the type-level. The type tells you exactly what to do next
with a socket. And GHC won't let you reuse old states by accident.
This is a prototypical example, demonstrating how linear types help us
to be more precise about the use of resources, so that the type
checker assists us in avoiding faulty resource use.

In the next post in the series, we'll take a look at how linear types
can pave the way towards safe zero-copy packed data exchange across
clusters.

[paper]: https://github.com/tweag/linear-types/releases/download/v2.0/hlt.pdf
[prototype]: https://github.com/tweag/ghc/tree/linear-types
[blog-post-one]: http://www.tweag.io/posts/2017-03-13-linear-types.html
[socket-library]: https://www.stackage.org/package/socket
[typestate-wikipedia]: https://en.wikipedia.org/wiki/Typestate_analysis
[gibbon]: http://dx.doi.org/10.4230/LIPIcs.ECOOP.2017.26
[cnf]: https://doi.org/10.1145/2858949.2784735
