---
title: Linear types<br>make performance more predictable
author: Jean-Philippe Bernardy and Arnaud Spiwack
featured: yes
tags: haskell
---

We're extending GHC with linear types.
<!--more-->

Ever since Jean-Yves Girard discovered [linear logic][linear-logic] in
1986, researchers around the world have been going "wow! resource
tracking, this must be useful for programming languages". After all,
any real computation on a real machine *takes* resources (memory
pages, disk blocks, interrupts, buffers etc) that then aren't
available anymore unless restored somehow. But despite this match
apparently made in heaven, research on this subject has remained
largely theoretical. It was becoming easy to just give up and assume
this nut was simply too tough to crack. We ourselves have been there,
but we recovered: we're having a go at extending GHC with linear
types.

Great! But why is this useful in a high-level programming language
like Haskell? You'll find the long of it in the [paper][paper] we just
submitted to ICFP'17 with Mathieu Boespflug, Ryan Newton and Simon
Peyton Jones. In this blog post, we'll briefly discuss (more in the
paper) how we're trying to achieve more predictable performance at
a smaller memory footprint in Haskell, and give you a preview of what
you might expect in your favourite compiler in the not-so-distant
future.

# A bit of history #

Once upon a time, Jean-Yves Girard was playing with ways to describe
the semantics of a λ-calculus with types, when he realised that from
a semantic perspective there was nothing essential about the arrow
type (the type for functions): it could be described in terms of two
simpler constructions. He followed this thread through and came up
with a strange beast he called _linear logic_.

## Two of everything ##

Linear logic had two of everything: two ways of building conjunctions
and disjunctions, two notions of truth, falsehood and two ways to
negate. It's a strange system, but perhaps not moreso than the zoo of
cute names and symbols Girard conferred to every construct. For the
purposes of this post, we'll only need one new symbol from this zoo:
$⊸$, which reads _lollipop_ (also called _linear arrow_, or _lolly_
for close friends).

If we transpose linear logic to describing the behaviour of programs
(by which point we talk about _linear types_), the linear arrow says
this: a function that has type $A⊸B$ is a function that has an $A$
available to compute with, _but it can only use it once_. Not twice,
not zero times: just once. It turns out that this property, which has
come to be known as _linearity_, is very useful for compilers of
functional programs.

## Typing resources ##

Programming language researchers quickly took notice. It was not long
before [Yves Lafont][lafont] proposed a language with safe memory
management yet without needing a garbage collector, thanks to linear
types. Philip Wadler [piled on][wadler-linear-types] a few years later
with a system also supporting state update while retaining the
properties of a pure language. Recently, researchers have even pushed
linear logic towards making it possible to reason about pointer
aliasing (the absence or presence of aliasing matters a lot in the
optimization and the correctness of C programs).

But despite all these early promises (and many more since then),
linear types didn't catch on in the mainstream. Mind you, there have
been workarounds. It turns out linear types are also useful to model
effects, but Haskell preferred monads for that purpose
instead. [Clean](http://clean.cs.ru.nl/) wanted safe mutable state,
but eschewed monads, using _uniqueness types_ instead. More
recently, [Rust](https://www.rust-lang.org/) rose to popularity with
a system of ownership which is not unlike Clean's uniqueness types.
Both are complex systems that permeate the entire language and
ecosystem in ways that make the learning curve pretty steep.

## Linear types as an extension ##

What if you could enjoy all your favourite goodies from your favourite
programming language, and yet be able to leverage linear types for
precise resource management exactly where it matters (as Lafont did by
avoiding garbage collection)? The result won't be as good as Rust for
what Rust does: it's a different trade-off where we assume that such
precision is only needed in small key areas of a program that
otherwise freely uses functional programming abstractions as we know
them today.

This is what we are proposing: a simple, unintrusive design that can
be grafted to your favourite functional language at no cost for the
programmer. We're working on GHC, but this would work just as well in
your favourite ML flavour.

# So why are we doing this? #

Among our many projects, we are working together
with [Seagate][seagate] and a number of consortium partners on
the [SAGE][sage] platform, an EU funded R&D project exploring how to
store massive amounts of data (in excess
of [one exabyte](https://en.wikipedia.org/wiki/Exabyte)) and query
this data efficiently. We use Haskell as a prototyping language to
fail fast when we're heading towards a dead end, backtrack and explore
a different direction at low cost, and refactor our existing code
easily when we commit to a different direction.

On this and other systems level projects, maintaining predictable
latencies is key. Pusher [recently documented][pusher] how this
matters to them too, to the point where they've
been [looking elsewhere][pusher-go] for good latencies. Our use cases
share the same characteristics. We decided to solve the problem by
asking less of the GC, while extending Haskell to make working outside
of the GC just as memory safe. You will find much more details in
the [paper][paper], but in summary linear types help in two ways:

- We can use linear types to manually, yet safely, manage data with
  `malloc`: because linearity forces the programmer using a value at
  least once, we can ensure that the programmer eventually calls
  `free`. And because it forces to use a value at most once, we can
  make sure that `free`-d data is never used (no use-after-free or
  double-free bugs). Anything that we `malloc` explicitly doesn't end
  up on the GC heap, so doesn't participate in increasing GC pause
  times.
- Linear types can make fusion *predictable* and *guaranteed*. Fusion
  is crucial to writing programs that are *both* modular and
  high-performance. But a common criticism, one that we've seen born
  out in practice, is that it's often hard to know for sure whether
  the compiler seized the opportunity to fuse intermediate data
  structures to reduce allocations, or not. This is still future work,
  but we're excited about the possibilities: since fusion leans
  heavily on inlining, and since linear functions are always safe to
  inline without duplicating work because they only use their argument
  once, it should be possible with a few extra tricks to get
  guaranteed fusion.

But the story gets better still. Linear types aren't only useful for
performance, they can also be key to correctness. SAGE is a complex
project with many communicating components. Linear types allow us to
model these interactions at the type level, to statically rule out
bugs where some component doesn't respect the protocol that was agreed
upon with others. We don't talk about that much in the paper, but
we'll touch on that here.

# What it will look like #

Let's start slow, with the `fst` function projecting the first
component from a pair:

```haskell
fst :: (a,b) -> a
fst (x,_) = x
```

So far, so standard. Now here is a new thing you _cannot_ do:

```haskell
fst :: (a,b) ⊸ a
fst (x,_) = x
```

This $⊸$ is the linear arrow: the type of functions which must use
their arguments exactly once. The first projection is _not_ linear,
since a part of the pair is silently dropped. So the type checker will
reject this program. It will also reject the diagonal, which uses its
argument twice:

```haskell
dup :: a ⊸ (a,a)
dup x = (x,x)
```

On the surface, this is almost the entire story: *the linear arrow
allows you to reject more programs*, of which `fst` and `dup` are
prototypical examples.

With that in mind, there's a lot that linear types can be used for.
One example is strongly typed protocols that check applications in
a distributed system interact as expected. We'll need a little bit of
kit first:

```haskell
data S a
data R a

newCaps :: (R a ⊸ S a ⊸ IO ()) ⊸ IO ()
send :: S a ⊸ a ⊸ IO ()
receive :: R a ⊸ (a ⊸ IO ()) ⊸ IO ()
```

In this API, `S` is a _send capability_: it gives the ability to send
a message. `R` is a _receive capability_. A pair of corresponding
capabilities is allocated by `newCaps`. The important thing to notice
is that since both capabilities are linear, both must be _consumed_
exactly once by a `send` and a `receive` respectively. Conversely, you
can't send/receive without having a corresponding send/receive
capability on hand.

As a first example, let's consider a protocol where a server expects
two integers and returns the sum to the client. A simple function,
except happening across a wire. This protocol is fully captured by the
type `(Int, Int, Int ⊸ IO ())`, which reads: receive two `Int`'s, then
send an `Int`. Using `⊸ IO ()` switches between "send" and "receive".

To implement a protocol `P`, one has to write an implementation (a
function) of type `P ⊸ IO ()`. The other side of the wire (the client)
must implement the dual protocol `P ⊸ IO ()`, with a function of type
`(P ⊸ IO ()) ⊸ IO ()`:

```haskell
type P = (Int, Int, Int ⊸ IO ())

server :: P ⊸ IO ()
server (n, p, k) = k (n + p)

client :: (P ⊸ IO ()) ⊸ IO ()
client sendToSrvr = newCaps $ \r s -> do
  sendToSrvr (42, 57, send s)
  receive r (\n -> print n)
```

We can see how typing works: `client` must create a function `Int ⊸ IO
()` for the server to send its result to. So it creates an `S`/`R`
pair, into which the server will be able to send exactly one result.
The `r` capability must also be consumed exactly once. Therefore, the
client must call `receive` and do something with the result (in this
case, it boringly prints it on a console).

Of course, one can replace all the linear arrows with regular arrows
`->` and the program stays well-typed. But *the value of linear types
lies in what they do not allow*: `client` cannot forget to call
`receive`, resulting in the server blocking on `send`; similarly, the
server is not allowed to return twice to `k` (_e.g._ `k (n+p) >>
k (n*p)` is not well-typed). This prevents all sort of bad
behaviours. It is not a novel idea; see the [paper][paper]'s "related work"
section if you're interested in the references.

To run the server and the client, we must assume an initial `S`/`R`
pair `s0`/`r0` (known only to the runner, for safety):

```haskell
runServer :: IO ()
runServer = receive r0 server

runClient :: IO ()
runClient = client (send s0)
```

We can make more complex examples by alternating the `⊸ IO ()`
construction to sequence the steps of the protocol. For instance, the
following asks first for a number, then the word "apple" singular or
pluralized, depending on the number, and returns a sentence:

```haskell
data Number = Singular | Plural
type P = (Int, (Number, (String, String ⊸ IO ()) ⊸ IO ()) ⊸ IO ())

server :: P ⊸ IO ()
server (n, k) =
    newCaps $ \r s -> do
    k (num, send s)
    receive r $ \(apples, k') -> do
      k' ("I have " ++ show n ++ apples)
  where
    num = if n == 1 then Singular else Plural

client :: (P ⊸ IO ()) ⊸ IO ()
client k =
    newCaps $ \r s -> do
      k (42,send s)
      receive r $ \(num,k') ->
        newCaps $ \r' s' -> do
          let apples
                | Singular <- num = "apple"
                | Plural <- num = "apples"
          k' (apples, send s')
          receive r' $ \sentence ->
            print sentence
```

Running the server and client will result in the client printing the
deliciously healthy sentence "I have 42 apples". Again, the value is
in what is rejected by the compiler, such as listening to `r` a second
time in the client rather than to `r'`.

An important takeaway is how haskelly this all looks: just replace
`->` by `⊸` and linearity kicks in. Usual datatypes, including tuples,
take a linear meaning in a linear context. The technical details of
how this is achieved are exposed in the [article][paper]. Edsko de
Vries wrote [a blog post][edsko-blog] where he compares the trade-offs
of related type systems; he comes in favour of a system where types
are segregated into linear types and unrestricted types, but our
position is that such a system, perhaps more flexible, would be more
complex to implement, especially if we want good sharing of code
between linear and non-linear contexts.

One thing that the article does not have, however, is a good
description of how our prototype implementation (and type inference)
works.

# Did you say prototype? #

Yes! There is a [branch of GHC][prototype] where we are implementing
the above proposal. At the time of writing the prototype is still
a bit crude: it handles the λ-calculus fragment properly, but `case`
and `data` do not have full support yet. We believe we have the hard
part behind us though: modifying the arrow type so that arrows carry
an annotation discriminating whether they are regular arrows (`->`) or
linear arrows (`⊸`).

Indeed, GHC uses and abuses the arrow type all over the type inference
mechanism. So making a change there required changes across many
files. As it turns out, however, the changes are rather systematic and
innocuous. The patch is currently under 1000 lines long. We are
targeting a merge by the time of the 8.4 release of GHC.

In a future post, we'll delve into what the paper doesn't cover, to
show how inferring linear types works.

Stay tuned!

[paper]: https://github.com/tweag/linear-types/releases/download/v1.0/hlt.pdf
[prototype]: https://github.com/tweag/ghc/tree/linear-types
[linear-logic]: http://dx.doi.org/10.1016/0304-3975(87)90045-4
[wadler-linear-types]: http://homepages.inf.ed.ac.uk/wadler/papers/linear/linear.ps.gz
[lafont]: http://dx.doi.org/10.1016/0304-3975(88)90100-4
[separation-logic]: https://en.wikipedia.org/wiki/Separation_logic
[mezzo]: http://dx.doi.org/10.1145/2837022
[seagate]: http://seagate.com
[sage]: http://sagestorage.eu/
[pusher]: https://blog.pusher.com/latency-working-set-ghc-gc-pick-two/
[pusher-go]: https://making.pusher.com/golangs-real-time-gc-in-theory-and-practice/
[honda-session]: http://dx.doi.org/10.1007/3-540-57208-2_35
[caires-pfenning]: http://dx.doi/org/10.1007/978-3-642-15375-4_16
[wadler-session]: https://doi.org/10.1145/2398856.2364568
[edsko-blog]: http://edsko.net/2017/01/08/linearity-in-haskell/
