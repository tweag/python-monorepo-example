---
title: "Linear types are merged in GHC"
shortTitle: "Linear types are merged in GHC"
author: Arnaud Spiwack
tags: [haskell, linear-types]
description: "Looking back at the journey which brought us there, and forward to what still lies ahead."
---

It's been almost 4 years. And what 4 years they were! We learned so
much. I am exhausted, but mostly, I'm happy, I'm thankful, and I'm
hopeful.

## How we got there

### Inception

The journey towards linear types in GHC started in the fall
of 2016. At this point in time, Jean-Philippe Bernardy and I had been
working on distributed storage R&D for a year or so. Prior to that we
had both fairly freshly published research into linear logic. And, we
started seeing opportunities for linear types pretty much
everywhere: for memory management, to generalise protocol types, to
manage buffers… It had become a recurring joke in this project: “You
know what would help with this? Linear types!”.

I guess that after repeating a joke enough, you start wondering
whether maybe it shouldn't be a joke. Maybe linear types would really
be helpful. Why not after all? And so we decided to [shave that
yak][yak-shaving].

I'll be honest though: I didn't think it was possible. I really
thought that to add linear types to a language, you would need to
redesign it from the ground up. That is, really create a new
language. But, fortunately, Jean-Philippe knew better. He came up with
a convincing design to add linear types to GHC. I was on board.

In the fall of 2016, together with my colleague Mathieu Boespflug, we
brainstormed for a couple of months, refined our design. In the
winter, we were joined by Simon Peyton Jones and Ryan Newton, in the
effort of turning our idea, still quite rough, into a design solid
enough to warrant an academic publication. By the fall of 2017, our
[article][linear-article] was accepted for publication at [POPL
2018][popl18] and accompanied by a prototype implementation.

### Proposal

The prototype only modified the front end of GHC and didn't touch Core
yet. Yet we felt confident enough to write a [proposal for extending
GHC with linear types][linear-proposal-round1] in November 2017. Over
the course of the three following weeks, this proposal attracted 200
comments. We eventually closed the proposal PR for heavy
revision. Because 200 comments sounded so massive at the time, we
elected to reopen the proposal [as a separate PR][linear-proposal],
with the memorable number 111, in February 2018. This second iteration
of the proposal was accepted [in October
2018][linear-acceptance]. Altogether, the proposal has gathered almost
600 comments, comparable only to the [record dot-syntax
proposal][record-dot-proposal]. What a ride!

While the proposal was being hotly debated, we carried on with the
implementation. An implementation doesn't lie: we started it to find out
what we might have missed in our design. And, indeed, we
found some holes in the design. Matthew Pickering joined us from April
to July 2018 as part of an internship.
He undertook the frankly thankless task of making the
first implementation of linear types inside Core. Just making GHC
compile again took about 2 months. In the process, Matthew [found
bugs][eta-expansion-bug] in our backward compatibility story. And so
we updated our design, and the proposal.

### Merge request

In October 2018, my colleague Krzysztof Gogolewski joined the
project. And, to give an idea of how unglamorous working on linear
types could be at times, proceeded to merge the current state of
linear types with GHC's master. A task which took upwards of two weeks
of full-time work. Over the course of the next year, Krzysztof
relentlessly squashed the remaining bugs and infelicities to the
proposal.

Why a year? Well, it turns out that one innocent looking design
choice: [𝜂-expanding data constructors][eta-expansion], ended up being
a massive undertaking. We have had to fight _bona fide_ bugs in GHC,
or simply implicit, unconscious, assumptions in the code which
required quite a bit of work to unravel.

It was the summer of 2019, and we were ready, or so we thought. We
arranged for a focused review session in the Fall, in Cambridge, with
Simon Peyton Jones and Richard Eisenberg. And glitches were found. In
the course of this implementation, a lot of research had happened: it
turns out that teaching linear types to Core is not as simple as just
implementing linear logic, or even the calculus from our
[article][linear-article], but this is a story for another time. We had
to do more research on the spot.

We addressed the glitches by January 2020. The rest of the year has
been focused on hunting performance regressions.

Let me be sure not to forget Richard Eisenberg's role in all
this. Richard shepherded the proposal, reviewed the entire 3000 line
patch (several times!), shepherded the merge, and helped out crucially
with performance. No mean task to be sure!

Special thanks go to Andreas Klebinger, who benchmarked the linear
types branch several times to help pinpoint the performance
regression, and to Ben Gamari, who ran and analysed an [8h
benchmark][head-hackage-benchmark] on the linear types patch.

And here we are. 4 years, nearly 30 bugs found in GHC (most we fixed
ourselves)[^upstream], and over 200 internal pull requests later. This
is very much a collective work, and I'm hugely thankful to everyone
involved.

## In GHC 8.12

### Linear types

There will be linear types in GHC 8.12. But don't expect a finished
product. This is our very first iteration, an [MVP][mvp-wiki] as it
were. This is as minimal a set of features we think can be useful for
anybody. But it's still very much aimed at early adopters and eager
tinkerers.

Turn on `-XLinearTypes`, and the first thing you will notice,
probably, is that the error messages are typically unhelpful: you will
get typing errors saying that you promised to use a variable linearly,
but didn't. How hasn't it been used linearly? Well, it's for you to
puzzle out. And while what went wrong is sometimes egregiously
obvious, it can often be tricky to figure the mistake out.

Plenty of things are missing from the proposal too. In no particular order:

- There is no infix syntax for multiplicity polymorphism.
  You can use the prefix form `FUN p a b` for an arrow with
  multiplicity `p`. However:
- Multiplicity polymorphism is mostly unsupported, and you can expect
  it to misbehave most of the time.
- Record fields are always linear.
- `let` and `where` bindings are never linear.
- The scrutinee of a `case` expression is always considered as being
  used non-linearly.

  A trick, found by my colleague Divesh Otwani, which you can use to work around
  this is to replace

  ```haskell
  case x of {…}
  ```

  with

  ```haskell
  x & \case {…}
  ```

  For a linear version of `(&)`

- Inference of linear types is very limited.

There are other missing bits; they are all documented in the manual.

And, of course, there will be bugs. Probably many. We will be happy to
receive your bug reports on the [GHC bug tracker][ghc-bug-tracker].

### Linear base

When GHC 8.12 is released, we will release the first version of
[linear-base], a toolkit to get you started with linear types. It
contains linearised versions of many functions from the Haskell base
library, [two kinds of functors][data-vs-control], [mutable data
structures with pure
API](https://github.com/tweag/linear-base/blob/a7fab85c2bd5ee12fde50adb48e2ce1f05db872e/src/Data/HashMap/Linear.hs),
a [monad for safe management of
resources](https://github.com/tweag/linear-base/blob/a7fab85c2bd5ee12fde50adb48e2ce1f05db872e/src/System/IO/Resource.hs),
[linear
optics](https://github.com/tweag/linear-base/blob/a7fab85c2bd5ee12fde50adb48e2ce1f05db872e/src/Control/Optics/Linear/Internal.hs),
an API for [allocation-free array
pipelines](https://github.com/tweag/linear-base/blob/a7fab85c2bd5ee12fde50adb48e2ce1f05db872e/src/Data/Array/Polarized.hs),
…

The linear-base library is currently being developed by my colleague Divesh
Otwani, and previously by Bhavik Mehta, during his summer internship
at Tweag.

## What's to come

Evidently, we still have a lot of work ahead of us to make linear
types in GHC a smooth experience. Completing the design of the
proposal, and, probably, going beyond the proposal.

But what's next, above all, is you. Until now, linear typing has been
mostly a subject for experts, who did propose many applications. But
what I, personally, find most exciting about this enterprise is that
adding linear types to a mainstream compiler makes them available to
many more people. And as clever as the experts are, they are no match
against many times more programmers armed with a type checker. Not
even close.

So by all means, play, tinker, experiment, come up with ideas, build
libraries. This will all be extremely fascinating. We provide you with
linear-base to get you started, but maybe it's not the right thing
for you, and then you can build your own. There is a huge design space to
explore. It's all ahead of us. And I, for one, can't wait to see what
the Haskell community comes up with.

See you all in 8.12!

[^upstream]: [#15840](https://gitlab.haskell.org/ghc/ghc/issues/15840), [#16074](https://gitlab.haskell.org/ghc/ghc/issues/16074), [#16208](https://gitlab.haskell.org/ghc/ghc/issues/16208), [#16221](https://gitlab.haskell.org/ghc/ghc/issues/16221), [#16254](https://gitlab.haskell.org/ghc/ghc/issues/16254), [#15941](https://gitlab.haskell.org/ghc/ghc/issues/15941), [#16468](https://gitlab.haskell.org/ghc/ghc/issues/16468), [#16565](https://gitlab.haskell.org/ghc/ghc/issues/16565), [#17201](https://gitlab.haskell.org/ghc/ghc/issues/17201), [#17536](https://gitlab.haskell.org/ghc/ghc/issues/17536), [#17812](https://gitlab.haskell.org/ghc/ghc/issues/17812), [#17817](https://gitlab.haskell.org/ghc/ghc/issues/17817), [#16288](https://gitlab.haskell.org/ghc/ghc/issues/16288), [#16296](https://gitlab.haskell.org/ghc/ghc/issues/16296), [#16456](https://gitlab.haskell.org/ghc/ghc/issues/16456), [#16781](https://gitlab.haskell.org/ghc/ghc/issues/16781), [#17213](https://gitlab.haskell.org/ghc/ghc/issues/17213), [#15771](https://gitlab.haskell.org/ghc/ghc/issues/15771), [#16592](https://gitlab.haskell.org/ghc/ghc/issues/16592), [#17221](https://gitlab.haskell.org/ghc/ghc/issues/17221), [#17530](https://gitlab.haskell.org/ghc/ghc/issues/17530), [#18302](https://gitlab.haskell.org/ghc/ghc/issues/18302), [haddock#1048](https://github.com/haskell/haddock/issues/1048), [!739](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/739), [!519](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/519), [!2859](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2859), [!2848](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2848), [!3301](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3301).

[data-vs-control]: https://www.tweag.io/blog/2020-01-16-data-vs-control/
[eta-expansion-bug]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#%CE%B7-expansion
[eta-expansion]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0111-linear-types.rst#linear-constructors-and-backward-compatibility
[ghc-bug-tracker]: https://gitlab.haskell.org/ghc/ghc/issues
[head-hackage-benchmark]: https://mail.haskell.org/pipermail/ghc-devs/2020-June/018972.html
[linear-acceptance]: https://github.com/ghc-proposals/ghc-proposals/pull/111#issuecomment-431944078
[linear-article]: https://dl.acm.org/doi/abs/10.1145/3158093
[linear-base]: https://github.com/tweag/linear-base/
[linear-proposal-round1]: https://github.com/ghc-proposals/ghc-proposals/pull/91
[linear-proposal]: https://github.com/ghc-proposals/ghc-proposals/pull/111
[mvp-wiki]: https://en.wikipedia.org/wiki/Minimum_viable_product
[popl18]: https://popl18.sigplan.org/
[record-dot-proposal]: https://github.com/ghc-proposals/ghc-proposals/pull/282
[yak-shaving]: https://seths.blog/2005/03/dont_shave_that/
