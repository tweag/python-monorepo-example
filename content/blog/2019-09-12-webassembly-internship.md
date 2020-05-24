---
redirect_from: [/posts/2019-09-12-webassembly-internship.html]
title: "War Stories of Asterius:  Numerics & Debugging"
shortTitle: "War Stories of Asterius"
author: "Siddharth Bhat"
tags: [internship, haskell, asterius]
description: "I got the opportunity to work on Asterius, a new Haskell to WebAssembly compiler, during my internship at Tweag. My task was to get anything numerics-related stabilized in its compiled code."
---

I got the opportunity to work on [Asterius](https://github.com/tweag/asterius#readme), a new Haskell to WebAssembly compiler, during my internship at Tweag. My task was to get
anything `numerics`-related stabilized in its compiled code. Generally, this meant experimenting with all
the conversion routines between `Float`, `Double`, `Rational`, `Int`,
and the ton of intrinsics that the [Glasgow Haskell Compiler](https://www.haskell.org/ghc/) (GHC) provides for these values.

**TLDR** I helped integrate a part of GHC's test suite with Asterius during my internship at Tweag. Now, it can pass almost all of the
`numerics` test suite. It was a really fun experience—I got to read a bunch
of GHC sources, fight with the garbage collector, and come out knowing a lot
more than I did when I went in. We also ended up making some modest contributions
upstream, to [`binaryen`](https://github.com/WebAssembly/binaryen/pulls?utf8=%E2%9C%93&q=author%3Abollu),
[`tasty`](https://github.com/feuerbach/tasty/pulls?utf8=%E2%9C%93&q=author%3Abollu),
and [GHC](https://gitlab.haskell.org/ghc/ghc/merge_requests?scope=all&utf8=%E2%9C%93&state=all&author_username=bollu).

## First steps: getting up to speed &middot; [PR #114](https://github.com/tweag/asterius/pull/114)

I spent my first week getting familiar with the Asterius codebase so I could fix a bug
with coercion from `Int64/Int32` values to `Int8` values. This task forced me to explore both
the Asterius codebase and the corresponding GHC sources that
were responsible for this bug. I continued making other small, localized
fixes that enabled me to get to know the rest of the codebase.

## Integrating the GHC test suite &middot; [PR #132](https://github.com/tweag/asterius/pull/132)

The next major thing we worked on was integrating a part of the GHC test suite
into Asterius—to enable us to sanity check our runtime against GHC's
battle-hardened test suite. The original GHC test suite is a large Python and
Makefile based project which needs significant work to reuse for Asterius,
so we chose an alternative approach: copy the single-module
`should-run` tests into our source tree, write a custom `tasty` driver to run
each test, compare against expected output, and write results into a CSV report.
The CSV reports are available as CI artifacts, so by diffing against the reports
of previous commits, regressions can be quickly observed.

The [initial](https://circleci.com/gh/tweag/asterius/3724) numbers were
interesting: of all 706 tests integrated so far, if we were optimists, then 380
were passing, which was about half of all tests. And if we were pessimists...

We rolled up our sleeves and got to work: I decided that by the end of the three months, I
wanted all tests in the `numerics/` subfolder stabilized. These tests mostly deal with
everything related to numbers including conversions between `Float`, `Double`, `Rational`, `Int`, `Word`,
the various intrinsics in [`GHC.Prim` for numbers](http://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Prim.html),
and tests for `Integer` support (implemented in Asterius with JavaScript `BigInt`s).

<h2>Stabilizing <code style="
    padding: .5rem .5em;
    font-size: 1;
    font-size: 110%;
    background-color: rgba(40, 18, 66, 0.07);
    border-radius: .3rem;
    "> numerics </code></h2>

The workflow to crush a bug was essentially:

1. Pick a failing test case that looks like a low-hanging fruit
2. Read the relevant GHC sources
3. Pin down what's going wrong by repeatedly shrinking and logging
4. Fix it, ensure the test case turns green
5. Goto step 1

It sounds quite repetitive, but it was anything but. I learned a lot of interesting details
about GHC's internals. For example, some of the ones I remember fondly are:

- [The printing of `Float` values is based on a paper: Printing Floating-Point Numbers Quickly and Accurately](https://www.cs.indiana.edu/~dyb/pubs/FP-Printing-PLDI96.pdf)
- [One can run GHC inside GDB to debug GHC's debug DWARF output](https://github.com/ghc/ghc/blob/535a26c90f458801aeb1e941a3f541200d171e8f/compiler/cmm/Debug.hs#L458)
- [GHC FFI crashes on C's varargs](https://github.com/ghc/ghc/blob/535a26c90f458801aeb1e941a3f541200d171e8f/testsuite/tests/rts/T7160.hs#L6)
- [GHC hangs on compiling large exponents](https://gitlab.haskell.org/ghc/ghc/issues/9059)
- [The implementation of `mfix` for `IO` uses an `MVar`](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/System.IO.html#fixIO)
- [The implementation of `cartesianProduct` from `Data.Set` is derived from an implementation written by Edward Kmett](http://hackage.haskell.org/package/containers-0.6.2.1/docs/src/Data.Set.Internal.html#cartesianProduct),
  with a statement about the optimality of the algorithm: "TODO: try to prove or refute it."

At the end of all of this, I had in total [59 PRs](https://github.com/tweag/asterius/pulls?utf8=%E2%9C%93&q=is%3Apr+author%3Abollu),
[40 of which got merged](https://github.com/tweag/asterius/pulls?utf8=%E2%9C%93&q=is%3Amerged+author%3Abollu) into Asterius.
The rest are either closed experimental branches, or open PRs waiting to be merged.

Our failure rate on GHC test suite has changed as:

- Then: `327/707` failures in total, `36/50` failures in `numerics` (collected from [commit `6290d24`](https://circleci.com/gh/tweag/asterius/5866#artifacts/containers/0))
- Now: `168/707` failures in total, `3/50` failures in `numerics` (collected from [commit `222858b`](https://circleci.com/gh/tweag/asterius/5866#artifacts/containers/0))

Most of the remaining bugs we categorized appear to fall into the following classes:

- A lack of runtime features like multi-threading, [STM](https://wiki.haskell.org/Software_transactional_memory), etc.
- A lack of full Unicode support
- Subtle issues in various parts of the runtime, e.g., the storage manager

## Aside: GC bug hunting

Eventually, we found ourselves running into bugs in our implementation of the
garbage collector in Asterius. These are usually _incredibly_ painful to debug.
Two major sources of the pain are:

- When the GC malfunctions, the heap is already in an inconsistent state,
  however, the final crash site can be quite far away. All we're left with is
  the final error message of the crash. We need to work backwards for a long
  time to locate the crime scene; worse, it's not even always obvious that the
  root cause of the bug lies in GC, judging from a seemingly irrelevant error
  message.
- The WebAssembly platform still lacks a good debugging story. Well-established
  tools like `gdb` aren't available; we don't even have standardized [DWARF](https://gitlab.haskell.org/ghc/ghc/wikis/dwarf)
  sections yet! The plain old logging approach is still the central way of
  hunting bugs, if not only.

Other than the seemingly endless loop of adding more logging logic and rerunning
tests, we do have some debugging-related infrastructure. Since Asterius is a
whole program compiler on the [`Cmm`](https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/cmm-type) level, it's possible to implement aggressive
link-time rewriting passes to add tracing or asserting logic. For example, we
implement the "memory trap" feature when the debugging mode is enabled: it
replaces all `wasm` load/store instructions with calls to our read/write barrier
functions implemented in JavaScript. These functions utilize the information in
the block allocator and check whether an address points to a region which is
already recycled by the copying GC. The memory trap is quite useful in making
GC-related bugs crash as early as possible, and we indeed spotted use-after-free
bugs with its help.

Another approach to check whether a runtime bug is GC-related: not running GC
at all! We implemented a "YOLO mode" in the runtime which disables all
evacuating/scavenging logic, and the only thing GC does is allocating new space
for the nursery and resuming Haskell execution. By running the test suite
with/without the YOLO flag and diffing the reports, we can quickly tell whether
a test failure is likely related to GC.

I also eventually ended up writing helpers to structure the heap and figure out what
arbitrary bit patterns I was looking at—[`bollu/biter`](https://github.com/bollu/biter)
was written during an afternoon of debugging some messed up floating-point representation bug
caused by incorrect bit manipulation.

Similarly, another technique I began using was to create debug logs that would emit Python code. This Python code would then
render the state of the heap at that given point in time: this is a much saner way to see what's
going on than view raw numbers or bit strings. For example:

<center>
<img title="Heap structure during crash: Gray is dead memory, green is memory that should be live" src="../img/posts/2019-07-31-webassembly-internship-heap-render-fib.png" style="max-width: 100%;max-height: 100%;"/>
</center>

The fact that the gray region overlaps with the green region is a Bad Thing
since we were freeing up some memory that is actually still kept alive by the
higher-level heap allocator. Without visualization, this sort of thing is tough to recognize when you're staring at nothing but pointers which look like:

```
9007160601084160, 9007160602132736, 9007160603181312, 9007160604229888, 9007160601084160...
```

So, the root cause of the bug above was some missing synchronization logic
between our two levels of allocators. We have a low-level block allocator which
allocates and frees large blocks of memory, growing the `wasm` linear memory when
needed; above that comes the heap allocator which keeps a pool of blocks to
serve as nurseries of Haskell heap objects. After a round of GC, we have a set
of "live" blocks which make up the "to-space" of copying GC, and we free all
memory outside the set. But we should have _also_ been keeping alive the blocks
in the pools owned by the heap allocator; otherwise a piece of already "freed"
memory can be provided as nurseries without proper initialization. Look at
the Python-generated graph, and the simple yet deadly problem is made obvious.

In general, debugging the GC took lots of patience and code. There are entire branches worth of history
spent debugging, that did not get merged into `master`.

## Wrapping up

I loved working on Asterius at Tweag! I got to contribute
stuff upstream, got my hands dirty with the garbage collector, the low-level `cbits` (C functions for various standard libraries), and
while helping a real-world project! I hope to continue working on this and get the number of
bugs down to zero.

Finally, Tweag's Paris office is a fun place to work! I picked up
(very little) French, a bunch about sampling and Markov chain Monte Carlo (MCMC) techniques, tidbits
of category theory and type theory, some differential geometry,
and enjoyed lunch conversations about topics ranging from physics to history. It was a delightful, rewarding experience—both personally and professionally!
