---
title: Profiling non-CPU time in Haskell
author: Facundo Domínguez
tags: [haskell, liquidhaskell, profiling]
description: "Limitations of the GHC profiler and announcement of the timestats library"
---

The [GHC profiler][ghc-profiler] is today one of the most advanced
profiling tools for analyzing how Haskell programs use CPU time and
memory. It produces information on what parts of a
program could use optimizations to speed it up.

Unfortunately, the very definition of the GHC profiler makes it of
[limited use][document-limitations] when estimating time on two
classes of computations: firstly, those that need to do blocking IO;
and secondly, some computations that invoke functions written in other
programming languages.
In these cases, one could just turn to a tool like
[ghc-events-analyze][ghc-events-analyze], however, it is not available
to plugins loaded in GHC 8, such as Liquid Haskell.

In this post I'm discussing a bit the limitations of the GHC
profiler, and I'm announcing [timestats][timestats], a simple
profiling library that you might want to grab when everything else
fails.

## The limitations of the GHC profiler

Consider the following program from a [GHC issue][document-limitations]:

```Haskell
main = g
g = getContents >>= print . length
```

If a person interacts with this program in a terminal, the program
is going to spend most of the time blocking while waiting for input.
The GHC profiler, however, won't account any time to function `g`.
This is because the profiler doesn't estimate wall-clock time but
CPU time and, so far, only for the Haskell portion of a program. As
function `g` uses little CPU, `g` gets little time attributed to it
in the reports of the profiler.

The example program is contrived but the implications are far reaching.
It is not uncommon for programs to block when doing IO to communicate
with a database, a remote server, a file system, or another local process.
The larger the program is, the more likely many of these blocking IO
calls are occurring during its lifetime.

The other class of problematic computations can be exemplified with:

```Haskell
{-# LANGUAGE CApiFFI #-}
foreign import capi unsafe "stdio.h getchar" getchar :: IO Int
main = g
g = getchar >>= print
```

Much like in the previous case, the program will spend most of the
time waiting for input. The important difference, though, is that the
program is calling into C first via the
[Foreign Function Interface][ffi], and only then blocking for input.
According to our assumptions this far, this shouldn't make a
difference in the results. The program uses little CPU time, so `g`
would not show any time attributed to it. This is not what happens
though!

Haskell programs can call functions written in other languages
using the so called foreign functions. Foreign functions come
in two flavors: safe and unsafe. The meaning of the flavors
doesn't affect the discussion here, but it is necessary to note
that the foreign function in our example is unsafe.

It turns out that the GHC profiler estimates CPU time for Haskell
computations. But if a computation calls an unsafe foreign function,
it will switch to estimating the wall-clock time of the foreign
call (!) and attribute it to the calling computation. The main
consequence of this behavior is that interpreting the results of
profiling requires knowledge of the call graph to learn which of
CPU or clock time is being measured at different places. The bigger
the application, the harder it becomes to interpret the results.

## timestats

`timestats` is a library that measures the time it takes to execute
selected fragments of a program. It requires the program to be instrumented
with calls that identify these fragments, and then relies on the
function [`getMonotonicTimeNSec`][monotonic-time] to measure the execution time.

It features a `measureM` function:

```Haskell
measureM :: MonadIO m => String -> m a -> m a
```

which associates a label with a monadic computation, measures
the time it takes to perform the computation, and records it in a hidden
and globally available location. If multiple `measureM` calls use the
same label, their time measures are added together.

```Haskell
import qualified Debug.TimeStats as TimeStats

a_task :: IO ()
a_task = TimeStats.measureM "a_task" $ ...

another_task :: IO ()
another_task = TimeStats.measureM "another_task" $ ...

main :: IO ()
main = do
  a_task
  a_task
  another_task
  TimeStats.printTimeStats
```

In the above example we have instrumented functions `a_task` and
`another_task`. Whenever they are invoked, we now collect timing measures.
At the end of the program, we added a call to print the measures that have
been collected so far. It produces an output like the following when
measuring is enabled.

```
      a_task: 2.055s  count: 2
another_task: 3.071s  count: 1
```

By default, no measures are collected, though. None of the calls coming
from `timestats` are meant to change the observable behavior of the
program unless the environment variable `DEBUG_TIMESTATS_ENABLE` is set
to any value.

## timestats in a larger program

I developed `timestats` when profiling [Liquid Haskell][liquidhaskell],
a GHC plugin that interacts with [SMT solvers][smt-solver]
that run in external processes. I needed to understand how much time
was spent in different stages of Liquid Haskell.

Besides the limitations with blocking IO, the GHC profiler posed the
challenge of making the profiling runtime available to Liquid Haskell
when loaded in GHC. GHC is not linked by default with this runtime.

In addition, Liquid Haskell support for GHC 9 is partial still, and the
profiling work was to be done on GHC 8. Now, GHC 8 is not delivered with
the _eventlog_ variant of the runtime system, which is necessary to use
`ghc-events-analyze`.

In contrast, with `timestats` I could arrive at the following summary
by inserting a handful of calls. These numbers are the aggregates of
processing multiple modules with Liquid Haskell.

```
     filterValid_:  6.611s  count:   1532
              ple:  2.135s  count:      5
           refine:  7.713s  count:     15
          solveCs: 20.059s  count:     15
     solveNative': 16.426s  count:     15
    typecheckHook: 24.995s  count:     16
```

`typecheckHook` is the top-level call of the Liquid Haskell plugin.
`filterValid_` captures much of the time that is spent communicating with
the SMT solver. `ple` is an algorithm that has undergone
[much optimization][optimization-plan] lately, to the point that it has
become rather negligible in this example.
`refine` captures the time spent on another verification
algorithm . `solveCs` and `solveNative'` stand for calls that deserve
further exploration.

For the insight that these numbers provide, it is quite a return in
exchange for inserting six instrumentation calls!

## Closing remarks

`timestats` is as basic to time profiling as print statements are to
debugging. It is a last resort when more sophisticated tools are out
of reach or otherwise inconvenient to use.
You are welcome to use `timestats` in your projects, hack it to your
needs, and share your experience.

In addition, I haven't found other resources yet that discuss the
limitations of the GHC profiler. I hope this post helps to raise
awareness so people can veer early for the alternatives whenever
appropriate. Happy profiling!

[document-limitations]: https://gitlab.haskell.org/ghc/ghc/-/issues/21764
[ffi]: https://downloads.haskell.org/ghc/9.2.3/docs/html/users_guide/exts/ffi.html
[ghc-event-log]: https://downloads.haskell.org/~ghc/9.2.3/docs/html/users_guide/runtime_control.html#rts-eventlog
[ghc-events-analyze]: https://github.com/well-typed/ghc-events-analyze
[ghc-profiler]: https://downloads.haskell.org/~ghc/9.2.3/docs/html/users_guide/profiling.html
[liquidhaskell]: https://github.com/ucsd-progsys/liquidhaskell
[monotonic-time]: https://hackage.haskell.org/package/base-4.16.2.0/docs/GHC-Clock.html#v:getMonotonicTimeNSec
[optimization-plan]: https://github.com/ucsd-progsys/liquid-fixpoint/issues/500
[smt-solver]: https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
[timestats]: https://github.com/tweag/timestats
