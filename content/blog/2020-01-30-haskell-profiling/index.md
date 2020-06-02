---
redirect_from: [/posts/2020-01-30-haskell-profiling.html]
title: "Locating Performance Bottlenecks in  Large Haskell codebases"
shortTitle: "Profiling Large Haskell Codebases"
author: Juan Raphael Diaz Simões
tags: [haskell]
description: "This post describes a profiling technique for Haskell codebases that yields faithful results and is well adapted to large repositories."
---

Recently, a major French bank asked Tweag to help fix a performance problem
they had in a large Haskell codebase. In this blog post, we show the technique
we used to locate the performance bottlenecks in that codebase. Once they were
located, we were able to address them and eventually obtain a speed-up of two
orders of magnitude. This post also includes a [repository][repository] with
sample code profiled using the method described here, with all runnable
commands in a Makefile—so you can easily experiment.

## Preamble: cost centers and inlining

GHC can mark some regions of code as _cost centers_, which are the
fundamental units in profiling reports. During the runtime of a profiled executable, it measures and reports the time and memory spent on evaluations of each cost center.

Since these pieces of code marked as cost centers must be clearly
identifiable at runtime,
they have a significant restriction—they cannot be inlined. Consequently, they cannot be optimized away.

Therefore, if we want to be sure we are profiling the same code we are running using a standard optimized binary, we must _not_ mark functions that
should be inlined for optimizations as cost centers. Since GHC is not aware
before compilation of which pieces of code will be inlined or not,
cost centers must be marked manually, rather than automatically. Since
automatic addition of cost centers is the [default][default], care must be
taken.

Marking cost centers manually also implies that library dependencies won't
be profiled at all. That's not a problem, since the goal of this technique is
to _locate_ performance bottlenecks, not to diagnose them. If you want to
single out some function from an external library, it suffices to bind it to
some local variable and mark this local variable as a cost center.

## Example code

Here's the code we are going to profile.
It consists of a small calculator with operations on integers (`Op`).

```haskell
module Main where

import Data.Foldable (foldl')
import qualified Data.Vector as Vector

main :: IO ()
main = do
  let
    pair n = [Add n, Sub n]
    ops = concatMap pair [0..10000]
    result = applyMany ops 0
  print result

data Op = Add Int | Sub Int
  deriving (Show, Eq)

addSlow :: Int -> Int -> Int
addSlow n k = Vector.foldl (+) k (Vector.replicate n 1)
{-# SCC addSlow #-}

sub :: Int -> Int -> Int
sub x y = y - x
{-# SCC sub #-}

apply :: Op -> Int -> Int
apply (Add n) = addSlow n
apply (Sub n) = sub n
{-# SCC apply #-}

applyMany :: [Op] -> Int -> Int
applyMany ops k = foldl' (\n op -> apply op n) k ops
```

Notice that we marked the functions `apply`, `sub`, and `addSlow` with [SCCs][scc_described] (_Set Cost Center_). The
reason for this is double:

- These are the functions we wish to observe, so they must appear in the
  profiling report.
- Inlining these functions would not significantly affect the
  performance of our software, so it is safe to mark them as
  SCCs, preventing inlining.

It's not easy to give general advice for identifying functions that are safe
to mark as cost centers. However, some pointers are:

- Recursive functions are never inlined, so marking them as an SCC
  should be safe.
- Functions that are subject to fusion, such as consumers and producers of
  lists and vectors, are very prone to be optimized away via rewrite rules, so
  much care should be taken when profiling them.

## Application of the method

The naive method (and easiest) is to use the default profiling methods from
Cabal or Stack, which add cost centers automatically when compiling with
profiling activated. We want to avoid it, so at compilation time we pass the
GHC flag `-fno-prof-auto` to the executable and its dependencies (see the
appendix). For Cabal one only needs to make the flag explicit:

```
cabal build --enable-profiling --ghc-options="-fno-prof-auto"
```

For Stack, one needs to add the following flags at the top-level to
`stack.yaml`:

```
apply-ghc-options: everything
rebuild-ghc-options: true
```

and build with:

```
stack build --profile --ghc-options="-fno-prof-auto"
```

In all of these methods, the executable should be run with `example +RTS -p` in
order to produce the `.prof` file, that can be analyzed with many tools, like
Flamegraph or Profiterole.

To show this choice of flags makes a difference, these are the runtimes of the
executable above in three situations:

- No profiling: 0.16s
- Profiling with default flags: 6.16s
- Profiling with overridden flags: 0.45s

The executable with profiling is always a little slower due to the
runtime cost of measuring and registering, but it should never have a different
complexity from the original executable: the difference should always be, at
most, linear.

We also see the difference on the profiling results in the `.prof` file. When
using the default flags, we see, after simplifying the file a little:

```
                                                                   individual     inherited
COST CENTRE                  MODULE                      entries  %time %alloc   %time %alloc

 CAF:main1                   Main                             0    0.0    0.0   100.0  100.0
  main                       Main                             0    0.0    0.0   100.0  100.0
   main.result               Main                             1    0.0    0.0   100.0  100.0
    applyMany                Main                             1    0.0    0.0   100.0  100.0
     applyMany.\             Main                         20002    0.0    0.0   100.0  100.0
      apply                  Main                         20002    0.0    0.0   100.0  100.0
       Main.apply            Main                         20002    0.0    0.0   100.0  100.0
        addSlow              Main                         10001    0.0    0.0    99.9  100.0
         Main.addSlow        Main                         10001    0.1    0.0    99.9  100.0
          >>=                Data.Vector.Fusion.Util  100020001   84.9   83.0    98.6   94.0
           basicUnsafeIndexM Data.Vector               49995000   13.6   11.1    13.6   11.1
            ...
```

which results in the following [flamegraph][flamegraph]:

<center>
<img title="profiling" src="./profiling-bad.svg"></img>
</center>

Here we see that the function `addSlow` is slow as expected, however,
most of the cost is in the `vector` library, in `(>>=)`. Why?
Because `replicate` failed to fuse with `foldl` in the code above, making
the slow code even more anomalous.

If we use the correct GHC flag instead, we see:

```
                                individual     inherited
COST CENTRE     MODULE entries %time %alloc   %time %alloc

 CAF:main1      Main       0     0.0    0.0   100.0  100.0
  Main.apply    Main   20002     0.0    0.0   100.0  100.0
   Main.addSlow Main   10001   100.0  100.0   100.0  100.0
   Main.sub     Main   10001     0.0    0.0     0.0    0.0
```

which results in the following flamegraph:

<center>
<img title="profiling" src="./profiling-good.svg"></img>
</center>

This is much more straightforward, showing that `addSlow` is the culprit
that should be corrected, without introducing spurious information to mislead
our profiling.

## Conclusion

This profiling technique is simple and should produce fairly predictable
results. This predictability is by design—code that can be inlined and be
optimized away should never be marked as a cost center, making the profiled
executable performance very similar to the regular executable one. This makes
sure that the profiling is _faithful_. This was essential for the codebase of
this French bank, since the default profiling report always pointed to the
wrong place, wasting time and resources.

This method works well with large codebases—one can start by profiling
key functions at the top level, and little by little add cost centers to
locations that are deeper in the file hierarchy, up to the point where it finds the
actual bottleneck.

Since this method requires very precise flags to work, one must be sure that
these are being passed correctly to the repository at hand. If even one
dependency gets cost centers automatically attributed, it can compromise
the whole profiling result. However, this should be easy to diagnose, since one
would see the dependency code in the profiling report.

Remember to check out the [repository][repository] we included—so you can
easily experiment with the code and commands above!

[scc]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#cost-centres-and-cost-centre-stacks
[scc_described]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#inserting-cost-centres-by-hand
[repository]: https://github.com/tweag/blog-resources/tree/master/profiling
[default]: https://www.haskell.org/cabal/users-guide/nix-local-build.html#cfg-field-profiling-detail
[flamegraph]: https://github.com/fpco/ghc-prof-flamegraph
