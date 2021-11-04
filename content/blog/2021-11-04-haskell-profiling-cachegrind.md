---
title: "A Haskell memory leak in way too much detail with Cachegrind"
shortTitle: "Profiling Haskell with Cachegrind"
author: Jeffrey Young
description: "Small tutorial on using valgrind to find hot spots in ghc and other Haskell programs"
tags: [haskell, profiling, tutorial]
---

Haskell's laziness gives us
[great
benefits](https://augustss.blogspot.com/2011/05/more-points-for-lazy-evaluation-in.html),
but comes with costs.
One of the most visible cost of laziness is hard to predict memory behavior, and so we
have, and need, tools to profile and measure the heap of our Haskell programs.
Most of the time, we live in a high-level world of type classes, monoids, and
isomorphisms. But at the end of the day our code needs to run on actual
machines, and when our code is run it needs to be performant. Typically, "be
performant", means "have reasonable computational complexity" and so we usually
focus on good asymptotic behavior instead of constant costs. But when our
programs meet our CPUs, constant costs are important, and thus having low level
tools is helpful for low level optimizations, i.e., when you want to squeeze
that last 10% out of your program.

Fortunately, the low level profiling story has improved. As of version 8.10, GHC
has basic support to export DWARF-compliant debugging information. This means
that we should be able to use
[Valgrind](https://www.valgrind.org/docs/manual/cg-manual.html) to inspect low
level details of our programs.

In spirit, the result of our valgrind analysis will be similar to
[ticky](https://gitlab.haskell.org/ghc/ghc/-/wikis/debugging/ticky-ticky)
profiling, which gives us allocations and entry counts for our functions.
However, with valgrind we can get lower level details, such as the raw
instruction counts _per function_ of our Haskell programs!

So, if you're interested in low-level optimization of your Haskell programs then
this article is for you. I'll demonstrate the use of a valgrind tool,
cachegrind, to inspect the behavior of the canonical leaky Haskell program: a
lazy left fold. You'll get three things: (1) a step-by-step tutorial on running
cachegrind on your programs. (2) a section-by-section breakdown of the
cachegrind profile, and (3) some guidance on interpreting the results.

## What is Cachegrind?

Cachegrind is a cache profiling and branch prediction tool. It takes your
program and inspects how the program interacts with your machine's cache
hierarchy and branch predictor. Why does this matter? Well some data structures,
such as the Hash Array Mapped Tries
([HAMTs](https://en.wikipedia.org/wiki/Hash_array_mapped_trie)) in the
unordered-containers library greatly benefit from caching behavior in modern
CPUs. Because HAMTs store arrays, modern CPUs will load the entire array into
the CPU caches on a write or a read, which leads to cache hits and avoids CPU
cycles that are wasted waiting for the needed memory locations to load into the
CPU caches. This is the reason why HAMTs are so heavily used in JVM based
languages such as Scala and Clojure; the JVM is very good at detecting and
performing this optimization. So even though we do not typically concern ourselves
with the CPU caching behavior of our programs, it becomes important when we want
to squeeze that last bit of performance. As always in these matters, throughput
is the goal, latency is the problem, and caching is the key.

## The small example

Consider this program, whose primary feature is to leak memory:

```haskell
-- Main.hs
module Main where

import Data.Foldable (foldr',foldl')

main :: IO ()
main = print $ "hello " ++ show (foo [1 .. 1000000 :: Integer])

foo :: [Integer] -> (Integer, Integer)
foo = foldl' go (0, 1)
  where go (a,b) x = (x + a, x - b)
```

The memory leak is in this line:

```haskell
  where go (a,b) x = (x + a, x - b)
```

Even though we have used the strict left fold `foldl'` our accumulation function
is still too lazy because the tuple `(a,b)` is a lazy tuple in both its `fst`
(`a`) and `snd` (`b`) arguments.

The fix is simple; we force the thunks in the `fst` and `snd` positions by
adding bang patterns inside the tuple:

```haskell
{-# LANGUAGE BangPatterns #-} -- new

module Main where

import Data.Foldable (foldr',foldl')

main :: IO ()
main = print $ "hello " ++ show (foo [1 .. 1000000 :: Integer])

foo :: [Integer] -> (Integer, Integer)
foo = foldl' go (0, 1)
  where go (!a,!b) x = (x + a, x - b) -- notice the bang patterns on a and b
```

So we know we have a memory leak and how to fix it; our goal is to detect that
leak with `cachegrind` to inspect how that leak manifests in the CPU cache
hierarchy. To use `cachegrind` (or, more generally, `valgrind`), we'll need to
instruct GHC to generate debugging information, e.g., `cabal build --ghc-options="-g -O2"`. Note that you should compile with `-O2` to get a
binary _as similar as possible_ to the binary you would actually ship.

We know we have a memory leak, so we should see a lot of cache misses and a
higher instruction count, because not only will we need to chase more pointers,
but Haskell's runtime system will have more work to do. To understand
`cachegrind`'s output, we'll look at the _non-leaky_ version first to have a
good idea of what to expect if there isn't a problem.

The invocation is simple, and expect your program to run _much_ slower than normal:

```
$ valgrind --tool=cachegrind ./dist-newstyle/build/x86_64-linux/ghc-8.10.4/leak-0.1.0.0/x/leak/build/leak/leak
==18410== Cachegrind, a cache and branch-prediction profiler
==18410== Copyright (C) 2002-2017, and GNU GPL'd, by Nicholas Nethercote et al.
==18410== Using Valgrind-3.16.1 and LibVEX; rerun with -h for copyright info
==18410== Command: ./dist-newstyle/build/x86_64-linux/ghc-8.10.4/leak-0.1.0.0/x/leak/build/leak/leak
==18410==
--18410-- warning: L3 cache found, using its data for the LL simulation.
"hello (500000500000,500001)"
...<bunch-of-other-output>...
```

Notice that I called `valgrind` on the binary
`./dist-newstyle/build/x86_64-linux/ghc-8.10.4/leak-0.1.0.0/x/leak/build/leak/leak`
rather than `cabal run`. For whatever reason, using `cabal run`, e.g.,
`valgrind --tool=cachegrind cabal run` loses the DWARF
symbols produced by the `-g` GHC flag and thus creates an empty cachegrind
profile.

The result of calling cachegrind produces two kinds of output. Lines beginning
with `==18410==`, are cachegrind summary output; these can be safely ignored for
now. The second kind of output is the line `"hello" ...`, which is the output of
our program.

The important result is the cachegrind profile produced in the same directory as
the invocation was called, in this case that is `~/tmp/leak`. The profile is a
file called `cachegrind.out.<pid>` where `<pid>` is the pid number of the
process created by your shell, on my machine this file was
`cachegrind.out.19438`. These files are raw output. To transform them into a
human readable format we use a tool called `cg_annotate` (short for
cachegrind annotate) that is packaged with valgrind, like this:

```
$ cg_annotate cachegrind.out.19438 > cachegrind.out.not-leaky
```

And now we can view our report. I'll only show the important pieces.
(If you do not have a wide monitor, then this report will be very ugly.)

Each section of the report is separated by dashes (`------`). The first section
is a summary of the simulated machine that cachegrind uses. It generates the
summary by inspecting your machine, mine looks like this:

```
--------------------------------------------------------------------------------
I1 cache:         65536 B, 64 B, 4-way associative
D1 cache:         32768 B, 64 B, 8-way associative
LL cache:         16777216 B, 64 B, 16-way associative
Command:          ./Main
Data file:        cachegrind.out.16623
Events recorded:  Ir I1mr ILmr Dr D1mr DLmr Dw D1mw DLmw
Events shown:     Ir I1mr ILmr Dr D1mr DLmr Dw D1mw DLmw
Event sort order: Ir I1mr ILmr Dr D1mr DLmr Dw D1mw DLmw
Thresholds:       0.1 100 100 100 100 100 100 100 100
Include dirs:
User annotated:
Auto-annotation:  on
```

These details are important, but for our purposes we'll skip over them. Just
know that they'll change depending on your machine's chipset. For a deeper dive
I recommend the
relevant [section](https://valgrind.org/docs/manual/cg-manual.html#cg-manual.the-output-preamble)
in the cachegrind manual.

The second section:

```sh
--------------------------------------------------------------------------------
Ir                   I1mr           ILmr           Dr                  D1mr             DLmr           Dw                  D1mw               DLmw
--------------------------------------------------------------------------------
315,065,912 (100.0%) 7,790 (100.0%) 4,021 (100.0%) 87,831,142 (100.0%) 134,139 (100.0%) 6,312 (100.0%) 46,945,020 (100.0%) 1,759,240 (100.0%) 68,891 (100.0%)  PROGRAM TOTALS
```

is a summary of totals for the program. Each column name is defined in the
cachegrind [manual](https://www.valgrind.org/docs/manual/cg-manual.html) but
I'll reproduce it here. There are 3 kinds of simulated CPU caches, a first-level
instruction cache (I1), a data cache (D1), and a "Last-Level" cache (LL), and
several kinds of metrics about these caches:

```sh
Ir   --> cache reads, specifically the number of instructions executed
I1mr --> instruction cache read misses (we needed to fetch something from RAM)
ILmr --> the "Last-Level" (LL) cache instruction read misses. Think an L3 cache
Dr   --> number of memory reads
D1mr --> number of data cache read misses
DLmr --> number of last level data cache read misses
Dw   --> number of memory writes
D1mw --> number of data cache write misses
DLmw --> number of last level data cache write misses
```

So now we can make sense of the report. From the program total section we see
that running our program required processing `315,065,912` instructions. The
amount of cache misses was low, with `7,790` instruction read misses, and
`4,021` last level read misses. Similarly, our program had `87,831,142` data
reads, and we had quite a few data cache write misses (`1,759,240`). These
results shouldn't be surprising, almost every program will have some read and
write misses, but this is a good base line for comparison. For the remainder of
this post, I'll be omitting the `I1mr` and `ILmr` columns, and the ratios
(`100.0%`). These columns are important, but not our primary concern; so I've
removed them to avoid line wraps, such as the wrap for `PROGRAM TOTALS`. When
relevant, I'll reproduce the ratios in text rather than in the tables.

Let's continue into the report. The third section:

```
--------------------------------
Ir           Dr          D1mr    DLmr   Dw          D1mw       DLmw     file:function
--------------------------------
146,150,652  42,041,086     314      1  18,013,694    506,849  18,916   ???:ghczmbignum_GHCziNumziInteger_integerAdd_info
 69,999,966  20,999,989      27      1   7,999,996    246,575   9,202   ???:ghczmbignum_GHCziNumziInteger_integerSub_info
 53,068,513  13,013,697       2      0  16,013,703  1,000,001  37,318   /home/doyougnu/tmp/leak/app//Main.hs:???
 39,000,049   9,000,012      81      3   4,000,005          1       1   ???:ghczmbignum_GHCziNumziInteger_integerGtzh_info
  1,807,138     903,491  27,372      0     246,435         26       0   ???:stg_gc_noregs
    996,435     359,937  27,619      0      27,810         27       0   /store/Programming/ghc-master/rts/sm/Storage.c:resetNurseries
    515,043     340,482  55,940      0      59,267          4       0   /store/Programming/ghc-master/rts/sm/BlockAlloc.c:countBlocks
    405,151     131,609   1,818    504      60,468          4       4   ???:do_lookup_x
```

is the same data broken down by descending instruction count (`Ir`) by function.
There are several things to notice. First, function names are
[z-encoded](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/symbol-names),
although they are still readable. Unsurprisingly, most of our instructions were
adding `ghczmbignum_GHCziNumziInteger_integerAdd_info` (146,150,652 (46.39%)
instructions), and subtracting
`???:ghczmbignum_GHCziNumziInteger_integerSub_info` (with 69,999,966 (22.22%)).
Our `Main` function is responsible for almost as many data writes (with
16,013,703 (34.11%)) and the majority of write misses (with 1,000,001 (56.84%))
for just the D1 cache.

The last section:

```
--------------------------------------------------------------------------------
-- Auto-annotated source: /home/doyougnu/tmp/leak/app//Main.hs
-------------------------------------------------------
Ir         Dr        D1mr       DLmr       Dw         D1mw      DLmw

         .         . .          .                   .         .      .   {-# LANGUAGE BangPatterns #-}
         .         . .          .                   .         .      .
         .         . .          .                   .         .      .   module Main where
         .         . .          .                   .         .      .
         .         . .          .                   .         .      .   import Data.Foldable (foldr',foldl')
         .         . .          .                   .         .      .
         .         . .          .                   .         .      .   main :: IO ()
        30         0 0          0                   4         0      0   main = print $ "hello " ++ (show $ foo [1 .. 1000000 :: Integer])
         .         . .          .                   .         .      .
         .         . .          .                   .         .      .   foo :: [Integer] -> (Integer, Integer)
15,041,101 4,013,698 0          0           3,000,004         1      1   foo = foldl' go (0, 1)
38,027,462 9,000,003 2          0          13,013,711 1,000,001 37,318     where go (!a,!b) x = (x + a, x - b)
```

is the same information, but broken out line-by-line per file. This is the
section that is worth most of your time. For our tiny program we have only one
file, `Main.hs`, and so this is the only file to inspect. In the real report
you'll have a bunch of annotated source from the runtime system which I have
omitted. Unsurprisingly, the annotated source points directly to the local
function `go`, as expected. Most of the work happens in the accumulator of the
strict left fold. Most of our data write misses come from our accumulator
function, and we see that `foo` only had a single missed write in the last level
and data cache. We'll return to this point later.

Alright, now for the leaky version:

```sh
-------------------------------------------------
Ir                      Dr                     D1mr                DLmr                Dw                     D1mw                DLmw
-------------------------------------------------
17,722,854,207 (100.0%) 6,937,629,327 (100.0%) 64,898,306 (100.0%) 39,650,087 (100.0%) 3,749,019,039 (100.0%) 18,682,395 (100.0%) 12,551,725 (100.0%)  PROGRAM TOTALS
```

We can see that our instruction count (`Ir`) has exploded from `315,065,912` to
`17,722,854,207`! That's many more instructions to process. Let's look at the
function section.

```sh
Ir                     D1mr                DLmr                D1mw                DLmw                file:function
---------------------------
2,851,984,623 (16.09%)  4,325,884 ( 6.67%)    789,401 ( 1.99%)        315 ( 0.00%)        65 ( 0.00%)  ???:evacuate
1,865,100,704 (10.52%)  4,141,338 ( 6.38%)  2,857,644 ( 7.21%) 11,806,905 (63.20%) 9,754,926 (77.72%)  ???:copy_tag
1,330,588,095 ( 7.51%)          0                   0                 569 ( 0.00%)         8 ( 0.00%)  ???:LOOKS_LIKE_INFO_PTR
1,285,525,857 ( 7.25%)     35,179 ( 0.05%)      5,617 ( 0.01%)        436 ( 0.00%)        23 ( 0.00%)  ???:LOOKS_LIKE_INFO_PTR_NOT_NULL
  956,209,814 ( 5.40%) 19,897,093 (30.66%)  6,802,815 (17.16%)        360 ( 0.00%)        21 ( 0.00%)  ???:LOOKS_LIKE_CLOSURE_PTR
  940,144,172 ( 5.30%)        541 ( 0.00%)         69 ( 0.00%)          6 ( 0.00%)         0           ???:scavenge_block
  851,875,808 ( 4.81%)          0                   0                 774 ( 0.00%)         6 ( 0.00%)  ???:INFO_PTR_TO_STRUCT
  791,197,818 ( 4.46%)        338 ( 0.00%)         84 ( 0.00%)        128 ( 0.00%)         6 ( 0.00%)  ???:alloc_in_moving_heap
  498,427,284 ( 2.81%)          0                   0               3,133 ( 0.02%)         0           ???:Bdescr
  478,104,914 ( 2.70%)          0                   0                 202 ( 0.00%)         3 ( 0.00%)  ???:UNTAG_CONST_CLOSURE
  446,714,992 ( 2.52%)          2 ( 0.00%)          0                  16 ( 0.00%)         4 ( 0.00%)  ???:alloc_for_copy
  417,480,380 ( 2.36%)    233,588 ( 0.36%)    193,590 ( 0.49%)          1 ( 0.00%)         0           ???:get_itbl
  326,467,351 ( 1.84%)          0                   0                 117 ( 0.00%)         7 ( 0.00%)  ???:GET_CLOSURE_TAG
  302,466,470 ( 1.71%)  2,976,163 ( 4.59%)  1,342,229 ( 3.39%)          0                  0           ???:evacuate_BLACKHOLE
  301,493,426 ( 1.70%)     34,069 ( 0.05%)        221 ( 0.00%)          0                  0           ???:scavenge_stack
  291,518,962 ( 1.64%)          0                   0                   1 ( 0.00%)         0           ???:UNTAG_CLOSURE
  273,950,830 ( 1.55%)         16 ( 0.00%)         12 ( 0.00%)          0                  0           ???:scavenge_thunk_srt
  177,938,980 ( 1.00%)    506,224 ( 0.78%)     10,352 ( 0.03%)         27 ( 0.00%)         0           ???:scavenge_mutable_list
  176,504,972 ( 1.00%) 19,583,208 (30.18%) 19,255,219 (48.56%)         27 ( 0.00%)         2 ( 0.00%)  ???:countBlocks
  170,901,058 ( 0.96%)        648 ( 0.00%)        387 ( 0.00%)          0                  0           ???:STATIC_LINK
  166,097,651 ( 0.94%)  1,810,517 ( 2.79%)  1,649,510 ( 4.16%)  1,121,003 ( 6.00%)   628,281 ( 5.01%)  ???:integerzmwiredzmin_GHCziIntegerziType_plusInteger_info
   82,121,083 ( 0.46%)  1,248,932 ( 1.92%)  1,187,596 ( 3.00%)  1,120,936 ( 6.00%)   637,117 ( 5.08%)  ???:integerzmwiredzmin_GHCziIntegerziType_minusInteger_info
   66,204,923 ( 0.37%)     20,660 ( 0.03%)         57 ( 0.00%)          0                  0           ???:closure_sizeW_
   64,000,464 ( 0.36%)          0                   0                  61 ( 0.00%)         0           ???:overwritingClosure
   63,945,983 ( 0.36%)     15,725 ( 0.02%)        312 ( 0.00%)    253,048 ( 1.35%)    13,443 ( 0.11%)  ???:recordMutableCap
   60,343,797 ( 0.34%)        245 ( 0.00%)         14 ( 0.00%)  2,000,002 (10.71%)    90,901 ( 0.72%)  /home/doyougnu/tmp/leak//app/Main.hs:Main_zdwgo_info
   57,140,616 ( 0.32%)    147,133 ( 0.23%)        171 ( 0.00%)          9 ( 0.00%)         1 ( 0.00%)  ???:alloc_todo_block
   43,000,043 ( 0.24%)        124 ( 0.00%)          9 ( 0.00%)          1 ( 0.00%)         1 ( 0.00%)  ???:integerzmwiredzmin_GHCziIntegerziType_gtIntegerzh_info
   40,990,314 ( 0.23%)          2 ( 0.00%)          0                   0                  0           ???:push_scanned_block
   32,982,071 ( 0.19%)    236,653 ( 0.36%)    188,783 ( 0.48%)     29,739 ( 0.16%)    29,696 ( 0.24%)  ???:freeGroup
   32,000,132 ( 0.18%)  1,107,641 ( 1.71%)  1,055,166 ( 2.66%)  1,004,515 ( 5.38%)   996,263 ( 7.94%)  /home/doyougnu/tmp/leak//app/Main.hs:???
```

Our table has similarly exploded, so much so that I've removed several rows, and
the `Dr` and `Dw` columns for a decent display, although I've preserved the
ratios to show how processing work has shifted. Look at all the extra processing
the runtime system had to do, and _just from missing two bangs_! In fact, almost
all the work is from the runtime system, which is expected for a memory leak.
However, a lot of this information is noise: we want to know where the leak is
for _our_ program. Ignoring all the runtime system information, there are only
two entries from the program:

```
-----------------------------
Ir                         D1mr                DLmr                  D1mw                DLmw
-----------------------------
   60,343,797 ( 0.34%)          245 ( 0.00%)         14 ( 0.00%)  2,000,002 (10.71%)    90,901 ( 0.72%)  /home/doyougnu/tmp/leak//app/Main.hs:Main_zdwgo_info
   32,000,132 ( 0.18%)    1,107,641 ( 1.71%)  1,055,166 ( 2.66%)  1,004,515 ( 5.38%)   996,263 ( 7.94%)  /home/doyougnu/tmp/leak//app/Main.hs:???
```

and one of those, `Main_zdwgo_info`, is the z-encoded `go` function.

Let's check the annotated source:

```
--------------------------------------------------------------------------------
-- Auto-annotated source: /home/doyougnu/tmp/leak//app/Main.hs
-------------------------------------------------------
Ir          Dr          D1mr       DLmr       Dw          D1mw       DLmw

         .           .          .          .           .          .        .   {-# LANGUAGE BangPatterns #-}
         .           .          .          .           .          .        .
         .           .          .          .           .          .        .   module Main where
         .           .          .          .           .          .        .
         .           .          .          .           .          .        .   import Data.Foldable (foldr',foldl')
         .           .          .          .           .          .        .
         .           .          .          .           .          .        .   main :: IO ()
        43           5          0          0           8          0        0   main = print $ "hello " ++ show (foo [1 .. 1000000 :: Integer])
         .           .          .          .           .          .        .
         .           .          .          .           .          .        .   foo :: [Integer] -> (Integer, Integer)
60,343,822  18,125,017        245         14  21,062,521  2,000,002   90,901   foo = foldl' go (0, 1)
32,000,135  10,000,042  1,107,641  1,055,166   8,000,038  1,004,515  996,263     where go (a,b) x = (x + a, x - b)
```

There are several things to notice. First, the instruction count for `foo` has
risen from `15,041,101 ( 4.77%)` to `60,343,822 ( 0.34%)`. Second, `go`'s
instruction count has _reduced_ from `38,027,462 (12.07%)` to `32,000,135 ( 0.18%)` because `go` has _less_ work to do; it only needs to allocate thunks!
Third, and this is the crucial point, is that we see `foldl'` has data cache
write misses _and_ data cache read misses. The lazy version shows `245 ( 0.00%)`
and `14 ( 0.00%)` read misses, and `2,000,002` `D1mw` and `90,901` `DLmw` write
misses, while the strict version has `0` read misses and `1 ( 0.00%) 1 ( 0.00%)`
write misses.

The read misses are interesting, not because of the difference in magnitude, but
because we shouldn't expect _any_ read misses for a tight fold such as `foo`. If
`foo` does not have any problems, then we would expect it to be tail-call
optimized into a tight loop. Thus, we would expect no read misses because the
memory location that must be read from _should_ be in the cache while `foo`
computes, leading to cache hits. That the lazy version shows _any_ cache read
misses indicates a problem.

The more interesting result is in the cache write misses. A cache write miss
occurs when we want to write some data, say an assembly operand, to the cache.
Before a write occurs, we check to see if that operand is already in the cache.
If it is then we have a cache hit; if it is not then we have a cache miss. So
this should make sense: we know that `foo` is going to write to a `(Integer, Integer)` to the data cache. We should expect that `foo` will compute
and then write the memory address containing the result to the cache _only
once_. If it doesn't write only once, then it is storing intermediate data which
is later read to finish the computation, i.e., it is allocating a thunk! So we
see that the strict version has a single write miss for both the 1 and LL
caches, most likely because the memory operand was not in the cache. It
shouldn't be: before calling `foo` we had not computed the result yet. In
contrast, the lazy version has over 2 million D1 write misses, clearly
indicating a memory leak.

## Summary and Guidance

To summarize, we can use GHC's `-g` flag to generate DWARF symbols for our
Haskell programs. With these symbols we can retrieve fine-grained data, such as
instruction count per line of source code. This information helps identify hot
spots in our code, detect memory leaks, and begin the process of optimizing our
programs. This article has been a very light introduction to cachegrind, but I
haven't covered everything. For example, cachegrind's second use is inspecting
the branch prediction behavior of programs. For the interested please see the
cachegrind manual linked below.

To close, I'd like to give some recommendations on how to use `cachegrind`
information. Getting the information is the easy part; understanding its
message, however, is more difficult. So here are my recommendations:

1. Create the _smallest_ representative example first. Cachegrind executes the
   input program much more slowly than normal and so creating a minimal example
   is beneficial not just for debugging but also to reduce the turn-around time
   of your profiling.
2. Try to use GHC's heap profiling tools first. If you have a memory leak it is
   likely that it will be more observable with a heap profile. Use a `ticky` or
   `prof` profile to find functions with many allocations and entry points, then
   use cachegrind to dig deeper. You should be using cachegrind when you
   _really_ need fine-grained, line-by-line details, or if you know you are
   writing something that _should_ interact with CPU caches in a particular way,
   such as HAMTs.
3. When you decide to use cachegrind, look at instruction counts first. You're
   specifically looking for lines of code that correspond to a large amount of
   instructions. Even if does not indicate a memory leak, it is still useful
   knowledge to identify a hot spot in your code. And always remember, the lower
   the instruction count, the better.
4. The data is important, but situating the data in context of your program is
   more important. You need to be able to ask "how many times do I expect this
   function to write or read", and "do these numbers make sense?". For example,
   we could only make sense of the write miss data by thinking about how many
   times our strict version _should_ write. In practice, this is the hardest
   part, but also the most rewarding. I prefer, and recommend, the tried and
   true method of staring at it until it makes sense, but your mileage may vary.

## Extra reading and sources

- [Valgrind user manual](https://www.valgrind.org/docs/manual/index.html)
- [Ram is the new disk - and how to measure its performance](https://tanelpoder.com/2015/08/09/ram-is-the-new-disk-and-how-to-measure-its-performance-part-1/)
- [Brendan Gregg's entire website](https://www.brendangregg.com/overview.html) (I highly recommend especially if you are into low level performance tuning)
- [The mature optimization handbook](http://carlos.bueno.org/optimization/) (A most valuable resource)
- [Extreme Cleverness: Functional Data Structures in Scala - Daniel Spiewak](https://youtu.be/pNhBQJN44YQ) (great overview of HAMTs in Scala)
