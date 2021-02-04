---
title: "Intern opening to improve GHC performance"
author: "Richard Eisenberg"
tags: [haskell]
description: "A call to action to improve GHC's performance via a paid internship"
---

[application]: https://boards.greenhouse.io/tweag/jobs/5048094002

Haskell's GHC compiler has notoriously slow compile times. In a bid to drive
the change it seeks in the world, Tweag is opening a call for applications for
an intern to work for three months solely on improving GHC's performance. This post describes a
number of approaches the intern might take. Most of these ideas are broadly
applicable to any Haskell program, and so even those of you who do not hack
regularly on GHC might find this useful.

Do you have the interest and skills to improve compile times? Please [apply][application]!
(More details about the application are at the bottom of this post.)

## Avenues toward performance improvement

- **Data structures**. Does GHC use the most efficient datatypes internally? For
  example, lists are very common in the GHC codebase. Are they effective? Maybe
  small arrays would be better. Maybe `Data.Seq` would be better. Ideally,
  we would come up with concrete guidelines for when to use each datatype.

- **Fusion**. Much data -- in particular, lists -- is meant to be fused away.
  Does this happen in practice? Fusion is implemented within GHC by using
  rewrite rules. However, GHC defines a good deal of its own low-level list-manipulation
  functions. Do these fuse correctly? The intern would look to find out.

- **Case-of-known-constructor**. A good deal of functions within GHC return a
  `Maybe`. This means that the functions invariably must allocate their result
  freshly. Is that really necessary? If the function is inlined into a context
  that immediately matches against the `Maybe`, then we can avoid this allocation,
  via the case-of-known-constructor optimization (already present).
  In the case of recursive functions, the allocation incurred by all the `Just`
  nodes may be significant. Another place
  where case-of-known-constructor may help is around tuples, which _also_ allocate.
  One possible direction here is to replace `Maybe (..., ...)` with `Maybe2 ... ...`,
  where `data Maybe2 a b = Just2 a b | Nothing2`. This change would reduce allocations.
  But is it worth it?

- **Laziness**. Many data structures in GHC are lazy. But perhaps some important
  data structures would be better with (some) strict fields. We could look at which
  data structures are in the most common use and see whether they can be optimized
  by adding strictness annotations.

- **Unboxed types**. Normal, boxed types are convenient, but unboxed types
  (including unboxed sums `(# ... | ... #)`) can be faster. If there is a tight
  loop, the speed of unboxed types may be worth the annoyance of working
  with them.

- **Data representation**. There is often a multitude of ways of representing
  data. For example, we could store the expression `f a b c` as `App (App (App f a) b) c`,
  as we do now. But maybe `Apps f [a,b,c]` would be better, or `Apps f (fromList [a,b,c])`
  (for some appropriate data structure's `fromList` function)
  would be even better. The intern could explore these possibilities.

- **Algorithms**. There are a number of algorithms that do unnecessary work within
  GHC. For example, we might want to know whether there are any free type variables
  in an expression. A simple way to do this is to gather all the free variables of
  an expression (there is a function for that) and then iterate throught the set
  of variables (there is a function for that) detecting type variables (there is
  a function for that). But this is madness: it allocates a ton of space in building
  the set, when a straightforward traversal of the expression would yield what we want,
  without allocating a whit. The intern would hunt down and kill such mad examples,
  possibly using abstractions like the [`foldTyCo`](https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Core/TyCo/Rep.hs#L1836) idea recently added. This is a way of
  doing lightweight traversals fast.

- **Heap profiling**. GHC has features enabling heap profiling, meaning that we
  can see what structures appear in the heap at regular time intervals. Either using
  existing performance tests or writing new ones, the intern could generate heap profiles,
  looking for something unexpected -- for example, that an entire half of the heap is
  just `:`-nodes.

- **Ticky-ticky profiling**. The ticky-ticky profiler is a low-level instrumentation
  of the optimized code, and it allows you to see exactly what is being allocated and
  when. Looking at this output is enlightening: a [recent profile](https://gitlab.haskell.org/ghc/ghc/-/issues/18541) shows, for example, 321,081 entries of the recursive helper
  function in the `length` function for lists. (Because it's the recursive helper function,
  this result means that a total of 321,081 list nodes were counted, not that `length`
  was called 321,081 times.) Yet I would hope that we almost never need to find the
  length of a linked list -- doing so is often a sign that you have the wrong data
  structure. Investigating this would likely lead to optimization opportunities.

- **Streams**. GHC has its own Stream module added by Simon Marlow 8 years ago
  ([commit](https://gitlab.haskell.org/ghc/ghc/commit/46a772f8efb7aa9d350227e8fd5d5809757c3f1e))
  which allows the STG-to-CMM codegen to run in constant space. This is
  great, but not great enough, since `Stream` isn't more widely used
  throughout the GHC codebase to replace lists, and the implemented
  operations of `Stream` are quite limited. The intern could
  look into chances to use `Stream` in more places and possibly port the `Stream` from
  `vector`, given that it implements much richer operations and is fine-tuned for
  fusion and performance.

- **Community-sourced opportunities**. The list here is surely incomplete.
  We can community-source other ideas to pursue.

**Success criterion**: I would expect that three months of directed effort should
yield at least a 10% improvement in compile times. This expectation could be way
off, admittedly.

The intern would also be required to share their knowledge:

- They would write a blog post on their work, which would
  hopefully offer readers insights in performance debugging beyond GHC.

- They would offer a knowledge sharing within Tweag, helping other to
  performance-tune their own code.

## Stretch Goals

Though the focus would remain on optimizing GHC, the intern
may be able to make progress at automating their work. Here are some ideas in
this vein:

- **New optimizations**. One idea above is to change `Maybe (ty1, ty2)` to `Maybe2 ty1 ty2`. However, (ignoring laziness) these types are isomorphic, and GHC should
  be able to transform one into the other. Why should we do this manually? (Not
  ignoring laziness: we may need strictness annotations to make them isomorphic, but
  that's not a problem.) Maybe the intern can design and implement a new optimization
  pass.

- **New data collection opportunities**. In performance-tuning the linear types
  branch, I really wanted to know what constructors were being allocated on the heap.
  This data was not easily available, but some poking around (and help from others)
  showed how it could be done easily. GHC was thus augmented to keep track of this
  information when requested. Other, similar measurement ideas may arise, as improvements
  to GHC that would benefit anyone doing profiling work.

- **Inspection testing**. There are often certain optimization expectations
  embodied in a chunk of code. For example, we might expect that inlining a certain
  function will eliminate some of its tests. However, how can we be sure these
  optimizations really take place? Joachim Breitner has written an _inspection
  testing_ tool that permits exactly this sort of unit test, as described
  in [this paper](https://arxiv.org/abs/1803.07130). We could imagine integrating
  inspection testing into the GHC and library testsuites.

## Conclusion

Do you have what it takes to optimize GHC and become a folk hero in the Haskell
world? I hope so, and I look forward to working with you! Apply using our
[online application][application]. Include a cover letter describing your Haskell
experience, any experience you already have with working on GHC (though none is
required), and any experience you already have with performance work (in any
language). We will collect applications until Thursday, March 4, 2021 (anywhere
on Earth) and then review all collected applications; we hope to be able to
update applicants on their status within days. The internship can start any time
after the offer is made, subject to mutual availability. Internships typically
last 12 weeks, although there may be some flexibility here. If you have
any questions, feel free to [email me directly](mailto:richard.eisenberg@tweag.io).
Please do [apply][application]!
