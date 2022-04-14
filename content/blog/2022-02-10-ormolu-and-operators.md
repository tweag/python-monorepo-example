---
title: "Ormolu: the challenge of formatting operator chains"
shortTitle: "Ormolu: formatting operator chains"
author: Thomas Bagrel
description: Why formatting chains of infix operators is hard in Haskell. And how I addressed it for Ormolu.
tags: [haskell]
---

The [Ormolu project](https://github.com/tweag/ormolu), led by Mark Karpov at Tweag, has been providing a reliable way to format Haskell source files for a few years now. It makes use of the parser of GHC itself, so as to prevent parsing issues that unfortunately some other Haskell formatters have.

Alexander Esgen detailed several improvements he made to Ormolu during his internship in [a previous blog post](https://www.tweag.io/blog/2021-10-28-hacking-on-ormolu/). In turn, this blog post will be focusing on the challenge of formatting infix operator chains in Haskell, which is notoriously difficult and has been my primary goal for the last two months.

### Operator chains

Almost all programming languages allow the use of infix operators, especially for the four basic numeric operators, `+`, `-`, `*` and `/`. Moreover, most of the time, these operators can be "chained" together without parentheses, and in that case, _fixity_ rules determine the meaning of an expression. Indeed, it's really neat to write `1 + 2 * 5 * 6` instead of `1 + ((2 * 5) * 6)`, which would be required without such rules. From now on, I will call such a sequence of operands interleaved with infix operators an _operator chain_.

An operator's fixity is composed of

- A _precedence_, which can be seen as a priority level. An infix operator with a higher precedence level will bind more tightly than an operator with a lower precedence level (e.g. `*` has precedence level 7, whereas `+` has precedence level 6 in Haskell).

- An _associativity direction_: an infix operator can be either left associative (`infixl`), right associative (`infixr`), or non-associative (`infix`) and indicates, in the absence of parentheses, in which order should the computations be made inside a group of operators having the same precedence level.

When several operators in a chain share the same precedence level (e.g. `1 + 2 - 3 - 4`), no operator has higher priority than the other, but we still need to order the computations. In order to do so, we need to look at the associativity direction of each operator.

The associativity direction is different from the mathematical definition of associativity: `+` and `*` are associative in the mathematical sense (because `(a + b) + c = a + (b + c)` and `(a * b) * c = a * (b * c)`), whereas `-` and `/` are not. However, all of these four operators are left associative, which means that, in a chain of `+` and `-`, or in a chain of `*` and `/`, the operations should be computed left-to-right: `1 + 2 - 3 - 4 = ((1 + 2) - 3) - 4`. On the other hand, the mathematical exponentiation, `^`, is right associative: `2^3^3` means `2^(3^3)`.

A non-associative operator cannot be chained with operators of the same precedence level, and often indicates that the operator has a different return type than those of its left and right operands (e.g. `/=` is non-associative, which is a sensible choice given its signature `Ord a => a -> a -> Bool`: `1 /= 2 /= 3` would make no sense). Likewise, two operators with the same precedence level, but with different associativity directions cannot be chained together without explicit parentheses. Fortunately, that particular combination is uncommon in practice.

### Formatting issues with the previous version of Ormolu

According to Ormolu's core principles, any single-line expression should stay single-line, and as a result, Ormolu doesn't need to do much work for a single-line operator chain, except from checking that spaces are effectively put around each infix operator.

For multi-line operator chains, however, it's a completely different story. As soon as one line break is introduced in an operator chain by the programmer, Ormolu is allowed to reformat the chain in a multi-line fashion, as long as the AST remains the same. But then, what should we do to produce the most readable and least surprising output?

In the previous version of Ormolu, operator chains weren't represented by a sequence of operands interleaved with infix operators, but rather as a binary tree `data OpTree = OpNode | OpBranch OpTree Operator OpTree`, directly extracted from the AST returned by `ghc-lib-parser`.

At this point, it is crucial to remember that Haskell allows its users to define custom infix operators, in any module. As a result, the fixity of each infix operator can't be known when a source file is parsed, because this operator could have been defined in any other source file of the project or of its dependencies. As a result, any operator chain is made into a degenerated binary tree in the AST, as if all operators had the same precedence level and were left associative.

```
1 + 4 * 1 - 2

becomes at the parsing stage:

      -
     / \
    *   2
   / \
  +   1
 / \
1   4
```

Ormolu used to format operator chains recursively in their binary tree form. This strategy implied that the deeper we got in the tree, the less information we had about the whole chain and its multi-line/single-line context. The most visible consequence of this strategy was the inconsistent number of operands on each line in a multi-line operator chain:

```haskell
chain1 =
  1 + 2 + 3 + 4
  + 5 + 6
```

would become

```haskell
chain1 =
  1 + 2 + 3 + 4
    + 5
    + 6
```

whereas this chain:

```haskell
chain2 =
  1 +
  2 + 3 + 4 + 5 + 6
```

would become

```haskell
chain2 =
  1
    + 2
    + 3
    + 4
    + 5
    + 6
```

### How to improve the situation

Two general styles are often considered for multi-line constructs (their names are taken from the JetBrains IDEs' formatter):

- The _wrap_ strategy, for which line breaks are introduced so as to meet a line length goal (for example, no more than 80 characters per line of code). As a result, the number of operands and operators on each line is variable, and the strategy is not really diff-friendly.
- The _chop down_ strategy, where every pair of operator and operand sits on its own line, without taking length into account.

Ormolu has no line goal system, so the first strategy is definitely not applicable. But the second one, albeit being diff-friendly, has unwanted consequences for chains that mix operators with different precedence levels.

For instance, consider this:

```haskell
chain3 =
  1
    + 2 * 5
    - 6 * 7
    + 5 + 8
```

With the chop-down strategy, it would become:

```haskell
chain3 =
  1
    + 2
    * 5
    - 6
    * 7
    + 5
    + 8
```

Although groups of form `a * b` bind more tightly than sums, they are now split into multiple lines. As a result, operator precedences (and thus, the computation order) are not easily readable from this formatted snippet.

This observation led me to the following idea:

> Ormolu should be aware of operator fixities, and operator chains should be represented by N-ary trees, where all the operators having the same precedence level (and their associated operands) sit at the same level in the tree.

As a result, I created the Haskell type `data NaryOpTree = OpNode Expr | OpBranches [OpTree] [Operator]` to represent operator chains, where `OpBranches [OpTree] [Operator]` represents a sub-chain of expressions interleaved with operators having the same precedence level.

The previous example (`chain3`) is now represented by:

```
   +  -   +  +       OpBranches
 /  |   \   \  \       [ OpNode 1,
1   |    \   5  8        OpBranches [OpNode 2, OpNode 5] ["*"],
    *      *             OpBranches [OpNode 6, OpNode 7] ["*"],
   / \    / \            OpNode 5,
  2   5  6   7           OpNode 8
                       ]
                       ["+", "-", "+", "+"]
```

As the formatting function is now aware of sub-chains (which, at their root, contain only operators having the same priority level), the arbitration between single-line and multi-line formatting (with the chop down strategy) can be done on a _per sub-chain_ basis, instead of being done for the chain as a whole.

With such rules, the `+ 5 + 8` at the end of the previous example will still be split into separate lines (for consistency, because the `+` sub-chain is globally multiline), but each `a * b` group will stay single-line:

```haskell
chain3 =
  1
    + 2 * 5
    - 6 * 7
    + 5
    + 8
```

To further improve readability, when a chain with mixed precedence levels is already completely chopped down, I suggested that we also indent by one extra level any operator that is on its own line and has a higher precedence level:

```haskell
chain4 =
  1
    + 2
    + 5
    * 6
    * 8
    + 7
```

become

```haskell
chain4 =
  1
    + 2
    + 5
      * 6
      * 8
    + 7
```

Still, even if this algorithm looked good, we needed to find a way to make Ormolu aware of operator fixities for this to work in practice.

### The quest for a fixity database

Fixity declarations can appear almost everywhere in Haskell: in the source file where the operator is actually used, in an imported module from the same project, in an imported module from an external dependency, in an implicitly imported module such as `Prelude` from `base`...

Parsing local fixity declarations automatically (that is to say, declarations made in the source files from the current project) would be rather difficult, because we can't ensure that Ormolu is always run on all the project files at once, and then, we could get different formatting results depending on which files have been formatted together. Also, Template Haskell, CPP blocks, or nested declaration in `where` clauses would need to be taken into account to get correct results, which is way beyond the scope of what Ormolu can do. Instead, we imagined a semi-automatic process to extract local fixity declarations, which can be overridden manually (see [#845](https://github.com/tweag/ormolu/issues/845) and [#846](https://github.com/tweag/ormolu/issues/846): this should be implemented soon).

Fortunately, most of the operators used in Haskell projects are not locally defined but rather come from either [`base`](https://hackage.haskell.org/package/base), or from other packages available on [Hackage](https://hackage.haskell.org/) (the package repository/index for Haskell).

Hackage makes the Haddock documentation of every package (which in turn contains operator definitions, with their fixity declarations) available, but such documentation doesn't have a normalized/easily parseable format on the website. Fortunately, [Hoogle](https://hoogle.haskell.org/) provides a database with "header files" in a plain text format for each package available on Hackage, available [here](https://hackage.haskell.org/packages/hoogle.tar.gz).

Using this database, I was able to write a script extracting fixity information for every possible operator from the 16 000 packages available on Hackage (among which, about 3000 packages define at least one operator).

Unfortunately, it isn't uncommon for a single operator to be defined in several packages, with completely different precedence levels and associativity directions. For instance, 24 000 operator fixity declarations are found in the Hoogle database, for "only" 5600 distinct operators.

When conflicting fixity declarations are encountered for an operator, and a such operator is used in a source file, Ormolu needs to decide which fixity declaration will be used. To choose the correct declaration in as many cases as possible, we settled on the following heuristic:

- if the operator is defined in `base`, then the fixity declaration from `base` is used
- (otherwise) if the operator is defined in a package listed in the project dependencies, then this fixity declaration is used (Ormolu is already using cabal files to detect language extensions, so it was rather easy to also read project dependencies from them)
- (otherwise) if the operator is defined in a GHC boot package (i.e. a package automatically downloaded with GHC, such as `bytestring`), then this fixity declaration is used
- otherwise, Ormolu uses a heuristic based on package download counts (extracted from Hackage) to guess the most probable fixity for the operator at hand.

The last point needs a bit of clarification. When a conflict about fixity declarations arise for a given operator (not previously found in `base`, dependencies, or boot packages), we distinguish two cases:

- either one package which declares this operator is extremely popular (according to the last 30-days download count available on Hackage), in which case it's safe to assume that, in most contexts, this fixity declaration will be the good one ;
- or no package stands out from the crowd, in which case, we _merge_ the conflicting declarations, by retaining the maximum amount of compatible information from them. For example, if both `infixl 7` and `infixl 9` declarations are found for `>>=>>`, we will store `infixl [|7,9|]` in the database. The function in charge of reassociating N-ary trees in Ormolu has, of course, been designed so as to accept a range of possible precedence levels for each operator, instead of a single integer.

### Optimizations

As described above, the construction of the fixity map should occur at runtime, because it depends on the project dependencies. However, doing so much processing upfront made Ormolu's startup time skyrocket. As a result, various performance improvements have been conducted by the project team, after an initial performance assessment:

- Using Template Haskell to parse at compile time the JSON file containing the fixity map extracted from Hackage/Hoogle (see [#841](https://github.com/tweag/ormolu/pull/841)). This change dropped the startup time of the new Ormolu version by a flat 150-200 ms (see [this](https://gist.github.com/amesgen/64e86711d21fb7e2c4fa5b597438a349) for detailed results).
- Using lazy maps and lazy map unions to delay the computations related to less commonly used operators (see [#847](https://github.com/tweag/ormolu/pull/847)). If all the operators used in a source file come from `base`, or from the project dependencies, then the cost of building the fixity map is divided by 8 (see [this](https://github.com/tweag/ormolu/pull/847#issue-1082260577) for detailed results).
- Memoization of the final fixity map produced by (`buildFixityMap'`), so that it is only built once when formatting multiple files from the same project (see [#848](https://github.com/tweag/ormolu/pull/848) and [#849](https://github.com/tweag/ormolu/pull/849)). With this change, the time taken to format a dozen 1-line files from a same project is divided by 5 (see [this](https://github.com/tweag/ormolu/pull/848#issue-1082573870) for detailed results).

In the end, we managed to keep the performance almost the same: it takes about 1500 ms to format the Ormolu codebase with this new version of Ormolu, compared to 1250 ms with revision [`551faf3`](https://github.com/tweag/ormolu/commit/551faf3ee931a5d348eb75885bb1250edb8f135f) (commit just preceding the introduction of the operator chain overhaul).

### Conclusion

Creating a new algorithm based on operator fixities to format operator chains wasn't easy: it took us more than two months, and raised many questions and obstacles that I can't detail here, but that you can find in [the dedicated PR](https://github.com/tweag/ormolu/pull/830/files). The new formatting strategy is already part of the `master` branch when I'm writing these lines, and will be shipped with the 0.5.0.0 release. I hope that it will address what have been a known weakness of Ormolu for a long time.

In the end, this journey has been incredibly instructive for me and has been a very pleasant experience. I learned a lot about the Haskell language, its parser, but also about the care and prudence required when applying changes to a formatter relied on by industrial actors. I met and discovered very nice and helpful people in the Ormolu team too, namely Mark Karpov and Alexander Esgen.
