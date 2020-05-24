---
title: "Ormolu: <br>Announcing First Release"
shortTitle: "Ormolu: Announcing First Release"
author: Mark Karpov, Utku Demir
tags: haskell
description: "We're happy to announce the first release of Ormolu, a formatter for Haskell source code."
---

We're happy to announce the first release of [Ormolu][ormolu-hackage], a
formatter for [Haskell][haskell_wiki_intro] source code. Some may remember our [first
post][first-post] from a couple months ago where we disclosed our 
work on the Ormolu project—but carefully called it “vaporware” then.
Times have changed; it's not anymore.

## Functionality

We've run Ormolu on large real-world projects, legacy codebases, and most
popular packages. We consider Ormolu usable:

* It formats all Haskell constructs and handles all language extensions.
* It places comments correctly.
* It performs some normalization of language pragmas, GHC/Haddock option
  pragmas, and import lists.
* It's fast enough to format large real-world codebases in seconds.
* Its output is almost always idempotent. _We'll get idempotence 100% right in the following releases._

## Style

Ormolu's original goal was to implement a formatting style close to 
one people already use. We also wanted a style that minimizes diffs. 
We met both goals in the first release, but you may notice
some unexpected stylistic decisions.

Let's look at an example:

```haskell
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | A formatter for Haskell source code.
module Ormolu
  ( ormolu,
    ormoluFile,
    ormoluStdin,
    -- ...
    withPrettyOrmoluExceptions,
  )
where

-- ...

-- | Load a file and format it. The file stays intact and the rendered
-- version is returned as 'Text'.
ormoluFile ::
  MonadIO m =>
  -- | Ormolu configuration
  Config ->
  -- | Location of source file
  FilePath ->
  -- | Resulting rendition
  m Text
ormoluFile cfg path =
  liftIO (readFile path) >>= ormolu cfg path
```

The snippet should look conventional except Ormolu placed commas and
function arrows at the ends of lines. Let's see why we
decided to place them there.

### Commas

While this popular formatting choice

```haskell
( "foo"
, "bar"
, "baz"
)
```

works in expressions, it's a parse error if used in a pattern, because
everything in a multiline pattern should be more indented than the opening
parenthesis. That's why we make an exception in our rendering rules—we move
the closing parenthesis one indentation level to the right on the rare occasions 
it's necessary. Re-arranging or shifting all commas would be too inconsistent in that
case, so we went with commas at the end of lines.

Did you notice that we also add trailing commas where
possible, for example, in export lists? Our ability to do this comes from a
relatively new feature in Haskell—it helps with Ormolu's goal of achieving
minimal diffs too. If we try to remember where leading commas come from, [Johan
Tibell's style guide][tibell-style-guide] comes to mind. The author [said
later][tibell-trailing]:

> […] I designed [Haskell style guide] to work with the lack of support for
> a trailing comma. If we supported a trailing comma my style guide would
> probably be different.

There you have it: GHC supports a trailing comma now, so it's not unreasonable to
start putting commas at the end of lines. This style is also more familiar
to programmers who come to Haskell from other languages.

### Function arrows

We faced another dilemma with placement of function arrows. The familiar
style is this:

```haskell
traverse
  :: Applicative f
  => (a -> f b)
  -> t a
  -> f (t b)
```

There is nothing wrong with it. It works perfectly well… with Haskell98. As
soon as we start adding more features to the type system, it's no longer
clear what is the best way to format type signatures:

```haskell
reassociateOpTreeWith
  :: forall ty op.
  [(RdrName, Fixity)]
  -> (op -> Maybe RdrName)
  -> OpTree ty op
  -> OpTree ty op
```

Here, we have had a hard time deciding how to format the type signature
because of `forall ty op.`. If we leave `[(RdrName, Fixity)]` like this,
it's not aligned with other arguments and looks quite different because it's
not prefixed by `(->)`.

We could try this:

```haskell
reassociateOpTreeWith
  :: forall ty op.
     [(RdrName, Fixity)]
  -> (op -> Maybe RdrName)
  -> OpTree ty op
  -> OpTree ty op
```

But then the first argument starts at a column that is not a multiple of our
indentation step. We could also try to put `.` on the same line as
`[(RdrName, Fixity)]` but `.` belongs to `forall ty op.`, so it's not
perfect.

In the future, there will be more additions to the type system:

* **[Linear types][linear-types]** will add a new type of arrow. It's clear that
  the new arrow `(#->)` will be at least three characters long and won't
  align with `::` and other arrows. What's more, `(#->)` is shorthand.
  In general linear arrows can have multiplicity annotations, like `p` in `Int #p-> Bool`.
  Multiplicities characterize use of the function argument, whose type is given immediately before the multiplicity, so it makes sense to group the argument type and the arrow on the same line.

* **[Dependent Haskell][dependent-haskell]** is going to add new constructions
  on the type level as well. They may bring us problems similar to the
  existing `forall`.

We found that all these problems get solved if we put arrows in trailing
position:

```haskell
reassociateOpTreeWith ::
  forall ty op.
  [(RdrName, Fixity)] ->
  (op -> Maybe RdrName) ->
  OpTree ty op ->
  OpTree ty op
```

This makes sense especially because `(->)` is right-associative.

The only problem with trailing arrows is per-argument Haddocks. They cannot
be placed after `(->)` so there are two options:

```haskell
reassociateOpTreeWith ::
  forall ty op.
  [(RdrName, Fixity)] {- ^ Fixity map for operators -} ->
  (op -> Maybe RdrName) {- ^ How to get the name of an operator -} ->
  OpTree ty op {- ^ Original 'OpTree' -} ->
  OpTree ty op {- ^ Re-associated 'OpTree' -}
```

or

```haskell
reassociateOpTreeWith ::
  forall ty op.
  -- | Fixity map for operators
  [(RdrName, Fixity)] ->
  -- | How to get the name of an operator
  (op -> Maybe RdrName) ->
  -- | Original 'OpTree'
  OpTree ty op ->
  -- | Re-associated 'OpTree'
  OpTree ty op
```

We went with the second version, which seems clearer and arguably encourages
writing more detailed Haddocks.

## Configuration and language extensions

Ormolu aims to have only one style, as noted in the [first
post][first-post]. That means no configuration and no configuration
file to keep.

Most language extensions co-exist peacefully so
they're turned on by default for every file. This way, Ormolu always works
with syntax that's enabled by the extensions—it doesn't need to search for
Cabal files to figure out which extensions to use—which simplifies the
usage.

There are a few exceptions though. You can find out which extensions are not
enabled by default like this:

```bash
$ ormolu --manual-exts
AlternativeLayoutRule
AlternativeLayoutRuleTransitional
Arrows
BangPatterns
Cpp
MagicHash
MonadComprehensions
PatternSynonyms
RecursiveDo
StaticPointers
TemplateHaskellQuotes
TransformListComp
TypeApplications
UnboxedSums
UnboxedTuples
UnicodeSyntax
```

Those should be enabled either on top of each file (recommended) or passed
with the `--ghc-opt` option.

## The next steps

Ormolu is now in beta stage, and it's available [here][ormolu-hackage] to download and try today. Next, we're going to concentrate on a few [idempotence bugs][idempotence-bugs]. They're low severity, but we've made them high priority, and we're confident we'll fix them.

Want to help improve Ormolu? Please suggest improvements, make contributions, and report any issues [here][report].

[ormolu-hackage]: https://hackage.haskell.org/package/ormolu
[first-post]: https://www.tweag.io/posts/2019-05-27-ormolu.html
[tibell-style-guide]: https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md
[tibell-trailing]: https://mail.haskell.org/pipermail/ghc-devs/2014-September/006365.html
[dependent-haskell]: https://gitlab.haskell.org/ghc/ghc/wikis/dependent-haskell
[linear-types]: https://github.com/ghc-proposals/ghc-proposals/pull/111
[idempotence-bugs]: https://github.com/tweag/ormolu/issues?q=is%3Aissue+is%3Aopen+label%3Aidempotence
[report]: https://github.com/tweag/ormolu/issues
[haskell_wiki_intro]: https://wiki.haskell.org/Introduction
