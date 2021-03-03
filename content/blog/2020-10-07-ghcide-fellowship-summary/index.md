---
title: "Making GHCIDE smarter and faster: a fellowship summary"
shortTitle: "GHCIDE Fellowship summary"
author: Zubin Duggal
description: "Summary of the Tweag open source fellowship work related to ghcide"
tags: [haskell, fellowship]
---

As a Tweag Open Source
[fellow](https://www.tweag.io/blog/2020-06-05-fellows-announce), I aimed to
improve and build on the Haskell IDE experience, mainly by contributing to the
`ghcide` and `haskell-language-server` projects. My main goals were to polish up
the overall experience, and integrate
[`hiedb`](https://github.com/wz1000/hiedb), a product of a Summer of Code
project last year, into `ghcide`.

The product of this fellowship was a good selection of `ghcide` and
`haskell-language-server` features that you can use right now, or will be able
to use very soon, including better search, richer information and more
efficient queries. Let's go through these features.

## hiedb: searching references and workspace symbols

[`hiedb`](https://github.com/wz1000/hiedb) is a tool to index and query `.hie`
files that I have been working on for some time. It reads `.hie` files and
extracts all sorts of useful information from them, storing it in a `sqlite`
database for fast and easy querying.

Integrating `hiedb` with `ghcide` has many obvious (and non-obvious) benefits.
For example, we can finally add support for `find reference`, as well as
allowing you to search across all the symbols defined in your project.

![](https://s6.gifyu.com/images/References.gif)

In addition to this, the `hiedb` database serves as an effective way to
persist information across `ghcide` runs, allowing greater responsiveness,
ease of use and flexibility to queries. `hiedb` also works extremely well for
saving information that is not local to a particular file, like definitions,
documentation, types of exported symbols and so on.

Under this paradigm, `ghcide` acts as an indexing service for `hiedb`,
generating .hi and .hie files which are indexed and saved in the database,
available for all future queries, even across restarts. A local cache of `.hie`
files/typechecked modules is maintained on top of this to answer queries for the
files the user is currently editing, while non-local information about other
files in the project is accessed through the database.

This work is being carried out in [this
branch](https://github.com/wz1000/ghcide/tree/hiedb-4) and should land in
mainline `ghcide` soon.

## Responsive IDEs using stale information

I discussed, in an [earlier blog
post](https://mpickering.github.io/ide/posts/2020-05-29-hiedb.html),
how `ghcide`
could only process a single request at a time, and was cancelling old requests,
which lead to slow response times, and features like completion being almost
unusable.

The solution mentioned in the blog post above has now been merged into mainline
`ghcide`, but with a few major changes. Pepe Iborra came up with an alternative
approach that allowed managing the Shake session in a more fine grained manner,
which let us eliminate needless restarts without the need for another queue.

Here are a few graphs that demonstrate the massive improvements in response times
that are achieved by using stale information:

![](./completions_after_edit.svg)

![](./hover_after_edit.svg)

As also mentioned in the blog post, my `hiedb-4` branch of `ghcide` can also pick on on
`.hie` files written by the previous run of `ghcide`, to allow you to immediately
use your IDE even before the initial configuring and typechecking step has run.

[![asciicast](https://asciinema.org/a/xkqfc5Fst9yC5gDaPskiNE5au.svg)](https://asciinema.org/a/xkqfc5Fst9yC5gDaPskiNE5au)

## Typechecking your entire project

With [this PR](https://github.com/digital-asset/ghcide/pull/688), `ghcide` will
typecheck your entire project on the initial run, and when you save a file, typecheck
all the files that (transitively) depend on that file to give you
up to date and accurate diagnostics for your entire project.

This behaviour is completely configurable, so if you don't want to see
diagnostics for your entire project and only want to see them for the files you
have open, you can configure `ghcide` to do so using your editor's LSP configuration.

We could not do this earlier because `ghcide` did not know what the module
graph of your project looked like. We have plumbed in this information to the
correct places now, so that `ghcide` can perform these crucial functions.

## Find all variables of a given type

Sometimes, you want to know all the places that could potentially be affected if
you change the definition of a type. Maybe you want to gauge how much a type is
used, to check how painful it would be to delete it. Well, now you can, using the
power of `hiedb` and `ghcide`. Just `find references` for a type, and your editor
will also highlight all the variables which mention it.

You can restrict the query to a particular depth. For example,

```haskell
foo :: Int
foo = 1

bar :: [Maybe Int]
bar = Just foo
```

the type of `foo` contains `Int` at a depth of 0. The type of
`bar` contains `Int` at a depth of 2.

![](./typerefs.gif)

Here, you can see that viewing references for `VFSHandle` also highlights all
the symbols that include `VFSHandle` in their type.

## Use the IDE on all your dependencies!

It is always fun to zip around your code using the `goto definition` and `find references` features. But this comes to quick stop as soon as you try to go to
the definition of a function not defined in your project, but imported from an
external dependency. Don't worry, `.hie` files can come to the rescue!

In addition to all sorts of other useful information, `.hie` files also contain
the original source of the Haskell file they were generated from. If `ghcide`
knows about the `.hie` files for your dependencies, it can use those to show you
the source.

This is also available on the `hiedb-4` branch.
First, you need to generate `.hie` files for your dependencies. This can be
easily done by adding the following to your `cabal.project`:

```cabal
package *
    ghc-options: -fwrite-ide-info -hiedir <some-directory>
```

Then you simply need to inform ghcide of the directory you told `ghc` to put
the `hie` files in, and you are ready to go! Thanks to all the useful
information in `.hie` files, many of the IDE features like types and
documentation on hover, go to definition, references and more will be available
on these files also, so your IDEing can continue on seamlessly.

![](./defs.gif)

Currently, it is not possible to navigate into boot libraries (the libraries that
ship with GHC) using this, as the standard distribution of GHC doesn't ship with
`.hie` files for these libraries, and it is quite an involved procedure to compile
these libraries yourself.

Hopefully, future versions of GHC will ship with `.hie` files so that we can
navigate into those too using `ghcide`.

## Scope aware local completions

Recently, Sandy Maguire wrote a plugin for `haskell-language-server` that added
the ability to
[case-split](https://github.com/haskell/haskell-language-server/pull/391). The
plugin required the ability to get all variables in scope at a particular point
in the source. For this, we needed a data structure that could store this
information and allow efficient queries. We settled on the an
[`IntervalMap`](https://hackage.haskell.org/package/fingertree-0.1.4.2/docs/Data-IntervalMap-FingerTree.html)
populated with scoping information obtained from the `.hie` file, which allows
querying all the identifiers available at a particular point, along with their
types.

Once we had this scoping information, it was very simple to use it to
augment the `ghcide` completion subsystem to generate accurate completion
for local variables. This is now available in the branch of `ghcide` used by
`haskell-language-server`, and will soon come to `ghcide`.

![](./completion.gif)

## A type safe interface for LSP

I have also been working on improvements to the `haskell-lsp` library that
powers `ghcide`, `hls` and `hie`, providing a Haskell interface for
the Language Server Protocol that makes it possible to communicate with editors . We are
in the process of moving to a type safe encoding of the `LSP` specification, which
should make the API much more usable and less prone to errors, as well as making
it much easier to detect any deviations from the specification.

This also leads to great improvements in the interface of the `lsp-test`
library, so that the compiler can infer and check the shape of the data
you are sending matches the method type

The [PR](https://github.com/alanz/haskell-lsp/pull/244) also brings a host of
other improvements and bug fixes for the `haskell-lsp` library, simplifying and
cleaning up much of the code and making the interface much more consistent.

## Coming soon

### Go to instance definition and view all usages of an instance

Recently, [one](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/1286) of my
MRs was merged into GHC, which adds information about typeclass evidence to
`.hie` files. This will allow for features like going to the definition of an
instance used at a particular point, or viewing all usages of a particular
instance across all your code. These features can be added to `hiedb` and
`ghcide` when `GHC` 9.0 lands.

I've implemented a proof of concept for this in the [`haddock`
hyperlinker](https://github.com/haskell/haddock/pull/1197):

![](./JumpToInstance.gif)

### Call Hierarchy graphs

The upcoming 3.16 version of the Language Server Protocol has added support for
Call Hierarchies in the editor. This will allow language servers to expose the
call graph of the project to user.

Fortunately for us, `hiedb` has supported generating call graphs for a while,
as Graphviz graphs. It will be relatively straightforward to adapt this
functionality for LSP.

### Types for all subexpressions in GHC

Currently, it is not possible to easier extract the type of arbitrary expressions from
the GHC AST. Currently the most straightforward way to do this is to desugar the expression,
and then extract the type from the desugared expression. Unfortunately, this approach doesn't
scale when you want to extract the types of all subexpressions in a particular program, since
it means that you have to desugar an exponential amount of expressions!

I have been working on a [custom compiler
pass](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3866) building off
of [work done](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/2182) by
Ben Gamari, which will annotate expressions with their types. This will allow
tooling such as `.hie` files to include this information, and will allow
`ghcide` to tell you the type of any arbitrary expression in your program.

## Final Thoughts

The open source fellowship was a great opportunity. It was very enlightening
and helpful to interact with Tweagers. I really enjoyed the freedom allowed to
choose where to focus my efforts myself. The funding allowed me to devote a
significant amount of time to working on open-source and improve the Haskell
developer experience.

IDEs are not easy to write and maintain, and they require
a lot of effort to develop and extend. Funding is absolutely critical for this.
Fortunately, this year we had two IDE related GSOC projects, along with this
Tweag Fellowship, so were able to make great strides with the help of all
other volunteers who work and contribute to these projects.
