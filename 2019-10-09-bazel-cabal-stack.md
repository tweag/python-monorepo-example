---
title: "Bazel, Cabal, Stack: <br/>Why choose when you can have them all?"
shortTitle: "Third-party Haskell libraries in Bazel"
author: Mathieu Boespflug, Andreas Herrmann
tags: bazel, haskell
description: "Bazel gets native support for third-party Haskell libraries and building Cabal packages since the 0.10 release of rules_haskell."
---

No new product created in Haskell ever starts from
scratch. [Hackage][hackage] hosts millions of lines of third-party
code, neatly and independently redistributable as [Cabal][cabal]
packages. Now, [Bazel][bazel] has native support for building Cabal packages 
since the 0.10 release of [rules_haskell][haskell-build].

Cabal packages themselves seldom start from scratch. That's why
packages typically have dozens of dependencies. Resolving version
bounds declared for all dependencies in the package metadata to a set
of concrete versions, and then downloading these dependencies, is
a painstaking task if done manually. Bazel can now use [Stack][stack]
to do this all automatically—the user only needs to provide the name of
a [Stackage snapshot][stackage-snapshot] and the names of
the packages they want to reuse for their project.

Users frequently ask which build tool to use for their next project.
It turns out that "all of them at once" is a compelling answer (including
Nix, though we covered that [previously][bazel-nix] and won't be
rehearsing that in this post).

## A Bazel primer

Bazel is a build tool originally created by Google. The key
attributes of Bazel are:

* Bazel is a *polyglot* build tool, supporting many different
  programming languages. This enables Bazel to be fast, because it can
  have a global and fine-grained view of the dependency graph, even
  across language boundaries.
* Bazel tries hard to guarantee build *correctness*. This means that
  after making a few localized changes to your source code, you don't need
  to start your build from scratch to be confident that others 
  get the same result. Incremental builds are guaranteed to yield the
  same result as full builds (under mild conditions we won't discuss
  here). This also means that it's safe to distribute builds to
  a large cluster of remote machines to make it finish *fast*. You
  still get the same result.
* Bazel is *extensible*. You can teach Bazel to build code in new
  programming languages that it didn't know about out-of-the-box.
  Doing so requires getting familiar with a simple Python-like
  language called [Starlark][starlark]. Unlike Make or [Shake][shake]
  rules, mechanisms and conventions exist to easily reuse Bazel rules
  across projects, leading to the emergence of an entire [ecosystem of
  rules][ecosystem_of_rules] that build on each other.

Internally, Google uses a variant of Bazel to build most of their
billions of lines of source code, thousands of times a day. If your
project has lots of components in a variety of different languages and
you don't want the hassle of lots of build systems too, or if you
simply want your builds to remain fast no matter how big your project
grows, you should probably be using Bazel (or [Buck][buck], Facebook's
equivalent).

The tool expects two types of files in your project:

* One or more `BUILD` files. Each `BUILD` file declares a set of
  targets. Each target is an instance of a *build rule*, like
  `haskell_library` for any reusable component in your project, or
  `haskell_binary` for the executables, or miscellaneous other build
  rules (like API documentation). See
  the [tutorial][rules_haskell-tutorial] for a longer introduction.
* A `WORKSPACE` file that allows you to invoke macros that perform
  some autodiscovery and automatically generate `BUILD` files from, say, third-party package metadata.

Here's how our solution to build third-party code works:

1. We define two new build rules: `haskell_cabal_library` and
   `haskell_cabal_binary`. These are like `haskell_library` and
   `haskell_binary`, respectively, except that Cabal is used to build
   the targets, rather than calling [GHC][ghc] (the Glasgow Haskell Compiler)
   directly.
1. A macro called `stack_snapshot` generates a `BUILD` file that
   declares a target for each Cabal package in the given snapshot that
   we'll be using in our project, directly or indirectly.

## Building a Cabal package

Let's say you have an existing Cabal library in your project.
Perhaps you would like it to be a Cabal library so that you
can publish it on Hackage. To expose it to downstream Haskell code
that [uses Bazel as the build tool][rules_haskell-announce], you can
write the following rule in a `BUILD` file:

```python
haskell_cabal_library(
    name = "mylib",
    version = "0.1",
    srcs = ["mylib.cabal", "Lib.hs"],
)
```

A binary could now depend on this Cabal library as well as on `base`
(which ships with the GHC toolchain):

```python
haskell_toolchain_library(name = "base")

haskell_binary(
    name = "myexe",
    srcs = ["Main.hs"],
    deps = [":base", ":mylib"],
)
```

In the above, we have three targets, each designated with a "label":
`:mylib`, `:base` and `:myexe`. The label is derived from the `name`
attribute that is mandatory for each
target. [rules_haskell][haskell-build] is a set of build rules for
Bazel. The build rules tell Bazel that it needs to call Cabal to build
a `haskell_cabal_library` target. Performing
this action produces several outputs, including on most platforms
a static library and a shared library (called `libHSmylib-0.1.a` and
`libHSmylib-0.1.so`, respectively). You don't need to remember the
names of any of the outputs, since you can simply pass a target as
a dependency to another, using the target's label. The build rules
tell Bazel which outputs from each one of a target's dependencies it needs to build 
the target. In this case, we are building
a binary with `:mylib` statically linked (the default), so the
`libHSmylib-0.1.a` output from that target is needed to build the
`:myexe` target.

## Building a Stackage snapshot

The ability to build libraries with or without Cabal given a short
target definition is great. But in practice, even these short target
definitions get tiring to write, for two reasons:

1. We don't want to have to write out the version numbers of each
   Cabal package explicitly. The great thing about Stackage snapshots
   is that a single snapshot name determines the version number for all
   packages. If only we could tell Bazel which snapshot we want to
   use, explicit version numbers for each package would no longer be
   necessary.
1. Cabal libraries on Hackage typically have many dependencies, which
   in turn have dependencies of their own. The full dependency graph
   can get very large, in the order of hundreds of nodes. Writing it
   out in full in the form of target definitions like above would be
   tiresome indeed.
   
[Stack][stack] already knows how to resolve a snapshot name to
a specific set of package versions. Stack also already knows where to
find these packages, on Hackage or any of its mirrors. Finally, Stack
already knows what the dependency graph looks like. So the
solution is to just call Stack. We added a workspace macro called
`stack_snapshot`. An example:

```python
stack_snapshot(
    name = "stackage",
    packages = ["conduit", "lens", "zlib-0.6.2"],
    snapshot = "lts-14.7",
)
```

The above generates a `BUILD` file behind the scenes with one
`haskell_cabal_library` per package listed in the `packages` attribute
and any transitive dependencies thereof. The result is essentially the
output of `stack dot`, which outputs a dependency graph munged into a form
cromulent for Bazel. This means that Bazel sees the same dependency
graph as Stack does, and can therefore parallelize the build on
multiple cores in exactly the same way Stack does. But because this is
Bazel, we can even distribute the build on multiple machines at once
(see below).

## Shared cache for Cabal libraries

The upshot is that you can now write polyglot projects that include
Haskell code and hundreds of third-party dependencies without
sweating. By building it with Bazel, you get the benefit of correct
caching to accelerate all your build jobs. The Bazel cache can be
remote and shared among all of your continuous integration worker
machines and even shared with all of your developers. Bazel's
correctness guarantees make this safe to do. If a branch was published
and the build succeeded, then any developer that checks out the branch
now benefits from fast builds.

## Conclusions

It's an interesting story that to achieve correct, reproducible, and
cacheable builds, we gainfully combined Haskell's three main build
technologies:

* Bazel to run build actions in parallel or distributed on many nodes
  in a build cluster,
* Cabal to interpret the metadata of existing third-party code and
  correctly construct shared and static libraries, and
* Stack to inform Bazel about where to find the source code for the
  third-party dependencies, what versions to use, and tell it what the
  dependency graph looks like.

Another interesting observation is that emulating Cabal is
*hard*. We previously collaborated with [Formation][formation] on [Hazel][hazel],
an effort to reimplement Cabal as a Bazel ruleset. It turned out that
getting the Cabal semantics exactly right for all packages on all
platforms (especially Windows) was exceedingly difficult. With the
current approach, we lose a few build parallelization opportunities,
but wrapping Cabal instead of reimplementing it leads to a much
simpler solution overall.

Have a look at Digital Asset's [DAML repository][daml_repository].
DAML is an example of a large Haskell project powered by Bazel
and rules_haskell. It's an open source smart contract language for building 
distributed applications. You can build the project using this new Stack and
Cabal support on Linux and macOS. Windows support is in progress. The
repository has around 150 direct Hackage dependencies and makes use
of advanced features such as a custom `stack snapshot`, custom package
flags, C library dependencies, and injecting vendored packages into
Stack's dependency graph.

[bazel]: https://bazel.build
[bazel-nix]: https://www.tweag.io/posts/2018-03-15-bazel-nix.html
[buck]: https://buck.build
[cabal]: https://www.haskell.org/cabal/users-guide/concepts-and-development.html
[daml_repository]: https://github.com/digital-asset/daml#readme
[ecosystem_of_rules]: https://docs.bazel.build/versions/master/rules.html
[formation]: https://formation.ai/about
[ghc]: https://www.haskell.org/ghc/
[graphviz]: https://www.graphviz.org/about/
[hackage]: https://hackage.haskell.org/
[haskell-build]: https://haskell.build
[hazel]: https://github.com/FormationAI/hazel#readme
[rules_haskell-announce]: https://www.tweag.io/posts/2018-02-28-bazel-haskell.html
[rules_haskell-tutorial]: https://rules-haskell.readthedocs.io/en/latest/
[shake]: https://hackage.haskell.org/package/shake
[stack]: https://haskellstack.org
[stackage]: https://stackage.org
[stackage-snapshot]: https://docs.haskellstack.org/en/stable/pantry/#snapshots
[starlark]: https://docs.bazel.build/versions/master/skylark/language.html

