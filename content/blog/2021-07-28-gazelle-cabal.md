---
title: "Convert Cabal-based projects to Bazel automatically"
author: Facundo Domínguez and Andreas Herrmann
tags: [bazel, haskell]
description: "Announcing the gazelle_cabal tool for automatic Bazel rules generation for Cabal projects"
---

If you have a large Haskell code base, organized in multiple [Cabal][cabal]
packages, with many system dependencies, and which takes very long
to build, then this post is for you. We describe herein
[gazelle_cabal][gazelle_cabal], a new tool that generates Haskell
rules to build with the [Bazel][bazel] build tool. It saves the
maintainer the trouble of writing these rules to begin with, and
then keeping them synchronized with the Cabal files whenever they
are modified.

Developer teams who want to make a gradual transition to Bazel can
continue to specify builds via Cabal files while still using Bazel to
build their artifacts.

## Why Bazel?

Bazel is known to offer caching for builds of multi-language projects.
Once the cache is hot, builds can avoid rebuilding many intermediate artifacts,
shortening the overall build times. This reduces the time and the cost of
running continuous integration systems and improves developer productivity
by decreasing the time it takes to rebuild when switching branches of the
project in a versioned control system.

Some support for incremental builds is implemented by most modern build
tools, `cabal-install` and [stack][stack] being no exception. However, the
tools specialized for Haskell have poor support for working with dependencies
written in other languages, and they readily discard old artifacts when
rebuilding them, which entails subsequent rebuilds when reverting changes.

Moving to Bazel is often a major investment for an existing project, though.
The recipes to build each artifact need to be rewritten, and the ways of the
new build system need to be learned. The `gazelle_cabal` tool helps with
some of that effort by extending the [gazelle][gazelle] tool, which provides
infrastructure to generate and update Bazel configuration files in general.

## Generating rules

In the happy path, one [configures][gazelle_cabal_configuration] `gazelle_cabal`
in a given repo, and then invokes

```bash
$ bazel run //:gazelle
```

The above will generate `BUILD.bazel` files next to each Cabal file,
containing the rules necessary to build the various components of the
Cabal package.

If the Cabal file reads

```
cabal-version:      2.4
name:               package-a
version:            0.1.0.0
...


library
    ...
executable executable-a
    ...
test-suite test-a
    ...
benchmark bench-a
    ...
```

The `BUILD.bazel` file will look like

```python
haskell_library(
    name = "package-a",
    ...
)
haskell_binary(
    name = "executable-a",
    ...
)
haskell_test(
    name = "test-a",
    ...
)
haskell_binary(
    name = "bench-a",
    ...
)
```

Additionally, one could invoke the following command to declare in
the `WORKSPACE` file all of the Haskell dependencies that the Cabal
packages need.

```bash
$ bazel run //:gazelle-update-repos
```

Which generates the following rule in the `WORKSPACE` file.

```python
stack_snapshot(
    name = "stackage",
    components = {
        "tasty-discover": [ "lib", "exe:tasty-discover" ],
        ...
    },
    packages = [
        "base",
        "tasty",
        "tasty-discover",
        "tasty-hunit",
        "void",
        ...
    ],
)
```

In this case, the user is expected to add other necessary attributes.
For instance, the [stack_snapshot][stack_snapshot] rule requires either
a `snapshot` or a `local_snapshot` attribute.

## Updating rules

Unlike other file generators, `gazelle_cabal` and `gazelle` extensions
in general don't overwrite the generated files on subsequent runs.
They rather blend updates to the rules with the contents of the existing files.
This simplifies considerably the customization of the output, which
otherwise would need to be specified with command line flags or
additional configuration files.

As an example, suppose we want to skip building a library on some
particular configuration. One way to deal with that in Bazel is to
specify a tag.

```python
haskell_library(
    name = "package-a",
    srcs = ...,
    tags = ["skip-ci"],
)
```

The next time that `gazelle_cabal` runs, it may modify other
attributes, but it will know to preserve the `tags` attribute
and any other attribute that doesn't need an update.

Even when attributes need to be updated, some parts of them can
still be preserved. A typical example is the package list in
the `WORKSPACE` file.

```
stack_snapshot(
    name = "stackage",
    ...
    packages = [
        "aeson",  # keep
        "base",
        "inspection-testing",
        "optparse-applicative",  # keep
        "path",  # keep
        "path-io",  # keep
        "tasty",
        "tasty-discover",
        "tasty-hunit",
        "void",
    ],
    snapshot = "lts-18.1",
)
```

Here, `gazelle_cabal` is free to add and remove any packages
in the `packages` attribute of the `stack_snapshot` rule as
long as they aren't marked with a `#keep` comment. When a
`#keep` comment is used, the corresponding package name needs
to be retained in all updates. And this applies to any list
of strings or labels in any attribute managed by the tool.

In the case of `stack_snapshot` this is handy to manage
dependency lists, where some packages are required by Cabal
files, and some packages are required by non-managed Haskell
rules in the same repository.

Also, note the `snapshot` attribute above, which is required
by the `stack_snapshot` rule and `gazelle_cabal` leaves
untouched.

## Related tools

Long time users of [`rules_haskell`][rules_haskell] may feel reminded of
[Hazel][hazel], which was the tool used to import Stackage dependencies into
Bazel builds before it was replaced by the [Cabal rules][cabal_rules_blog].

Hazel was similar to `gazelle_cabal` in that it parsed Cabal files and
generated `haskell_library` or `haskell_binary` targets to build these Cabal
packages with Bazel.
However, Hazel was intended for a different use-case. Namely, importing
external dependencies into the project and building them with Bazel.
This meant that it had to be able to fully automatically generate working build
definitions for all required external dependencies.

Many packages are easy to translate, however, some packages make use of
advanced Cabal features such as custom setup scripts or configure scripts in
ways that can be difficult to translate fully automatically.
The [Cabal rules][cabal-rules] avoid these issues by building external dependencies with
Cabal, meaning no conversion is required.
They are still the recommended way to build external Haskell dependencies.

This Gazelle extension, on the other hand, is intended for Cabal packages
situated in your code base, where changes to the Cabal file or code for
compatibility are more convenient to make. As mentioned before, Gazelle can also
preserve user defined adjustments to generated Bazel rules when needed.

Of course this raises the question, why not just use the Cabal rules for these
packages?

Firstly, if using Cabal rules, it would still be up to the user to write on
each rule the list of Haskell dependencies that each package needs to build,
whereas `gazelle_cabal` can take care of that.

Also, the regular Haskell rules are better suited for the main code base than the
Cabal rules. For example, only regular Haskell rules can be loaded into GHCi by
source using [haskell_repl][haskell_repl]. Cabal rules, in contrast, can only be loaded as
precompiled libraries.

Regular Haskell rules can also generate finer-grained actions providing
faster incremental builds.
The Cabal rules, on the other hand, generate large monolithic actions
(e.g. generating `haddock` documentation together with linking static and
dynamic libraries) and have to do additional work for compatibility between
Bazel and Cabal.
This overhead is not a big issue for third party dependencies that are rarely
changed and most often fetched from cache. However, it is an issue for targets
that are changed frequently.

## Closing remarks

This project was possible thanks to the generous funding from
[Symbiont][symbiont] and their test bed for the initial
implementation. We made a case of `gazelle_cabal` as a tool
to help development teams transitioning to Bazel builds. As the
tool is adopted by other projects, we look forward to receiving
contributions and smoothing the user experience.

[bazel]: https://bazel.build
[cabal]: https://www.haskell.org/cabal
[cabal-rules]: https://api.haskell.build/haskell/cabal.html
[gazelle]: https://github.com/bazelbuild/bazel-gazelle
[gazelle_cabal]: https://github.com/tweag/gazelle_cabal
[gazelle_cabal_configuration]: https://github.com/tweag/gazelle_cabal#configuration
[stack]: https://docs.haskellstack.org/en/stable/README/
[rules_haskell]: https://github.com/tweag/rules_haskell
[hazel]: https://github.com/FormationAI/hazel
[haskell_repl]: https://api.haskell.build/haskell/defs.html#haskell_repl
[cabal_rules_blog]: https://www.tweag.io/blog/2019-10-09-bazel-cabal-stack/
[stack_snapshot]: https://api.haskell.build/haskell/cabal.html#stack_snapshot
[symbiont]: https://symbiont.io
