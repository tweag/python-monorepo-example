---
title: "A taste of Bazel: build a library, a service and hspec tests"
shortTitle: A Bazel for Haskell tutorial
author: Clément Hurlin
tags: [bazel, haskell]
description: "How to build a complete Haskell project with Bazel."
---

We described [several][inline-java] [times][inline-js]
[before][inline-r] how to interface Haskell with other languages. Such
interfaces between languages are important because projects rarely use
a single language: they are _polyglot_. So build systems should be
polyglot too. It's [better to have one polyglot build
system][why-bazel] than many single purpose ones. The headline benefit
is that a single build system can achieve better caching, better
parallelism, and therefore faster builds. It's also easier to avoid
correctness issues.

[Bazel](https://bazel.build/), open sourced by Google in 2015, is one
such polyglot build system. Using [rules_haskell][rules_haskell],
Bazel supports Haskell. In this post, we'll show how to get started
with Bazel on a small but non-trivial project, featuring a library,
a web service and an [Hspec][hspec] test suite.

[inline-java]: https://www.tweag.io/posts/2017-08-17-inline-code.html
[inline-js]: https://www.tweag.io/posts/2019-05-09-inline-js.html
[inline-r]: https://www.tweag.io/posts/2015-09-08-programming-r-at-native-speed-in-haskell.html
[why-bazel]: https://www.tweag.io/posts/2018-02-28-bazel-haskell.html
[rules_haskell]: https://haskell.build
[hspec]: http://hspec.github.io/

## The Bazel workspace

Bazel has two kinds of files, which both use Python syntax:

- A unique `WORKSPACE` file, which identifies a Bazel workspace. The `WORKSPACE` file is the only
  place where additional code and data outside of the workspace can be pulled
  in.
- `BUILD.bazel` files containing a specification of the build
  dependency graph. Bazel is designed for _modularity_ and to scale to
  very large repositories, so you can break up the dependency graph
  spec in many `BUILD.bazel` files scattered across your repository.

First, create a `WORKSPACE` file at the root of your project
using the [start][start] script. The created file looks like this:

```python
workspace(name = "your_cool_project_name")

# Import the rule that can download tarballs using HTTP.
load(
    "@bazel_tools//tools/build_defs/repo:http.bzl",
    "http_archive",
)

http_archive(
    name = "rules_haskell",
    strip_prefix = "rules_haskell-0.12",
    urls = ["https://github.com/tweag/rules_haskell/archive/v0.12.tar.gz"],
)

load(
    "@rules_haskell//haskell:repositories.bzl",
    "rules_haskell_dependencies",
)

# Setup rules_haskell.
rules_haskell_dependencies()

load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

# Download a GHC binary distribution from haskell.org
# and register it as an available toolchain.
rules_haskell_toolchains(version = "8.6.5")
```

The `WORKSPACE` file is very explicit. There's a good reason for this.
To be fast, Bazel is lazy: Bazel caches and reuses the result of previous builds if possible
and loads build files _only if required_. To do that reliably,
Bazel tracks changes of both **source files** and **build files**. Tracking
build files requires tightly controlling what they include,
hence the use of `load` statements to make symbols available
(`http_archive` is the first such symbol in the snippet above).

[start]: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#starting-a-new-project

## Using Stackage

To convert your [Stack][stack] project to Bazel we will make use of
the [`stack_snapshot`][stack_snapshot] rule.

[stack_snapshot]: https://release.api.haskell.build/haskell/cabal.html#stack_snapshot

Add what follows to the `WORKSPACE` file, adapting the list
of package according to what your project requires:

```python
stack_snapshot(
    name = "stackage",
    packages = [
        "aeson",
        "base",
        "directory",
        "filepath",
        "hspec",
        "optparse-applicative",
        # more packages
    ],
    snapshot = "lts-14.11",
)
```

The `packages` attribute lists the packages to pull from
[stackage.org](https://www.stackage.org). The `snapshot` attribute specifies the
desired snapshot version, as in your `stack.yaml`.

Calling `stack_snapshot` with `name = stackage` in the `WORKSPACE`
file extends the Bazel workspace to include third-party code
downloaded from Hackage, in an _external repository_ called
`@stackage`. This repository includes a number of targets, like
`@stackage//:aeson` or `@stackage//:filepath`, also defined by
`stack_snapshot`, based on metadata downloaded from Stackage. Under
the hood, [Bazel uses Stack][bazel-stack-nix] to process this metadata.

[bazel-stack-nix]: https://www.tweag.io/posts/2019-10-09-bazel-cabal-stack.html

## Compiling sources

Let's say that your project has the following layout for its
Haskell code:

```plain
.
└── haskell
    ├── exe
    ├── src
    └── test
```

- library code lives in `haskell/src`,
- the executable's code lives in `haskell/exe`, and
- test code lives in `haskell/test`.

Declaring how to build the library code is as simple as adding
the following in `haskell/src/BUILD.bazel`:

```python
load("@rules_haskell//haskell:defs.bzl", "haskell_library")

haskell_library(
    name = "mylib",
    srcs = glob(["**/*.hs"]),  # match all .hs files
    deps = [
        "@stackage//:aeson",
        "@stackage//:base",
        "@stackage//:filepath",
    ],
    # Make library code available to executable and tests
    visibility = [
        "//haskell/exe:__pkg__",
        "//haskell/test:__pkg__",
    ],
)
```

Then, declare how the executable code is built with the
following `haskell/exe/BUILD.bazel`:

```python
load("@rules_haskell//haskell:defs.bzl", "haskell_binary")

haskell_binary(
    name = "server",
    srcs = ["Main.hs"],
    deps = [
        "@stackage//:base",
        "@stackage//:optparse-applicative",
        # Depend on the library defined above
        "//haskell/src:mylib",
    ],
)
```

And finally for the tests in `haskell/test/BUILD.bazel`:

```python
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_test",
)

haskell_test(
    name = "tests",
    # Lists the source files containing tests,
    srcs = ["AppSpec.hs", "Spec.hs"],
    tools = ["@hspec-discover"],
    compiler_flags = [
        "-XCPP",
        "-DHSPEC_DISCOVER=$(location @hspec-discover)"
    ],
    deps = [
        "@stackage//:base",
        "@stackage//:hspec",
        "//haskell/src:mylib",
    ],
)
```

This rule states that the executable
[hspec-discover](http://hspec.github.io/hspec-discover.html) is required, via
the `tools` field. See [this
snippet](https://github.com/aherrmann/stack_snapshot_example/blob/0db0b540583e3061ac2b7b154a60d726ef227311/WORKSPACE#L78-L105)
for the full `WORKSPACE` file to see how this binary is obtained via a simple
application of the
[`haskell_cabal_binary`](https://release.api.haskell.build/haskell/cabal.html#stack_snapshot#haskell_cabal_binary)
rule. The rule also makes this executable available to the compiler, using the
[Make-like](https://docs.bazel.build/versions/master/be/make-variables.html)
variable `$(location ...)` to find out the runtime path to the executable.
This syntax might seem verbose. But rather than grabbing whatever
`hspec-discover` happens to be in the `$PATH`, what we're doing here
is telling Bazel exactly which binary we want to use for the above
target. When the project grows large, other parts of the build could
use a different `hspec-discover`, potentially. If `hspec-discover`
ever changes, Bazel knows exactly what needs to be rebuilt: the
`:tests` target above, since `hspec-discover` is a dependency,
but not `:mylib` or `:server`.

## Conclusion

Even small projects require important features from a build system:

- mechanisms to specify exactly what compiler toolchain we want to
  use, to make the build reproducible,
- a way to resolve symbolic names to specific package versions on
  Hackage, using package snapshots,
- the ability to build preprocessors (like `hspec-discover`) and tell
  build targets about their location,

What we have shown is that Bazel today supports doing all of the
above. Alternatives like [cabal-install][cabal-install] and [Stack][stack]
support this too, and
for small to medium sized projects, they work just fine and are
simpler to handle than the Bazel workhorse. But I hope I've given you
here a glimpse of what the Bazel way looks like: make _all_
dependencies explicit including binary dependencies, design for
cacheability, and infinite extensibility using custom build rules
`load`ed from your own Python-syntax `.bzl` files like we do in all the
build and workspace files above.

The
sample files in this blog post have been extracted from a Bazel-based
version of [servant](https://github.com/haskell-servant)'s
[example-servant-minimal][bazel-stack-example]. Head there to see the
full example.

You may also want to read the [`rules_haskell`
documentation](https://rules-haskell.readthedocs.io/en/latest/), for
more advanced use cases.

Then you can move on to build the rest of your polyglot project with
Bazel, too. Bazel supports [a large number of languages][bazel-rules-list].
And when the project gets big and the build times
substantial, turn to [shared caching][bazel-remote-cache]
among all developers.

[bazel-stack-example]: https://github.com/aherrmann/stack_snapshot_example
[bazel-remote-cache]: https://www.tweag.io/posts/2020-04-09-bazel-remote-cache.html
[bazel-rules-list]: https://docs.bazel.build/versions/master/rules.html
[cabal-install]: https://hackage.haskell.org/package/cabal-install
[stack]: https://docs.haskellstack.org/en/stable/README/
