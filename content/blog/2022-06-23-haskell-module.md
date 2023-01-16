---
title: "Incremental builds for Haskell with Bazel"
author: Facundo Domínguez
tags: [bazel, haskell]
description: "Announcing the new haskell_module rule in rules_haskell"
---

Building Haskell code with Bazel brings some benefits, like builds
that are hermetic (i.e. easy to reproduce), caching which allows to
switch branches in your version control system and still
have fast rebuilds, and tracking of cross-language dependencies.

However, till recently, changes to the source code of a module in a
library would require all of the modules in the library to be
rebuilt, which could be a serious limitation when using Bazel to
build frequent and small changes to the code. This was a consequence
of Haskell rules only being able to express dependencies
between libraries and binaries.

In this post we describe [`haskell_module`][haskell_module], a new
rule in [`rules_haskell`][rules_haskell], which allows to express
the dependencies between modules. With this rule, Bazel has a
higher resolution of the dependency graph and can skip building
modules that are certainly not affected by a change.

## Building libraries the old way

Suppose we wanted to build a library with only two modules. In
Bazel+`rules_haskell` configuration this would be written as

```python
haskell_library(
    name = "lib",
    srcs = ["A.hs", "B.hs"],
)
```

This rule produces a Bazel action that ends up calling `ghc` something like

```bash
ghc -no-link A.hs B.hs
```

which would then be followed by another action to do the linking.

The inputs to this action are the compiler itself and the source files.
Bazel ensures that dependencies are not missing in the build configuration
by only exposing declared dependencies to the action, this is a strategy most
commonly known as sandboxing.

The action above produces as output the files `A.hi`, `B.hi`, `A.o`, and
`B.o`. Unfortunately, Bazel does not allow us to declare these files
as both outputs and inputs to the compile action since this would create a loop on the
dependency graph. The main consequence, is that we cannot take advantage
of ghc's [recompilation checker][recompilation_checker], which would give
the action the chance of not rebuilding an unmodified module, should the
action be called upon changes to either of the modules.

## Building with `haskell_module`

With the new `haskell_module` rule, we can write instead

```python
haskell_library(
    name = "lib",
    modules = [":A", ":B"],
)

haskell_module(
    name = "A",
    src = "A.hs",
)

haskell_module(
    name = "B",
    src = "B.hs",
)
```

Building with this configuration now creates the actions

```bash
ghc -c A.hs
ghc -c B.hs
```

which are then followed by the linking step. Because modules are
built in different actions now, Bazel can distinguish that when
only one of the modules has been modified, it doesn't need to
rerun the action to build the other module.

## Further build parallelism

Now that the actions are broken down by module, it is possible for
Bazel to run these actions in parallel. Consider a new Haskell module
that is added to the library `lib`.

```haskell
-- C.hs
module C where

import A
import B
```

We can express the dependencies between the modules with

```python
haskell_module(
    name = "C",
    src = "C.hs",
    deps = [":A", ":B"]
)

haskell_library(
    name = "lib",
    modules = [":A", ":B", ":C"],
)
```

The dependency graph in Bazel now reflects the dependency graph
implied by import declarations in Haskell modules. A first consequence
of this, is that Bazel is now on equal footing with `ghc` to decide
how to do parallel builds.

Furthermore, while `ghc` can build in parallel the modules of a
library, Bazel is not limited by the library boundaries. Say
module `A` comes from a library dependency instead of being in the
same library as `C`.

```python
haskell_module(
    name = "C",
    src = "C.hs",
    deps = [":B"],
    # rules_haskell slang to express that ":A" comes
    # from another library (the other library must be listed
    # in narrowed_deps of the enclosing haskell_library)
    cross_library_deps = [":A"],
)

haskell_library(
    name = "lib",
    modules = [":B", ":C"],
    # Other libraries with modules that might be referred
    # as dependencies in haskell_module rules.
    narrowed_deps = [":libA"],
)

haskell_library(
    name = "libA",
    modules = [":A"],
)
```

In this scenario, Bazel can still build both modules `A` and `B` in
parallel, while most build tools for Haskell would insist on building
library `libA` ahead of building any module in library `lib`.
And this difference stands even if module C uses Template Haskell.

```haskell
-- C.hs
{-# LANGUAGE TemplateHaskell #-}
module C where

import A
import B

$(return A.decls)
```

Now we tell to `rules_haskell` that `C` needs Template Haskell with the
`enable_th` attribute.

```python
haskell_module(
    name = "C",
    src = "C.hs",
    deps = [":B"],
    enable_th = True,
    cross_library_deps = [":A"],
)
```

When bazel tries to build `C`, it still can build modules `A` and `B`
in parallel. The effect of `enable_th` is that the object files of `A`
and `B` will be exposed to the build action.

```bash
ghc -c C.hs A.o B.o ...
```

Again, it isn't necessary to build or link `libA` ahead of building the modules
in `lib`. However, as far as I'm aware `rules_haskell` is the first
implementation to support this.

# Keeping source code and build configuration in sync

It would be pretty annoying to update the build configuration every time
we add or remove import declarations in a source file. Suppose, module
`C` no longer needs module `A`.

```haskell
module C where

-- import A
import B
```

Now our build configuration is outdated since it incorrectly claims that
module `C` depends on module `A`. While removing the dependency in the
build configuration is not difficult, we get little help from
`rules_haskell` to detect and fix these situations that are all too
common when a project is under active development.

To streamline edits to the configuration, Tweag has developed
[`gazelle_haskell_modules`][gazelle_haskell_modules], a
[gazelle][gazelle] extension that scans the source code and updates
the build configuration automatically. Whenever the import declarations
change in any module of a project, a single invocation of a command
will update the build configuration to match it.

```bash
bazel run //:gazelle_haskell_modules
```

`gazelle_haskell_modules` will discover Haskell modules, it will update
the dependencies of library and `haskell_module` rules, it will update
the `enable_th` attribute, it will add `haskell_module` rules when new
source files are added, and it can remove `haskell_module` rules when
source files are deleted or moved.

# Limitations

When lowering the granularity of build actions, new kinds of phenomena
enter the scene. If we start progressively diminishing the amount of
work that each action does, eventually housekeeping tasks that we
perform for each action will start having a cost comparable to that
of the action itself.

One of the new outstanding overheads comes from sandboxing.
There are different techniques to
implement sandboxing, and it turns out that the sandboxing done by
default in Bazel can account for near 20% of the builds when using
`haskell_module`. This can be reduced by either putting the sandboxes
in an in-memory file system (Bazel option [`--sandbox_base`][sandbox_base]) or by
reusing part of the setup from one sandbox to another (Bazel option
[`--experimental_reuse_sandbox_directories`][reuse_sandbox_directories]).

Another source of overhead is the startup time of the compiler.
When `ghc` is requested to build a module, it first needs to read the
interface files of any imported modules, and may need to read interface
files of other transitively imported modules. All this reading and
loading needs to be done for every invocation, and when using
`haskell_module` it can account for 25% of the build time once we
have eliminated sandboxing overheads. The remedy to reduce the
startup overhead is to use a compilation server or persistent worker
in Bazel slang. By having a compiler running on a process that can
receive multiple requests to compile modules, we only pay for the
startup costs once. Unfortunately, implementing a persistent worker
that handles sandboxing correctly
[poses a few challenges][haskell_module_worker] that still need to be
solved.

Lastly, disabling `ghc`s recompilation checker is a
limitation. If there is a change in a module M deep in the dependency
graph, chances are that the change won't become visible in the
interface file of every module that imports M transitively. This is
because interface files only account for some aspects of a module
which are relevant to the modules that import them. At the point
where interface files don't change anymore, the recompilation checker
would kick in if artifacts from earlier compilations are made available
to the build process.

`rules_haskell`, however, has to provide the interface file of M as
input to the actions that build every module that imports M transitively. This
is because the compiler might need to read the interface file for M,
and the build system can't predict reliably whether it will be needed
or not. If the interface file of M changes, then Bazel will arrange to
rerun all those actions whether the interface files of the modules
along the path in the dependency graph are changed or not. A
remedy for this could be to use a feature of Bazel to report when an
input hasn't been used (i.e. [`unused_inputs_list`][unused_inputs_list]),
which at the time of this writing still needs to be
[investigated][pending_unused_inputs_list]. Please, see the follow up
[post][recompilation_avoidance] on the evolution of this issue since
the time of this writing.

## Performance assessment

In general, we found that `haskell_module` can build faster than
`haskell_library` alone whether builds are incremental or not. The
typical setup with `haskell_library` doesn't try to use multicore
support in `ghc`, and thus the finer-grained `haskell_module` rules
would use more parallelism. For instance, building [Cabal with
`haskell_library`][cabal_bazel] alone takes 2 minutes, whereas using
`haskell_module` and 8 CPUs takes 1 minute.

When compared to `stack` or `cabal-install`, `haskell_module` can
do faster on builds from scratch. This is, again, because Bazel can
use more parallelism than the Haskell-specific tools, which usually
would involve at most one CPU per package.

When doing incremental builds, though, both `stack` and `cabal-install`
can use the recompilation checker, and for changes deep in the
dependency graph with little propagation, `haskell_module` is not able
to beat them yet. For changes near the build targets, or which force
more recompilation, `haskell_module` would be more competitive.

## Closing remarks

This project was possible thanks to the generous funding from
[Symbiont][symbiont] and their test bed for the initial
implementation. In this post we showed how `haskell_module` and
`gazelle_haskell_module` can be used to have incremental builds
and more build parallelism than was possible with existing tools.
In combination with [`gazelle_cabal`][gazelle_cabal_post], it is
easier to migrate a Haskell project to use Bazel these days, and
then to generate finely-grained `haskell_module` rules with
`gazelle_haskell_modules`. We look forward to hearing from your
experience with these tools and receiving your contributions!

[bazel]: https://bazel.build
[cabal]: https://www.haskell.org/cabal
[cabal_bazel]: https://github.com/facundominguez/cabal/tree/fd/bazel
[cabal-rules]: https://api.haskell.build/haskell/cabal.html
[gazelle]: https://github.com/bazelbuild/bazel-gazelle
[gazelle_cabal]: https://github.com/tweag/gazelle_cabal
[gazelle_cabal_post]: https://www.tweag.io/blog/2021-07-28-gazelle-cabal/
[gazelle_haskell_modules]: https://github.com/tweag/gazelle_haskell_modules
[haskell_module]: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#building-incrementally
[haskell_module_worker]: https://github.com/tweag/rules_haskell/issues/1758
[pending_unused_inputs_list]: https://github.com/tweag/rules_haskell/issues/1760
[stack]: https://docs.haskellstack.org/en/stable/README/
[recompilation_checker]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/recompilation-avoidance
[recompilation_avoidance]: https://www.tweag.io/blog/2022-11-03-blog_recompilation/
[reuse_sandbox_directories]: https://bazel.build/reference/command-line-reference#flag--experimental_reuse_sandbox_directories
[rules_haskell]: https://github.com/tweag/rules_haskell
[sandbox_base]: https://bazel.build/reference/command-line-reference#flag--sandbox_base
[symbiont]: https://symbiont.io
[unused_inputs_list]: https://bazel.build/rules/lib/actions#run
