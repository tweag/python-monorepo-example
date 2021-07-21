---
title: "How to build hybrid Haskell and Java programs"
shortTitle: "Haskell and Java built with Bazel"
author: Facundo Domínguez, Andreas Herrmann
tags: [haskell, inline-java, bazel]
description: "Discussion on using Bazel with inline-java"
---

When working in a sufficiently large Haskell project,
one eventually faces the question of whether to
implement missing libraries in Haskell or borrow the implementation from
another language, such as Java.

For a long time now, it has been possible to combine Haskell and
Java programs using [inline-java][inline-java].
Calling Java functions from Haskell is easy enough.

```Haskell
import Language.Java (J, JType(Class)) -- from jvm
import Language.Java.Inline (imports, java) -- from inline-java

imports "org.apache.commons.collections4.map.*"

createOrderedMap :: IO (J ('Class "org.apache.commons.collections4.OrderedMap"))
createOrderedMap = [java| new LinkedMap() |]
```

The `ghc` and `javac` compilers can cooperate to build this program provided
that both know where to find the program's dependencies. Back in the day, when
`inline-java` was being born, there was no build tool capable of
pulling the dependencies of both Haskell and Java, or at least not
without additional customization. Since then, however, Tweag has invested effort into
enabling [Bazel][bazel], a polyglot build system, to build Haskell
programs. In this blog post we go over the problems of integrating
build tools designed for different languages, and how they can be
addressed with Bazel, as an example of a single tool that builds them
all. More specifically, this post also serves as a tutorial for using
`inline-java` with Bazel, which is a requirement for [the latest][inline-java-0.11]
`inline-java` release.

## Dependencies done the hard way

Suppose we rely on [cabal-install][cabal] or [stack][stack] to install
the Haskell dependencies. This would make the `inline-java`
and `jvm` packages available. But these tools are specialized to build Haskell
packages. If our program also depends on the `commons-collections4`
Java package, we can't rely on Cabal to build it. We need help from some other Java-specific package manager.

We could rely on [maven][maven] or [gradle][gradle] to install
`common-collections4`. At that point we can build our project by
invoking `ghc` and telling `javac` where to find the java dependencies
in the system via environment variables (i.e. `CLASSPATH`).

With some extra work, we could even coordinate the build systems so
one calls to the other to collect all the necessary dependencies and
invoke the compilers. This is, in fact, what `inline-java` did until
recently.

But there is a severe limitation to this approach: no build system can
track changes to files in the jurisdiction of the other build system.
The `Cabal`-based build system is not going to notice if we change the
`gradle` configuration to build a different version of
`common-collections4`, or if we change a source file on the Java side.
Easy enough, we could run `gradle` every time we want to rebuild our
program, just in case something changed in the Java side. But then,
should the Haskell build system rebuild the Haskell side? Or should it
reuse the artifacts produced in an earlier build?

We could continue to extend the integration between build systems
so one can detect if the artifacts produced by the other have changed.
Unfortunately, this is a non-trivial task and leads to reimplementing
features that build systems already implement for their respective
languages. We would be responsible for detecting changes on every
dependency crossing the language boundary.

If that didn't sound bad enough, incremental builds is not the only
concern requiring coordination. Running tests, building deployable
artifacts, remote builds and caches, also involve both build systems.

## Dependencies with a polyglot build system

A straightforward answer is
to use only one build system to build all languages in the project.
We chose to turn to Bazel for our building needs.

Bazel lets us express dependencies between artifacts
written in various languages. In this respect, it is similar
to `make`. However, Bazel comes equipped with sets of rules, such as [rules_haskell][rules-haskell],
for many languages, which know how to invoke compilers and
linkers; these rules are distributed as libraries and are reusable across projects. With `make`,
the user must manually encode all of this knowledge in a `Makefile`
herself. It's not the subject of this blog post, but Bazel comes with
a number of other perks, such as hermeticity of builds for
reproducibility, [distributed
builds](https://www.buildbuddy.io/), and [remote caching][remote-caching].

We offer a [working example][classpath-example] in the `inline-java`
repository.
In order to specify how to build our Haskell program, we start by importing
a rule to build Haskell binaries from [rules_haskell][rules-haskell].

```python
# file: BUILD.bazel
load(
  "@rules_haskell//haskell:defs.bzl",
  "haskell_binary",
)

haskell_binary(
    name = "example",
    srcs = ['Main.hs'],
    extra_srcs = ["@openjdk//:rpath"],
    compiler_flags = [
        "-optl-Wl,@$(location @openjdk//:libjvm.so)",
        "-threaded",
    ],
    deps = [
        "//jvm",
        "//jni",
        "//:inline-java",
        "@rules_haskell//tools/runfiles",
        "@stackage//:base",
        "@stackage//:text",
    ] + java_deps,
    data = [ ":jar_deploy.jar" ],
    plugins = ["//:inline-java-plugin"],
)

java_deps = [
    "@maven//:org_apache_commons_commons_collections4_4_1",
    ]
```

The `load` instruction is all we need to do to invoke the
`rules_haskell` library and access its various rules. Here we use the
`haskell_binary` rule to build our hybrid program.

Besides the fact that `Main.hs` is written in both Haskell and Java,
one can tell the hybrid nature of the artifact by observing the
dependencies in the `deps` attribute, which refers to both Haskell
and Java libraries.

- `//jvm`, `//jni` and `//:inline-java` refer to Haskell libraries
  implemented in the current repository
- `@rules_haskell//tools/runfiles` refers to a special
  Haskell library defined as part of the `rules_haskell` rule
  set. More on this below.
- `@stackage//:base` and `@stackage//:text` refer to Haskell packages
  coming from `stackage`
- `@maven//:org_apache_commons_commons_collections4_4_1` is a Java
  library coming from a `maven` repository

[Additional configuration in the WORKSPACE file][workspace-file]
makes precise the location of dependencies coming from `rules_haskell`,
`stackage` and `maven`, where the `stackage` snapshot and the list of
`maven` repositories is specified.
More information on the integration with `stackage` can be found in
[an earlier post][bazel-stackage] and with `maven` in the
[documentation of `rules_jvm_external`][bazel-maven].

While the dependencies in `deps` are made available at build time,
the attribute `data = [":jar_deploy.jar"]` declares a runtime
dependency. Specifically, it makes the Java artifacts available to the
Haskell program.
The `":jar_deploy.jar"` target is a `jar` file produced with the following
rule from the Java rule set.
A future version of `rules_haskell` may generate this runtime
dependency automatically, but for the time being we need to add it manually.

```python
java_binary(
    name = "jar",
    main_class = "dummy",
    runtime_deps = java_deps,
)
```

Now, when there are changes in the Java dependencies, Bazel will know
to rebuild the Haskell artifacts, and only if there were changes.

Bazel makes sure that `jar_deploy.jar` is built, and stores it in an
appropriate location. But nothing, so far, tells the Haskell
program where to find this file. This is where the `runfiles` library
comes into play. Bazel lays out runtime dependencies, such as
`jar_deploy.jar` following known rules; the `runfiles` library
abstracts over these rules. To complete our example, we need the `main`
function to call to the `runfiles` library and discover the `jar`
file's location.

```Haskell
import qualified Bazel.Runfiles as Runfiles
import Data.String (fromString)
import Language.Java (J, JType(Class), withJVM)
...

main = do
    r <- Runfiles.create
    let jarPath = Runfiles.rlocation r "my_workspace/example/jar_deploy.jar"
    withJVM [ "-Djava.class.path=" <> fromString jarPath ] $
      void createOrderedMap
```

## Closing thoughts

Mixing languages is challenging both from the perspective of the
compiler and of the build
system. In this blog post we provided an overview of the
challenges of integrating build systems, and how a unifying build
system can offer a more practical framework for reusing
language-specific knowledge.

Bazel is a materialization of this unifying build system, where rule
sets are the units of reuse. We recently moved `inline-java` builds
to rely on Bazel, by depending strongly on the rule sets for
Haskell and Java. This implies that end users will also have to use Bazel.
Though this means departing from `stack` and `cabal-install`,
the build tools that Haskellers are used to, we hope that it will
offer a better path for adopters to build their multi-language projects.
And since `rules_haskell` [can still build][rules-haskell-cabal]
`Cabal` packages and `stackage` snapshots via Cabal-the-library, going
with Bazel doesn't forego the community effort invested in curating
the many packages in the Haskell ecosystem.

[bazel]: https://bazel.build/
[bazel-maven]: https://github.com/bazelbuild/rules_jvm_external#usage
[bazel-stackage]: https://www.tweag.io/blog/2019-10-09-bazel-cabal-stack/
[rules-haskell-cabal]: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#building-cabal-packages
[cabal]: https://www.haskell.org/cabal/
[classpath-example]: https://github.com/tweag/inline-java/tree/master/examples/classpath
[inline-java]: https://www.tweag.io/blog/tags/inline-java
[inline-java-0.11]: https://github.com/tweag/inline-java/releases/tag/v0.11.0
[gradle]: https://gradle.org/
[maven]: https://maven.apache.org/
[remote-caching]: https://www.tweag.io/blog/2020-04-09-bazel-remote-cache/
[rules-haskell]: https://haskell.build/
[stack]: https://www.haskellstack.org
[workspace-file]: https://github.com/tweag/inline-java/blob/e661e87d4f70a51af657659305cd80dd155a9647/WORKSPACE
