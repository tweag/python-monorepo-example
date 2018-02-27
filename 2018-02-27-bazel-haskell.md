---
title: Build large polyglot projects with Bazel... <br/>now with Haskell support
shortTitle: Bazel now supports building Haskell
author: Mathieu Boespflug, Mark Karpov, Mateusz Kowalczyk
---

Publishing code to the world is easy these days: take your code, tack
some extra metadata onto it, call that a package and upload it
to [npmjs.com][npmjs] / [crates.io][crates-io] / [Hackage][hackage] /
etc. It's also easy for other developers to include your code as part
of their project: build tools are available to pull your package from
where you published it, along with any packages that yours in turn
depends on, and build everything in the right order. Haskellers have
the good fortune of having at least two great options to choose from
when it comes to build tools: Stack and cabal-install. Both tools
support building Haskell code, but what they also have in common is
they are very much Haskell-centric.

In short, these tools work great for projects that are a) meant to be
open source or easily open-sourceable, b) small and c) mostly or
exclusively Haskell. Over the past couple of months, we have been
focused on building a solution for the opposite use case: corporate
monorepos, which are a) closed source b) large and c) almost always
a mix of very many languages. It turns out that Google, Facebook,
Twitter and many others have needed a solution to this use case for
many years now. They came up
with [Bazel][bazel-home], [Buck][buck-home], [Pants][pants-home] and
many others, respectively. So all we needed to do
was [add Haskell support][rules_haskell-home] to one of these existing
solutions. We chose Google's [Bazel][bazel-home].

[npmjs]: https://www.npmjs.com/
[crates-io]: https://crates.io/
[hackage]: https://hackage.haskell.org/
[bazel-home]: https://bazel.build/
[buck-home]: https://buckbuild.com/
[pants-home]: https://www.pantsbuild.org/
[rules_haskell-home]: https://github.com/tweag/rules_haskell

## Polyglot monorepos

Beyond a certain size, it's unrealistic to expect that the most cost
effective way to implement a project is to do so using only one
language. On many of our customer projects, in practice we end up with
a mix of Haskell, Java, Scala, C/C++, R, Python and even FORTRAN.
Quite simply because it's much cheaper to reuse existing code, or to
play on the particular strengths of a programming language, than to
reimplement everything using a uniform stack.

Case in point: we have multiple projects involving compute-intensive
models of (bio)physical phenomena. These models are typically
implemented in Haskell, with some basic numerical routines offloaded
to C and FORTRAN. For faster response times, we transparently
distribute these workloads on multiple machines
using [Apache Spark][spark], which is written in Scala and Java. We do
so using [sparkle][sparkle], which in turn
uses [inline-java][inline-java] and related packages for
high-performance bridging between Haskell and the JVM.

What Spark wants is a full-fledged application with a Java-style
`main` function as the entrypoint,

```java
public static void main(String[] args) {
    // Call Haskell code
	...
}
```

packaged as a [JAR file][jar-format]. So to build an application that
we can run on many nodes of a large cluster, we need to build C or
FORTRAN numerical routines as a library, build a Haskell binary
dynamically linked to said library, then build the Java glue code as
a JAR and finally perform cunning tricks to inject the Haskell binary
into the resulting JAR.

Today, we do this with the help of multiple different build systems
(one for each implementation language used) that have no knowledge of
each other, along with some ad hoc scripting. Each build system
represents build targets as nodes in a dependency graph. But with this
situation the dependency graph of each build system is entirely opaque
to every other system. No single system has an overall view of the
dependency graph. This situation has a number of drawbacks:

* Continuous integration builds are **slow**, due to lost
  parallelization opportunities. If you have many cores available, the
  more a build system has detailed information about dependencies, the
  more it can parallelize execution of build tasks on all cores
  throughout. For example, Stack and cabal-install can build multiple
  packages in parallel, but when the package dependency graph is
  mostly one long chain, packages will be built one after the other on
  a single core. It's tempting to parallelize *both* at the
  Stack/Cabal level and the GHC level, but that can lead to
  oversubscribing memory and CPU cores and ultimately, thrashing (not
  to mention [enduring scaling bugs][ghc-make-slowdown] killing
  throughput).
* Partial rebuilds are often **wrong**, because it's hard to verify
  that all dependencies across build systems were *accurately
  declared*. For example, if Cabal doesn't know that Java files are
  also dependencies and Gradle needs to be called again if they
  change, then partial rebuilds might lead to incorrect results.
* Conversely opportunities for partial rebuilds instead of full
  rebuilds are sometimes lost, leading to **slow rebuilds**. This
  happens when dependencies are accurately declared, but not
  *precisely declared*. For instance, it's a shame to rebuild all of
  dependent package B, along with anything that depends on it, after
  a simple change to package A that did not cause any of the interface
  files for modules in A to change.
* The resulting system is brittle, complex and therefore **hard to
  maintain**, because it's an accumulation of multiple build
  subsystems configured in ways custom to each language. You have
  a `stack.yaml` file in YAML syntax for Stack, a `package.cabal` file
  in Cabal syntax for each package, a `build.gradle` file that is
  a Groovy script or a `pom.xml` file in XML syntax for Java packages,
  `Makefile`'s and autoconf-hell for C/C++ etc.

Wouldn't it be great if instead of this inefficient mishmash of build
system stuff, we could have a *single build system* dealing with
everything, using a *single configuration* drawn from files in
a *uniform and easy-to-read* syntax? Wouldn't it be great if these
build system configuration files were all very short instantiations of
standard rules succinctly encapsulating best practices for how to
build C/C++ libraries, Java or Scala packages, Haskell apps, etc?

What we're after is a uniform way to locate, navigate and build code,
no matter the language. A good first step in this direction is to
store all code in a single repository. Dan
Luu [says][dan-luu-monorepo] it this way:

> With a monorepo, projects can be organized and grouped together in
> whatever way you find to be most logically consistent, and not just
> because your version control system forces you to organize things in
> a particular way. [...] A side effect of the simplified organization
> is that it’s easier to navigate projects. The monorepos I’ve used
> let you essentially navigate as if everything is on a networked file
> system, re-using the idiom that’s used to navigate within projects.
> Multi repo setups usually have two separate levels of navigation –
> the filesystem idiom that’s used inside projects, and then
> a meta-level for navigating between projects.

All code in your entire company universe is readily accessible with
a simple `cd some/code/somewhere`. It then becomes natural to make
each code directory map to one code component buildable independently
and uniformly. All we need is a build system that can scale to
building all the code everywhere as fast as possible, or indeed any
component of it, where code components simply map to directories on
the filesystem. We expect build configuration to be modular: each code
component is packaged in a single directory with a single, succinct,
declarative `BUILD` file describing how to build everything inside
using always the same build configuration syntax. No brittle
multi-language build scripts that require specialist knowledge to hack
and induce a terrible bus factor on project maintenance and
development.

That's how development can scale to large sizes. In fact these
monorepos can grow [very large indeed][google-monorepo].

[inline-java]: https://github.com/tweag/inline-java
[spark]: https://spark.apache.org/
[sparkle]: https://github.com/tweag/sparkle
[dan-luu-monorepo]: https://danluu.com/monorepo/
[google-monorepo]: https://cacm.acm.org/magazines/2016/7/204032-why-google-stores-billions-of-lines-of-code-in-a-single-repository/fulltext
[twitter-monorepo]: http://www.gigamonkeys.com/flowers/
[jar-format]: https://en.wikipedia.org/wiki/JAR_(file_format)
[ghc-make-slowdown]: https://ghc.haskell.org/trac/ghc/ticket/9221

## Google Bazel

One large monorepo facilitates writing tooling that will work with the
entire thing. Google did just that with their Blaze build system, open
sourced in 2015 as [Bazel][bazel-home] (the two systems are
technically distinct but share most of the code). As we touched upon
above, a crucial property of building code at scale is that partial
rebuilds should be *correct* and *fast*. This implies that
dependencies should be declared accurately and precisely
(respectively).

Bazel tries hard to offer guarantees that dependencies are at least
complete (this property is called *build hermiticity*). It does so by
sandboxing builds, making only the declared inputs to a build action
available. In this way, if the build action in reality depends on any
undeclared inputs, the build action will consistently fail, because
anything outside of the sandbox is simply not available. This has an
important consequence: even with huge amounts of code, developers can
be very confident indeed that partial rebuilds will yield exactly the
same result as full rebuilds, as should be the case if all
dependencies are correctly specified. Developers never need to `make
clean` just in case, so very seldom need to ever wait for a full
rebuild.

Better still, Bazel has good support for local and distributed
caching, optimized dependency analysis and parallel execution, so you
get fast and incremental builds provided dependencies are declared
precisely. Bazel is clever. Rules can have multiple outputs produced
by sequences of actions, but Bazel will only rerun precisely those
actions that produce the smallest subset of outputs strictly necessary
to build the given top-level target given on the command-line.

Bazel is very serious about recompilation avoidance. Because it knows
the entirety of the dependency graph, it can detect precisely which
parts of the build graph are invariant under a rebuild of some of the
depndencies. For example, it will relink your Haskell app but won't
rebuild any Haskell code if only a C file changed without altering any
C header files, even if this was a C file of a package deep in the
dependency graph.

By using Bazel, you get to piggyback on all the work towards
performance tuning, improving scalability, and tooling development
(static analysis, documentation generators, debuggers, profilers, etc)
by Google engineers over a period of nearly 10 years. And since its
open sourcing, that of a community of engineers at companies using
Bazel, such as Stripe, Uber, Asana, Dropbox etc.

Better still, Bazel already has support for building a variety of
languages, including C/C++, Rust, Scala, Java, Objective-C, etc. By
using Bazel, we get to reuse best practices for building each of these
languages. And focus entirely on Haskell support.

## How we added Haskell support

Bazel uses a subset of Python syntax for `BUILD` files. Each
"component" in your project typically gets one such file. Here is
an [example build description][inline-java-build] for inline-java,
involving the creation of one C library and one Haskell library:

```python
cc_library(
  name = "bctable",
  srcs = ["cbits/bctable.c"],
  hdrs = ["cbits/bctable.h"],
)

haskell_library(
  name = "inline-java",
  src_strip_prefix = "src",
  srcs = glob(['src/**/*.hs', 'src/**/*.hsc']),
  deps = [ "//jni", "//jvm", ":bctable"],
  prebuilt_dependencies = [
    "base",
    "bytestring",
    ...
    "template-haskell",
    "temporary",
  ],
)
```

That's it! Each target definition is an instantiation of some rule
(`cc_library`, `haskell_library`, etc). Each rule is either primitive
to Bazel or implemented in an extension language (also a subset of
Python) called [Skylark][skylark-doc]. These rules are not meant to be
highly configurable or particularly general: they are meant to capture
best practices once and for all. This is why `BUILD` files are
typically quite small. Unlike a `Makefile`, you won't find actual
commands in a `BUILD` file. These focus purely on the *what*, not the
*how*, which is fairly constrained, by design.

We have [rules for building Haskell][haskell-build] libraries
(packages), binaries, tests and Haddock documentation. Libraries or
binaries can freely provide C/C++ or Java targets as dependencies, or
use preprocessors such as `hsc2hs`. Yet all in all, the entire support
for building Haskell currently weighs in
at [barely 2,000 lines of Skylark code][rules_haskell-code]. The heavy
lifting is done by Bazel itself, which implements once and for all, in
a way common to all languages, `BUILD` file evaluation, dependency
graph analysis and parallel execution.

[inline-java-build]: https://github.com/tweag/inline-java/blob/master/BUILD
[haskell-build]: http://haskell.build/
[rules_haskell-code]: https://github.com/tweag/rules_haskell/tree/master/haskell
[skylark-doc]: https://docs.bazel.build/versions/master/skylark/language.html

## Next steps

At this point, [rules_haskell][rules_haskell-home], the set of Bazel
rules for Haskell, is in Beta. We've been internally dog fooding it on
a few projects, and today [Adjoint.io][adjoint-io] and a few others
have already deployed it for their CI. We have not implemented special
support for building packages from Hackage, instead relying on
existing tools (in particular Nix) to provide those. We use Bazel to
build our own code only, not upstream dependencies. But there are
experiments underway to use Bazel for upstream dependencies as well.
We encourage you to experiment with rules_haskell
and [report any issues][rules_haskell-issues] you find. We'd love your
feedback!

In future posts, we'll explore:

* using Bazel's remote cache to very aggressively yet safely cache
  everything that can be to further speed up CI builds,
* playing on the respective strengths of both [Bazel][bazel-home]
  and [Nix][nix-home] for truly reproducible builds,
* Packaging deployable binaries and containers.

[adjoint-io]: https://www.adjoint.io/
[rules_haskell-issues]: https://github.com/tweag/rules_haskell/issues
[nix-home]: https://nixos.org/nix/
