---
title: "Bazel's Persistent Worker Mode for GHC: <br/>An Industrial Internship"
shortTitle: "Bazel's Persistent Worker Mode for GHC"
author: Artem Pelenitsyn
tags: internship, bazel, haskell
description: "I got the opportunity to work on Bazel's Persistent Worker Mode for GHC during my internship at Tweag. My goal was to improve the mode of communication between Bazel and the Haskell GHC compiler."
---

I got the opportunity to work on Bazel's Persistent Worker Mode for [Haskell GHC][ghc] during my internship at Tweag. Let's begin with some context. The [`rules_haskell` project][haskell-build] adds support for Haskell components in software based on the [Bazel build system][bazel-build]. By default, compiling an individual Haskell target triggers a separate sandboxed GHC invocation. This approach is not optimal for two reasons: the recurring cost of compiler startups and the potential loss in incremental builds.
Bazel has a special mode of communication with a compiler to resolve the issue. My internship goal was to improve the method of communication between Bazel and the Haskell GHC compiler by adding support for this _persistent worker mode_ in  `rules_haskell`. Let's explore what I learned and what I was able to accomplish.

[haskell-build]: https://haskell.build
[bazel-build]: https://bazel.build
[ghc]: https://www.haskell.org/ghc/
## Call for Persistent Compilers

Consider the following example of a C++ application build script in Bazel.


```python
cc_library(
    name = "Lib",
    srcs = ["A.cpp", "B.cpp"]
)
cc_binary(
    name = "Bin",
    srcs = ["Main.cpp"]
    deps = [":Lib"]
)
```

Bazel's built-in `cc_library` and `cc_binary` are rules to describe C++ build targets. We have two targets in this application, called `Lib` and `Bin`. The `Lib` library target depends on two source files, `A.cpp` and `B.cpp`; the `Bin` binary target depends on `Lib` and the `Main.cpp` source file.

Bazel controls the order in which targets are built and does not depend on a programming language in question. But how a target is built, does, hence the names like `cc_binary`, `haskell_binary`, etc. For instance, each Haskell target is built with just a pair of calls to a compiler (one for compiling and one for linking). In the case of C++, however, every file is compiled by a separate call to a compiler. On the one hand, the one-call-per-file strategy wastes time on repetitive compiler startups, which may form a significant cost in languages like Scala or Haskell but not a big deal for C++. On the other hand, this strategy creates an additional opportunity for improved incremental building. Let's consider each of these two observations in more detail.

### Startup Times Saved

The opportunity to save on startup times was pointed out in [_Persistent Worker Processes for Bazel_][bazel-worker-blog], the original Bazel Blog post on the topic. The Bazel team demonstrated the significant benefits of using a persistent compiler process for Java, where startups are expensive and JIT needs runtime stats to do its job effectively. Other JVM languages, e.g., Scala and Kotlin, followed this path.

[bazel-worker-blog]: https://blog.bazel.build/2015/12/10/java-workers.html

Today, many of the main Bazel-enabled languages hosted under [`bazelbuild`][bazelbuild-org] have persistent workers, but not all of them benefit from warm startup and caching JIT data as much as the JVM-based languages do. Luckily, there is another way to improve performance with a persistent compiler process, namely, reusing auxiliary build artifacts that did not change since the last build—incremental builds.

### Chasing Incremental Builds

Fast, correct incremental builds are such a fundamental Bazel goal they're the first thing mentioned in [_Why Bazel?_](bazel-build) at the top of Bazel's homepage. To fulfill this promise, though, Bazel needs sufficient knowledge about dependencies between the build artifacts. Let's get back to our example to explain this better.

[bazel-build]: https://bazel.build/

After the `Bin` target has been built once, any change to `Main.cpp` would require recompiling `Main.cpp` and relinking `Bin`, but does not require rebuilding `Lib`. This is the _inter-target incrementality_ supported by Bazel universally: no specific knowledge about the programming language is needed, and the logic fully translates to `rules_haskell` and its `haskell_library`/`haskell_binary` rules.

The difference comes when after a full build you make a change in, e.g., `A.cpp`. As we know, `.cpp` files are compiled separately, and the knowledge is encoded in the `cc_library`; therefore, Bazel would only recompile `A.cpp`, but not `B.cpp`. In contrast, the [recompilation strategy for Haskell is rather subtle][ghc-wiki-recomp]: whether you need to recompile the module `B` in similar circumstances roughly depends on whether there is an import of `A` inside `B`. This goes beyond the knowledge of `haskell_library`, and the rule will simply recompile all the modules in the `Lib` target. The bottom-line is: due to the difference in the language nature, Bazel supports _sub-target incrementality_ for C++ components but not for Haskell components.

[bazelbuild-org]: https://github.com/bazelbuild/
[ghc-modes]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html#modes-of-operation
[ghc-wiki-recomp]: https://gitlab.haskell.org/ghc/ghc/wikis/commentary/compiler/recompilation-avoidance

Is it possible to get better incremental builds for Haskell components? Almost certainly, yes. In fact, this was one of the driving powers of the project. The persistent worker mode opens an opportunity for, first, the sub-target dependency analysis using GHC API and, second, caching of auxiliary build artifacts (e.g., `.hi` and `.o` files) to save work during rebuilds.

Unfortunately, it's hard to get the implementation of incremental builds right, and only a few Bazel-aware languages support sub-target incremental builds (e.g., `rules_swift` and one of several [`rules_scala` forks][rules-scsala-zinc] that employ the [Zinc compiler][zinc]). I did not get to implementing incremental builds in my project. I spent most of my time finding the right way to integrate the worker mode into `rules_haskell`.

[rules-scsala-zinc]: https://github.com/higherkindness/rules_scala#readme
[zinc]: https://github.com/sbt/zinc#readme

## Worker Mechanics

### First Step: Bazel Interface

When Bazel encounters a compile-like command for the first time, it spawns a persistent process. This child process gets its `stdin`/`stdout` redirected to talk directly to Bazel. The process listens on its `stdin` for a compilation request and upon processing one, sends back a response. Bazel speaks to its workers in a simple [Protobuf-based protocol][worker-protocol]: a request is a list of filepaths with the corresponding file hashes; a response is an exit code and a string with other textual output the worker decides to report (e.g., compiler's warning messages).

All in all, this scheme looks straightforward except an IPC solution based on `stdin`/`stdout` complicates debugging by an order of magnitude. For example, sometimes GHC sends diagnostic messages to `stdout` instead of `stderr`, and sometimes you cannot mute such messages ([I solved][ghc-loading-message-pr] one [particularly annoying instance][ghc-loading-message-issue] of the problem during this work). One might hope redirecting `stdout` helps, but some standard tricks may fail for all sorts of reasons; e.g., concurrency employed by Bazel [bit me][ghc-hdup-issue] when running `rules_haskell`'s test suite under the worker strategy.

[ghc-loading-message-pr]: https://gitlab.haskell.org/ghc/ghc/merge_requests/1308
[ghc-loading-message-issue]: https://gitlab.haskell.org/ghc/ghc/issues/16879
[ghc-hdup-issue]: https://gitlab.haskell.org/ghc/ghc/issues/16819
[worker-protocol]: https://github.com/bazelbuild/bazel/blob/master/src/main/protobuf/worker_protocol.proto

### Second Step: Protobuf for Haskell

Several libraries implement support for Protobuf in the Haskell ecosystem. We chose [`proto-lens`][proto-lens] which allows us to generate Haskell definitions from a `.proto` description and conveniently access data with lenses. 

One obstacle with `proto-lens` was that they silently (and [unconsciously, it seems][proto-lens-issue]) dropped support for parsing messages from an unbounded stream. That means once you have a handle to read in a message from, you have to specify the size of the bytestring you're going to read before the parser can get its hands on it. The length of a message is variable and encoded as a [variable-length integer][protobuf-varint] sent in front of every message. The `proto-lens` library had internal machinery to read `varint`s but lacked a reasonable interface to employ it when receiving messages. [I fixed this][proto-lens-pr].

[protobuf-varint]: https://developers.google.com/protocol-buffers/docs/encoding
[proto-lens]: http://hackage.haskell.org/package/proto-lens
[proto-lens-pr]: https://github.com/google/proto-lens/pull/324
[proto-lens-issue]: https://github.com/google/proto-lens/issues/61

### Third Step: GHC API

The worker application is a simple single-threaded server creating a fresh GHC session for every request. One issue I hit when employing GHC API to the `rules_haskell` use case is that we use separate GHC calls to compile and then link. The Hello-World example for using the GHC API in the GHC User Guide does not cover the latter use case (where you should run GHC in the "one-shot" instead of the `--make` mode), and I ended up copying some parts of the GHC driver to support this use case, since GHC doesn't export its [driver module][ghc-driver], unfortunately.

[ghc-driver]: https://gitlab.haskell.org/ghc/ghc/blob/master/ghc/Main.hs

## Integration and Tests

Since version 0.27 (June 2019) Bazel picks the worker strategy as the default one if it is available for action at all. Finding a convenient way to override the default was not straightforward—I had to rework the solution several times through [both][rh-pr-1] of [my PRs][rh-pr-2] to `rules_haskell`.

[worker-no-tools]: https://groups.google.com/forum/#!topic/bazel-dev/qXmYU7EymrM
[rh-pr-1]: https://github.com/tweag/rules_haskell/pull/1024
[rh-pr-2]: https://github.com/tweag/rules_haskell/pull/1055

The final version of the interface to activate the worker mode consists (as now described in the [`rules_haskell` docs][worker-doc]) of a couple of actions: one has to, first, load the worker's dependencies in the `WORKSPACE` file, and, second, pass a command-line argument when starting the build:

```bash
bazel build my_target --define use_worker=True
```

The `--define` syntax is heavyweight due to Bazel's [initial reluctance][bazel-custom-keys] to provide user-defined command-line arguments. Recently, Bazel added special support for this feature but, [as I discovered][bazel-build-configs-issue], the implementation of the feature has issues in realistic applications going beyond Hello-World.

[bazel-custom-keys]: https://docs.bazel.build/versions/master/configurable-attributes.html#custom-keys
[worker-doc]: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#persistent-worker-mode-experimental
[bazel-build-configs-issue]: https://github.com/bazelbuild/bazel/issues/9177

The worker mode passed the whole test suite without a glitch from the first time; this is impressive given that the test suite contains tricky examples, e.g., with GHCi, C dependencies, and even GHC plugins. There's only one gotcha to take into account: the worker is not sandboxed by default. In some cases, GHC prefers rebuilding a perfectly valid target when it has access to the target's source. This will fail if GHC is not provided with sufficient dependencies. There were about 4 test cases out of 96 that failed due to this. The solution is to simply always use sandboxing passing `--worker_sandboxing` in the command-line.

We did not get to rigorous performance measurements, but there are some promising observations even for the current implementation lacking sub-target incrementality. First of all, I assembled a sample ten module project where each module held one function calling the function from the previous module. Every module turned into a separate target in the `BUILD` script, forming a deep target dependency tree. For this setup, I observed 10–15% speedup for the worker-enabled version of `rules_haskell` (excluding the time for building the worker). On the other hand, running the `rules_haskell` test suite did not show significant improvements on the worker-enabled version (2–3% speedup). I attribute this difference to two features of the test suite: first, the suite holds a fair amount of non-Haskell code, which dims the worker effect on build time; second, the suite represents a very shallow dependency graph, unlike in the first experiment with ten modules. Overall, there is a hope for a speedup in projects with deep dependency graphs.

## GHC Persistent Worker in Context

There are many efforts underway to make GHC friendlier in various client-server kind of scenarios including IDEs (e.g., [HIE][hie] and now [hie-bios][hie-bios] in [ghcide][ghc-ide]), interactive sessions (e.g., this [issue][multipackage-issue] and this [PR][multipackage-pr]), and, finally, build systems (e.g., the recent [Extended Dependency Generation proposal][dep-gen-proposal] by David Eichmann and his work on [cloud builds for the Shake-based GHC build system Hadrian][cloud-hadrian]). Indeed, we can now see some level of the convergence [foreseen by Edward Z. Yang][ezyang-convergence] among compilers, build systems, and IDEs happening in the Haskellverse today.

[hie]: https://github.com/haskell/haskell-ide-engine#readme
[hie-bios]: https://github.com/mpickering/hie-bios#readme
[ghc-ide]: https://github.com/digital-asset/ghcide#readme
[gsoc-hie]: https://summerofcode.withgoogle.com/projects/#6687588310581248
[ezyang-convergence]: http://blog.ezyang.com/2015/12/the-convergence-of-compilers-build-systems-and-package-managers/
[multipackage-issue]: https://gitlab.haskell.org/ghc/ghc/issues/10827
[multipackage-pr]: https://gitlab.haskell.org/ghc/ghc/merge_requests/935
[dep-gen-proposal]: https://github.com/ghc-proposals/ghc-proposals/pull/245
[cloud-hadrian]: https://well-typed.com/blog/2019/08/exploring-cloud-builds-in-hadrian/

What about incremental builds? David's findings suggest: GHC's ability to communicate detailed source file dependencies allows for fine-grained control over the build process. Under his proposal, the build system might get to decide when to call GHC and only ever call it in the "one-shot" mode. This decision logic could hardly fit in `rules_haskell` main code but seems perfectly relevant to the worker implementation.

Although I did not get to incremental builds, I did some experiments with warm GHC startups. None of those ended up in the current implementation since I think there is room for improvement here. I believe one of the possible ways to improve GHC session startup times is caching package data for packages loaded from the global package database. To me, this loading stage looks like the most expensive action during GHC session initialization. Sadly, I found there were not enough utilities exported from GHC's `Packages.hs` to tune this behavior.

## Acknowledgments

I'm grateful to Tweag for giving me an exciting opportunity to work in an industrial setting; to Mathieu Boespflug for suggesting a project that not only kindled my Haskell passion but also pushed me outside my comfort zone to learn a new thing (Bazel); to Andreas Herrmann, my mentor in this project, for providing endless insightful help and feedback; to all other Tweagers, who either directly helped me or brought me excitement by demonstrating their exceptional engineering skills as well as creativity.

## Annotated References

Here's a summary of my contributions and some pointers to possible future directions for improvement of the persistent worker mode in `rules_haskell`.

1. Worker pull requests to `rules_haskell`: [[1]][rh-pr-1], [[2]][rh-pr-1]. The second one adds the worker sources and reworks good part of the first because the initial strategy to implement the switch between the regular and the worker modes forced the user to download worker dependencies anyway. The current strategy based on `config_setting`/`select` does not have the flaw. It can be improved when the [Bazel Custom keys][bazel-custom-keys] issue is resolved.

2. [The initial worker repository][worker-repo]. Unlike its replica inside `rules_haskell`, which just holds static Protobuf descriptions in Haskell, the repository implements proper generation of those descriptions from a `.proto` file. Notably, the repository contains the [`reuse-ghc-session`][worker-repo-session-reuse] branch, which explores a warm startup of a GHC session. It is blocked because once all package databases are loaded into a session, they cannot be easily unloaded with just the utilities exported from GHC's `Packages.hs`.

[worker-repo]: https://github.com/tweag/bazel-worker#readme
[worker-repo-session-reuse]: https://github.com/tweag/bazel-worker/tree/reuse-ghc-session

3. [GHC's `hDuplicate` issue][ghc-hdup-issue], which makes it harder to design protocols around `stdin`/`stdout` if you want to intercept certain writes to `stdout` (or reads from `stdin`).

4. [My PR to `proto-lens`][proto-lens-pr] fixing [the issue][proto-lens-issue] with no support for streaming reads.

5. [My PR to GHC][ghc-loading-message-pr] allowing to mute the `Loading package environment` message with `-v0`—it required more refactoring than one might imagine.

6. David Eichmann's [Extended Dependency Generation (EDG) GHC proposal][dep-gen-proposal] suggesting that no build system or IDE should ever call GHC in the `--make` mode. Instead, GHC should be able to dump all the necessary information about dependencies into a machine-readable format. Once you have that file, with dependencies recorded, you only need GHC to compile individual files, the "one-shot" mode, and never the normal `--make` mode. This approach would liberate us from certain shortcomings of the `--make` mode like timestamp-based recompilation checking.

