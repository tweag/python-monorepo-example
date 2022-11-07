---
title: Implicit Dependencies in Build Systems
author: Robin Bate Boerop
tags: [nix, bazel]
---

In making a build system for your software, you codified the dependencies
between its parts. But, did you account for _implicit_ software dependencies,
like system libraries and compiler toolchains?

Implicit dependencies give rise to the biggest and most common problem with
software builds - the lack of _hermeticity_. Without hermetic builds,
_reproducibility_ and _cacheability_ are lost.

This post motivates the desire for reproducibility and cacheability, and
explains how we achieve hermetic, reproducible, highly cacheable builds by
taking control of implicit dependencies.

### Reproducibility

Consider a developer newly approaching a code repository. After cloning the
repo, the developer must install a long list of “build requirements” and plod
through multiple steps of “setup”, only to find that, yes indeed, the build
fails. Yet, it worked just fine for their colleague! The developer, typically
not expert in build tooling, must debug the mysterious failure not of their
making. This is bad for morale and for productivity.

This happens because the build is not _reproducible_.

One very common reason for the failure is that the compiler toolchain on the
developer’s system is different from that of the colleague. This happens even
with build systems that use sophisticated build software, like [Bazel][1].
Bazel implicitly uses whatever system libraries and compilers are currently
installed in the developer's environment.

A common workaround is to provide developers with a Docker image equipped with
a certain compiler toolchain and system libraries, and then to mandate that the
Bazel build occurs in that context.

That solution has a number of drawbacks. First, if the developer is using
macOS, the virtualized build context runs substantially slower. Second, the
Bazel build cache, developer secrets, and the source code remain outside of the
image and this adds complexity to the Docker invocation. Third, the Docker
image must be rebuilt and redistributed as dependencies change and that’s extra
maintenance. Fourth, and this is the biggest issue, Docker image builds are
themselves not reproducible - they nearly always rely on some external state
that does not remain constant across build invocations, and that means the
build can fail for reasons unrelated to the developer’s code.

A better solution is to use [Nix][2] to supply the compiler toolchain and
system library dependencies. Nix is a software package management system
somewhat like Debian’s APT or macOS’s Homebrew. Nix goes much farther to help
developers control their environments. It is unsurpassed when it comes to
reproducible builds of software packages.

Nix facilitates use of the Nixpkgs package set. That set is [the largest single
set of software packages][4]. It is also the [freshest][5] package set. It
provides build instructions that work both on Linux and macOS. Developers can
easily pin any software package at an exact version.

Learn more about [using Nix with Bazel, here][3].

### Cacheability

Not only should builds be reproducible, but they should also be fast. Fast
builds are achieved by caching intermediate build results. Cache entries are
keyed based on the precise dependencies as well as the build instructions that
produce the entries. Builds will only benefit from a (shared, distributed)
cache when they have matching dependencies. Otherwise, cache keys (which depend
on the precise dependencies) will be different, and there will be cache misses.
This means that the developer will have to rebuild targets locally. These
unnecessary local rebuilds slow development.

The solution is to [make the implicit dependencies into explicit ones, again
using Nix][3], making sure to configure and use a shared Nix cache.

Learn more about [configuring a shared Bazel cache, here][7].

### Conclusion

It is important to eliminate implicit dependencies in your build system in
order to retain build reproducibility and cacheability. Identify Nix packages
that can replace the implicit dependencies of your Bazel build and use
[rules_nixpkgs][6] to declare them as explicit dependencies. That will yield a
fast, correct, hermetic build.

[1]: /blog/tags/bazel
[2]: /blog/tags/nix
[3]: /blog/2018-03-15-bazel-nix/
[4]: https://repology.org/repositories/statistics/nonunique
[5]: https://repology.org/repositories/statistics/newest
[6]: https://github.com/tweag/rules_nixpkgs
[7]: /blog/2020-04-09-bazel-remote-cache/
