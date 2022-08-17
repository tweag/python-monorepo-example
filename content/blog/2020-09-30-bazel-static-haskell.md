---
title: "Fully statically linked Haskell binaries with Bazel"
author: Will Jones (Habito) and Andreas Herrmann
tags: [bazel, haskell, nix]
description: "Bazel gains support for creating fully statically linked Haskell binaries with rules_haskell and rules_nixpkgs."
---

Deploying and packaging Haskell applications can be challenging at times, and
runtime library dependencies are one reason for this. Statically linked
binaries have no such dependencies and are therefore easier to deploy. They
can also be quicker to start, since no dynamic loading is needed. In exchange,
all used symbols must be bundled into the application, which may lead to larger
artifacts.

Thanks to the contribution of Will Jones of [Habito][habito][^1],
[`rules_haskell`][rules_haskell], the Haskell [Bazel][bazel] extension, has
[gained support][static_linking_pr] for fully static linking of Haskell
binaries.

Habito uses Bazel to develop, build, test and deploy Haskell code in a minimal
Docker container. By building fully-statically-linked binaries, Docker
packaging (using [`rules_docker`][rules_docker]) becomes straightforward and
easy to integrate into existing build workflows. A static binary can also be
`strip`ped once it is built to reduce the size of production artifacts. With
static binaries, what you see (just the binary) is what you get, and this is
powerful.

In the following, we will discuss the technical challenges of statically
linking Haskell binaries and how these challenges are addressed in
`rules_haskell`. Spoiler alert: Nix is an important part of the solution.
Finally, we will show you how you can create your own fully statically linked
Haskell binaries with Bazel and Nix.

## Technical challenges

Creating fully statically linked Haskell binaries is not without challenges.
The main difficulties for doing so are:

- Not all library dependencies are suited for statically linked binaries.
- Compiling template Haskell requires dynamic libraries on Linux by default.

### Library dependencies

Like most binaries on Linux, the Haskell compiler GHC is typically configured to
link against the [GNU C library `glibc`][glibc]. However, `glibc` is not
designed to support fully static linking and explicitly [depends on dynamic
linking][glibc_requires_dynamic] in some use cases. The alternative C library
[`musl`][musl] is designed to support fully static linking.

Relatedly, there may be licensing reasons to not link some libraries
statically. Common instances in the Haskell ecosystem are again `glibc` which
is licensed under GPL, and the core Haskell dependency `libgmp` which is
licensed under LGPL. For the latter GHC can be configured to use the core
package `integer-simple` instead of `integer-gmp`.

Fortunately, the Nix community has made [great
progress][nixpkgs-fully-static-haskell] towards fully statically linked Haskell
binaries and we can build on much of this work in `rules_haskell`. The
[`rules_nixpkgs`][rules_nixpkgs] extension makes it possible to import Nix derivations
into a Bazel project, and `rules_haskell` has first class support for
Nix-provided GHC toolchains using `rules_nixpkgs` under the hood. In
particular, it can import a GHC toolchain based on `musl` from
[static-haskell-nix][static-haskell-nix].

### Template Haskell

By default GHC is configured to require dynamic libraries when compiling
template Haskell. GHC's runtime system (RTS) can be built in various
combinations of so called [ways][ghc-ways]. The relevant way in this context is
called _dynamic_. On Linux, GHC itself is built with a dynamic RTS. However,
statically linked code is targeting a non-dynamic RTS. This may sound familiar
if you ever tried to compile code using template Haskell in profiling mode. As
the [GHC user guide][th-profiling] points out, when evaluating template Haskell
splices, GHC will execute compiled expressions in its built-in bytecode
interpreter and this code has to be compatible with the RTS of GHC itself. In
short, a GHC configured with a dynamic RTS will not be able to load static
Haskell libraries to evaluate template Haskell splices.

One way to solve this issue is to compile all Haskell libraries twice, once
with dynamic linking and once with static linking. C library dependencies will
similarly need to be available in both static and dynamic forms. This is the
approach taken by `static-haskell-nix`. However, in the context of Bazel we
found it preferable to only compile Haskell libraries once in static form and
also only have to provide C libraries in static form. To achieve this we need
to build GHC with a static RTS and to make sure that Haskell code is
compiled as position independent code so that it can be loaded into a running
GHC for template Haskell splices. Thanks to Nix, it is easy to override the GHC
derivation to include the necessary configuration.

## Make your project fully statically linked

How can you benefit from this? In this section we will show how you can setup a
Bazel Haskell project for fully static linking with Nix. For further details
please refer to the corresponding documentation on
[haskell.build][use-case-docs-static]. A fully working example repository is
available [here][minirepo]. For a primer on setting up a Bazel Haskell project
take a look at [this tutorial][blog_bazel_haskell_tutorial].

First, you need to configure a Nixpkgs repository that defines a GHC toolchain
for fully static linking based on musl. We start by pulling in a base Nixpkgs
revision and the `static-haskell-nix` project. Create a `default.nix`,
with the following.

```nix
let
  baseNixpkgs = builtins.fetchTarball {
    name = "nixos-nixpkgs";
    url = "https://github.com/NixOS/nixpkgs/archive/dca182df882db483cea5bb0115fea82304157ba1.tar.gz";
    sha256 = "0193bpsg1ssr93ihndyv7shz6ivsm8cvaxxl72mc7vfb8d1bwx55";
  };

  staticHaskellNixpkgs = builtins.fetchTarball
    "https://github.com/nh2/static-haskell-nix/archive/dbce18f4808d27f6a51ce31585078b49c86bd2b5.tar.gz";
in
```

Then we import a Haskell package set based on `musl` from `static-haskell-nix`.
The package set provides GHC and various Haskell packages. However, we will
only use the GHC compiler and use Bazel to build other Haskell packages.

```nix
let
  staticHaskellPkgs = (
    import (staticHaskellNixpkgs + "/survey/default.nix") {}
  ).approachPkgs;
in
```

Next we define a Nixpkgs [overlay][nixpkgs-overlay] that introduces a GHC based
on `musl` that is configured to use a static runtime system and core packages
built with position independent code so that they can be loaded for template
Haskell.

```nix
let
  overlay = self: super: {
    staticHaskell = staticHaskellPkgs.extend (selfSH: superSH: {
      ghc = (superSH.ghc.override {
        enableRelocatedStaticLibs = true;
        enableShared = false;
      }).overrideAttrs (oldAttrs: {
        preConfigure = ''
          ${oldAttrs.preConfigure or ""}
          echo "GhcLibHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
          echo "GhcRtsHcOpts += -fPIC -fexternal-dynamic-refs" >> mk/build.mk
        '';
      });
    });
  };
in
```

Finally, we extend the base Nixpkgs revision with the overlay. This makes the
newly configured GHC available under the Nix attribute path
`staticHaskell.ghc`.

```nix
  args@{ overlays ? [], ... }:
    import baseNixpkgs (args // {
      overlays = [overlay] ++ overlays;
    })
```

This concludes the Nix part of the setup and we can move on to the Bazel part.

You can import this Nixpkgs repository into Bazel by adding the following lines
to your `WORKSPACE` file.

```python
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
)
nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "default.nix",
)
```

Now you can define a GHC toolchain for `rules_haskell` that uses the Nix built
GHC defined above. Note how we declare that this toolchain has a static RTS and
is configured for fully static linking. Add the following lines to your
`WORKSPACE` file.

```python
load(
    "@rules_haskell//haskell:nixpkgs.bzl",
    "haskell_register_ghc_nixpkgs",
)
haskell_register_ghc_nixpkgs(
    version = "X.Y.Z",  # Make sure this matches the GHC version.
    attribute_path = "staticHaskell.ghc",
    repositories = {"nixpkgs": "@nixpkgs"},
    static_runtime = True,
    fully_static_link = True,
)
```

GHC relies on the C compiler and linker during compilation. `rules_haskell`
will always use the C compiler and linker provided by the active Bazel C
toolchain. We need to make sure that we use a musl-based C toolchain as well.
Here we will use the same Nix-provided C toolchain that is used by
static-haskell-nix to build GHC.

```python
load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_cc_configure",
)
nixpkgs_cc_configure(
    repository = "@nixpkgs",
    nix_file_content = """
      with import <nixpkgs> { config = {}; overlays = []; }; buildEnv {
        name = "bazel-cc-toolchain";
        paths = [ staticHaskell.stdenv.cc staticHaskell.binutils ];
      }
    """,
)
```

Finally, everything is configured for fully static linking. You can define a
Bazel target for a fully statically linked Haskell binary as follows.

```python
haskell_binary(
    name = "example",
    srcs = ["Main.hs"],
    features = ["fully_static_link"],
)
```

You can build your binary and confirm that it is fully statically linked as follows.

```
$ bazel build //:example
$ ldd bazel-bin/example
      not a dynamic executable
```

## Conclusion

If you're interested in further exploring the benefits of fully statically linked
binaries, you might [combine them with `rules_docker`][use-case-docs-docker] (e.g. through its
`container_image` rule) to build Docker images as Habito have done. With
a rich enough set of Bazel rules and dependency specifications, it's possible
to reduce your build and deployment workflow to a `bazel test` and `bazel run`!

The current implementation depends on a Nix-provided GHC toolchain capable of
fully static linking that is imported into Bazel using `rules_nixpkgs`.
However, there is no reason why it shouldn't be possible to use a GHC
distribution capable of fully static linking that was provided by other means,
for example a Docker image such as [`ghc-musl`][ghc-musl]. Get in touch if you
would like to create fully statically linked Haskell binaries with Bazel but
can't or don't want to integrate Nix into your build. Contributions are
welcome!

We thank Habito for their contributions to `rules_haskell`.

[bazel]: https://bazel.build
[blog_bazel_haskell_tutorial]: https://www.tweag.io/blog/2020-05-06-convert-haskell-project-to-bazel/
[blog_bazel]: https://www.tweag.io/blog/tags/bazel/
[ghc-musl]: https://github.com/utdemir/ghc-musl
[ghc-ways]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/compiler-ways
[glibc]: https://www.gnu.org/software/libc/
[glibc_requires_dynamic]: https://sourceware.org/glibc/wiki/FAQ#Even_statically_linked_programs_need_some_shared_libraries_which_is_not_acceptable_for_me.__What_can_I_do.3F
[habito]: https://www.habito.com/
[list_of_rules]: https://docs.bazel.build/versions/master/rules.html
[minirepo]: https://github.com/lunaris/minirepo
[musl]: https://musl.libc.org/about.html
[nixpkgs-fully-static-haskell]: https://github.com/NixOS/nixpkgs/issues/43795
[nixpkgs-overlay]: https://nixos.org/nixpkgs/manual/#chap-overlays
[rules_haskell]: https://github.com/tweag/rules_haskell
[rules_nixpkgs]: https://github.com/tweag/rules_nixpkgs
[rules_docker]: https://github.com/bazelbuild/rules_docker
[static-haskell-nix]: https://github.com/nh2/static-haskell-nix
[static_linking_pr]: https://github.com/tweag/rules_haskell/pull/1390
[th-profiling]: https://downloads.haskell.org/ghc/8.10.1/docs/html/users_guide/glasgow_exts.html#using-template-haskell-with-profiling
[use-case-docs-static]: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#building-fully-statically-linked-binaries
[use-case-docs-docker]: https://rules-haskell.readthedocs.io/en/latest/haskell-use-cases.html#containerization-with-rules-docker

[^1]:
    [Habito][habito] is fixing mortgages and making homebuying fit for the
    future. Habito gives people tools, jargon-free knowledge and expert support
    to help them buy and finance their homes. Built on a rich foundation of
    functional programming and other cutting-edge technology, Habito is a long
    time user of and contributor to `rules_haskell`.

<!--
[glibc_not_static]: https://stackoverflow.com/a/57478728/841562
[haskell_build]: https://haskell.build
[blog_bazel_haskell_ann]: https://www.tweag.io/blog/2018-02-28-bazel-haskell/
[blog_bazel_nix]: https://www.tweag.io/blog/2018-03-15-bazel-nix/
[blog_bazel_cabal]: https://www.tweag.io/blog/2019-10-09-bazel-cabal-stack/
-->
