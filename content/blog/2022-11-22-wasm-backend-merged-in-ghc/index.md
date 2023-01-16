---
title: "WebAssembly backend merged into GHC"
shortTitle: "WebAssembly backend merged into GHC"
author: Cheng Shao
tags: [haskell, ghc, webassembly]
description: "Announcing the GHC WebAssembly backend – present and future"
---

Tweag has been working on a GHC WebAssembly backend for some time.
Recently, the WebAssembly backend [merge request][wasm-mr] has landed
in GHC, and is on course to appear in the upcoming 9.6 release series.
This post will give a quick demonstration of how to try it out
locally, and explain what comes in this patch and what will be coming next.

## Playing with WASM locally

If you're using nix on x86_64-linux, compiling a Haskell program to a
self-contained wasm module is as simple as:

```sh
$ nix shell https://gitlab.haskell.org/ghc/ghc-wasm-meta/-/archive/master/ghc-wasm-meta-master.tar.gz
$ echo 'main = putStrLn "hello world"' > hello.hs
$ wasm32-wasi-ghc hello.hs -o hello.wasm
[1 of 2] Compiling Main             ( hello.hs, hello.o )
[2 of 2] Linking hello.wasm
$ wasmtime ./hello.wasm
hello world
```

There's also a non-nix installation script. Check the [ghc-wasm-meta]
repo's README for details.

What's interesting about the example above? It doesn't need any
companion JavaScript code, and runs on a variety of wasm engines that
support [wasi], including but not limited to: [wasmtime], [wasmedge],
[wasmer] and [wasm3]. Compared to the legacy [`asterius`][asterius]
project, there are also a few other serious benefits:

- The killer feature is being able to use GHC's own RTS code for garbage
  collection and other runtime functionality. The GHC RTS is way more
  robust, feature-complete and performant than `asterius`'s legacy
  JavaScript runtime. Lots of Haskell features that never worked in
  `asterius` (e.g. STM or profiling) now work out of the box.
- It has proper support for compiling and linking C/C++ code. Terms
  and conditions apply here, but there's still a high chance the
  `cbits` in your packages will work out of the box.
- Since it uses LLVM for linking, the linking step is orders of
  magnitudes faster than `asterius`, which uses a custom
  object format and linking logic.
- GHC CI tests a [program][ghc-hello] that uses the GHC API to parse a
  Haskell module. `ghc` is a big package and depends on everything in
  the boot libraries, so even having only a part of GHC frontend
  working in pure wasm is already pretty cool, and it certainly
  provides more assurance than a simple "hello world". `asterius`
  never had `ghc` in its boot libraries.

## What is in this merge request

The GHC wasm backend merge request's commit [history][wasm-mr-history]
is carefully structured to contain mostly small and easy to review
patches. The changeset can be roughly grouped into:

- Enhancing the build system, making it aware of the `wasm32-wasi`
  target, and avoid compiling stuff not supported on that target
- Avoiding the usage of POSIX features not supported on `wasm32-wasi` – various places need to be patched, like the RTS, `base` or `unix`
- Doing various other RTS fixes, for issues that didn't break other
  GHC targets by pure luck
- Enhancing the GHC driver with certain wasm-specific logic – most of
  the time due to the need to workaround some upstream issues in LLVM
- Modeling the wasm structured control flow, and implementing the
  algorithm to translate arbitrary Cmm control flow graphs to it – this
  part of the work was done by my colleague Norman Ramsey, and well explained
  in his ICFP 2022 [paper][relooper-paper]
- Implementing the wasm native code generator (NCG), which translates Cmm to
  assembly code – unlike NCGs for other targets, the wasm NCG uses a
  dependently-typed IR to preserve type safety of the wasm value
  stack, and this has proved to be helpful in catching some errors
  early on when writing the NCG
- Serving the binary distributions as CI artifacts, and there's
  already some basic testing

GHC is a rapidly evolving project, and merging the wasm backend does not
make it immune to potential future breakages. For me, it's not just an
honor to implement wasm support, but also a personal commitment to
maintain it, prevent bit-rotting, and make sure that the [bus
factor][bus-factor] of this work goes beyond 1 in the future. This is made possible
by Tweag's long term support.

## What comes next

### JavaScript FFI

`asterius` had a rich JavaScript FFI implementation, allowing one to
import JavaScript functions into Haskell, pass arbitrary JavaScript
values as first-class Haskell values, and export Haskell functions to
be called by JavaScript. Furthermore, the JavaScript async functions
worked naturally with the Haskell threading system, so that when a Haskell
thread is blocked on an async JavaScript call, the runtime executes
other threads instead of blocking completely.

This is the first of `asterius` main features that I plan to port to
GHC's wasm backend. You don't pay for JavaScript if you don't use it.
We've already gained good experience with wasm/js interoperability,
but this time I will need to do non-trivial refactorings in the GHC
RTS storage manager and scheduler to achieve the same. So this will
take some time and may not make it into GHC 9.6.1.

### Template Haskell

`asterius` had limited support for Template Haskell. Template Haskell
requires dynamically linking Haskell code, but how dynamic linking is
supposed to work in wasm is still unclear, so `asterius` cheated by
doing static linking each time a TH splice was evaluated. Since the
runtime heap state isn't preserved between splice evals, when the TH
splices are stateful, this approach won't work, but it's been proven
to work surprisingly well for a lot of TH splices in the wild.

I plan to add Template Haskell support for GHC's wasm backend in a
similar way. Pure TH splices (e.g. generating optics for datatypes)
are likely to work, and work much faster than `asterius` thanks to the much
improved linking performance. But splices with side effects (e.g.
`gitrev` that needs to spawn a `git` subprocess), may not work if
the side effect isn't a supported WASI operation.

Since implementing proper dynamic linking isn't planned yet, ghci
wouldn't work in GHC's wasm backend in the near future.

### More things to come

There are also other things planned in addition to the above features,
including but not limited to:

- Using the GHC issue tracker for bugfixes/feature planning and
  discussions, for better transparency of my work
- Running the full GHC testsuite and nofib benchmarks
- Supporting cross-compiling to wasm from more host systems
- Wasm-related patches to common Hackage dependencies, or a Hackage
  overlay for wasm

[asterius]: https://github.com/tweag/asterius
[bus-factor]: https://en.wikipedia.org/wiki/Bus_factor
[ghc-hello]: https://gitlab.haskell.org/ghc/ghc/-/blob/master/.gitlab/hello.hs
[ghc-wasm-meta]: https://gitlab.haskell.org/ghc/ghc-wasm-meta
[relooper-paper]: https://dl.acm.org/doi/10.1145/3547621
[wasi]: https://github.com/WebAssembly/WASI
[wasmedge]: https://wasmedge.org
[wasmer]: https://wasmer.io
[wasmtime]: https://wasmtime.dev
[wasm-mr]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9204
[wasm-mr-history]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9204/commits
[wasm3]: https://github.com/wasm3/wasm3
