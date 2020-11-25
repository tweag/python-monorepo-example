---
title: "A tale of Template Haskell and cross compilation"
shortTitle: "A tale of Template Haskell and cross compilation"
author: Cheng Shao, Georgios Karachalias
tags: [haskell, asterius]
description: "Why Template Haskell is hard for cross compilation and how Asterius does it"
---

Template Haskell (TH) is a widely used yet controversial language extension. You have
probably used it in your own code; with a single line of splice code, you can
achieve tasks like deriving instances and embedding files easily. And you might
also have heard the reasons why people may dislike it: it slows down
compilation, breaks encapsulation, arbitrary IO at compile time is risky, etc.

But it is less well known that Template Haskell also makes cross compilation
with GHC harder. In this post, we'll show why this is a challenge, some existing
solutions developed by the community, and in particular, how this problem is
addressed by [Asterius][asterius].

## Just run some code at compile time, what can go wrong?

Conceptually, Template Haskell is a principled way of generating Haskell AST at
compile time, like in the simplified example below:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Data.Char
import Language.Haskell.TH.Syntax
import System.Process

gitRev :: String
gitRev =
  $( do
       rev <-
         runIO $
           filter isHexDigit <$> readProcess "git" ["rev-parse", "HEAD"] ""
       liftString rev
   )
```

Suppose we'd like to define a `gitRev` string that represents the current `git`
revision in the project repository. This can be done using an expression splice:
it is written using the `$(...)` syntax, and the content within `$()` is an
expression of type `Q Exp`, representing a compile-time computation that returns
an `Exp` value, which is, in this case, the current `git` revision as a string
literal.

Splice code lives in the `Q` monad, which manages the context for Template
Haskell and provides a rich set of interfaces. Inside `Q` we can query info
about datatypes or functions, allocate fresh identifiers, etc. Arbitrary `IO`
actions may also be run inside `Q`. Here, we run `git rev-parse HEAD` to obtain
the `git` revision and then return it. When GHC compiles this module, the splice
is replaced with a string literal, and compilation moves on.

So at first glance, Template Haskell is just about running user code at compile
time, what can go wrong? All is right for most developers, who compile to the
same platform they run GHC on, but there's trouble ahead when you try to do
cross compilation...

## The what and why of cross compilation

Suppose we'd like to write a Haskell app for an Android phone or a Raspberry
Pi. It's possible to bootstrap a native GHC release on them and use it to
compile stuff, but given the limited hardware resources of these machines, it's
wiser to run GHC on a proper x64 build server and emit code for these ARM
devices. When we do so, we're performing _cross compilation_. Some terminology:

- The _host_ platform is where we run GHC to compile stuff.
- The _build_ platform is where we compile GHC. For simplicity, we assume
  build=host and only use the host term from now on.
- The _target_ platform is where we run the compiled Haskell app. When
  host=target, the GHC is a _native_ GHC, otherwise it's a _cross_ GHC.

For a native GHC, Template Haskell isn't a problem, since GHC can link and run its
emitted code just like native dynamic libraries. But this doesn't work
out-of-the-box for a cross GHC.

Over the years, people have come up with different approaches to address the
cross compilation issue of Template Haskell, each coming with its own rough
edges; more details follow in later sections.

## Only run TH on the host platform

If we can't run emitted code, then how about we don't run it at all and stay
with a cross GHC without TH support? We'll preprocess the cross GHC input code,
strip usages of the Template Haskell extension, and replace all TH splices with
the expanded code. And the way to expand the splices would be... using a native
GHC to compile it!

There's a GHC flag `-ddump-splices` which dumps the expanded splices code.
Unfortunately, the dump output has extra text decorations and isn't proper Haskell
source code, so it takes more work to use the dumps. Here's a list of known
implementations of the splice dump approach:

- [`EvilSplicer`][evilsplicer] uses a `parsec`-based parser to process the dumps
  for later consumption of cross GHC. It was used in the
  [`git-annex`][git-annex] project until late 2018.
- [`ZeroTH`][zeroth] is a tool which does something similar, and includes a CLI
  and `Cabal`-related helper functions.
- [`reflex-platform`][reflex-platform] uses a patched native GHC which dumps the
  expanded splices as proper Haskell source code, and feeds into [GHCJS][ghcjs].

However, making native/cross GHC work together is not trivial:

- Unlike `gcc` or `clang` which can emit code for other platforms by simply
  adding relevant CLI flags, a GHC installation can only emit code for a single
  target platform configured at its build time. So two different GHC
  installations must be managed in isolated places.
- Native/cross GHC must have the same version and process the same build plan to
  minimize the chance of emitting wrong code. Say package `foo` includes a TH
  splice that uses package `bar`, if native/cross GHC sees different versions
  (or even same version but different build plan) of `bar`, the splice behavior
  could potentially differ, expanding into wrong code that may be silently
  consumed by cross GHC.

Given the complexity of the required hacks and GHC/Cabal's lack of cross
compilation support, it's common to use an external build system (e.g.
[Nix][nix]) to encapsulate this mechanism.

Other than saving dumps of expanded splices, there is another solution to only
run TH splice code on the host platform: the same GHC always compile everything
to both host/target code in one invocation! When running TH, we can just load
host code just like native GHC. This requires quite some customization of GHC
behavior and is only possible for 3rd-party compilers based on GHC API. In
fact, GHCJS used this approach in its earliest days.

### Pros and cons of running TH on the host platform

Running TH on the host platform works for pure splices, which can only do
things like reifying info and generating ASTs. It should also work pretty well
for side-effecting splices which reads files, spawns processes or fires
missiles, since the splice behavior should be just the same as when we use a
native GHC to compile stuff.

But is this the end of story? Not yet. Here's one immediate problem: the
native/cross GHC may not consume the same Haskell sources despite our best
efforts.

- Haskell modules may use the `CPP` extension with target-specific macros, so
  when you compile for different targets, you see different top-level
  definitions.
- Cabal files may also check implementation/platform/etc, and end up with
  different flags or even different modules to be consumed by GHC.

The problems above will likely trigger compile-time errors. And there's an even
stealthier problem that may lead to generating incorrect code instead of a
crash: the architecture difference of host/target, e.g. word size or endianness.
For instance, a TH splice may make use of `sizeOf (undefined :: Int)`, which is
4 on 32-bit target platforms, and if the host platform is 64-bit, then the TH
splice will see 8, which sneaks into the emitted code without a single warning.

## Run TH code on the target platform

As explained in earlier sections, vanilla GHC can only link and run host code.
Would it be possible to teach GHC to link and run target code? The answer is
yes. The key to supporting running non-native code is RPC (Remote Procedure
Calls). GHC needs to call into target code to obtain the splice expansion
result; the target code needs to call GHC to do reification. These calls are
achieved via exchanging serialized messages between GHC and the loaded splices.
Since there is a fixed set of operations allowed in the `Q` monad (as methods of
the `Quasi` class), the operations and the results can be encoded as a
serializable `Message` datatype.

This RPC approach to run TH code is standardized in the [external
interpreter][ghc-external-interpreter] feature. When running TH, GHC starts an
external process calls `iserv`, pipes messages to `iserv` and tells it to load
archives, objects, etc and link code. After a splice starts running in `iserv`,
`iserv` may send queries back to GHC and get results. Finally, the splice
expansion result is sent back to GHC.

The external interpreter opens up the possibility of using various emulators
(e.g. `wine` for windows, `node` for js/wasm or even `qemu` for exotic
platforms) to run target code for TH. GHC itself doesn't need to care about how
the code is actually linked and run in `iserv`, and TH should work as long as
our target-specific `iserv` can properly process the messages.

This approach was pioneered by GHCJS, and later made it into upstream GHC by
7.10. Other than GHCJS, known users include:

- GHC itself, even in native GHC! But why bother? Well, suppose we're compiling
  a profiled library with TH usage. Since profiled code follows different
  runtime conventions and links with profiled runtime, in the early days, a
  profiled GHC executable was needed. Now, we can simply use a profiled `iserv`
  executable, and avoid the extra profiling overhead in GHC.
- [haskell.nix][haskell.nix], which includes support for cross-compiling to
  Windows via `wine` emulation of TH code.
- [Mobile Haskell][mobile-haskell], which are ARM-targetting GHC distributions.
  They use Android/iOS emulators to set up the splice runtime environment. GHC
  talks to an `iserv-proxy` process via pipes, and `iserv-proxy` merely relays
  the messages to the real `iserv` program in the emulator via a socket.
- The [Eta][eta] Haskell-to-JVM compiler.
- [Asterius][asterius], which uses `node` for running the WebAssembly &
  JavaScript code.

### Pros and cons of running TH on the target platform

Compared to running TH on the host platform, there are a few benefits to
running it on the target platform:

- No host/target incoherence issues, as explained in earlier sections.
- Less hacky and more standardized. Although upstream GHC won't likely contain
  `iserv` implementations for all interesting target platforms out there,
  developers can just roll their own if needed.
- Simpler, since there isn't a bunch of hacks to be packaged via nix anymore,
  and it works with vanilla `cabal`/`stack`.

It would be tempting to announce TH for cross compilation is now a solved
problem! Turns out it's not. Recall how TH enables running arbitrary `IO` in
splices? It's used in some popular packages, e.g. `gitrev` for obtaining `git`
revisions, and `file-embed` for embedding files. For native GHC, the `IO`
actions have full access to the host system: its file-system, external tooling,
etc. But for cross GHC, the `IO` actions may be run in a sandbox without those
facilities, so these packages and their dependents will then fail to compile!

Host-specific side effects in TH splices is still possible, but require
case-by-case analysis and patching. Instead of `runIO someOperation`, we can
directly add `someOperation` to the methods of the `Quasi` class, and patch the package to use
it. When splices gets run, `iserv` will simply send a `SomeOperation` message to
GHC, and GHC can run it on the host and deliver the serialized result back. This
is used by MobileHaskell to support packages like `file-embed`.

## Template Haskell in Asterius

Asterius uses the external interpreter approach to support Template Haskell. The
execution of compiled WebAssembly code and the JavaScript runtime is done in a
`node` process. Since `node` runs on the same machine along the compiler, it can
access the same resources as a native GHC does, so as long as an operation is
supported in the Asterius `node` runtime, it'll work in a TH splice. This has
worked pretty well so far, given the [list][asterius-known-to-compile] of
known-to-compile packages.

The current Asterius TH implementation comes with one limit though: no state is
persisted across different splices in the same module, due to the lack of a true
dynamic linker/runtime. Why would someone want to do this? One example is
reusing an expensive resource across all splices, be it a process handle,
network socket or anything else:

```haskell
import Language.Haskell.TH.Syntax

data Resource

newResource :: IO Resource

freeResource :: Resource -> IO ()

useResource :: Q Resource
useResource = do
  m <- getQ
  case m of
    Just r -> pure r
    _ -> do
      r <- runIO newResource
      addModFinalizer $ runIO $ freeResource r
      putQ r
      pure r
```

In the example above, we have `newResource`/`freeResource` for
allocating/freeing an expensive resource. Then we can implement `useResource`
which attempts to get a `Resource` from the TH session, and initialize one if
it's not present. The registered finalizer will be run after all splices in the
same module has been expanded.

Rest assured, cross-splice state persistence is quite rare in practice. So for
typical TH scenarios, our current implementation should be sufficient.

## Summary

Cross compilation isn't a daily use case for most Haskellers out there, so they
may not realize that features like TH can't be taken for granted in a cross
setting. We hope the writing above helps a bit in raising the awareness in our
community.

You can help in improving the situation by thinking twice before rolling up the
sleeves and reaching for things like TH with `runIO`, Plugins or custom
`Setup.hs`. Would it work if GHC is targeting another platform and configured
with a different toolchain? Even without actual testing in the end, this extra
mindset has the potential of avoiding frustration of your project's future users
:)

For interested readers, we also recommend taking a look at the pending GHC stage
hygiene [proposal][ghc-stage-hygiene] which includes some quality discussion.

[asterius]: https://github.com/tweag/Asterius
[asterius-diagrams]: https://www.tweag.io/blog/2019-12-19-Asterius-diagrams
[asterius-known-to-compile]: https://github.com/tweag/Asterius/issues/354
[binaryen]: https://github.com/WebAssembly/binaryen
[eta]: https://github.com/typelead/eta
[evilsplicer]: http://source.git-annex.branchable.com/?p=source.git;a=blob;f=Build/EvilSplicer.hs;h=e07034c5b05f47c316a1e68e6a85d54335c8e253;hb=aaa841e60a55524c3efb5e9783b8e6074d2413cc
[ghc-external-interpreter]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/external-interpreter
[ghc-stage-hygiene]: https://github.com/ghc-proposals/ghc-proposals/pull/243
[ghci-msg]: https://gitlab.haskell.org/ghc/ghc/-/blob/master/libraries/ghci/GHCi/Message.hs
[ghcjs]: https://github.com/ghcjs/ghcjs
[git-annex]: https://git-annex.branchable.com
[haskell.nix]: https://github.com/input-output-hk/haskell.nix
[inline-js]: https://github.com/tweag/inline-js
[inline-js-post]: https://www.tweag.io/blog/2019-05-09-inline-js/
[lens]: https://hackage.haskell.org/package/lens
[mobile-haskell]: https://medium.com/@zw3rk
[nix]: https://nixos.org
[pic]: https://en.wikipedia.org/wiki/Position-independent_code
[reflex-platform]: https://github.com/reflex-frp/reflex-platform
[reify]: https://hackage.haskell.org/package/template-haskell-2.16.0.0/docs/Language-Haskell-TH.html#v:reify
[singletons]: https://hackage.haskell.org/package/singletons
[wasm-dynamic-linking]: https://github.com/WebAssembly/tool-conventions/blob/master/DynamicLinking.md
[wasm-lazy-compilation]: https://bugs.chromium.org/p/v8/issues/detail?id=5991
[wasm-lazy-validation]: https://bugs.chromium.org/p/v8/issues/detail?id=9003
[wasm-no-checks]: https://codereview.chromium.org/2511113002
[wasm-tail-calls]: https://github.com/WebAssembly/tail-call
[zeroth]: https://hackage.haskell.org/package/zeroth
