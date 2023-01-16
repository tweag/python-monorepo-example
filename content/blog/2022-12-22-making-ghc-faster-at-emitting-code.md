---
title: "Making GHC faster at emitting code"
author: Alexis King
tags: [haskell, ghc]
description: "How we gained 2–3% improvements in compile times by making GHC better at printing code."
---

One common complaint from industrial users of Haskell is that of compilation times: they are sometimes painfully slow. Some of that slowness is difficult to avoid—no matter how you slice it, typechecking and optimizing Haskell code takes a lot of work—but nobody would argue that there is not ample room for improvement. For the past few months, Krzysztof Gogolewski and I have had the opportunity to work with [Mercury][] to identify what some of those improvements might be, and I am pleased to report that [our first major patch][the patch] toward that end will be part of GHC 9.6.

For readers who just want the numbers, we’ve seen **a 2–3% reduction in compile times and a 5–10% reduction in allocations, on average, for _unoptimized_ builds**,[^1] measured on a few large Haskell codebases. Due to the nature of the patch, some modules benefit much more than others. Remarkably, these gains come purely from targeted improvements to the mechanism by which GHC emits compiled code.

[^1]: Optimized builds are also improved by this patch, but the improvements are proportionally smaller, since GHC spends much more time in the optimizer but roughly the same amount of time in the code generator.

## The mechanics of GHC code generation

When asked where GHC spends most of its time, the front- and middle-ends of the compiler would probably most readily come to mind for most GHC developers. After all, most of the really interesting bits of GHC are in the typechecker and Core-to-Core optimizer, and those are certainly where GHC does its cleverest work. Nevertheless, the backend is a rather essential part of any compiler, since at the end of the day, its main job is to produce an executable artifact.

As of version 9.6, GHC supports five backends: the native code generator (`-fasm`), the bytecode compiler (`-fbyte-code`), LLVM (`-fllvm`), [JavaScript](https://engineering.iog.io/2022-12-13-ghc-js-backend-merged/), and [WebAssembly](https://www.tweag.io/blog/2022-11-22-wasm-backend-merged-in-ghc/).[^2] The native code generator (NCG) is the default outside of GHCi, so it is used by the overwhelming majority of Haskell programs, and it is the backend targeted by our patch. As its name implies, the NCG generates architecture-specific native code: it lowers Haskell programs into one of x86, x86_64, AArch64, or PowerPC assembly.

[^2]: Technically, GHC supports a sixth backend, the C code generator, which can be used on platforms the NCG does not (yet) support. However, GHC is not built with support for this backend by default, as it is really only intended to facilitate porting GHC to a new platform, so most users have little reason to know it exists.

However, the NCG does _not_ itself produce binary object files. Instead, it generates textual assembly code and uses the system toolchain to assemble it into native code objects. This separation of labor means that GHC does not need to know anything about the binary structure of object files themselves, which vary from platform to platform even if they share the same underlying architecture. For example, Linux distributions use [ELF](https://en.wikipedia.org/wiki/Executable_and_Linkable_Format), macOS uses [Mach-O](https://en.wikipedia.org/wiki/Mach-O), and Windows uses [PE](https://en.wikipedia.org/wiki/Portable_Executable), all of which are quite different even when the underlying architecture is the same. System-specific assemblers and linkers abstract over many of these differences, which allows the NCG to generate assembly in essentially the same format regardless of platform.

One implication of this design is that, in practice, the NCG spends a surprisingly large amount of its time writing ASCII text. The overhead of producing this text, versus producing a binary format, is fairly small—assembly language is infamously terse—but it is nevertheless crucial that GHC produce it as efficiently as possible. It is not uncommon for Haskell modules to generate _megabytes_ worth of assembly, so even relatively small savings can rapidly add up.

Given that the task is both important and fairly simple—just write the mnemonic and arguments for each instruction—one would expect GHC to use a straightforward and efficient strategy, with I/O overhead dwarfing any computational cost. However, to my surprise, when I profiled the NCG and looked at where the time was being spent, an enormous amount of time and memory was spent printing code!

## The problem: using the pretty-printer for code generation

Investigation into the implementation of the NCG revealed the source of the problem: GHC was using its internal `SDoc` abstraction to print assembly code. For readers unfamiliar with `SDoc`, it is an implementation of [pretty-printing combinators in the style of John Hughes](https://link.springer.com/chapter/10.1007/3-540-59451-5_3). The idea is fairly simple. A few basic constructor functions create the “atoms” of `SDoc`, with signatures such as the following:

```haskell
empty :: SDoc
char  :: Char -> SDoc
text  :: String -> SDoc
```

These atoms can then be pasted together using composition operators like `<>`, which pastes two `SDoc`s together horizontally, or `$$`, which pastes them together vertically. The key feature of `SDoc` is that it knows how to handle _block indentation_ and _line wrapping_, both of which are particularly important when pretty-printing Haskell code. For example, consider the following interaction in GHCi:

```
ghci> not ["foo", "bar", "some really really really really really long string"]
<interactive>: error:
    • Couldn't match expected type ‘Bool’ with actual type ‘[String]’
    • In the first argument of ‘not’, namely
        ‘["foo", "bar",
          "some really really really really really long string"]’
```

Note how GHC automatically breaks the long list literal in the error message across two lines, and it knows that it can fit `["foo", "bar",` together on the first line and that it should indent the continuation line by an additional space. This is all handled automatically when printing an `SDoc`: it keeps track of column position and can intelligently choose where to insert line breaks and indentation based on grouping structure. Most users only see this functionality used in warnings and error messages, but this automatic formatting is so useful that GHC uses `SDoc` pervasively to format debug output (such as the Core dumps produced by `-ddump-simpl`).

However, thoughtful readers will note that none of this functionality is remotely useful when generating assembly code. Though the details vary from architecture to architecture, the basic structure of assembly is consistent, and it usually looks roughly like this:

```s
.globl M.f_info
.type M.f_info, @function
M.f_info:
_blk_czO:
  leaq -24(%rbp),%rax
  cmpq %r15,%rax
  jb _blk_czP
_blk_czQ:
  movq %r14,%rax
  movl $GHC.Num.$fNumInt_closure,%r14d
  movq $stg_ap_pp_info,-24(%rbp)
  movq %rax,-16(%rbp)
  movq $stg_INTLIKE_closure+273,-8(%rbp)
  addq $-24,%rbp
  jmp GHC.Num.+_info
```

Assembly language is an incredibly regular, line-oriented format that contains a single instruction, label, or assembler directive per line. Moreover, the assembly generated by GHC is exclusively intended for machine consumption, so any cycles spent at compile time trying to format it neatly are essentially wasted. As profiling GHC revealed, this cost is not just theoretical—using `SDoc` for code generation has a real and measurable performance impact:

- Printing `SDoc` values always counts columns for every string printed so that it can implement its line-breaking machinery.

- More significantly, since `SDoc` is designed to handle nested expression structure, every `SDoc` is represented internally as a tree, which must be allocated when it is constructed and subsequently walked when it is printed.

- GHC doesn’t actually allocate a giant `SDoc` tree for each basic block before printing it because `SDoc` values are actually constructed lazily, which means the assembly printing is effectively streamed. But this is in some sense even _worse_, because it means constructing each `SDoc` value allocates numerous thunks, all of which are soon forced anyway.

In [the issue I opened on the GHC bug tracker][the issue] about this, I put some concrete numbers to this overhead:

> Building GHC with profiling (and inserting _all_ cost centres by hand to avoid optimization changes) reveals that GHC spends roughly **15% of its time and 17% of its allocations inside** `pprNativeCode`. Now, sure, at the end of the day, there’s a lot of code to print, so _any_ implementation is going to take a decent chunk of time, but the current implementation is very much _not_ I/O-bound: only about 0.2% of the total execution is spent inside `hPutBuf` (measured via the eventlog, not cost centre profiling).

Clearly, this implementation strategy had to go.

### Why did GHC do this?

One might reasonably ask why in the world the developers of GHC ever thought using its pretty-printer inside its code generation hot loop was an acceptable strategy. I don’t have any solid evidence that supports one particular answer, but after developing our alternative strategy and running into several challenges along the way, I think I can provide some educated guesses.

First, the simple _convenience_ of using `SDoc` for printing should not be understated. GHC has grown a nice internal library for constructing `SDoc`s of all shapes and sizes, and its implementation already does a lot of work to be reasonably efficient, given what `SDoc` is designed to do. It should be emphasized that it would be very easy to do significantly _worse_ than `SDoc` in this regard: building up a big `SDoc` is still enormously more efficient than concatenating together a giant `String`.

Second, real effort was put into tuning `SDoc` so that it was at least serviceable for its use in the code generator. Prior to our changes, the `SDoc` renderer included a special mode (known internally as `LeftMode`) entirely dedicated to printing assembly. Furthermore, Simon Marlow [added a special buffering optimization](https://gitlab.haskell.org/ghc/ghc/-/commit/ac88f113abdec1edbffb6d2f97323e81f82908e7) to the codegen printer in 2005, which reportedly resulted in a 10% reduction in compile time for non-optimized code, so it would be entirely unfair to say that nobody understood the importance of the codegen printer on performance. Indeed, a 10% improvement for compile times is more than the improvement we found with our patch, which suggests that the optimizations made to `SDoc` had already squeezed out a sizable portion of the performance available to be gained.

Third, and probably more importantly, `SDoc`’s ubiquitous presence within GHC makes it a powerful code sharing mechanism. Thousands of types defined by GHC provide instances of the `Outputable` class (which is like `Show` but provides conversion to `SDoc` instead of to `String`), and this is how a great deal of serialization logic has effectively been centralized. For example, the canonical textual formats for identifiers, package names, and module names are all implemented in terms of `SDoc`, and this ensures that they appear consistently in all compiler output. When GHC is invoked with the `-dppr-debug` flag, which enables more verbose and explicit printing, the effect it has on identifiers applies _globally_, in everything from error messages to debug dumps, because it all goes through the same code path. This is a wonderful thing, as it is quite a nightmare to debug a compiler issue if two different parts of the compiler are actually operating on the same thing, but they’re printing it in ways that look different.

Of course, much of this configurability is unimportant for the code generator, but many of the internal consistency requirements are still incredibly relevant. For example, when GHC generates code, every exported definition must be assigned a unique symbolic name that can be referenced from other modules and resolved during linking. Since native code objects have no notion of namespacing, GHC uses a name mangling scheme that prefixes each identifier with the names of their originating module and package, and this must be consistent with other parts of the compiler to ensure everything works smoothly. It would be quite unfortunate if GHC developers had to carefully keep two definitions of all of this functionality in delicate sync: one definition for debug output and another for the code generator.

Given all of the above, it’s no surprise that GHC has continued to use `SDoc` for so long: any replacement had big shoes to fill. Indeed, improving on the performance of `SDoc` while preserving its convenience and its utility as a common source of truth turned out to be something of a challenge.

## The solution: a fast reimplementation of the `SDoc` interface

In principle, the solution was obvious from the start: we ought to produce an optimized implementation of a subset of the `SDoc` API. That would allow us to write code that is parameterized over the choice of implementation, achieving the code sharing we need while still permitting two different runtime implementations. In practice, however, getting this right proved remarkably tricky!

The core linguistic mechanism that Haskell provides for parameterizing a definition over a choice of implementation is typeclasses. Indeed, we can easily define an `IsDoc` typeclass with all the essential operations:

```haskell
class IsDoc a where
  empty :: a
  char  :: Char -> a
  text  :: String -> a
  (<>)  :: a -> a -> a -- horizontal composition
  ($$)  :: a -> a -> a -- vertical composition
```

Then we can update all the shared code to use `IsDoc` instead of `SDoc`, and we can define a second implementation for use in the code generator. However, this simple approach runs into two problems: one semantic and one operational. Let’s take a look at each problem (and how we solved them) in turn.

### Problem 1: Preventing misuses of `IsDoc`

It may not be obvious, but the above definition of `IsDoc` is unfortunately slightly too expressive. Here’s one example of an expression permitted by the above interface that causes trouble:

```haskell
text "foo " <> (text "bar" $$ text "baz")
```

Remember that `SDoc` understands grouping structure, so given the above expression, it will automatically indent `baz` in the output:

```
foo bar
    baz
```

This is rather nice, but we don’t want our fast implementation of `IsDoc` to have to count columns or worry about grouping structure, so it can’t possibly handle this properly. Fortunately, it turns out that none of the printing code shared between the code generator and debug output actually uses vertical composition this way! After all, most of the shared logic pertains to printing things like identifier names, which are atomic units that will never be broken across multiple lines. This allows us to stratify `IsDoc` into two classes, one for single lines of output and one for multiline blocks:

```haskell
class IsLine a where
  empty :: a
  char  :: Char -> a
  text  :: String -> a
  (<>)  :: a -> a -> a -- horizontal composition

class IsDoc a where
  type Line a = r | r -> a
  line  :: Line a -> a
  ($$)  :: a -> a -> a -- vertical composition
```

Single lines can be composed horizontally, but composing lines vertically requires first explicitly “locking up” each line using `line`, after which no further horizontal composition can take place.

The `Line` associated type relates instances of the two classes, and since `SDoc` supports arbitrary horizontal and vertical composition, `Line SDoc` is just `SDoc`. But for our optimized implementation, we define two distinct types, which we’ve called `HLine` and `HDoc` (where `H` stands for “handle”, because internally they really write directly to a `Handle`). This leaves us with the following instances:

```haskell
instance IsLine SDoc
instance IsDoc SDoc where
  type Line SDoc = SDoc

instance IsLine HLine
instance IsDoc HDoc where
  type Line HDoc = HLine
```

Since the definition of `Line SDoc` is just `SDoc`, existing code using `SDoc` does not need to care about the single-line/multi-line distinction. But since `HLine` and `HDoc` are distinct types, the type system guarantees that two `HDoc`s will only ever be vertically composed, never horizontally composed. This makes the problematic expression from above ill-typed, and the crisis is averted.

In practice, our final implementation is a _little_ more elaborate than what is described here (we actually have a shared superclass between `IsLine` and `IsDoc`), but this communicates the essential idea. For the gory details, you can read [the lengthy comment I included on the design](https://gitlab.haskell.org/ghc/ghc/-/blob/451aeac3b07f171f148995717d0d9a1eefe08f0e/compiler/GHC/Utils/Outputable.hs#L1448-1574).

### Problem 2: Ensuring specialization

The second issue we ran into is one [I’ve actually spoken about at some length before](https://www.youtube.com/watch?v=0jI-AlWEwYI), namely the perils of relying upon typeclass specialization. To understand why this is so important, let’s consider the concrete definition of `HLine`:

```haskell
newtype HLine = HLine { runHLine :: SDocContext -> Handle -> IO () }

instance IsLine HLine where
  text str = HLine (\_ h -> hPutStr h str)
  HLine f <> HLine g = HLine (\ctx h -> f ctx h *> g ctx h)
  ...
```

Essentially, `HLine` is just a function that writes its output directly to a `Handle`, and composition of `HLine`s just applies each function in sequence. This is pretty much guaranteed to be faster than `SDoc`, since it does less bookkeeping, but without optimizations, it’s not nearly as free as we would like. To understand the issue, consider a simple expression like `text "hello, " <> text "world!"`. If the uses of `text` and `<>` aren’t inlined, each call will allocate a closure, which is needlessly wasteful: we still end up creating a tree of heap objects like we did with `SDoc`. The only difference is that now they’re function closures rather than data constructors.

Fortunately, `text` and `<>` are very small, so GHC is quite keen to inline them. And after inlining, we get the following expression:

```haskell
let f = \_ h -> hPutStr h "hello, "
    g = \_ h -> hPutStr h "world!"
in HLine (\ctx h -> f ctx h *> g ctx h)
```

GHC can then further simplify this by inlining the uses of `f` and `g`, leaving us with the following:

```haskell
HLine (\_ h -> do hPutStr h "hello, "
                  hPutStr h "world!")
```

Note that we have ended up with precisely the direct, obvious, imperative function that writes two strings to a `Handle`! Moreover, the function has no free variables, so it does not need to allocate a closure at runtime. This is exactly what we want.

The problem is that inlining `text` and `<>` is only possible if they are used monomorphically. If we were to instead write

```haskell
helloWorld :: IsLine a => a
helloWorld = text "hello, " <> text "world!"
```

then GHC could not possibly inline `text` and `<>`, as it does not yet know which instance of `IsLine` will be used! This is where typeclass specialization comes in: if GHC can see that `helloWorld` is applied at type `HLine`, it can generate a specialized monomorphic version. Unfortunately, this automatic specialization only works if the monomorphic use appears within the same module where `helloWorld` is defined. If it isn’t, then GHC will not specialize `helloWorld`, and these precious optimizations will never happen.

Sadly, the only way to force GHC to perform cross-module specialization is to use explicit `SPECIALIZE` pragmas on every definition you want to be specialized. This required adding dozens of pragmas of roughly the following form to various definitions throughout GHC:

```haskell
{-# SPECIALIZE helloWorld :: HLine #-} -- see Note [SPECIALIZE to HDoc]
```

It’s a frustrating solution, but it does work,[^3] so it’s the one we eventually reluctantly accepted.

[^3]: This isn’t _quite_ the whole story, as our approach initially ran into some trouble due to [a shortcoming in the specializer](https://gitlab.haskell.org/ghc/ghc/-/issues/21851). Fortunately, [SPJ implemented a fix](https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8897), which should also make specialization more robust in general going forward.

It’s worth reflecting a little on just how difficult this currently is to get right in Haskell. In other programming languages, programming against an interface and supplying multiple implementations is something that people do all the time without worrying nearly so much about the performance subtleties of doing so. In languages like C++ and Rust, specialization is guaranteed (since those languages never use dictionary passing), whereas in languages like Java, the JIT can dynamically detect monomorphic calls and specialize them on the fly. Both of these approaches have downsides, but they provide a great deal of utility, and they provide an important set of tools for writing efficient code without needing to break abstraction boundaries.

It would be nice to have some way to obtain similar guarantees in Haskell. One potential solution would be to allow attaching a pragma to the `IsLine` and `IsDoc` _classes_ to request that GHC aggressively specialize all definitions that use them. Another approach would be to use a different parameterization strategy entirely, such as higher-order modules. (Sadly, Backpack is too entangled with Cabal to even consider using in GHC.) A full discussion of all the potential approaches and their respective tradeoffs is well outside the scope of this blog post, but it’s something I’d love to explore more in the future.

## Final thoughts

Eliminating the use of the pretty-printer in GHC’s code generator in a way that both obtained good performance and was minimally invasive turned out to be a subtler task than we had initially hoped. Krzysztof and I spent a great deal of time trying different ways to tease apart bits of code that needed to be shared between debug dumps and code generation, and not all parts of our final design were completely satisfying. Nevertheless, I think the results speak for themselves: the NCG now takes a little over half the time and allocates as little as 20% as much memory as it used to for many Haskell programs. Since code generation is only one part of GHC’s compilation pipeline, the effects on overall compiler performance are not nearly so extreme, but they are still quite welcome.

There is probably more room for improvement in this direction. [We have not yet attempted to extend our changes to the LLVM backend](https://gitlab.haskell.org/ghc/ghc/-/issues/22455), so currently it still uses `SDoc`—replacing those uses of `SDoc` with uses of `HLine` and `HDoc` would be an easy win for anyone using the LLVM backend. For now, however, we’ve decided to turn our attention to some other parts of the compiler that seem more likely to provide a larger benefit for more people.

Finally, I’d like to explicitly thank [Mercury][] for funding this work. Valuable as it may be, improving the performance of a large, complex codebase like GHC can be difficult and time consuming, so it is no surprise that it is a historically underfunded task. We look forward to continuing to collaborate with them in the future.

[mercury]: https://mercury.com/
[the issue]: https://gitlab.haskell.org/ghc/ghc/-/issues/21853
[the patch]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/9157
