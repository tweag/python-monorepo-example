---
title: "End-to-end compilation of fib from GHC to WebAssembly"
shortTitle: Announcing MVP of asterius
author: Shao Cheng
---

[`asterius`](https://github.com/tweag/asterius) is an experimental WebAssembly backend for GHC. We aim to support most GHC features while making the emitted WebAssembly code as small and fast as possible. In recent days, the project readed an exciting milestone: for the first time, simple Haskell functions can be compiled all the way down to WebAssembly from GHC and executed with a real JavaScript runtime. This is an important milestone but still the beginning of the road. It's good time to publicly announce our endeavour to provide Haskell with a high-performance target that you can run in your browser.

In this post, we'll demonstrate an example, explain how it works, and give a summary of other Haskell to WebAssembly efforts and our further roadmap.

## Running the Fibonacci example

The fibonacci example is in the [`fib`](https://github.com/tweag/asterius/tree/master/asterius/test/fib) directory of the test suite. The content of `fib.hs` is as follows:

```haskell
{-# LANGUAGE BangPatterns #-}

fib :: Int -> Int
fib n = go 0 1 0
  where
    go !acc0 acc1 i
      | i == n = acc0
      | otherwise = go acc1 (acc0 + acc1) (i + 1)

foreign import ccall unsafe "print_int" print_int :: Int -> IO ()

main :: IO ()
main = print_int $ fib 10
```

Before compiling `fib.hs`, we need to build `asterius` with a custom GHC (see the [Building Guide](https://tweag.github.io/asterius/building/) for details). Then we can run `ahc-boot`, which compiles `base` and its dependent packages to WebAssembly.

After that, we can simply invoke `ahc-link` like this:

```bash
$ ahc-link --input asterius/test/fib/fib.hs
```

This will compile `fib.hs` and generate `fib.wasm` and `fib.js` in the same directory. `fib.wasm` is the binary of a standalone WebAssembly module, with an exported `main` function as its entry point. `fib.js` contains inline binary of the module, along with a few generated JavaScript functions required by the module imports. It can be run with Node.js like this:

```bash
$ node asterius/test/fib/fib.js
```

The program will calculate `fib 10` and print the result via `console.log`. Note that the garbage collector is not implemented yet, so if you drop that bang pattern or write a `fib` using naive recursion, the generated code is likely to crash at runtime due to heap overflow!

As of this example, `fib.wasm` is only about 7KB and `fib.js` is about 20KB. Regarding our goal of generating small & fast code, this is a good start.

## The Haskell to WebAssembly story

The WebAssembly platform is still in its infancy. The 1.0 version of WebAssembly is [already published][webassembly-spec] and implemented in all major browsers, supporting computation with basic integral/floating types, structured control flow, a 32-bit address space and importing/exporting functions which also operate on basic types.

WebAssembly v1.0 is an "minumum viable product (MVP)". For use cases like compiling CPU-bound applications written in C++ or Rust, what's present in the MVP is sufficient, but things get nasty when we need to constantly cross the JavaScript/WebAssembly boundary, operate on garbage-collected objects and invoke the Web API. There are proposals for exposing the JavaScript engine's garbage collector, SIMD primitives and host bindings to WebAssembly programs, but the timetable is unclear.

`asterius` is not the first attempt to bring Haskell to WebAssembly. There are two other similar projects that we know of: [WebGHC](https://github.com/WebGHC) and [dhc](https://github.com/dfinity/dhc).

WebGHC started as a Summer of Haskell 2017 project. Its primary approach is using the LLVM toolchain to build a complete cross-compiler and port the whole Haskell/C runtime to WebAssembly. There are Nix expressions to set up LLVM and unregisterised GHC, compile some C examples, and there's continuing efforts to make the Haskell runtime work on WebAssembly. The approach is quite ambitious, and will surely enjoy the best compatability once the whole runtime actually gets to run.

dhc takes a radically different route. It does not compile Haskell via GHC; it implements a minimal Haskell-like functional language, and emits WebAssembly code from that language. The basic examples run fine, and they are gradually implementing more language features at the frontend.

On the spectrum of Haskell to WebAssembly compilers, `asterius` is somewhere in between WebGHC and dhc. It uses GHC as the frontend and aims to support most of GHC's language features and primitives exposed in the standard library. However, we don't use the LLVM toolchain to cross-compile to WebAssembly; we implement the cmm-to-wasm code generator as yet another native backend, and any non-Haskell logic of the runtime is hand-written WebAssembly code, which means we're simulating various `rts` interfaces to the degree that a significant portion of vanilla Haskell code becomes runnable.

There are several reasons behind the approach of `asterius`:

* We'd like to support Haskell-as-in-GHC.
* There exist different ways of mapping Haskell to WebAssembly. We'd like to hand-write the codegen and have tight control over generated code, so it becomes possible to evaluate them and even pick the most suitable one under different scenarios.
* We want to avoid introducing unused code into compiler output. This requires very aggressive link-time optimization, generating different runtime code for each output, or even altering the normal GHC codegen.

## The code generator of `asterius`

`asterius` starts code generation from Cmm. Cmm is a mostly platform-independent intermediate representation (IR) in GHC, abstracting away from details like register allocation and such. Currently GHC only supports dumping Cmm to a pretty-printed text form and doesn't expose in-memory Cmm, so we implemented `ghc-toolkit` which uses the GHC frontend plugin mechanism to redirect `ghc` invocations to our own handlers, then uses the GHC hooks mechanism to replace the normal pipeline with our own. Then we can lay our hands on in-memory representations of various GHC IRs like Core, STG and Cmm. Users of `ghc-toolkit` need not to worry about all the dirty job involving GHC API and can focus on manipulating IRs.

Although being sufficiently low level, compiling Cmm to WebAssembly still faces a few challenges. The most notable challenge is implementing control-flow. There are two kinds of control-flow transfer in Cmm: in-function and cross-function. A Cmm function is a collection of basic blocks, and each basic block transfers to either a block in the same function, or the entry block of another function. The problem is: WebAssembly has no notion of "basic block"; it enforces structured control flow, which means one must use scoped blocks/loops to achieve branching.

The most straightforward method to implement control-flow is to use a top-level loop combined with a "label pointer" to implement the state machine for branching, and each branching instruction simply becomes an assignment to the label pointer. Since GHC is good at using the worker/wrapper transformation to generate fast code with tight in-function loops, this approach introduces considerable overhead for even in-function branching and will surely worsen the performance.

In the case of in-function branching, where the destination labels are known labels, one can instead use a "Relooper" algorithm which inputs a control-flow graph and outputs a program which uses high-level loops to efficiently implement branching. The Relooper algorithm is pioneered by the Emscripten compiler and implemented in `binaryen`, and we use `binaryen`'s relooper to handle in-function branching.

Function calls are trickier. There is no explicit tail call operator in WebAssembly, all function calls grow the control stack. Furthermore, the destination is often a computed label rather than a known label, and the Relooper algorithm is uncapable of handling computed gotos. One possible solution is collecting all Cmm functions into one WebAssembly function, use a "label pointer", but instead of using a naive loop to handle all branching, insert a "dispatcher" block and translate all function calls to "branching to dispatcher". The dispatcher block reads the label pointer and jumps to the right label, using either a huge switch table or a binary decision tree. The relooper will consume the gigantic control-flow graph and hopefully preserve the performance of in-function branching. We experimented with this approach and discovered that with a large number of blocks in a single function, `binaryen` is not scalable enough.

The current approach for handling function call is compiling each Cmm function to a single WebAssembly function. Upon a Cmm call, the WebAssembly function returns a function pointer to the destination, and the `StgRun` mini-interpreter function uses the WebAssembly function table to perform an indirect call. This will introduce some overhead for each Cmm call, since the spec of WebAssembly MVP states that indirect calls are accompanied with runtime type checks, but it scales to a large number of Cmm functions.

Cmm assumes existence of a 64-bit address space (because our host GHC is 64-bit) and various local/global Cmm registers. Since we only have 32-bit address space in WebAssembly, we still assume all addresses are 64-bit, and only wrap them with an `i64.wrap` when marshalling a load/store instruction. As for registers, we simply implement local registers with local variables of WebAssembly functions, and use global variables for global registers.

As mentioned above, we use `binaryen` to handle WebAssembly generation. When compiling each Haskell module, we obtain the in-memory representation of Cmm, generate a Haskellish IR which closely maps to `binaryen` IR and serialize that IR. The booting process is simply compiling `base` and its dependent packages into that IR.

## The linker and runtime of `asterius`

The WebAssembly MVP does not yet have an official standard about the format of a linkable WebAssembly object file. There exist various "linkers" out there, like `lld` or `wasm-merge`, non of which have a stable C API or strip dead code as aggressively as we expect, so we also implement our own linker.

There are two notions of a "module" in asterius. One is `AsteriusModule`, which is generated from a vanilla Haskell/Cmm module. An `AsteriusModule` is simply a collection of static data segments and functions. All `AsteriusModule`s are collected into a single immutable store and serialized during booting. When invoking `ahc-link`, the store is deserialized and all `AsteriusModule`s are available in memory. Implementing the store as a single immutable data structure makes it quite easy to implement the linker's logic.

When we need to produce a standalone WebAssembly module, we start from `Main_main_closure` (which corresponds to the `main` top-level binding of `Main`) and recursively fetch all static data or function which is depended by that root symbol. If the linking succeeds, we end up with a single `AsteriusModule` which is self-contained except imported external JavaScript functions. We can then perform symbol resolution: confirm the absolute address of all static data/function, rewrite the module and replace the unresolved symbols with the addresses. The resolved module can be fed to `binaryen` for final code generation.

When performing linking, it is possible to encounter symbols for unknown or unsupported functions. If it's in a C file and we didn't hand-write its implementation, we won't find it in the store; when marshalling a normal Cmm function, we may also encounter unsupported instructions, in which case we save the error message in `AsteriusModule`. The linker can output a detailed report for each linking request, and the report contains the symbol set of unknown/unsupported entities, and the dependency graph between symbols of supported entities (it can even render the graph in GraphViz format so you can use a 3rd-party visualizer to view it!). The linker reports are quite useful when developing `asterius`, since we can constantly check what's missing in order to support compiling desired examples.

The Cmm code emitted by GHC or written in `rts` already assumes the existence of some C interfaces of `rts`, e.g. `CurrentNursery`, `CurrentTSO` and such. Those interfaces are defined in C headers of `rts`, but since we aren't doing any C-to-WebAssembly code generation, we need to hand write some WebAssembly code to set up the interfaces. This has proved to be the hardest part of writing `asterius`, since the GHC runtime is quite complex, and WebAssembly doesn't have a good debugging story.

Currently, we implemented a simplified storage manager and scheduler. The storage manager respects the block allocator interface, and is capable of allocating fresh blocks at runtime using WebAssembly's `grow_memory` operator. Garbage collection isn't implemented yet, and when a stack/heap check fails, the program simply crashes with a "memory overflow" error code. The scheduler does not handle interrupting/resuming a Haskell thread and only runs to completion once the closure of `Main.main` is entered.

## What's there and what's missing

Let's review what `asterius` is capable of and what it isn't. Pure functions involving only "simple" types like integers and floats are known to work. As for I/O, there is a simple `print_int`, and that's pretty much it for now. But it should be easy to add something like `print_char` which sends a Haskell `Char#` to a TTY-style terminal.

Currently, `asterius` reports linker error when more advanced datatypes are involved. This is related to the `Typeable`-related bindings generated by `ghc`, they are quite complicated and can be introduced even if the program isn't actually using any `Typeable`-related feature. For example, you may accidently get a transitive dependency on `__hsbase_MD5Init`, which is related to calculating MD5 digests, but your program is simply using a `scanl` over a `[Int]` to calculate a factorial. We are trying to find a workaround for this critical issue.

The next intermediate project goal of `asterius` is to implement a stub garbage collector. After `asterius` is capable of producing a long-running WebAssembly program, it becomes possible to actually integrate it into a web frontend workflow. After that we'll gradually make more Haskell programs work out the box, improve JavaScript interoperability, and attempt more radical experiments like switching away from current Cmm-based codegen and utilize WebAssembly's native GC mechanism when it's present.
