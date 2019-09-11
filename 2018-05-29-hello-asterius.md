---
title: "Fibonacci compiles end-to-end —<br/> Haskell to WebAssembly via GHC"
shortTitle: Compiling Haskell to WebAssembly
author: Shao Cheng
tags: haskell, asterius
---

[Asterius](https://github.com/tweag/asterius) is an experimental WebAssembly backend for GHC. We aim to support most GHC features while making the emitted WebAssembly code as small and fast as possible. The project recently reached an exciting milestone: for the first time, simple Haskell functions can be compiled all the way down to WebAssembly from Haskell and executed with a real JavaScript runtime! While this is an important milestone, it is merely the beginning of the road. Nevertheless, it is a good time to publicly announce our endeavour to provide Haskell with a high-performance target that you can run in your browser.

In this post, we'll explain by example how it works, provide a summary of other Haskell-to-WebAssembly efforts, and outline our roadmap.

## Running the Fibonacci example

The Fibonacci example is in the [`fib`](https://github.com/tweag/asterius/tree/master/asterius/test/fib) directory of the test suite. The content of `fib.hs` is as follows:

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

Before compiling `fib.hs`, we need to build Asterius with a custom version of GHC — see the [Building Guide](https://tweag.github.io/asterius/building/) for details. Then, we can run `ahc-boot`, which compiles `base` and its dependent packages to WebAssembly.

After that, we can simply invoke `ahc-link` like this:

```bash
$ ahc-link --input asterius/test/fib/fib.hs
```

This will compile `fib.hs` and generate `fib.wasm` and `fib.js` in the same directory. The file `fib.wasm` contains the binary of a standalone WebAssembly module, with an exported `main` function as its entry point. Moreover, `fib.js` contains an inline binary of that module, along with a few generated JavaScript functions required by the module imports. It can be run with Node.js like this:

```bash
$ node asterius/test/fib/fib.js
```

The program will calculate `fib 10` and print the result via `console.log`. Note that the garbage collector is not implemented yet, so if you drop that bang pattern or write a `fib` using naive recursion, the generated code is likely to crash at runtime due to heap overflow!

With the current version of Asterius, `fib.wasm` is only about 7KB and `fib.js` is about 20KB. Regarding our goal of generating small & fast code, this is a good start; at least, concerning the size of the generated code.

## The Haskell to WebAssembly story

The WebAssembly platform is still in its infancy. The 1.0 version of WebAssembly is [already published](http://webassembly.github.io/spec/) and implemented in all major browsers, supporting computation with basic integral & floating types, structured control flow, a 32-bit address space and importing & exporting functions which also operate on basic types.

WebAssembly v1.0 is a "minimum viable product (MVP)". For use cases like compiling CPU-bound applications written in C++ or Rust, what's present in the MVP is sufficient, but things get nasty when we need to constantly cross the JavaScript/WebAssembly boundary, operate on garbage-collected objects and invoke the Web API. There are proposals for exposing the JavaScript engine's garbage collector, SIMD primitives and host bindings to WebAssembly programs, but the timetable is unclear.

Asterius is not the first attempt at bringing Haskell to WebAssembly. There are two other similar projects that we know of: [WebGHC](https://github.com/WebGHC) and [dhc](https://github.com/dfinity/dhc).

WebGHC started as a Summer of Haskell 2017 project. Its primary approach is using the LLVM toolchain to build a complete cross-compiler and port the whole Haskell/C runtime to WebAssembly. There are Nix expressions to set up LLVM and unregisterised GHC, compile some C examples, and there are continuing efforts to make the Haskell runtime work on WebAssembly. The approach is quite ambitious, and will surely enjoy the best compatability once the whole runtime actually is up and running.

In contrast, dhc takes a radically different route. It does not compile Haskell via GHC; it implements a minimal Haskell-like functional language, and emits WebAssembly from that language. The basic examples run fine, and they are gradually implementing more language features in the frontend.

Within this spectrum of Haskell-to-WebAssembly compilers, Asterius is somewhere in between WebGHC and dhc. It uses GHC as the frontend and aims to support most of GHC's language features and primitives exposed in the standard library. However, we don't use the LLVM toolchain to cross-compile to WebAssembly; we implement the cmm-to-wasm code generator as yet another native backend, and any non-Haskell logic of the runtime is hand-written WebAssembly code, which means we're simulating various `rts` interfaces to the degree that a significant portion of vanilla Haskell code becomes runnable.

There are several reasons behind the approach of Asterius:

* We'd like to support the exact same flavour of Haskell as GHC does.
* There exist different ways of mapping Haskell to WebAssembly. We'd like to customize the code generator and have tight control over the generated code, so that we can evaluate different code generation strategies and maybe even dynamically pick the most suitable strategy for a given Haskell program.
* We want to avoid introducing unused code into the compiler output. This requires very aggressive link-time optimization, generating different runtime code for each output, and even altering normal GHC code generation.

Most importantly, we're keen take a *gradual, incremental* approach to bringing Haskell to WebAssembly. The GHC runtime system is written with a traditional operating system environment in mind, making use of the C runtime, OS specific I/O multiplexing not available in WebGHC, POSIX threads and all the rest of it. While projects like [Emscripten](https://github.com/kripken/emscripten) try to emulate some if not all of the above, porting a complete runtime along with its dependent libraries is a non-trivial challenge. Yet to get a simple example like Fibonacci run to completion, the portion of `rts` required to port to WebAssembly is quite small.

Once trivial examples like Fibonacci work, we gain confidence that we've set foot on a reasonable path. We can then iteratively write more complex examples, check what's missing and implement the missing bits. If some critical flaw arises, we get feedback early.

## The code generator of Asterius

Asterius starts code generation from Cmm. Cmm is a mostly platform-independent intermediate representation (IR) in GHC, abstracting away from details like register allocation and such. Currently GHC only supports dumping Cmm to a pretty-printed text form and doesn't expose in-memory Cmm, so we implemented the package `ghc-toolkit`, which uses the GHC frontend plugin mechanism to redirect `ghc` invocations to our own handlers, then uses the GHC hooks mechanism to replace the normal pipeline with our own. As a result, we can lay our hands on the in-memory representations of various GHC IRs like Core, STG, and Cmm. Users of `ghc-toolkit` need not worry about all the low-level coding involving the GHC API and can focus on manipulating IRs.

Although being sufficiently low level, compiling Cmm to WebAssembly still poses a few challenges. The most notable of those is implementing control-flow. There are two kinds of control-flow transfer in Cmm: in-function and cross-function. A Cmm function is a collection of basic blocks, and each basic block transfers to either a block in the same function, or the entry block of another function. The problem is: WebAssembly has no notion of "basic block"; it enforces structured control flow, which means one must use scoped blocks/loops to achieve branching.

The most straightforward method to implement control-flow is to use a top-level loop combined with a "label pointer" to implement a state machine for branching, and each branching instruction simply becomes an assignment to the label pointer. Since GHC is good at using the worker/wrapper transformation to generate fast code with tight in-function loops, this approach introduces considerable overhead for even in-function branching and will surely lead to unsatisfactory performance.

In the case of in-function branching, where the destination labels are known labels, we can make use of a "Relooper" algorithm, which converts a control-flow graph into a program that uses high-level loops to efficiently implement branching. This idea of a Relooper algorithm was pioneered by the [Emscripten compiler](http://emscripten.org/) and implemented in [`binaryen`](https://github.com/WebAssembly/binaryen), and we use `binaryen`'s relooper to handle in-function branching.

Function calls are trickier. There is no explicit tail call operator in WebAssembly, all function calls grow the control stack. Furthermore, the destination is often a computed label rather than a statically known label, and the Relooper algorithm is incapable of handling computed gotos. One possible solution is to transform the program by collecting all Cmm functions into one WebAssembly function, use a "label pointer", and then, instead of using a naive loop to handle all branching, insert a "dispatcher" block and translate all function calls to "branching to dispatcher". The dispatcher block reads the label pointer and jumps to the right label, using either a huge switch table or a binary decision tree. The relooper will consume the gigantic control-flow graph and hopefully preserve the performance of in-function branching. We experimented with this approach and discovered that with a large number of blocks in a single function, `binaryen` is not sufficiently scalable for this approach to be practical.

Hence, our current approach to handling function calls is to compile each Cmm function to a single WebAssembly function. Upon a Cmm call, the WebAssembly function returns a function pointer to the destination, and the `StgRun` mini-interpreter function uses the WebAssembly function table to perform an indirect call. This will introduce some overhead for each Cmm call, since the spec of WebAssembly MVP states that indirect calls are accompanied with runtime type checks, but it scales to a large number of Cmm functions.

Cmm assumes the existence of a 64-bit address space (because our host GHC is 64-bit) and various local/global Cmm registers. Since we only have a 32-bit address space in WebAssembly, we still assume all addresses are 64-bit, and only wrap them with an `i64.wrap` when marshalling a load/store instruction. As for registers, we simply implement local registers with local variables of WebAssembly functions, and use global variables for global registers.

As mentioned above, we use `binaryen` to handle WebAssembly generation. When compiling each Haskell module, we obtain the in-memory representation of Cmm, generate a Haskellish IR which closely maps to `binaryen`'s IR and serialize that IR. The booting process simply consists of compiling `base` and its dependent packages into that IR.

## The linker and runtime of Asterius

The WebAssembly MVP does not yet include an official standard concerning the format of a linkable WebAssembly object file. There exist various "linkers" out there, like `lld` or `wasm-merge`, none of which have a stable C API or strip dead code as aggressively as we expect, so we also implement our own linker, at least for the time being.

There are two notions of a "module" in asterius, both of which are defined in `Asterius.Types`. One is `AsteriusModule`, which is generated from a vanilla Haskell/Cmm module. An `AsteriusModule` is simply a collection of static data segments and functions. All `AsteriusModule`s are collected into a single immutable store and serialized during booting. When invoking `ahc-link`, the store is deserialized and all `AsteriusModule`s are available in memory. Implementing the store as a single immutable data structure makes it quite easy to implement the linker's logic.

The other type of module is simply `Module`. It's the standalone WebAssembly module type we send to `binaryen` for generating WebAssembly binary. The reason for having a separate `Module` is that we need extra metadata like function tables, which is calculated at link-time. When we need to produce a standalone WebAssembly module, we start from `Main_main_closure` (which corresponds to the `main` top-level binding of `Main`) and recursively fetch all static data and functions on which the root symbol depends. If linking succeeds, we end up with a single `AsteriusModule` which is self-contained except for imported external JavaScript functions. We can then perform symbol resolution as follows: confirm the absolute address of all static data and functions and rewrite the module by replacing the unresolved symbols with the appropriate addresses. The resolved module can be fed to `binaryen` for final code generation.

When performing linking, it is possible to encounter symbols for unknown or unsupported functions. If it's in a C file and we didn't hand-write its implementation, we won't find it in the store; when marshalling a normal Cmm function, we may also encounter unsupported instructions, in which case we save the error message in `AsteriusModule`. The linker can output a detailed report for each linking request, and the report contains the symbol set of unknown and unsupported entities, and the dependency graph between symbols of supported entities — it can even render the graph in GraphViz format so you can use a 3rd-party visualizer to view it! The linker reports proved rather useful when developing Asterius, since we can constantly check what's missing in order to support compiling the desired examples.

The Cmm code emitted by GHC or contained in the `rts` already assumes the existence of some C interfaces of the `rts`; e.g., `CurrentNursery`, `CurrentTSO` and so forth. Those interfaces are defined in C headers of `rts`, but since we aren't doing any C-to-WebAssembly code generation, we need to hand write some WebAssembly code to set up these interfaces. This has proved to be the hardest part of writing Asterius, since the GHC runtime is quite complex, and WebAssembly doesn't have a good debugging story yet.

Currently, we implemented a simplified storage manager and scheduler. The storage manager respects the block allocator interface, and is capable of allocating fresh blocks at runtime using WebAssembly's `grow_memory` operator. Garbage collection isn't implemented yet, and when a stack/heap check fails, the program simply crashes with a "memory overflow" error code. The scheduler does not handle interrupting/resuming a Haskell thread and only runs to completion once the closure of `Main.main` is entered.

## What's there and what's missing

Let's review what Asterius is capable of and what it isn't. Pure functions involving only "simple" types like integers and floats are known to work. As for I/O, there is a simple `print_int`, and that's pretty much it for now. But it should be easy to add something like `print_char` which sends a Haskell `Char#` to a TTY-style terminal.

Currently, Asterius reports linker errors when more advanced datatypes are involved. This is related to the `Typeable`-related bindings generated by `ghc`, they are quite complicated and can be introduced even if the program isn't actually using any `Typeable`-related feature. For example, you may accidently get a transitive dependency on `__hsbase_MD5Init`, which is related to calculating MD5 digests, but your program is simply using a `scanl` over a `[Int]` to calculate a factorial. We are trying to find a workaround for this critical issue.

The next intermediate project goal of Asterius is to implement a stub garbage collector. After Asterius is capable of producing a long-running WebAssembly program, it becomes possible to actually integrate it into a web frontend workflow. After that we'll gradually make more Haskell programs work out the box, improve JavaScript interoperability, and attempt more radical experiments like switching away from the current Cmm-based code generator and utilizing WebAssembly's native GC mechanism when it is present.

## What you can do to help

Even if Asterius is in its early days, there's plenty you can do to help! Here are some thoughts regarding potential improvements, and we're looking forward to discussion and contribution.

### Introducing the LLVM toolchain

Currently Asterius does not support compiling C. However, being able to compile simple C files in `cbits` of common packages (like `bytestring`) would be a big win, since they often have little additional dependency other than the C runtime, and compiling from C to WebAssembly is relatively well-tested by projects like Emscripten.

We'll need to modify our codegen to suit LLVM's calling convention, and standardize on a compatible linker and object store format. In return, we don't need to write a WebAssembly shim every time we encounter a dependency on foreign C function, and it may even be possible to cross-compile parts of the `rts` itself, reducing the workload of implementing the Asterius runtime.

### GHC as a Cabal package

As mentioned earlier, there are times we want to expose GHC's inner workings, retrieve its in-memory data or alter its behavior. Although GHC has mechanisms like plugins and hooks, ideally we'd like to have a stage 1 GHC available as a Cabal package. The advantages are:

* Decoupling the stage 0 / stage 1 GHC build time configurations. For example, currently Asterius requires GHC to disable `TABLES_NEXT_TO_CODE` when performing codegen, but we don't care if the host GHC used to compile Asterius shares the same config. Also, we can use a stage 1 GHC which targets 32-bit platform, then we don't need to coerce from 64-bit addresses here and there.
* Easing the maintainance of custom GHC patches. Currently we maintain a `ghc-toolkit` package which works by pasting and patching code from the GHC tree. Integrating upstream changes is troublesome, because these patches might break non-WebAssembly targets.

### Enhancing the debugging experience

WebAssembly currently has limited support for debugging: browsers like Chrome or Firefox support inspecting S-expression format, setting breakpoints and inspecting variables. The source map feature also makes it possible to debug the "source language" right in a browser pane.

In practice, in-browser debugging of WebAssembly hasn't been quite helpful to find bugs in Asterius's output code. Asterius currently has a "tracing mode" which outputs logs of control-flow transfer as the code runs. The tracing mode should be further enhanced, supporting customization of verbosity levels, hooking more WebAssembly instructions and printing more comprehensive logs.
