---
title: "asterius: Bringing Haskell to WebAssembly"
shortTitle: Announcing MVP of asterius
author: Shao Cheng
---

[`asterius`](https://github.com/tweag/asterius) is a Haskell to WebAssembly compiler, with the goal of supporting most GHC features while making the emitted WebAssembly code as small and fast as possible. The project has recently reached its MVP stage, which means it's capable of compiling some simple examples and running the output with a real JavaScript runtime. We'd like to demonstrate an example, explain how it works, and give a summary of other Haskell to WebAssembly efforts and our further roadmap.

## Running the Fibonacci example

The fibonacci example is in the [`fib`](https://github.com/tweag/asterius/tree/master/asterius/test/fib) directory of the test suite. The content of `fib.hs` is as follows:

```haskell
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}

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

Before compiling `fib.hs`, we need to build `asterius` with a custom ghc (see the [Building Guide](https://tweag.github.io/asterius/building/) for details). Then we can run `ahc-boot`, which compiles `base` and its dependent packages to WebAssembly.

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

The WebAssembly platform is still at its infancy. The MVP version of WebAssembly is already published and implemented in major JavaScript runtimes, supporting computation with basic integral/floating types, structured control flow, 32-bit address space and importing/exporting functions which also operate on basic types. For use cases like compiling CPU-bound applications written in C++ or Rust, what's present in MVP is sufficient, but things get nasty when we need to constantly cross the JavaScript/WebAssembly boundary, operate on garbage-collected objects and invoke the Web API. There is ongoing official proposal of adding garbage-collection and host bindings to WebAssembly, but the timetable is unclear.

`asterius` is not the first attempt to bring Haskell to the WebAssembly platform. There are two other similar projects we know of: [WebGHC](https://github.com/WebGHC) and [dhc](https://github.com/dfinity/dhc).

WebGHC was a Summer of Haskell 2017 project, and its primary approach is using the LLVM toolchain to build a complete cross-compiler and port the whole Haskell/C runtime to WebAssembly. There are Nix expressions to set up LLVM and unregisterised ghc, compile some C examples, and there's continuing efforts to make the Haskell runtime work on WebAssembly. The approach is quite ambitious, and will surely enjoy the best compatability once the whole runtime actually gets to run.

dhc takes a radically different route. It does not compile Haskell via ghc; it implements a minimal Haskell-like functional language, and emits WebAssembly code from that language. The basic examples run fine, and they are gradually implementing more language features at the frontend.

On the spectrum of Haskell to WebAssembly compilers, `asterius` is somewhere between WebGHC and dhc.
It uses ghc as the frontend and aims to support most of ghc language features and the standard library. However, we don't use the LLVM toolchain to cross-compile to WebAssembly; we implement the cmm-to-wasm code generator just like yet another native backend, and any non-Haskell logic of the runtime is hand-written WebAssembly code, which means we're simulating various `rts` interfaces to the degree that a significant portion of vanilla Haskell code become runnable.

There are several reasons behind the approach of `asterius`:

* We'd like to support Haskell-as-in-GHC.
* There exist different ways of mapping Haskell to WebAssembly. We'd like to hand-write the codegen and have tight control over generated code, so it becomes possible to evaluate them and even pick the most suitable one under different scenarios.
* We dislike introducing unused code into compiler output. This requires very aggressive link-time optimization, generating different runtime code for each output, or even altering normal ghc codegen.

## The code generator of `asterius`

`asterius` starts code generation from Cmm. Cmm is a mostly platform-independent IR of ghc, abstracting away from details like register allocation and such. Although being sufficiently low level, compiling Cmm to WebAssembly still faces a few challenges.

The main challenge is implementing control-flow. There are two kinds of control-flow transfer in Cmm: in-function and cross-function. A Cmm function is a collection of basic blocks, and each basic block transfers to either a block in the same function, or the entry block of another function. The problem is: WebAssembly has no notion of "basic block"; it enforces structured control flow, which means one must use scoped blocks/loops to achieve branching.

The most straightforward method to implement control-flow is to use a top-level loop combined with a "label pointer" to implement the state machine for branching, and each branching instruction simply becomes an assignment to the label pointer. Since ghc is good at using worker/wrapper transformation to generate fast code with tight in-function loops, this approach introduces considerable overhead for even in-function branching and will surely worsen the performance.

In the case of in-function branching, where the destination labels are known labels, one can instead use a "Relooper" algorithm which inputs a control-flow graph and outputs a program which uses high-level loops to efficiently implement branching. The Relooper algorithm is pioneered by the Emscripten compiler and implemented in `binaryen`, and we use `binaryen`'s relooper to handle in-function branching.

Function calls are trickier. There is no explicit tail call operator in WebAssembly, all function calls grow the control stack. Furthermore, the destination is often a computed label rather than a known label, and the Relooper algorithm is uncapable of handling computed gotos. One possible solution is collecting all Cmm functions into one WebAssembly function, use a "label pointer", but instead of using a naive loop to handle all branching, insert a "dispatcher" block and translate all function calls to "branching to dispatcher". The dispatcher block reads the label pointer and jumps to the right label, using either a huge switch table or a binary decision tree. The relooper will consume the gigantic control-flow graph and hopefully preserve the performance of in-function branching. We experimented with this approach and discovered that with a large number of blocks in a single function, `binaryen` is not scalable enough.

The current approach for handling function call is compiling each Cmm function to a single WebAssembly function. Upon a Cmm call, the WebAssembly function returns a function pointer to the destination, and the `StgRun` mini-interpreter function uses the WebAssembly function table to perform an indirect call. This will introduce some overhead for each Cmm call, since the spec of WebAssembly MVP states that indirect calls are accompanied with runtime type checks, but it scales to a large number of Cmm functions.

As mentioned above, we use `binaryen` to handle WebAssembly generation. When compiling each Haskell module, we obtain the in-memory representation of Cmm, generate a Haskellish IR which closely maps to `binaryen` IR and serialize that IR. The booting process is simply compiling `base` and its dependent packages into that IR.

## The linker and runtime of `asterius`

TODO

## TODO

Explanation of project architecture, codegen and runtime; what will be there in the future; possibly some more explanation of what do we have in wasm for now; etc
