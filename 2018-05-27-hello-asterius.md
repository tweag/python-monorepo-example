---
title: "asterius: Bringing Haskell to WebAssembly"
shortTitle: Announcing MVP of asterius
author: Shao Cheng
---

[`asterius`](https://github.com/tweag/asterius) is a Haskell to WebAssembly compiler, with the goal of supporting most GHC features while making the emitted WebAssembly code as small and fast as possible. The project has recently reached its MVP stage, which means it's capable of compiling some simple examples and running the output with a real JavaScript runtime. We'd like to demonstrate an example, explain how it works, and give a summary of other Haskell to WebAssembly efforts and our further roadmap.

# Running the Fibonacci example

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

# The Haskell to WebAssembly story

`asterius` is not the first attempt to bring Haskell to the WebAssembly platform. There are two other similar projects we know of: [WebGHC](https://github.com/WebGHC) and [dhc](https://github.com/dfinity/dhc).

WebGHC was a Summer of Haskell 2017 project, and its primary approach is using the LLVM toolchain to build a complete cross-compiler and port the whole Haskell/C runtime to WebAssembly. There are Nix expressions to set up LLVM and unregisterised ghc, compile some C examples, and there's continuing efforts to make the Haskell runtime work on WebAssembly. The approach is quite ambitious, and will surely enjoy the best compatability once the whole runtime actually gets to run.

dhc takes a radically different route. It does not compile Haskell via ghc; it implements a minimal Haskell-like functional language, and emits WebAssembly code from that language. The basic examples run fine, and they are gradually implementing more language features at the frontend.

On the spectrum of Haskell to WebAssembly compilers, `asterius` is somewhere between WebGHC and dhc. It uses ghc as the frontend and aims to support most of ghc language features and the standard library. However, we don't use the LLVM toolchain to cross-compile to WebAssembly; we implement the cmm-to-wasm code generator just like yet another native backend, and any non-Haskell logic of the runtime is hand-written WebAssembly code, which means we're simulating various `rts` interfaces to the degree that a significant portion of vanilla Haskell code become runnable.


# TODO

Explanation of project architecture, codegen and runtime; what will be there in the future; possibly some more explanation of what do we have in wasm for now; etc
