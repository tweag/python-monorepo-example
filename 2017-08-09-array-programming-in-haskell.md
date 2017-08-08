---
title: Array programming in Haskell
author: Manuel M T Chakravarty
featured: yes
---

Array programming in Haskell is attractive, but it is also somewhat confusing. Haskell’s functional syntax is close to the mathematical notation that is often used to develop and explain the algorithms on which array programs are based, which leads to great clarity of expression. At the same time, we can execute Haskell array code with performance that is competitive with more low-level, tedious to write and debug code. That is the good news, and it is why array programming in Haskell is increasingly gaining traction.

On the confusing side, there are several different array libraries with overlapping feature sets, but distinct focus, strengths, and level of support, and often they have got widely different performance characteristics. This can make it hard to know where to start. At some point, we, as a community, need to bring some order into this plethora of options, so that others who want to use Haskell for array programming will see a clear path ahead, but that is a story for another time.

For now, I’d like to dedicate a series of blog posts to introducing you to some of the more popular options for array programming in Haskell. In the reminder of this post, we will look at the various kinds of array libraries that we have got at our disposal and what the relative strengths and weaknesses are. In subsequent posts, we will focus on individual packages and see how we can use them.

## Standard routines
The simplest way to approach array programming is by relying on a set of standardised array routines with well-understood semantics, performance characteristics, and applications, such as [BLAS (Basic Linear Algebra Subprograms)](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) and [LAPACK (Linear Algebra Package)](https://en.wikipedia.org/wiki/LAPACK). In the Haskell ecosystem, this functionality is provided by the package [`hmatrix`: Numeric Linear Algebra](https://hackage.haskell.org/package/hmatrix), which is simply a Haskell wrapper around the BLAS and LAPACK C libraries. It also has a few companion libraries to support the additional functionality found in the [GNU Scientific Library](https://en.wikipedia.org/wiki/GNU_Scientific_Library) and the [GNU Linear Programming Kit - Wikipedia](https://en.wikipedia.org/wiki/GNU_Linear_Programming_Kit).

In addition to the CPU libraries, [`cublas`](http://hackage.haskell.org/package/cublas) package provides access to GPU-accelerated variants of these standard array routines. There are a few further packages on Hackage, but they all don’t appear to be actively maintained.

## Standard arrays
When the standard routines are not sufficient for the problem at hand, we can fall back to a range of libraries that provide basic array building blocks that we can use to implement our own array algorithms. This is where the choice starts to get overwhelming. We have the standard, immutable and lazy [Haskell 2010 arrays](https://www.haskell.org/onlinereport/haskell2010/haskellch14.html#x22-20100014). On top of that, the [`array`](http://hackage.haskell.org/package/array) package bundled with GHC provides a range of mutable and immutable arrays for use with boxed and unboxed data, including support for low-level, C-style, hand-tuned imperative array code. There are a few more packages that provide similar functionality, but differ in how arrays are stored and how memory is managed.

## Single-core
Beyond `array`, we can categorise different packages by whether they are designed to support parallelism by simultaneously executing code on multiple CPU or GPU cores. The most popular choice for fast single-core arrays is the package [vector: Efficient Arrays](https://hackage.haskell.org/package/vector) — one of the spin-offs of the [Data Parallel Haskell](https://wiki.haskell.org/GHC/Data_Parallel_Haskell) project. The package `vector` produces not only highly efficient code using a range of array fusion techniques, it also supports a wide range of array flavours, including the storage of boxed and unboxed data. Due to its popularity, `vector` has grown its own ecosystem of interoperability and extension, including access to highly optimised Fourier transforms based on [FFTW](https://en.wikipedia.org/wiki/FFTW).

The fundamental difference between `array` and `vector` is that `array` provides a mostly index-based interface to the programmer, which allows for great control, but also imposes an imperative style of programming. In contrast, `vector` favours whole-vector processing *collective operations* — also referred to as [wholemeal programming](http://www.cs.ox.ac.uk/ralf.hinze/publications/ICFP09.pdf). This raises the level of abstractions, but it also puts a larger burden on `vector` and the Haskell compiler to compile this high-level code to efficient machine code.

## Multicore
If we want parallelism, the choice is mostly between [repa: High performance, regular, shape polymorphic parallel arrays](http://hackage.haskell.org/package/repa) (another spin-off from [Data Parallel Haskell](https://wiki.haskell.org/GHC/Data_Parallel_Haskell)) and [Accelerate: High-Performance Haskell](http://www.acceleratehs.org) (the successor to [Data Parallel Haskell](https://wiki.haskell.org/GHC/Data_Parallel_Haskell)). Both libraries have many similarities, for example, in the support for shape-polymorphic array code, but also exhibit a fundamental difference: Repa is a standard collection-oriented Haskell library , much like `vector`, whereas Accelerate is an *embedded* array language in Haskell. 

The major technical consequence out of that distinction is that Repa code is compiled prior to runtime with GHC’s standard code generator. In contrast, Accelerate code is just-in-time compiled at application runtime using a custom code generator. In fact, there are multiple code generators, which results in support for multicore CPUs as well as GPUs. This added flexibility and, in many cases, performance advantage comes at the cost of a somewhat less expressive and more involved programming model.

## Beyond
In addition to the packages explicitly mentioned above, there are many experimental, incomplete, or unsupported alternatives. Some of them incorporate great ideas and some show impressive performance. However, the criteria for the packages explicitly mentioned in this post are that a package must (1) be available on Hackage, (2) be actively maintained, and (3) be used outside of the group that develops it.

 [Let me know](email:manuel.chakravarty@tweag.io) what your favourite array library is and what you are using it for. In the next instalment, we will have a closer look at `hmatrix`.
