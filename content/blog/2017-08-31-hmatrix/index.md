---
redirect_from: [/posts/2017-08-31-hmatrix.html]
title: Enter the matrix, Haskell style
author: Manuel M T Chakravarty
featured: yes
tags: [haskell]
---

_This is the second post in a series about array programming in Haskell — you might be interested in the [previous post](http://www.tweag.io/posts/2017-08-09-array-programming-in-haskell.html), too._

Matrices are the bread and butter of most scientific and numeric computing. It is not surprising then that there is a range of standard libraries and interfaces, first and foremost [BLAS (Basic Linear Algebra Subprograms)](http://www.netlib.org/blas/) and [LAPACK — Linear Algebra PACKage](http://www.netlib.org/lapack/), which have been around since the FORTRAN days, and more recently also the [GSL (GNU Scientific Library)](https://www.gnu.org/software/gsl/).

In Haskell, [`hmatrix`](https://github.com/AlbertoRuiz/hmatrix) provides a uniform interface to much of the functionality of these three libraries. The functionality is split over four packages, [`hmatrix`](https://hackage.haskell.org/package/hmatrix) (linear algebra), [`hmatrix-gsl`](http://hackage.haskell.org/package/hmatrix-gsl) (common numeric computations), [`hmatrix-gsl-stats`](https://hackage.haskell.org/package/hmatrix-gsl-stats)(GSL statistics), and [`hmatrix-special`](http://hackage.haskell.org/package/hmatrix-special) (the ”special” functions of GSL). Due to the popularity of `hmatrix`, there exists a whole ecosystem of packages on [Hackage](http://hackage.haskell.org) that either build on `hmatrix` , provide bindings to other standard C libraries by extending the `hmatrix` interface, and implement adaptors to interoperate with other array libraries (such as [hmatrix-repa](https://hackage.haskell.org/package/hmatrix-repa)).

## Vectors and matrices

At the core of `hmatrix` are the `Vector` and `Matrix` data types,

```
data Vector e
data Matrix e
```

The `Vector` type is based on the `Storable` instances of the widely used [`vector` ](https://hackage.haskell.org/package/vector) package and `Matrix` adopts a matrix representation that enables the efficient use of BLAS routines. Values of both types can be easily created from lists and come with the expected basic vector and matrix functions. For example, we have

```
hmatrix> vector [1,2,3] * vector [3,0,-2]
[3.0,0.0,-6.0]
```

Although, the result is displayed in list syntax, it is indeed a `Vector`.

```
hmatrix> :t vector [1,2,3] * vector [3,0,-2]
vector [1,2,3] * vector [3,0,-2] :: Vector R
```

The element type `R` is one of several type synonyms used in some of the `hmatrix` interface:

```
type R = Double
type C = Complex Double
type I = CInt
type Z = Int64
```

Similarly, for matrices, we have

```
hmatrix> matrix 3 [1..9] * ident 3
(3><3)
 [ 1.0, 0.0, 0.0
 , 0.0, 5.0, 0.0
 , 0.0, 0.0, 9.0 ]
hmatrix> :t matrix 3 [1..9] * ident 3
matrix 3 [1..9] * ident 3 :: Matrix R
```

where `matrix 3` turns a list into a matrix with `3` columns and `ident` produces the identity matrix of a given size.

## Generalised matrices

In addition to the dense `Vector` and `Matrix` types, `hmatrix` also provides a ”general” matrix type `GMatrix` that provides optimised representations for dense, sparse, diagonal, banded, and constant matrices of `Double`s (aka `R`). For example, the following sparse 2x2000 matrix with two non-zero elements is represented as follows in the [compressed sparse row (CSR)](https://en.wikipedia.org/wiki/Sparse_matrix#Compressed_sparse_row_.28CSR.2C_CRS_or_Yale_format.29) format:

```
hmatrix> mkSparse [((0,999),1.0),((1,1999),2.0)]
SparseR
{ gmCSR = CSR
          { csrVals = [1.0,2.0]
          , csrCols = [1000,2000]
          , csrRows = [1,2,3]
          , csrNRows = 2
          , csrNCols = 2000
          }
, nRows = 2
, nCols = 2000
}
```

The package includes an implementation of the conjugate gradient method for sparse linear systems.

## Functionality

In addition to the basic operations on vectors and matrices, the core `hmatrix` package provides solvers for linear systems, computes inverses, determinants, singular value decomposition, eigenvalues & eigenvectors, QR, Cholesky & LU factorisation, and some other common matrix operations.

On top of that `hmatrix-gsl` covers integration, differentiation, FFT, solving general polynomial equations, minimization of a multidimensional functions, multidimensional root finding, ordinary differential equations, nonlinear least-squares fitting, and interpolation routines. Moreover, `hmatrix-gsl-stats` includes random distribution functions, linear regression functions, histograms, permutations, and common statistics functions (mean, variance, standard deviation, and so on).

Finally, `hmatrix-special` provides Airy, Bessel, Clausen, Coulomb wave, coupling coefficient, Dawson, Debye, dilogarithm, elliptic integral, Jacobian elliptic, Fermi-Dirac integral, Gegenbauer, hypergeometric, Laguerre, Lambert W, synchrotron, digamma, trigamma, and transport, Riemann zeta functions as well as Gamma distributions, Legendre polynomials, and common trigonometric and exponential functions.

## An example: minima of arbitrary multidimensional functions

For example, to find the minimum of a function

```
f [x,y] = 10*(x-1)^2 + 20*(y-2)^2 + 30
```

without providing a gradient, we can define

```
minimizeS :: ([Double] -> Double) -> [Double] -> ([Double], Matrix Double)
minimizeS f xi
  = minimize NMSimplex2 1E-2 100 (replicate (length xi) 1) f xi
```

using the `minimize` function of `hmatrix-gsl`’s [Numeric.GSL.Minimization](https://www.stackage.org/haddock/lts-8.9/hmatrix-gsl-0.18.0.1/Numeric-GSL-Minimization.html). It provides us with the minimum as well as the path taken by the algorithm to reach that solution. Using gnuplot by way of `hmatrix`’ undocumented `Graphics.Plot` interface, to visualise the path, we get

![Minimise Plot](./Hmatrix-Minimise-Plot.svg)

For the full example code, see [minimize.hs](https://raw.githubusercontent.com/albertoruiz/hmatrix/master/examples/minimize.hs).

## Type safety

A common mistake in array programming is to apply an operation to one or more matrices whose dimensions violate a precondition of the operation. For example, for matrix multiplication to be well-defined for two matrices `a` and `b`, we require `a` to be *i*x*j* if `b` is *j*x*k*; i.e., the number of `a`’s columns needs to coincide with the number of `b`’s rows. Like most array libraries in Haskell, we cannot express such a constraint in `hmatrix`’ standard interface, where the type of a matrix is independent of the size of its dimensions.

The _static interface_ of `matrix` provides some of `hmatrix`’ functionality with an alternative API, based on [GHC’s type literals extension](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-level-literals), which allows matrix size constraints to be expressed. We will discuss this interface in more detail in a future blog post.

## Context

If you are coming from Python, then `hmatrix` will be the closest to `numpy` and `scipy` that you will find in the Haskell ecosystem. The packages in both languages are realised by wrapping highly optimised standard routines written in low-level languages (such as BLAS and LAPACK) as libraries. However, the functionality they provide is not directly comparable as, despite a strong overlap, both provide functionality that is absent in the other. While `numpy` and `scipy` shine with maturity and very widespread use, Haskell offers increased safety and the potential to provide additional high-performance functionality and fusion of computational kernels in Haskell itself (i.e., without the need to drop down to C or C++) by combining `hmatrix` with some of the array libraries that we will discuss in the forthcoming posts in this series.
