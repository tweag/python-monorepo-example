---
title: "Staged programming with typeclasses"
author: Thomas Bagrel
tags: [haskell]
description: "Introduction to staged programming in Haskell with typeclasses"
---

Staged programming consists of evaluating parts of a program at compile time
for greater efficiency at runtime, as some computations would have already been
executed or made more efficient during compilation. The poster child for
staged programming is the exponential function: to compute `a^b`, if `b` is
known at compile time, `a^b` can be replaced by `b` explicit
multiplications. Staged programming allows you to write
`a^5`, but have the expression compile to `a*a*a*a*a`.

In Haskell, the traditional way to do staged programming is to
[reach for Template Haskell][beautiful-template-haskell]. Template Haskell
is, after all, designed for this purpose and gives you strong
guarantees that the produced code is indeed `a*a*a*a*a`, as
desired. On the other hand it does feel a little heavyweight and
programmers, in practice, tend to avoid exposing Template Haskell in
their interfaces.

In this blog post, I want to present another way to do staged programming that
is more lightweight, and feels more like a native Haskell solution, but, in
exchange, offers fewer guarantees. At its core, what is needed for staged
programming is to distinguish between what is statically known and what is
dynamically known. In Template Haskell, static and dynamic information is
classified by whether an expression is within a quotation or not. But there is
another way to signal statically-known information in Haskell: types.

This is what we are going to do in this blog post: passing
statically-known arguments at the type level. I've
[used this technique in linear-base][linear-base-staging].

## Natural numbers at the type level

Haskell offers a native kind `Nat` of type-level natural numbers. We
could pass the (statically known) exponent as `Nat`, in fact we
eventually will, but it is difficult to consume numbers of kind `Nat`
because GHC doesn't know enough about them (for instance, GHC doesn't
know that `n+1` is equivalent to `1+n`).

Instead, we will use an inductive encoding of the natural numbers:
[the Peano encoding][peano-naturals].

```haskell
data Peano
  = Z         -- zero
  | S Peano   -- successor of another peano number
```

In this encoding, 3 is written `S (S (S Z))`.

Normally, `Peano` would live at the type level, and both `Z` and `S` would
live at the term level (they're data constructors after all). But thanks to
the `DataKinds` extension – which allows data constructors to be promoted to
types – we can also use `Peano` as the kind of type-level `Z` and `S`.

Now let's return to the `power` function. We will first create a typeclass
`RecurseOnPeano`, that will contain the `power` function (and that could host
any other recursive metaprogramming function that operates on `Peano`s):

```haskell
class RecurseOnPeano (n :: Peano) where
  power :: Int -> Int
```

The `power` function only needs one term-level parameter: the number that will
be multiplied by itself `n` times. Indeed, the exponent is already "supplied"
as a type-level parameter `n`. In fact, the signature of the `power` function
outside the typeclass would be:

```haskell
power :: forall (n :: Peano). RecurseOnPeano n => Int -> Int
```

At a call site, the type-level parameter `n` will be supplied to the function
through a _type application_, using the dedicated `@` symbol (e.g.
`power @(S (S Z)) 4`). It isn't possible to omit the type parameter `n` at a
call site because there is no way for GHC to deduce it from the type of a
term-level parameter of the function. So we need to enable the
`AllowAmbiguousTypes` extension.

The implementation of the `power` function will be defined through two
instances of the `RecurseOnPeano` typeclass – one for the base case (`n = Z`),
and one for the recursive case (`n = S n'`) – as one would do in a term-level
recursive function.

The first instance is relatively straightforward as `x^0 = 1` for every
positive integer `x`:

```haskell
instance RecurseOnPeano Z where
  power _ = 1
```

For the second instance we want to write `power @(S n) x = x * power @n x`. But
to use `power @n x`, `n` needs to fulfill the `RecurseOnPeano` constraint too.
In the end, that yields:

```haskell
instance RecurseOnPeano n => RecurseOnPeano (S n) where
  power x = x * power @n x
```

We now have a first working example:

```haskell
-- <<<<<<<<<<<<< file CompileRecurse.hs >>>>>>>>>>>>>

{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module CompileRecurse where
import GHC.TypeLits

data Peano = Z | S Peano

class RecurseOnPeano (n :: Peano) where
  power :: Int -> Int

instance RecurseOnPeano Z where
  power _ = 1
  {-# INLINE power #-}
instance RecurseOnPeano n => RecurseOnPeano (S n) where
  power x = x * power @n x
  {-# INLINE power #-}

-- <<<<<<<<<<<<< file Main.hs >>>>>>>>>>>>>

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where
import CompileRecurse

main :: IO ()
main = print $ power @(S (S (S Z))) 2  -- this should print 8
```

Many languages extensions are required for this example to work:

- `KindSignatures` permits the syntax `(n :: Peano)` to restrict the
  `RecurseOnPeano` class to types of the `Peano` kind.
- `TypeApplications` gives the `@type` syntax to supply type-level parameters.
- `DataKinds` allows us to promote the `Peano` data type to the kind level.
- `ScopedTypeVariables` is needed to be able to refer to `n` in the body of
  `power` in the second instance of `RecurseOnPeano`.
- `AllowAmbiguousTypes` is needed when we declare a typeclass function in
  which the term-level parameters (if there are any) are not sufficient to
  infer the type-level parameters (and thus require an explicit type
  application at the call site).

I also added `{-# INLINE #-}` pragmas on the `power` implementations, because
we indeed want GHC to inline these to achieve our initial goal. For such a
simple example, GHC would inline them by default, but it's better to be
explicit about our intent here.

You can now validate that the `power @(S (S (S Z))) 2` encoding for `2^3`
indeed prints `8` on the terminal.

## From `Peano` type-level integers to GHC `Nat`s

Writing `S (S (S Z))` is not very convenient. We would definitely prefer to
write `3` instead. And that is possible, if we allow a bit more complexity in
our code.

Number literals, such as `3`, when used at the type level are of kind `Nat`
from `GHC.TypeLits`.

Unfortunately, if we completely replace our home-made `Peano`s with GHC `Nat`s,
we will run into some issues of overlapping instances in the `RecurseOnPeano`
typeclass.[^1]

A solution can be found by using the `{-# OVERLAPPING #-}` and
`{-# OVERLAPPABLE #-}` pragmas, but it is quite fragile: instance selection is
no longer driven by types or structure but rather by a manual override. And
[the rules for such an override][overlapping-rules] are rather complex,
especially when more than two instances are involved; in the case at hand, we
might want to add a third instance with a specific implementation for `n = 1`.

Instead, we will add a type family (that is, a function from types to types) to
convert from `Nat`s to `Peano`s, and add an auxiliary function `power'` that
will take a type-level `Nat` instead of a type-level `Peano`:

```haskell
-- <<<<<<<<<<<<< add to file CompileRecurse.hs >>>>>>>>>>>>>

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

type family NatToPeano n where
  NatToPeano 0 = Z
  NatToPeano n = S (NatToPeano (n - 1))

-- 'RecurseOnPeano (NatToPeano n) =>' means that the ¨Peano equivalent of n
-- must be an instance of RecurseOnPeano to get access to 'power'
power' :: forall (n :: Nat). (RecurseOnPeano (NatToPeano n)) => Int -> Int
power' = power @(NatToPeano n)

-- <<<<<<<<<<<<< change in file Main.hs >>>>>>>>>>>>>

main = print $ power' @3 2  -- this should still print 8
```

Our function is still working as expected, and is now more convenient to use!

## A look under the hood

Our initial goal was to unroll the `power'` function at compile
time. Let's check whether this promise holds.

We will create a new test file `test/CompileRecurseTests.hs` and set specific
GHC options so that we can take a look at the generated Core[^2] code for our
project:

```haskell
{-# OPTIONS_GHC -O -ddump-simpl -dsuppress-all -dsuppress-uniques -ddump-to-file #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import CompileRecurse

myFunc :: Int -> Int
myFunc x = power' @3 x + 1

main :: IO ()
main = return ()
```

The following GHC flags are used:

- `-O` enables optimizations in GHC.
- `-ddump-simpl` requests the Core code after the output of the simplifier.
- `-dsuppress-all` and `-dsuppress-uniques` reduce the verbosity of the output
  (otherwise, searching for a specific piece of code would become very
  tedious).
- Finally, `-ddump-to-file` asks for the output to be written to a file in the
  build directory.

With the above options, compiling and running the test suite creates a
file `CompileRecurseTests.dump-simpl` deep down in the build tree.[^3]
If we ignore all the lines about `$trModule`, we get:

```
-- RHS size: {terms: 12, types: 3, coercions: 0, joins: 0/0}
myFunc
  = \ x -> case x of { I# x1 -> I# (+# (*# x1 (*# x1 x1)) 1#) }
```

`I#` is the "boxing" constructor for integers, that is, the one taking an
unboxed integer (`Int#`) and creating a Haskell `Int` (an integer behind a
pointer). `+#` and `*#` are the equivalent of arithmetic functions `+` and `*`
for unboxed integers `Int#`.

We can see that `myFunc`

- takes an `Int`,
- unboxes its value,
- makes the 2 product operations corresponding to the inlined `power' @3 x`,
- adds `1`, and finally,
- boxes the result once again to produce an `Int`.

There is no mention of `power'` here, so the function has been successfully
inlined!

## Inspection testing

Checking manually whether or not the inlining has happened – by looking
through the `.dump-simpl` file after every change – is really impractical.
Instead, it is possible to use the [`inspection-testing`][inspection-testing]
and [`tasty-inspection-testing`][tasty-inspection-testing] libraries to
automate such a process.

To do this, we simply need to introduce a function `myFunc'` – corresponding to
what we expect to be the optimized and inlined form of `myFunc` – and then we
check that both `myFunc` and `myFunc'` result in the same generated Core code
by using the specific `===` comparison operator (and a little bit of Template
Haskell too):

```haskell
{-# OPTIONS_GHC -O -dno-suppress-type-signatures -fplugin=Test.Tasty.Inspection.Plugin #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Tasty
import Test.Tasty.Inspection
import CompileRecurse

myFunc :: Int -> Int
myFunc x = power' @3 x + 1

myFunc' :: Int -> Int
myFunc' x = x * (x * x) + 1

main :: IO ()
main = defaultMain . testGroup "Inspection testing of power'" $
  [ $(inspectTest $ 'myFunc === 'myFunc') ]
```

Running the test suite gives:

```
Inspection testing of power'
  myFunc === myFunc': OK

All 1 tests passed (0.01s)
```

If both functions didn't result in the same generated Core code – e.g. if we
wrote `(x * x) * x + 1` instead of `x * (x * x) + 1` in `myFunc'` – we would
get:

```
Inspection testing of power'
  myFunc === myFunc': FAIL
    LHS:
        [ ... ]
        myFunc
          = \ (x [Dmd=<S,1*U(U)>] :: Int) ->
              case x of { I# x1 -> I# (+# (*# x1 (*# x1 x1)) 1#) }
    RHS:
        [ ... ]
        myFunc'
          = \ (x [Dmd=<S,1*U(U)>] :: Int) ->
              case x of { I# x -> I# (+# (*# (*# x x) x) 1#) }

1 out of 1 tests failed (0.01s)
typeclass-blogpost> Test suite inspection-tests failed
```

In this way, the correct inlining of `power'` can be checked automatically
after each change to the codebase!

## Conclusion

This was a brief introduction to staged programming in Haskell, leveraging the
type (and typeclass) system as a lightweight alternative to Template Haskell.
The technique detailed in this article has been implemented in real-world
contexts to create variadic functions like [`printf`][variadic-printf],
and I hope that you will find many other useful applications for it!

I would like to give a special thank you to Arnaud Spiwack who both taught me
this technique in the first place, and then helped me to greatly improve this blog
post.

<!-- Footnotes -->

[^1]:
  In short, this is because GHC can't distinguish between the base and
  recursive instances with `Nat`s as easily as it can with `Peano`s

[^2]: Core is the main intermediate language used inside GHC.
[^3]:
  In my case, the full path was:
  `.stack-work/dist/x86_64-linux-nix/Cabal-3.4.1.0/build/compile-recurse-tests/compile-recurse-tests-tmp/test/CompileRecurseTests.dump-simpl`.

<!-- Links -->

[linear-base-staging]: https://hackage.haskell.org/package/linear-base-0.2.0/docs/Data-Replicator-Linear.html#v:elim
[peano-naturals]: https://wiki.haskell.org/Peano_numbers
[beautiful-template-haskell]: https://www.youtube.com/watch?v=AzJVFkm42zM
[overlapping-rules]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/instances.html#extension-OverlappingInstances
[inspection-testing]: https://hackage.haskell.org/package/tasty-inspection-testing
[tasty-inspection-testing]: https://hackage.haskell.org/package/inspection-testing
[variadic-printf]: https://hackage.haskell.org/package/base-4.17.0.0/docs/Text-Printf.html#v:printf
