---
title: "Why Liquid Haskell matters"
author: Facundo Domínguez
tags: [liquidhaskell, haskell, formal-methods]
description: "On the relevance of Liquid Haskell in programming languages"
---

Since the inception of the Haskell language, the community around it has
steadily innovated with language extensions and abstractions. To mention
a few, we had the `IO` monad, all flavors of type classes and constraints,
Template Haskell, generalized algebraic data types (GADTs), data kinds, etc.
Then we had to deal with the programs resulting from all this innovation,
which eventually sprang feelings that ranged from sticking to
[Simple Haskell][simple-haskell] to supporting full motion towards
[Dependent Haskell][dependent-haskell].

There seems to be two design goals at conflict in current discussions.
On the one hand, we want our programs to be easy to maintain. For
instance, changes to one part of the code shouldn't propagate to many
others, and the meaning of a piece of code should remain reasonably
deducible from the text.

On the other hand, we want our programs to do what we mean, and we want
assistance from the computer to help us achieve this goal. Our human brains
can only deal with a limited amount of complexity, and industrial-scale
software has largely exceeded our capacity in the last several
decades. But can we prove the correctness of our programs with a type-system
that doesn't make them hard to read or change?

In this post, I'm arguing that [Liquid Haskell][liquid-haskell] offers an alternative
angle to approach this question. Liquid Haskell is a tool that can analyse
a program and calculate proof obligations that would ensure that the program
meets some specification (not unlike [Dafny][dafny], [Why3][why3], or [F\*][fstar]).
The specification is included in the program
as a special comment inserted by the programmer. The compiler ignores this
comment, but Liquid Haskell can find it. Once the proof obligations are
identified, they are given to a theorem prover (an [SMT][smt] solver specifically)
in an attempt to save the programmer the trouble of writing a proof.

The conjecture that I pose is that many of the properties
that would usually require dependent types to ensure at compile time, can
be described in Liquid Haskell specifications. The point is not so much that proofs
are easier with Liquid Haskell, but rather that we are in front of an
approach that integrates well with a programming language as is, and yet
it leverages the power of tools specialized to reason about logic when
verifying programs.

## Verifying functions

Let us consider an indexing function for lists.

```Haskell
elemAt :: [a] -> Int -> a
elemAt (x :  _) 0 = x
elemAt (_ : xs) i = elemAt xs (i-1)
```

This function can fail at runtime if the index is negative or greater
or equal to the length of the list. If we wanted to do indexing safely, a
formulation with dependent types could change the type of the
function. The list type is replaced with the type of lists with a given
length, and the integer type is replaced with the type of natural numbers
smaller than the length.

```Haskell
elemAt1 :: Vec a n -> Fin n -> a
elemAt1 xs i = case (i, xs) of
  (FZ, x :>  _) -> x
  (FS i, _ :> xs) -> elemAt1 xs i

-- Vec a n = lists of length n
data Vec :: Type -> Nat -> Type where
  VNil :: Vec a Zero
  (:>) :: a -> Vec a n -> Vec a (Succ n)

-- Fin n = natural numbers smaller than n
data Fin :: Nat -> Type where
  FZ :: Fin (Succ n)
  FS :: Fin n -> Fin (Succ n)

data Nat = Zero | Succ Nat
```

We include the types of `Vec` and `Fin` to illustrate some of the
complexity that needs to be introduced to work with dependent types,
but we are not overly concerned with their details. A gentle
presentation of these and the following examples with dependent types
can be found in [this tutorial][stitch-tutorial].

The original function has been rewritten to be total. The types of the
arguments have been changed to exclude from the domain the invalid
indices. Being a total function, it can no longer fail at runtime.
Moreover, the new types force all invocations to provide a valid index.
In the transformation, though, we lost the original simplicity. The
list type is no longer the standard list type for which many functions
are offered in the standard libraries, and the efficient `Int` type has
been replaced by a representation that counts sticks.
To avoid conversions between the standard types and the new `Vec` and
`Fin` types, we could be tempted to use these types in code that needs
list indexing, which then would make this change less local than we
could wish for.

With Liquid Haskell, instead, we could get our safety guarantees by writing a
specification of the original function expressed with predicate logic.
We use a refinement type on the index argument to state its validity.

```Haskell
-- specifications go between special comments `{-@ ... @-}`
{-@ elemAt :: xs:[a] -> { i:Int | 0 <= i && i < len xs } -> a @-}
elemAt :: [a] -> Int -> a
elemAt (x :  _) 0 = x
elemAt (_ : xs) i = elemAt xs (i-1)
```

A refinement type denotes a subtype of another type,
characterized by a given predicate. In our example, `{ i:Int | 0 <= i && i < len xs }`
is a subtype of `Int`, whose values `i` satisfy the predicate
`0 <= i && i < len xs`. The specifications allow us to name the arguments
of functions and to refer to them in the predicates, such as
the list argument named `xs`.
The language used to describe a predicate is not Haskell, but a separate
language with functions and logical connectives. When predicates need to
refer to Haskell functions, the functions can be translated to the logic.

The function `elemAt` continues to be partial in Haskell, but it
is total according to its specification. And Liquid Haskell is powerful
enough to check that `elemAt` meets the
specification, i.e. given a valid index, the function won't fail.
It is a choice of the user to use Liquid Haskell to ensure that the
invocations meet the specification as well, or to leave
the invocations unverified. The
solution not only leaves the original function untouched, which dependent
types could also achieve, but it is remarkably economical in the way of
expressing the property we care about.

## Beyond bounds checking

Bounds checking is useful enough, but given the ability of Liquid
Haskell to translate pure Haskell functions into the logic, we can
aspire to verify other kinds of properties too.
The following example is a datatype that can be used to represent untyped
lambda expressions. It has constructors for variables, lambda abstractions,
and applications.

```Haskell
-- | Unchecked expression, indexed by a bound of the allowed free variables
data UExp (n :: Nat)
  = UVar (Fin n)   -- ^ de Bruijn index for a variable
  | ULam (UExp (Succ n))
  | UApp (UExp n) (UExp n)
  deriving Show
```

Variables are represented with a de Bruijn index. That is, the variable
`UVar i` refers to the variable bound by the i-th lambda abstraction surrounding
the variable occurence. Thus, `ULam (ULam (UVar FZ))` stands for the lambda expression
`\x . \y . y`, and `ULam (ULam (UVar (FS FZ)))` stands for `\x. \y. x`.

The type index of `UExp` allows us to refer conveniently to closed expressions as
`UExp Zero`. In a closed expression, all variables have a matching lambda
abstraction. Thus, `ULam (UVar FZ) :: UExp Zero` type checks, but
`UVar FZ :: UExp Zero` wouldn't typecheck
because there is no lambda abstraction to bind `UVar FZ` and it has type
`forall n. UExp (Succ n)`.

Besides the fact that we are still counting sticks to identify variables,
the type index needs to be carried around everywhere we deal with these
expressions regardless of the task at hand.
The Liquid Haskell counterpart looks as follows.

```Haskell
{-@
data UExp
  = UVar ({i:Int | 0 <= i}) // indices are specified to be non-negative
  | ULam UExp
  | UApp UExp UExp
@-}
data UExp
  = UVar Int
  | ULam UExp
  | UApp UExp UExp

-- | Computes an upper bound of the variables that appear free
-- in an expression.
{-@ reflect freeVarBound @-}
{-@ freeVarBound :: UExp -> { i:Int | 0 <= i } @-}
freeVarBound :: UExp -> Int
freeVarBound (UVar v) = v + 1
freeVarBound (ULam body) = max 0 (freeVarBound body - 1)
freeVarBound (UApp e1 e2) = max (freeVarBound e1) (freeVarBound e2)
```

With these definitions we can mimic the original indexed type
with the following type synonyms.

```Haskell
-- Type synonym parameters starting with an upper case letter stand
-- for variables that are substituted with term-level expressions
-- instead of types.
{-@ type UExpN N = { e:UExp | freeVarBound e <= N } @-}
{-@ type ClosedUExp = UExpN 0 @-}
```

One feature of the Liquid Haskell way is that we don't need to use
a type index in the Haskell datatypes. We are only concerned with
the ability to express the type of closed expressions in specifications,
not in the actual program.

Another feature is that we are using a simple Haskell function to
express what the bound of free variables is for a particular expression,
and then we are using this function in the logic to say that an
expression is closed. The `reflect` keyword in the
specification of `freeVarBound` is directing Liquid Haskell to translate
the function to the logic language.

Now we can test our type specifications with example expressions,
and this is possible without a single GADT or a promoted data
constructor.

```Haskell
{-@ e0 :: ClosedUExp @-}
e0 :: UExp
e0 = ULam (UVar 0)

{-@ e1 :: ClosedUExp @-} -- Fails verification
{-@ e1 :: UExpN 1 @-} -- Passes verification
e1 :: UExp
e1 = UVar 0
```

## Beyond closed expressions

As a final example, let us consider a juicier Haskell datatype to
represent typed lambda expressions. These expressions are functions
manipulating values of some opaque type `T`.

```Haskell
-- | @Exp ctx ty@ is a well-typed expression of type @ty@ in context
-- @ctx@. Note that a context is a list of types, where a type's index
-- in the list indicates the de Bruijn index of the associated term-level
-- variable.
data Exp :: forall n. Vec Ty n -> Ty -> Type where
  Var   :: Elem ctx ty -> Exp ctx ty
  Lam   :: Exp (arg :> ctx) res -> Exp ctx (arg :-> res)
  App   :: Exp ctx (arg :-> res) -> Exp ctx arg -> Exp ctx res

-- | A type encoding types. @T@ is the atom while @:->@ refers to
-- the type arrow '->'.
data Ty = T | Ty :-> Ty

-- | @Elem xs x@ is evidence that @x@ is in the vector @xs@.
-- @EZ :: Elem xs x@ is evidence that @x@ is the first element of @xs@.
-- @ES ev :: Elem xs x@ is evidence that @x@ is one position later in
-- @xs@ than is indicated in @ev@
data Elem :: forall a n. Vec a n -> a -> Type where
  EZ :: Elem (x :> xs) x
  ES :: Elem xs x -> Elem (y :> xs)
```

The `Exp` datatype still tracks whether variables are in scope, this time
with an `Elem` type that stands for a proof that a variable is in the
typing context. Additionally, the datatype ensures that the expressions
are well typed.

In Liquid Haskell we start with untyped expressions as before, with the
novelty that the `ULam` constructor now has an extra argument to
indicate the type of the binding. The extra argument is necessary to
implement a type inference function named `inferType`.

```Haskell
data UExp
  = UVar Int
  | ULam Ty UExp
  | UApp UExp UExp
  deriving Show

{-@ reflect elemAt @-}
{-@ reflect inferType @-}
{-@ inferType :: ctx:[Ty] -> UExpN (len ctx) -> Maybe Ty @-}
inferType :: [Ty] -> UExp -> Maybe Ty
inferType ctx (UVar i) = Just (elemAt ctx i)
inferType ctx (ULam t body) =
  case inferType (t : ctx) body of
    Just r -> Just (t :-> r)
    Nothing -> Nothing
inferType ctx (UApp e0 e1) =
  case inferType ctx e0 of
    Just (a :-> r) -> case inferType ctx e1 of
      Just t -> if a == t then Just r else Nothing
      Nothing -> Nothing
    _ -> Nothing
```

Liquid Haskell verifies that the list indexing in the first equation of
`inferType` is safe, thanks to the refinement type of the
expression that ensures that the index is within the bounds of the list.
The same refinement type ensures that we don't forget to grow the context
when we infer the type for the body of a lambda expression. And we also
get the assurance that `inferType` terminates on inputs that meet the
specification.
In order to express well-typed terms, we only need now a type synonym.

```Haskell
{-@ type WellTypedExp CTX TY = { e:UExp | freeVarBound e <= len CTX && inferType CTX e == Just TY } @-}

{-@ e2 :: WellTypedExp [T] T @-}
e2 :: UExp
e2 = UVar 0

{-@ e3 :: WellTypedExp [] (T :-> T) @-}
e3 :: UExp
e3 = ULam T (UVar 0)
```

The specification of well-typed terms borrows the function `inferType`
from Haskell, which has been translated to the logic. This allows to
reuse the understanding that the user has of `inferType` to express
what being well-typed means. When verifying `e2` and `e3`, Liquid
Haskell relies on the the SMT solver and other reasoning mechanisms
[of its own][ple].

## Summing up

The examples in this post that use Liquid Haskell can be found
[here][whylh-example].
If you would like to see more substantial use of Liquid Haskell, there are two
implementations of an interpreter for lambda expressions that can be
compared side by side. [The first implementation][stitch] relies on
emulating dependent types in Haskell and was implemented by fellow
Tweager Richard Eisenberg. [The second implementation][stitch-lh] is my
reimplementation of Richard's using Liquid Haskell.

In this post I have presented some examples where Liquid Haskell
is expressive enough to deal with properties that are typically
associated with the need of a dependently-typed language. Yet,
unlike dependent types, Liquid Haskell allows us to keep programs
to verify unchanged from the perspective of the compiler, and the
trouble of discharging proof obligations is addressed in cooperation
with theorem provers.

The downside of this way is that we now have to deal with the
complexities of the integration. We need to orchestrate the communication
between the different tools, translate our programs into the logics
of the theorem provers, and translate the errors back to terms that
the user can relate to the program being verified. There are more
opportunities to make mistakes in the seams between the tools, while
dependently-typed languages are often built on a single compiler with
a possibly smaller trusted core.

On the user side, though, we can turn Liquid Haskell into a philosophy
for approaching verification. We are not necessarily doomed
to build a behemoth language-implementation capable of verifying it all.
Instead, we can separate our verification from our programming needs
and tackle each group with dedicated tools in order to better reuse
code and techniques on both fronts.

[dafny]: https://github.com/dafny-lang/dafny
[dependent-haskell]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
[fstar]: https://www.fstar-lang.org
[liquid-haskell]: https://ucsd-progsys.github.io/liquidhaskell-blog/
[ple]: https://arxiv.org/abs/1711.03842
[simple-haskell]: https://www.simplehaskell.org/
[smt]: https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
[stitch]: https://gitlab.com/goldfirere/stitch
[stitch-lh]: https://github.com/facundominguez/stitch-lh
[stitch-tutorial]: https://dl.acm.org/doi/abs/10.1145/3406088.3409015
[why3]: http://why3.lri.fr
[whylh-example]: https://github.com/ucsd-progsys/liquidhaskell/blob/361a6af39d54e1fbf5cc123a358e1add9d26c88a/tests/pos/WhyLH.hs
