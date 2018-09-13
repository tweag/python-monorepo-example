---
title: "capabilities-via: ReaderT pattern without boilerplate"
shortTitle: Announcing capabilities-via
author: Andreas Hermann & Arnaud Spiwack
---

Let's talk about the [ReaderT pattern][readert]. Ostensibly, this is a
blog post about preferring to encode state as `IORef`-s when you
can. But that's now how _we_ read it. Instead, we see a story about
using extensional type classes describing specifically the effects
that functions use (the `MonadBalance` story, in the [ReaderT pattern
blog post][readert]). We call such dedicated type classes
capabilities. Here is an excellent [blog post][three-layer-cake] from
Matt Parsons which takes this aspect to heart.

Capabilities-via is a library about these capabilities.

[readert]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
[three-layer-cake]: http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html

## The difference with the Mtl

How is using dedicated capabilities, like in Snoyman's and Parsons's
blog posts any different from using the well-trodden mtl? Very! You
see, the mtl's classes `MonadReader`, `MonadState`, and all that, are
_intensional_: they reflect how the monad has been constructed. A
monad `M` is a `MonadState` because it is a stack of monad
transformers, one of which is a `StateT`.

This is because of what Haskell instances mean: if I have an instance

```haskell
instance MonadState s m => MonadState s (ReaderT r m)
```

while it may look like we're saying that it _suffices_ to have
`MonadState s m` for `ReaderT r m` to be `MonadState r`, what we are
really saying is that `MonadState s (ReaderT r m)` _means_ that
`MonadState s m`. It defines a computation, rather than a
deduction. In particular, we are not permitted to add an instance

```haskell
instance MonadState S (ReaderT (IORef S) IO)
```

You may want to work around this issue using `{-# OVERLAPPING #-}`
instances. However, in doing so, you are acting against the semantics
of instances, and heading for troubles. For an example of issues with
overlapping instances, see the warning at the end of the [Overlapping
instances
section](https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#overlapping-instances)
of the GHC manual.

In contrast, what the ReaderT pattern is advertising, is extensional
classes, indicating what an individual function _can_ do (hence the
name "capability"), regardless of how the monad is implemen

- intensionality
- shortcomings of mtl:
  - composability (several state of the same type, see Andreas below)

## The problem

- a lot of boilerplate
- a lot of boilerplate
- a lot of repetition

## DerivingVia

See Andreas below

## Enter capabilities-via

# Andreas's writeup

[capabilities-via][capabilities-via] leaverages the newly added
[`DerivingVia`][deriving-via] language extension to allow you to generate
instances for tagged capability type-classes, such as `HasState`, for newtype wrappers
choosing the strategy that best fits your application.


## What is DerivingVia

First of all, what is this new language extension?

In short, `DerivingVia` is similar to `GeneralizedNewtypeDeriving`,
but allows you to choose a different combination of `newtype` wrappers
for each instance that you're deriving. For example:

``` haskell
{-# LANGUAGE DerivingVia #-}
import Data.Monoid (Sum (..))
newtype MyInt = MyInt Int
  deriving (Monoid, Semigroup) via Sum Int
  deriving Num via Int
```

In the above snippet we define `MyInt` which wraps an `Int`,
and derive two instances for it.
The `Monoid` instance is taken from `Sum Int`,
and the `Num` instance is taken directly from `Int`.
Note the `via` keyword in the deriving clauses.
(In this example we could also have derived the `Num` instance
using `GeneralizedNewtypeDeriving`.)

The [proposal][proposal] and the [paper][paper] give a more detailed account.

[proposal]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-deriving-via.rst
[paper]: https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf


## Capabilities

What do we mean when we say capabilities?
We mean a collection of effectful operations in a monad `m` capture by a type-class.
The well known `mtl` type-classes, e.g. `MonadReader r`, `MonadState s`,
are a good example of this.
However, less generic application defined collections of operations also fit the bill.
Contrary to the `mtl` type-classes the capabilities provided by this library
are also tagged, to allow composition of programs using similar capabilities.
We will see what this means in the next section


## Why not just use mtl?

The monad transformer library has been the go to library for common effects
such as reader, writer, state for a long time. There are two parts to it.

On the one hand mtl re-exports the monad transformers of the `tranformers` library,
such as `ReaderT`, or `StateT`. These can be combined to construct a monad transformer
that covers multiple effects in one. E.g.

```
-- Provides Reader in Ctx and State in A and B.
type MyM a = ReaderT Ctx (StateT A (State B)) a
```

However,
these transformers alone do not offer a compelling API,
as we have to manually apply `lift` the right number of times
to reach a particular layer in the stack.

```
askCtx = ask :: MyM Ctx
getA = lift get :: MyM A
getB = lift (lift get) :: MyM B
```

This is where mtl offers type-classes that hide the `lift`-ing from us.

```
askCtx :: MonadReader Ctx m => m Ctx
askCtx = ask
getA :: MonadState A m => m A
getA = get
getB :: MonadState B m => m B
getB = get
```

However, if we try to combine `getA` and `getB` as follows:

``` haskell
ouch :: MyM B
ouch = getA >> getB
```

Then we get a compiler error complaining that `A` and `B` don't match.
(If `A` and `B` were the same type,
then `getA` and `getB` would just operate on the same layer,
while the other layer would be silently ignored.)

With mtl we can mix state with reader with writer, no problem.
But, we cannot mix two states, or two readers, etc.

Furthermore, large transformer stacks can hinder GHC's optimizations
(XXX: link?),
and there is an implementation cost to the mtl approach,
which is known as the n^2 instances problem.
Each transformer comes with its own type class, e.g. `ReaderT` and `MonadReader`,
and for each transformer we have to write an instance for every class.

If the transformer stack is based on `IO`,
then the [`ReaderT` pattern][reader-t-pattern] gives a few more arguments
against using monad transformer stacks.

[reader-t-pattern]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern


## Enter capabilities-via

The capabilities that this library provides are tagged.
This allows to combine multiple state computations as in the example above.
E.g.

``` haskell
getAB :: (HasState "a" A m, HasState "b" B m) => m B
getAB = get @"a" >> get @"b"
```

And instead of providing monad transformers to be stacked
the library provides composable `newtype` wrappers that allow you to express
stategies by which GHC should derive instances for different capabilities
for your application monad.

For example you can derive `HasReader` from an mtl `MonadReader`
using the `newtype` wrapper of the same name.

XXX: Should we rename the `MonadReader`, `MonadState` strategies to `MTL`?

``` haskell
import Control.Monad.Reader (ReaderT (..))
import HasReader

data AppData = ...
newtype AppM a = AppM (ReaderT AppData IO a)
  deriving (HasReader "app-data" AppData) via
    MonadReader (ReaderT AppData IO)
```

``` haskell
newtype MyM a = MyM (ReaderT Ctx (StateT A (State B)) a)
  deriving (Functor, Applicative, Monad)
  -- Using the MonadReader instance of ReaderT
  deriving (HasReader "ctx" Ctx) via
    MonadReader (ReaderT Ctx (StateT A (State B)))
  -- Using the MonadState instance of ReaderT StateT
  deriving (HasState "a" A) via
    MonadState (ReaderT Ctx (StateT A (State B)))
  -- Lifting the MonadState instance from the inner StateT
  deriving (HasState "b" B) via
    Lift (ReaderT Ctx (Lift (StateT A (MonadState (State B)))))
```

<!--  LocalWords:  intensional
 -->
