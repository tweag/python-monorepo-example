---
title: "capabilities-via: ReaderT pattern without boilerplate"
shortTitle: Announcing capabilities-via
author: Andreas Hermann & Arnaud Spiwack
---

Let's talk about the [ReaderT pattern][readert]. Ostensibly, this is a
blog post about preferring to encode state as `IORef`-s when you
can. But that's not how _we_ read it. Instead, we see a story about
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
of instances, and heading for trouble. For an example of issues with
overlapping instances, see the warning at the end of the [Overlapping
instances
section](https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#overlapping-instances)
of the GHC manual.

In contrast, what the ReaderT pattern is advertising, is extensional
classes, indicating what an individual function _is allowed_ to do
(hence the name "capability"), regardless of how the monad is
implemented.


## The problem

Irrespective of whether you are willing to twist the arm of the Mtl
with `{-# OVERLAPPING #-}` instances, when programming with
capabilities you will run into two kind of issues. The first is
probably the lesser of the two, but has been a known pain point with
the Mtl for a while, is that the Mtl uses the type of (say) state to
discriminate layers. In other words: you can't have two states of the
same type in your capabilities.

This, of course, is not a problem if you are writing your own type
classes: you can simply use a different class for each state. This
leads us to the second, more serious issue: lack of inference. With
all its faults, the Mtl gives a very comfortable environment: it
defines plenty of generic instances, so that when we give a concrete
monad stack, then all the instances are automatically computed for
us. In contrast, with capabilities and the ReaderT pattern, we collect
all the capabilities, and assemble a bespoke type to handle all of
these instances. Haskell's instance resolution is simply not equipped
for this.

The bottom line is: an insane amount of boilerplate. Custom type class
definitions. A slew of instances at each main entry point (where a
concrete type is defined for the monad).

## DerivingVia

What we would really like is a way to use type-class instances the
other way around, compared to instance resolution. Instead of reading

```haskell
instance MonadState s m => MonadState s (ReaderT r m)
```

as saying that `MonadState`, on `ReaderT` means that `Monad s m`, and
fixing the implementation, we would like to read it as: if I have an
implementation of `Monad s m`, then this is a _possible_
implementation of `MonadState` on a `ReaderT`.

This is made possible by a new language extension available in the
freshly released GHC 8.6: `DerivingVia`.

In short, `DerivingVia` is a generalisation of
`GeneralizedNewtypeDeriving` which allows you, not only to derive an
instance _for_ a newtype, but also _from_ a newtype, or, and this is
most relevant for us, from a combination of newtypes. For example:

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

You will find a more complete introduction in [Baldur Blondal's
talk][stolen-instances] on the subject. If you want all the details,
head to the [proposal][proposal] or the [paper][paper].

[stolen-instances]: https://skillsmatter.com/skillscasts/10934-lightning-talk-stolen-instances-taste-just-fine
[proposal]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-deriving-via.rst
[paper]: https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf

## Enter capabilities-via

<!-- TODO: rework the transition -->

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

## A word on free monads


<!--  LocalWords:  intensional
 -->
