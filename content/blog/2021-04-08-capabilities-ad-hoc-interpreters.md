---
title: "Ad-hoc interpreters with capability"
description: "Define effect interpreters dynamically using reflection"
author: Gaël Deest, Andreas Herrmann
tags: [haskell, capability]
---

The [capability][capability] library is an alternative to the venerable `mtl`
(see our earlier [blog posts][capability-tag] on the subject).
It features a set of
“`mtl`-style” type classes, representing effects, along with deriving combinators
to define interpreters as type class instances. It relies on the `-XDerivingVia`
extension to discharge effects declaratively, close to the definition of the
application's monad. Business logic can be written in a familiar, idiomatic way.

As an example, consider the following computation:

```haskell
testParity :: (HasReader "foo" Int m, HasState "bar" Bool m) => m ()
testParity = do
  num <- ask @"foo"
  put @"bar" (even num)
```

This function assumes a Reader effect `"foo"` of type `Int`, and a State effect
`"bar"` of type `Bool`. It computes whether or not `"foo"` is an even number and
stores the result in `"bar"`.

Save for the tags `"foo"` and `"bar"`, used to enable multiple Reader or State
effects within the same monad (an impossible thing with `mtl`), this is fairly
standard Haskell: Type classes are used to constrain what kind of effects the
function can perform, while decoupling the computation from any concrete
implementation. At use-site, it relies on GHC's built-in resolution mechanism to
“inject” required dependencies. Any seasoned Haskeller should feel right at home
!

## Providing instances

To actually call this admittedly silly function, we need to provide interpreters
for the `"foo"` and `"bar"` effects. Following the [`ReaderT`][readert-io]
design pattern, we'll pack everything we need into a single context record, then
interpret our effects over this context in the `IO` monad, using the deriving
combinators provided by the library:

```haskell
data Ctx = Ctx { foo :: Int, bar :: IORef Bool }
  deriving Generic

newtype M a = M { runM :: Ctx -> IO a }
  deriving (Functor, Applicative, Monad) via ReaderT Ctx IO
  -- Use DerivingVia to derive a HasReader instance.
  deriving (HasReader "foo" Int, HasSource "foo" Int) via
    -- Pick the field foo from the Ctx record in the ReaderT environment.
    Field "foo" "ctx" (MonadReader (ReaderT Ctx IO))
  -- Use DerivingVia to derive a HasState instance.
  deriving (HasState "bar" Bool, HasSource "bar" Bool, HasSink "bar" Bool) via
    -- Convert a reader of IORef to a state capability.
    ReaderIORef (Field "bar" "ctx" (MonadReader (ReaderT Ctx IO)))
```

Thus equipped, we can now make use of our `testParity` function in an actual
program:

```haskell
example :: IO ()
example = do
    rEven <- newIORef False
    runM testParity (Ctx 2 rEven)
    readIORef rEven >>= print
```

How do we test a function such as `testParity` in isolation? In our contrived
example, this is quite easy: the `example` function could be easily converted
into a test-case. In the Real World™, though, our context `Ctx` could be much
bigger, providing a pool of database connections, logging handles, etc. Surely,
we don't want to spawn a database instance to test such a simple
function!

## _Ad-hoc_ interpreters

In previous iterations of `capability`, the solution to this problem would have
been to create a _new_ monad for testing purposes, leaving out the capabilities
we don't want. While it works, it is not always the best tool for the job:

- You need to define a new monad for each combination of effects you want to
  test.
- Test cases are no longer self-contained; their implementation is spread
  across multiple places. It makes things less readable and harder to maintain.

A solution, supported by fancier effect system libraries such as
[polysemy][polysemy] or [fused-effects][fused-effects], is to define _ad-hoc_
interpreters in the executable code itself. At first glance, it might seem like
this is not possible in `capability`. Indeed, since interpreters are provided as
type class instances, and type classes are an inherently static mechanism, surely
there is no way of specifying those dynamically. Or is there?

As of version `0.4.0.0`, the `capability` library features an experimental
[Capability.Reflection][capability-reflection] module, addressing this very
limitation. It is inspired by, and uses, Edward Kmett's [reflection][reflection-hackage]
library, and uses similar type class ~~wrangling~~ magic to let you define
interpreters as explicit dictionaries.

### Interpreters as reified dictionaries

Making use of those new features, the `example` function can be rewritten as:

```haskell
import qualified Control.Monad.Reader as MTLReader

example :: IO ()
example = do
    let
      runTestParity :: (Int, IORef Bool) -> IO ()
      runTestParity (foo, bar) =
        flip MTLReader.runReaderT foo $
        -- Interpret the effects into 'ReaderT Int IO'.
        --
        -- Write the 'HasReader "foo" Int' dictionary
        -- in terms of mtl functions.
        --
        -- Forward the 'MonadIO' capability.
        interpret @"foo" @'[MonadIO] ReifiedReader
          { _reader = MTLReader.reader
          , _local = MTLReader.local
          , _readerSource = ReifiedSource
              { _await = MTLReader.ask }
          } $
        -- Use 'MonadIO' to write the 'HasState "bar" Bool' dictionary.
        -- Forward the 'HasReader "foo" Int' capability.
        --
        -- The 'MonadIO' capability is not forwarded, and hence forgotten.
        interpret @"bar" @'[HasReader "foo" Int] ReifiedState
          { _state = \f -> do
              b <- liftIO $ readIORef bar
              let (a, b') = f b
              liftIO $ writeIORef bar b'
              pure a
          , _stateSource = ReifiedSource
              { _await = liftIO $ readIORef bar }
          , _stateSink = ReifiedSink
              { _yield = liftIO . writeIORef bar }
          }
        testParity

    rEven <- newIORef False
    runTestParity (2, rEven)
    readIORef rEven >>= print
```

Defining a test monad is no longer required: the effects are interpreted
directly in terms of the underlying `ReaderT Int IO` monad. Type-class
dictionaries are passed to the `interpret` function as mere records of functions
and superclass dictionaries — just like GHC does under the hood as hidden
parameters when we use statically defined instances.

Let's dissect the `ReifiedReader` dictionary:

```haskell
ReifiedReader
  { _reader = MTLReader.reader
  , _local = MTLReader.local
  , _readerSource = ReifiedSource
        { _await = MTLReader.ask }
  }
```

Omitting the extra `Proxy#` arguments, which are here for technical reasons, the
first two attributes, `_reader` and `_local`, correspond directly to the methods
of the `HasReader t` type class:

```haskell
class (Monad m, HasSource tag r m) => HasReader (tag :: k) (r :: *) (m :: * -> *) | tag m -> r where
  local_ :: Proxy# tag -> (r -> r) -> m a -> m a
  reader_ :: Proxy# tag -> (r -> a) -> m a
```

The `_readerSource` argument, on the other hand, represents the dictionary of
the `HasSource` superclass:

```haskell
class Monad m => HasSource (tag :: k) (a :: *) (m :: * -> *) | tag m -> a where
  await_ :: Proxy# tag -> m a
```

### Abstracting interpreters

This is quite boilerplatey, though. If we're writing a lot of test cases, we are
bound to redefine those interpreters several times. This is tedious,
error-prone, and clutters our beautiful test logic. Maybe this is could all be
factored out? Sure thing!

```haskell
interpretFoo
  :: forall cs m a. (MTLReader.MonadReader Int m, All cs m)
  => (forall m'. All (HasReader "foo" Int : cs) m' => m' a)
  -> m a
interpretFoo =
  interpret @"foo" @cs ReifiedReader
    { _reader = MTLReader.reader
    , _local = MTLReader.local
    , _readerSource = ReifiedSource
        { _await = MTLReader.ask }
    }

interpretBar
  :: forall cs m a. (MonadIO m, All cs m)
  => IORef Bool
  -> (forall m'. All (HasState "bar" Bool : cs) m' => m' a)
  -> m a
interpretBar bar =
  interpret @"bar" @cs ReifiedState
    { _state = \f -> do
        b <- liftIO $ readIORef bar
        let (a, b') = f b
        liftIO $ writeIORef bar b'
        pure a
    , _stateSource = ReifiedSource
        { _await = liftIO $ readIORef bar }
    , _stateSink = ReifiedSink
        { _yield = liftIO . writeIORef bar }
     }
```

These two functions follow a similar pattern. Let's have a closer look at the
type of `interpretBar` to understand what is going on:

```haskell
interpretBar
  :: forall cs m a. (MonadIO m, All cs m)
  => IORef Bool
  -> (forall m'. All (HasState "bar" Bool : cs) m' => m' a)
  -> m a
```

- The (typelevel) `cs :: [(* -> *) -> Constraint]` argument is a list of
  _capabilities_ that we wish to retain in the underlying action.
- Since we interpret the State effect with a mutable `IORef` reference, we
  require that the underlying monad be an instance of `MonadIO`. Moreover, we
  ask that our target monad also implement all the required capabilities by
  adding the `All cs m` constraint to the context (`All` is a type family that
  applies a list of capabilities to a monad to generate a single constraint;
  for example, `All '[MonadIO, HasSource "baz" Baz] m` is equivalent to
  `(MonadIO m, HasSource "baz" Baz m)`).
- The `IORef` used to store our state is passed as a standard function
  argument. This was not possible without _ad-hoc_ interpreters: we
  needed to add the `IORef` to the `Ctx` type. With _ad-hoc_
  interpreters, on the other hand, we can write instances which
  capture references in their closures.
- The last argument is a monadic action that makes use of `HasState "bar" Bool`
  along with the forwarded `cs` capabilities. It is required to be _polymorphic_
  in the monad type, which guarantees that the action cannot use other effects.

Now that we have factored out the interpretation of the `"foo"` and `"bar"`
effects into dedicated functions, they can be neatly composed to provide just
the effects we need to run `testParity`:

```haskell
example :: IO ()
example = do
    let
      runTestParity :: (Int, IORef Bool) -> IO ()
      runTestParity (foo, bar) = flip MTLReader.runReaderT foo $
        interpretFoo @'[MonadIO] $
        interpretBar @'[HasReader "foo" Int] bar $
        testParity

    rEven <- newIORef False
    runTestParity (2, rEven)
    readIORef rEven >>= print
```

## Deriving capabilities

Truth be told, in this example, the dictionaries we've been writing aren't so different
from a custom type class with capabilities provided by deriving-via. While the extra power that comes with
dynamic dictionaries can be very useful, it isn't always warranted.

There is a middle ground, however: we can provide capabilities
locally, but with deriving-via combinators using a function that we call
[`derive`][capability-derive]. You would typically use `derive` to derive high-level
capabilities from lower-level capabilities. In our case, we can replace:

```haskell
runTestParity :: (Int, IORef Bool) -> IO ()
runTestParity (foo, bar) = flip MTLReader.runReaderT foo $
  interpretFoo @'[MonadIO] $
  interpretBar @'[HasReader "foo" Int] bar $
  testParity
```

with:

```haskell
runTestParity :: (Int, IORef Bool) -> IO ()
runTestParity ctx = flip MTLReader.runReaderT ctx $
  derive
     -- Strategy
     @(ReaderIORef :.: Rename 2 :.: Pos 2 _ :.: MonadReader)
     -- New capability
     @'[HasState "bar" Bool]
     -- Forwarded capability
     @'[MTLReader.MonadReader (Int, IORef Bool)] $

  derive
     @(Rename 1 :.: Pos 1 _ :.: MonadReader)
     @'[HasReader "foo" Int]
     @'[HasState "bar" Bool]

  testParity
```

thus getting rid of the `interpret{Foo,Bar}` helpers entirely. For instance, the
`HasState "bar" Bool` capability is derived from the `IORef Bool` in the second
position of the tuple provided by the ambient `MonadReader (Int, IORef Bool)`
instance. Think `DerivingVia`, but dynamically!

## Conclusion

Wrapping things up:

- At its core, the `capability` library is just `mtl` on steroids, modeling
  effects with type classes.
- The standard way of using `capability` is to define interpreters
  declaratively, using the provided combinators; this programming-style does
  not allow defining _ad-hoc_ interpreters, at runtime.
- The new version of `capability` provides a way of overcoming this limitation
  with reified dictionaries.
- Standard deriving strategies can be used to provide dynamic instances with
  less boilerplate, using the underlying deriving mechanism.

Writing tests is just one example. Another application might be to dynamically
select the interpretation of an effect based on a configuration parameter. All
this is still experimental: the API and ergonomics are likely to change a bit
over the next few releases, but we hope this post motivates you to give it a
try.

[readert-io]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/
[capability]: https://github.com/tweag/capability
[capability-reflection]: https://hackage.haskell.org/package/capability-0.4.0.0/docs/Capability-Reflection.html
[capability-derive]: https://hackage.haskell.org/package/capability-0.4.0.0/docs/Capability-Derive.html
[capability-tag]: https://www.tweag.io/blog/tags/capability
[polysemy]: https://github.com/polysemy-research/polysemy
[fused-effects]: https://github.com/fused-effects/fused-effects
[reflection-hackage]: https://hackage.haskell.org/package/reflection-2.1.6/docs/Data-Reflection.html
