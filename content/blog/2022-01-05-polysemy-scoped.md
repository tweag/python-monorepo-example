---
title: "Scoped Effect Resources for Polysemy"
author: Torsten Schmits
tags: [haskell]
description: "How to transparently separate resource allocation from business logic for scoped effects in Polysemy"
---

Effect systems like [Polysemy] provide the programmer with a flexible way to keep the business logic of a program as
flexible as possible by separating the _definition_ of effects and their _interpretation_.
This is useful for many reasons, but especially for testing and mocking.
For example, instead of using an interpreter that runs a task over the network, one can swap it for an in-memory
implementation when running tests. This allows to test features in isolation.

To achieve this goal, effects should clearly convey functionality without exposing their implementation.
In many cases, this means exposing interpreters involving low-level constructs, such as `IO`, `StateT` or exceptions, only at the
outermost levels of the application.

However, for some kinds of effects, it can be hard to design an expressive interface due to the semantics of their
primitive resources.
One instance of those are resources whose lifetime is scoped to a small part of a program (called a _region_ in this
post), like a database connection.

In this post I will show why these effects are tricky, and outline the thought process that led to a solution that
allows for transparent locally scoped resources.

## The Use Case: A Synchronization Effect

Take for example an abstraction of an `MVar`, named `Sync`, used to signal a synchronization point between two threads
in this program:

```haskell
import Polysemy
import Polysemy.Async

program :: Sem [Sync, Async, Output Text] ()
program = do
  async do -- uses Async
    output "background thread" -- uses Output Text
    signal -- uses Sync
  wait -- uses Sync
  output "main thread" -- uses Output Text

main :: IO ()
main = do
  log <- (runFinal . asyncToIOFinal . runOutputList . interpretSync) do
    program
    program
  traverse_ putStrLn log
```

The semantics of `Sync` are that `wait` should block until `signal` gets executed; and when running `program` twice in a
row, the semantics shouldn't change.

A simple implementation might look like the following:

```haskell
data Sync :: Effect where
  Wait :: Sync m ()
  Signal :: Sync m ()

interpretSync ::
  Member (Embed IO) r =>
  InterpreterFor Sync r
interpretSync sem =
  mv <- embed newEmptyMVar
  run mv sem
  where
    run mv =
      interpret \case
        Wait -> embed (takeMVar mv)
        Signal -> embed (putMVar mv)
```

This interpreter chooses a concrete implementation with the primitives `MVar` and `IO`, which embody the "low-level
constructs" that, as mentioned in the introduction, should be run as far removed from the logic as possible.

Despite the `MVar` being shared among the two executions of `program`, this construct works as intended, since the calls
to `wait` are sequential.

However, the problems of this naive implementation start to show when running two instances of `program`
concurrently, causing a race condition – the second call to `wait` might take the `MVar` while the first call to
`signal` is executed. In other words, the interpreter cannot distinguish between the consumers of the effect:

```haskell
main :: IO ()
main = do
  (runFinal . asyncToIOFinal . interpretSync) do
    async program
    program
```

## Leaky Abstraction: Using Interpreters in Business Logic

A straightforward solution for the race condition above would be to run `interpretSync` directly at the call site.

```haskell
program :: Sem [Async, Output, Embed IO] ()
program = do
  interpretSync do -- Transforms the `Sync` requirement to `Embed IO`
    async do
      output "background thread"
      signal
    wait
    output "main thread"
```

This solution is nice because it restricts the use of the corresponding `MVar` to the region in which it is
used.
A restriction of a resource to a region, or _scoping_ of a resource, is commonly performed using the `bracket`
combinator; the resource in question for this example is the `MVar`.

Unfortunately, like `bracket`, the interpreter acquires a _concrete_ resource in the supposedly abstract business logic
that propagates its constraints to any program that calls this function, as is evident from the `Embed IO` member
constraint.

This issue might be more clearly undesirable for effects that do actual I/O work, like database transactions:

```haskell
data Database :: Effect where
  Query :: AbstractQuery a -> Database query m a
  Transact :: m a -> Database query m a

interpretDatabasePostgres ::
  Member PostgresConnection r =>
  InterpreterFor (Database PostgresQuery) r
interpretDatabasePostgres =
  undefined

postgresProgram ::
  Member PostgresConnection r =>
  Sem r ()
postgresProgram =
  interpretDatabasePostgres do
    transact do
      query (AbstractQuery.fetchById 1)
```

This effect's implementation (only sketched here) is more complex than `Sync`'s, but it
illustrates how committing to a concrete resource (here, a database connection) can
ruin the flexibility that effect systems provide -- using `interpretDatabasePostgres` in `postgresProgram`
causes the implementation to be fixed to PostgreSQL, prohibiting the testing of
`postgresProgram` with an in-memory version of `Database`.

## The Old Interpreter Switcheroo: Hiding the Implementation with Higher-Order Effects

In order to fix that implementation leak, the _scoping_ part of `interpretSync`/`interpretDatabasePostgres` has to be
separated from the rest of the interpretation, so that the interpreter for `Wait` and `Signal` is provided with a
dynamically allocated resource.

`Transact`'s signature hints at a feature that can be exploited to achieve this: _Higher-order effects_.
This term denotes an effect constructor that uses the monad `m` in its parameters, allowing it to store an entire region
for evaluation in an interpreter.

A higher-order `Sync.use :: Member Sync r => Sem r a -> Sem r a` should have the following semantics, using `program`
from before:

```haskell
main :: IO ()
main = do
  (runFinal . asyncToIOFinal . interpretSync) do
    async (Sync.use program) -- both calls to `use` should have their own `MVar`
    Sync.use program
```

This snippet introduces a new effect constructor, `Sync.use`, which stores one instance of `program`.
Higher-order regions are notoriously difficult to deal with in interpreters, so the following sketches a simplified
version:

```haskell
data Sync :: Effect where
  Wait :: Sync m ()
  Signal :: Sync m ()
  Use :: m a -> Sync m a

interpretSyncWithMVar ::
  Members [Error Text, Embed IO] r =>
  MVar () ->
  InterpreterFor Sync r
interpretSyncWithMVar mv =
  interpretH \case
    Wait -> embed (takeMVar mv)
    Signal -> embed (putMVar mv)
    Use region -> do
      mv <- embed newEmptyMVar
      interpretSyncWithMVar mv =<< runT region

interpretSync ::
  Members [Error Text, Embed IO] r =>
  InterpreterFor Sync r
interpretSync sem =
  interpretH \case
    Wait -> throw "Called Wait without Use"
    Signal -> throw "Called Signal without Use"
    Use region -> do
      mv <- embed newEmptyMVar
      interpretSyncWithMVar mv =<< runT region
```

These two interpreters split the work – `interpretSync` is allocating the `MVar` resource, while `interpretSyncWithMVar`
implements the effect logic, the former refusing to handle any action it's not equipped to deal with by throwing
runtime errors.

Our interpreter, `interpretSync`, makes use of one of Polysemy's
features for higher-order interpretation: the function `runT` does not
interpret the `Sync` effect in `region`. This lets us _switch_ from
`interpretSync` to `interpretSyncWithMVar` when interpreting `Use`.

The caveat of this solution is that runtime errors don't prevent incorrect programs from being compiled; in other words,
the interpreter is unsound.
In the following example, it allows an accidental call to `wait` outside of the `use` region:

```haskell
prog1 :: Members [Sync, Async] r => Sem r ()
prog1 = do
  use do
    async do
      doStuff
      signal
      doOtherStuff
  wait
```

In the rest of this article, however, I will build upon this idea, and
describe the general scoped-resource abstraction that was built for
Polysemy, where only sound programs can be written.

## Generalizing the Problem

Let's forget the specifics of `Sync` and focus on the subject matter: the allocation of resources scoped to a region of
the program.
The interpreter for a resource scoping effect, aptly named `Scoped`, should:

- Allocate a resource (the `MVar`) whose lifetime is restricted to the region in which the effect is used
- Allow multiple resource allocations within one interpreter
- Hide as much of the implementation from the use site as possible
- Be sound, i.e. not require exceptions for incorrect use
- Allow the business logic to explicitly specify where the resource is used, without knowledge of its implementation
  details

In the previous section the job of the outer interpreter, `interpretSync`, was precisely to acquire the resource and
pass it to the inner interpreter, `interpretSyncWithMVar`, which executes the effect-specific logic.
Consequently, the generalized version of it takes a resource acquisition action and a parameterized interpreter:

```haskell
interpretScoped ::
  Sem r resource ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
interpretScoped acquireResource scopedInterpreter = ...
```

This already looks better – the second parameter's type has the exact shape of
`interpretSyncWithMVar`.
The implementation now has to acquire the resource with the first argument and use the second one to interpret the
higher-order region.

Note that `interpretScoped` is an interpreter for `Scoped resource effect`. You should understand a program with effect `Scoped resource effect` as a program which can use `effect` _under the condition_ that
it has acquired a `resource`. The inner region (stored in a `Use` in
our example), on the other hand, does actually use `effect`
directly. So the inner interpreter is an interpreter for `effect` itself.

We will also need something to play the role of `Use`: a function
to allocate a resource for a region. The region uses `effect`, but it
is used in a program that uses `Scoped resource effect`. In Polysemy, a
function that changes the effects available to a region is written as
an interpreter, which we will be calling `scoped`.

```haskell
scoped ::
  Member (Scoped resource effect) r =>
  InterpreterFor effect r
```

In our concrete example, the `effect` parameter is `Sync`,
but the `resource` parameter must stay polymorphic, because the concrete implementation should remain hidden from the
business logic.
The hard part, however, is figuring out the implementation of `scoped`, and this requires some knowledge about
Polysemy's internals.

## Here Be Dragons: The Full Implementation

The `Scoped resource effect` effect must perform two tasks:

- Store the region in which the scope should be active
- Interpret effects of type `effect` in a scope where a `resource` exists

This suggests these two constructors for `Scoped`:

```haskell
data Scoped (resource :: Type) (effect :: Effect) :: Effect where
  InScope :: (resource -> m a) -> Scoped resource effect m a
  Run :: resource -> effect m a -> Scoped resource effect m a

scoped ::
  Member (Scoped resource effect) r =>
  InterpreterFor effect r
scoped region =
  send $ InScope \resource -> transform (Run resource) region
```

We can store a region with `InScope`. Regions are stored as functions
`resource -> m a` so that the interpreter will be able to create and
inject a scoped resource. We then store the resource in the `Run`
constructor, which simply pairs up an `effect` with the scoped
resource. The implementation of `scoped` uses the `transform` combinator
from Polysemy, which converts an effect type into another.

The implementation of the interpreter, `interpretScoped`, follows:

```haskell
interpretScoped ::
  Sem r resource ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped resource effect) r
interpretScoped acquireResource scopedInterpreter =
  interpretH \case
    Run resource action ->
      scopedInterpreter resource (send action)
    InScope region -> do
      resource <- acquireResource
      interpretScoped (region resource)
```

Now `Sync` can be interpreted in terms of `Scoped` with all its benefits:

```haskell
data Sync :: Effect where
  Wait :: Sync m ()
  Signal :: Sync m ()

interpretSync ::
  Member (Embed IO) r =>
  MVar () ->
  InterpreterFor Sync r
interpretSync mv =
  interpret \case
    Wait -> embed (takeMVar mv)
    Signal -> embed (putMVar mv)

program :: Sem [Scoped resource Sync, Async, Output Text] ()
program = do
  scoped do
    async do
      output "background thread"
      signal
    wait
    output "main thread"

main :: IO ()
main = do
  log <- (runFinal . asyncToIOFinal . runOutputList . interpretScoped (embed newEmptyMVar) interpretSync) do
    async program
    program
  traverse_ putStrLn log
```

The `resource` parameter stays polymorphic in `program`, to be instantiated as `MVar` only when `interpretSync` is
called in `main`, thereby hiding the implementation from the logic, while providing the mechanism by which GHC connects
the resource to the use site.

## Wrapping Up

I've worked with Polysemy quite intensely, but when I started using this pattern I was surprised at the ergonomics it
displays in practice.
I already built several useful effects with it, like a publish/subscribe mechanism built on `unagi` channels that
duplicates the channel for each subscriber:

```haskell
program = do
  async do
    Events.subscribe do
      assertEqual 1 =<< Events.consume
  async do
    Events.subscribe do
      assertEqual 1 =<< Events.consume
  Events.publish 1
```

Finally, I'd like to acknowledge the brilliant people who made this possible:
Love Waern, whose genius manifested the magic of the implementation, Georgi Lyubenov, who stated the problem that
motivated it, and Sandy Maguire, the creator of the amazing Polysemy.

[polysemy]: https://hackage.haskell.org/package/polysemy
[polysemy-conc]: https://github.com/tek/polysemy-conc/blob/master/packages/conc/lib/Polysemy/Conc/Interpreter/Scoped.hs
