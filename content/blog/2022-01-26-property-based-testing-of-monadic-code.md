---
title: "A Case Study on Correctness and Safety Testing of Stateful Systems"
author: Victor Miraldo
tags: [haskell, formal-methods, blockchain]
description: "How to automatically inject attacks and faults in safety tests using random traces."
---

Writing a test suite is a crucial step when developing high-assurance software.
Yet, identifying meaningful properties of the system and giving it suitable
inputs may be hard. This is especially challenging for code involving _state_ --
besides properties and inputs, one must also take into account the state of the
system at all times.

It is inconvenient to construct states suitable for testing by hand. An
alternative is to build sequences of inputs that can be iterated over the
initial state, placing the system in a state we want to test. These sequences
of inputs are called _traces_.

For these traces to be useful for testing, it must be possible to:

1. report traces to the user in an informative way when some post-condition fails
1. modify traces by changing some or all inputs sent in them

While (1) is fairly simple to accomplish, (2) requires significantly more
advanced techniques.

In this post I will present a technique to do testing using traces which can
can be used with arbitrary stateful systems. They were applied in practice in
[cooked-validators][cooked-validators], which was developed as a tool for
writing [Plutus][plutus] validator scripts.

## Context: auditing smart contracts

During this post I will use smart contracts as a guiding example, but no
specific knowledge on them is required. The unfamiliar reader can think of
smart contracts as arbitrary code whose state is stored in the blockchain, and which
is triggered by transactions.

While auditing [Plutus][plutus] smart contracts, we want to write tests
using a function `validateTx` that pretends to run a transaction in a blockchain.
Its type is:

```haskell
validateTx :: (Monad m) => Tx -> StateT BlockChainSt (ExceptT BlockChainErr m) TxId
```

Smart contracts will live somewhere in the state, of type `BlockChainSt`, and their
implementation gets to decide whether some transaction is valid or not.
If a contract rejects a transaction, `validateTx` will throw an error.
If all contracts involved in a transaction agree, the overall blockchain
state changes and we return the transaction id of that particular transaction.

In this context, a typical test consists of generating and executing a sequence
of transactions then checking some predicate on the resulting
state. For example, say that we are simulating a crowdfunding-like
contract. One property we expect to hold is that the owner can
withdraw the funds _if and only if_ the threshold has been reached. To check
that, we can write a number of traces that reach the threshold,
execute them and ensure the owner can withdraw the funds.
We might also want to transform those traces that succeed with certain
attacks or faults and ensure they fail.

## An Appetizer

Lets start by defining a convenient type synonym which represents computations
that manipulate the blockchain:

```haskell
type DirectT m = StateT BlockChainSt (ExceptT BlockChainErr m)
```

Coming back to the crowdfunding example, we might want to express a trace
that opens a crowdfunding vault with a threshold of 10000 units, then deposits 8000 units,
followed by depositing an additional 5000 units, and finally attempts withdraw, which should succeed.
Using monadic syntax, this could be written as `example` below:

```haskell
example :: (Monad m) => DirectT m ()
example = void $ do
  vaultId <- validateTx (openCrowdFundingWithLimit 10000)
  validateTx (fundVault vaultId 8000)
  validateTx (fundVault vaultId 5000)
  validateTx (withdrawVault vaultId)
```

Here, our example uses the transaction id from the first transaction to identify a particular vault
but ignores the other `TxId`s. To witness that `example` succeeds, we can run it from the initial state and
witness that it does not return an error. What if it doesn't succeed, though?
As a user, we would like to see a description of all the transactions that led to
the failure, so we can diagnose any potential issue.

Even better, say we want to express that if either funding transaction above happened to
fund 3001 units less, the trace should fail: the transactions deposit a total of 13000 units
into the vault, if we take more than 3000 units from anywhere then the threshold won't be met
and the trace should fail. In fact, we want to be able to write something like:

```haskell
example' = somewhere (subtractValFromFundVault 3001) example
```

The `somewhere` modality affects specific transactions over
a trace, yielding a set of traces and enabling us to
inject faults and attacks. In `example'` above, `subtractValFromFundVault`
has type `Tx -> Maybe Tx`. Say we defined it to return `Just` if and only if it is applied to
a `fundVault` transaction. In that case, `example'` represents the
following two traces:

- First trace
  - `openCrowdFunding`
  - `fundVault vault 4999`
  - `fundVault 5000`
  - `withdraw`
- Second trace
  - `openCrowdFunding`
  - `fundVault vault 8000`
  - `fundVault 1999`
  - `withdraw`

The original `example` trace is not present because `somewhere`
applies the modifier to exactly one transaction in the trace, and the
original transaction has the modifier applied _nowhere_.

Finally, note how the monadic structure ensures
that we can write dynamic traces: we can use a generated `vaultId`
to validate subsequent transactions. More generally, traces often
depend on values that are returned from previous transactions.
One such case that shows up in practice all the time is the
[creation of NFTs][nft], which often requires some hash of some previously submitted
transaction id.

We have just covered the essentials of `cooked-validators`.
If you are only interested in using our library, you can continue
with the tutorials and examples in the [repository][cooked-validators].
In the rest of the post I will go over some of the more technical internal details.

## Free Monads and Staging

So how does this work? The core idea is to have two monads:

- `DirectT`, that executes operations directly but doesn't support `somewhere`
- `Staged`, that is an AST which can be modified, and interpreted into `DirectT`

In order to be able to choose which monad we want, we abstract the
core functionality we need in a type class called `MonadBlockChain`
and write all of our functions with type: `(MonadBlockChain m) => ... -> m res`.
For simplicity, let's assume there are two base
functions needed: validating a transaction and returning some id if
validation succeeds, and a function to lookup the resources that
belong to a certain address. We would then define `MonadBlockChain` as
follows:

```haskell
class (Monad m) => MonadBlockChain m where
  validateTx :: Tx -> m TxId
  lkupResources :: Address -> m [Resource]

instance (Monad m) => MonadBlockChain (DirectT m) where
  ...

instance MonadBlockChain Staged where
  ...
```

The idea behind `Staged` is to reify the base operations, declared in `MonadBlockChain`, into a GADT:

```haskell
data Op :: * -> * where
  ValidateTx :: Tx -> Op TxId
  LkupResources :: Address -> Op [Resources]
```

Then we want an interpretation of `Op` in terms of the `DirectT` monad, called `interpretOp`:

```haskell
interpretOp :: (Monad m) => Op a -> DirectT m a
interpretOp (ValidateTx tx) = validateTx tx
interpretOp (LkupResources addr) = lkpResources addr
```

With that at hand, we need to be able to construct values of type `Op` and chain
them together. For this, we can use something like the [Free Monad][free-monads]
to build up an AST using operations in
`Op`. Unfortunately, `Op` above is not a functor, hence, we _cannot_
just take the free monad of `Op`. We must construct the
operational monad of `Op`, which is also called the [_Freer_ monad][freer-monad]:

```haskell
data Freer (op :: Type -> Type) :: Type -> Type where
  Return :: a -> Freer a
  Bind   :: op a -> (a -> Freer b) -> Freer b
```

Our `Staged` monad then is just a particular instantiation of `Freer`:

```haskell
type Staged = Freer Op
```

With that setup, we can now write a first interpreter for `Staged`. Let's warm
up with a trivial interpreter which only interprets returns and binds in the
target monad:

```haskell
interpret :: (Monad m) => Staged a -> DirectT m a
interpret (Return a) = return a
interpret (Bind op cont) = interpretOp op >>= interpret . cont
```

### Constructing the AST

All that is left for us in order to use `DirectT` and `Staged` interchangeably is
the instance for `MonadBlockChain Staged`, which is easy to write:

```haskell
instance MonadBlockChain Staged where
  validateTx tx = Bind (ValidateTx tx) Return
  lkupResource addr = Bind (LkupResource addr) Return
```

### Probabilistic traces for property-based testing

Up until now we can build ASTs with `Staged` which represent traces
and can be interpreted with `DirectT` to witness their success or failure.
That is good enough for unit testing, but we want to have property-based tests.
The [`GenT`][gent] transformer, is the key to achieve this, enabling us to
write probabilistic traces:

```haskell
instance (MonadBlockChain m) => MonadBlockChain (GenT m) where
  validateTx = lift . validateTx
  lkupResource = lift . lkupResource
```

We can rewrite our original example as a polymorphic, property-based
test. The test will exercise the property that for all `threshold` and
`x < threshold`, if we open a vault for `threshold` units then issue
two transactions depositing and arbitrary `x` and `threshold - x`
units, we must be able to withdraw the funds.

```haskell
example :: (MonadBlockChain m) => GenT m ()
example = void $ do
  threshold <- choose (5000, 15000)
  vaultId <- validateTx (openCrowdFundingWithLimit threshold)
  x <- choose (1, threshold - 1)
  validateTx (fundVault vaultId x)
  validateTx (fundVault vaultId (threshold - x))
  validateTx (withdrawVault vaultId)
```

Here, `example` can be thought of as a probability distribution of traces.
Calling `runGenT example :: Staged ()` samples that probability distribution, yielding
one trace at random that picks a particular instantiation of `threshold` and `x`.

We tie everything together with a simple `forAllTr` combinator which
samples a distribution of traces, then interprets the random trace
and applies a predicate to its result:

```haskell
forAllTr :: GenT Staged a -> (Either BlockChainErr (a, BlockChainSt) -> Property) -> Property
forAllTr pTr prop = forAllBlind (runGenT pTr) $ \tr -> prop (runDirect (interpret tr) st0)
```

A test that checks that all traces in the distribution `example` above succeed can
then be written as:

```haskell
exampleOk :: Property
exampleOk = forAllTr example (property . isRight)
```

Up to this point we have addressed our first requirement only:
express _traces_ for property-based testing. However, we did it in a way that
will enable us to address the second and third requirements: inform the
user if something goes wrong and modify traces with combinators like `somewhere`.

### What went wrong? Returning traces

If the `exampleOk` test fails at any point, the programmer has no idea of what went wrong.
To improve this, we must change our `interpret` function to inspect and record the transactions
being validated. This can be done with a modified version of `forAllTr`:

```haskell
interpret :: (Monad m) => Staged a -> DirectT (WriterT [Tx] m) a
interpret (Return a) = return a
interpret (Bind op cont) = tellIfTx op >> interpretOp op >>= interpret . cont
  where tellIfTx (ValidateTx tx) = tell [tx]
        tellIfTx _ = return ()

forAllTr :: GenT Staged a -> (Either BlockChainErr (a, BlockChainSt) -> Property) -> Property
forAllTr pTr prop = forAllBlind (runGenT pTr) $ \tr
  -> let (res, descr) = runDirect (interpret tr) st0
      in counterexample (show descr) $ prop res
```

The type `DirectT (WriterT [Tx] m) a` is isomorphic to `BlockChainSt -> (Either BlockChainErr (a, BlockChainSt), [Tx])`,
which gives us the sequence of transactions that yielded the given result, even if the result is
actually an error, which is important.

Note that we could not just write a function `observe :: Staged a -> [Tx]`,
which extracts all the transactions used in a `Staged` trace. This would be
possible if we relied on `Applicative` instead of `Monad`. Yet, in our case `Monad` is necessary because
we may need the result of previous computations in order to construct
the next transaction (think of hashes or signatures, for instance). This happens constantly in practice.

### Modifying traces: adding modalities

At this point, we can write probabilistic traces and have a description of
what happened if something goes wrong, but we still cannot modify transactions
within traces. To accomplish that we will take inspiration in modal logic
and define two dual functions:

```haskell
somewhere  :: (Tx -> Maybe Tx) -> Staged a -> Staged a
everywhere :: (Tx -> Maybe Tx) -> Staged a -> Staged a
```

These combinators take a partial function: this way they are able not to affect certain
transactions. Some transformations might only make sense for some transactions.
Additionally, the semantics of `Staged` will be enriched. We now need:

```haskell
interpret :: Staged a -> DirectT (WriterT TraceDescr []) a
```

which is equivalent to the type `Staged a -> BlockChainSt -> [(Either BlockChainErr (a, BlockChainSt), TraceDescr)]`.
A `Staged` AST now denotes a set of possible executions in
the `DirectT` monad, each of which has its own description of which
transactions led to that particular outcome.

The `everywhere f tr` combinator maps a transformation `f` to every
transaction `tx` in every outcome of `tr` such that `isJust (f tx)`.
Its dual, `somewhere f tr` will fork new outcomes from `tr` such that
in each one, `f` was applied to exactly one transaction `tx` such that
`isJust (f tx)`. To illustrate this, let `a, a', b, b'` and `c` be
arbitrary, but different, transactions. Now we define a
transformation:

```
f :: Tx -> Maybe Tx
f a = Just a'
f b = Just b'
f _ = Nothing
```

Interpreting `everywhere f (mapM_ validateTx [a,b,c])` should be equivalent to:

```haskell
\st -> [ runDirect (validateTx a' >> validateTx b' >> validateTx c) st ]
```

That is, it returns the same set of traces, but with all possible transactions modified
by `f`. Those that `f` returned `Nothing` are left untouched.

Interpreting `somewhere f (mapM_ validateTx [a,b,c])` should be equivalent to:

```haskell
\st -> [ runDirect (validateTx a' >> validateTx b  >> validateTx c) st
       , runDirect (validateTx a  >> validateTx b' >> validateTx c) st
       ]
```

The [final definition of `interpret`][interpret] gets a little
involved, hence we will omit it here but we invite the interested
reader to go check its source. The central idea in implementing it is
maintaining a set of modalities that are yet to be consumed in the
current branch. For instance, if we are interpreting a trace
`somewhere f tr`, we collect `Somewhere f` as a modality that needs to
be consumed and continue interpreting `tr`. Say we are interpreting
`Bind (ValidateTx tx) rest` with two modalities to be consumed,
`[Somewhere f, Everywhere g]`. We can either consume the `Somewhere`
now, applying `f` to `tx`, or later, within `rest`. Yet, we must consume
and preserve the `Everywhere g` in either case.

## Closing thoughts

The combination of techniques presented in this post address an
important obstacle in writing _good_ test suites over code involving a state monad.
In practice, they enable us to write complex traces for
[Plutus][plutus] contracts, and we can use `somewhere` and
`everywhere` to exhaustively attempt to execute an attack
within some trace.

Having combinators is preferable to generating traces with hardcoded transformations.
The resulting code is easier to understand and we get to write less code.
A trace `somewhere f tr` doesn't need to be fixed if something in `tr` changes.
Moreover, we only have to maintain `tr`, and not a family of variations of `tr` that
applies `f` to its different transactions. In general, it is useful to automatically rule out
certain kinds of bad traces, which can be obtained as modifications over good traces
through the injection of attacks and faults.

[plutus]: https://plutus.readthedocs.io/en/latest/
[nft]: https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week5.html#nfts
[cooked-validators]: https://github.com/tweag/plutus-libs/tree/main/cooked-validators
[interpret]: https://github.com/tweag/plutus-libs/blob/30f4c061cc8d38e5968bbb6418b40a6f4e9e25fa/cooked-validators/src/Cooked/MockChain/Monad/Staged.hs#L39
[free-monads]: https://www.tweag.io/blog/2018-02-05-free-monads/
[freer-monad]: https://dl.acm.org/doi/abs/10.1145/2887747.2804319
[gent]: https://hackage.haskell.org/package/quickcheck-transformer
