---
title: Free monads for cheap interpreters
author: James Haydon
featured: yes
---

The utility of free monads can show up in surprising places. One of
the projects I'm working on is an AI, and part of the strategy that it
uses for responding to user input is quite simple: it generates many
possible responses, and then evaluates them. Most of the computations
it generates will be malformed, and so will fail; we just want to skip
over these as quickly as possible and move onto the next possibility.
In summary:

- A system generates many possible *effectful* computations only one of which
  will ultimately be used to form a response.
- The computations have to be executed in order to even be considered.
- Most of the computations will fail. Sometimes because of I/O, but mostly
  because the computation is malformed.
- We only want the effects of the chosen query to actually execute.

A lot of the I/O is very slow (involving expensive requests to other APIs), and a
computation may make plenty of these requests only to fail later for a
completely unrelated and trivial reason. The system will usually go through a
large number of failing computation before hitting on one that succeeds, so we
want failing computation to fail as quickly as possible. To do this we write
different interpreters which mock certain APIs, providing realistic values for
the rest of the computation. These interpreters will filter out dud computations
in stages.

Free monads are a nice way to structure this problem because interpretations of
free monads can be defined, composed and combined very flexibly, allowing us to
build up a library of interpreters for solving our problem.

## What is a free monad?

*Interpreting* means giving meaning to some piece of data, and the meaning is
often provided by stuff that gets done, which in Haskell corresponds to monads.
Free monads are very easy to interpret (in other monads) because they are
*free*, and the definition of a free object (e.g. in category theory) says that
they are easy to map *from*. So that's the basic idea behind free monads: easy
to interpret.

Specifically, a free object is *generated* by something less complex, and then
to map to something we now only need to provide a definition over the generating
object (which is easier, since it's got less structure).

To give an example, in high-school you may have been asked to manipulate lots of
maps <code>f :: ℝ<sup>n</sup> -&gt; ℝ<sup>m</sup></code>. Instead of defining the function `f`
over *all* the points of <code>ℝ<sup>n</sup></code>, which would be tedious, we just define
it over the `n` points `(1,0,..,0)`, `(0,1,0,..,0)`, etc. This is enough because
<code>ℝ<sup>n</sup></code> happens to be a free object over any set of vectors that form a
  *basis*. These `n` points get mapped to `n` vectors in <code>ℝ<sup>m</sup></code>, which we
stick together to form a grid of numbers: now you have a matrix. The matrices
are much more economical and much easier to manipulate.

This is the essence of the advantage of free monads: *morphisms between free
monads are economical and easy to manipulate.* In fact the manipulations can be
very similar to those on matrices.

Let's dive in. We will generate monads with functors (because functors are
easier than monads). Given a functor `f`, `Free f` is the monad it generates:

```haskell
data Free f a
  = Pure a
  | Free (f (Free f a))
```

To try and understand this definition, let's assume `f` is a functor, and we let
`m` be `Free f`, then what are the types of `Pure` and `Free`?

```haskell
Pure :: a -> m a        -- looks like pure
Free :: f (m a) -> m a  -- looks like join
```

So essentially all we are doing is "adjoining" these two operators, which we
well know are what makes up a monad. When you apply `Free` to a functor, you get
another functor back:

```haskell
instance Functor f => Functor (Free f) where
  fmap g (Free fx) = Free (fmap g <$> fx)
  fmap g (Pure x)  = Pure (g x)
```

But more is true, `Free f` is a monad:
```haskell
instance Functor f => Monad (Free f) where
  return = Pure
  Pure x  >>= g  =  g x
  Free fx >>= g  =  Free ((>>= g) <$> fx)
```

To really understand the properties of this construction, we need *natural
transformations*, which is what we use when we want to talk about mapping one
functor into another.

```haskell
infixr 0 ~>
type f ~> g = forall x. f x -> g x
```

An actual natural transformation `phi` should obey this law:
```haskell
fmap t . phi = phi . fmap t
```

One important piece of structure is that not only does `Free` take functors to
functors, it also maps natural transformations to natural transformations:

```haskell
freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM phi (Pure x) = Pure x
freeM phi (Free fx) = Free $ phi (freeM phi <$> fx)
```

This makes `Free` a *functor*, not a Haskell-functor, but a functor of
categories: from the category of functors and natural transformations to itself.

If `m` is already a monad, then there is a special interpretation of `Free m`
into itself, which we'll have more to say about later:

```haskell
monad :: Monad m => Free m ~> m
monad (Pure x) = pure x
monad (Free mfx) = do
  fx <- mfx
  monad fx
```

If you have two monads, `m` and `n`, then the proper notion of morphism between
them is a _monad morphism_. This is a natural transformation `phi :: m ~> n`
with a bunch of extra properties that make sure you aren't doing something
insane.

First of all: `phi . pure = pure`. That is, it should take pure values to pure
values. Second, if we have something of type `m (m a)` there are now several
ways we can get and `n a`:
- Sequence in `m`, and then translate: `phi . join`.
- Or, translate the two parts independently, and then sequence in `n`: `join .
(fmap phi) . phi`.

If you want to translate between monads in a sensible way,
these should produce the same thing!

Monad morphisms are a more precise term for what we've loosely been calling
"interpretations" up till now.

The neat thing about free monads is that interpretations are _cheap_;
interpreting them in other monads is easy. The idea is that all we need is a
natural transformation of functors, and then we get a morphism of monads, for
free.

```haskell
-- | Buy one natural transformation, and get this monad morphism for free!
interp :: (Functor f, Monad m) => f ~> m -> Free f ~> m
interp phi = monad . freeM phi
```

Great! So let's recap: `Free` is actually a functor mapping Haskell-functors to
Haskell-monads and morphisms of monads `Free f ~> m` are the same as natural
transformations of functors `f ~> m` (via `interp`). Furthermore, **ALL**
interpretations of `Free f` can be obtained by using the `interp` function. So
you don't need to ever worry about some complicated interpretation not being
definable with `interp`.

The functor `Free` itself defines a monad (of the categorical sort) on the
category of haskell-functors. And the algebras of this monad are.. _monads_!
(the Haskell ones.)

So in fact `Free` is so essential to the concept of monad that it contains the
definition of what a monad is within itself, and so, we could redefine Haskell's
monad typeclass (we'll call our new class `Monad'`) as just being algebras for
`Free`:

```haskell
class Functor m => Monad' m where
  monad :: Free m ~> m
```

An amusing exercise is to write the `Monad' m => Monad m` instance. Try on your
own but here's the answer if you can't be bothered:

```haskell
pure' :: Monad' m => a -> m a
pure' = monad . Pure

join' :: Monad' m => m (m a) -> m a
join' = monad . Free . fmap (Free . fmap Pure)
```

## Free monads in the real world

Okay, so how do we use free monads in a codebase?

The idea is to create languages defined by functors for each piece of
functionality in our system. These can be thought of as APIs.

```haskell
-- | Key value store functionality.
data KeyValF a
  = GetKey String (Maybe String -> a)
  | PutKey String String a
  deriving (Functor)

-- | Console functionality.
data ConsoleF a
  = PutStrLn String a
  | GetLine (String -> a)
  deriving (Functor)

type Console = Free ConsoleF
type KeyVal = Free KeyValF
```

The following function helps when actually coding against these sorts of API:

```haskell
liftF :: Functor f => f a -> Free f a
liftF command = Free (fmap Pure command)
```

Since then we can create helper functions like so:

```haskell
getKey :: String -> KeyVal (Maybe String)
getKey k = liftF (GetKey k id)

putStrLn :: String -> Console ()
putStrLn s = liftF (PutStrLn s ())

getLine :: Console String
getLine = liftF (GetLine id)
```

At the top-most level, you want to create a functor representing your business
logic. In this case, we can imagine making software for people who want to
organise social clubs.

```haskell
data ClubF a
  = GetClubMembers String (Maybe [String] -> a)
  | GetMemberClubs String (Maybe [String] -> a)
  | GetInput (String -> a)
  | Display String a
  deriving (Functor)

type Club = Free ClubF

-- plus helper functions
```

Now we can define our business logic in a clean, abstract way:
```haskell
-- | Given a club id, shows the list of "sibling" clubs.
showClubSiblings :: Club ()
showClubSiblings = do
  display "Enter club Id:"
  clubId <- getInput
  mmembers <- getClubMembers clubId
  case mmembers of
    Nothing -> display "Sorry, that club does not exist!"
    Just members -> do
      r <- sequence <$> traverse getMemberClubs members
      case r of
        Nothing -> display "Error getting club members."
        Just clubIdGroups -> do
          let siblings = nub $ concat clubIdGroups
          display $ "Here are the siblings of club " ++ clubId ++ ":"
          display (intercalate ", " siblings)
```

Remember when we talked about matrices? Matrices can easily be multiplied and
spliced together to make new matrices. The same is true of natural
transformations; they can be composed (this is just `.`) and "co-paired":

```haskell
sumNat :: (f ~> t) -> (g ~> t) -> (Sum f g) ~> t
sumNat phi _   (InL x) = phi x
sumNat _   psi (InR x) = psi x
```
because `Sum` is the coproduct in the category of functors.

Using some helper functions:

```haskell
left :: (Functor f, Functor g) => Free f ~> Free (Sum f g)
left = freeM InL

right :: (Functor f, Functor g) => Free g ~> Free (Sum f g)
right = freeM InR
```

We can (finally!) start writing some interpretations:

```haskell
-- Console in IO:
consoleIO :: ConsoleF ~> IO
consoleIO (PutStrLn s v) = do
  Prelude.putStrLn s
  pure v
consoleIO (GetLine cb) = do
  s <- Prelude.getLine
  pure (cb s)

-- KeyValue in IO via Redis.
keyValIO :: KeyValF ~> IO
keyValIO (GetKey k cb) = do
  r <- Redis.lookupKey k
  pure (cb r)
keyValIO (PutKey k v n) = do
  Redis.putKeyVal k v
  pure n
```

If we wanted to use a different key-value store one day, all we'd have to do is
swap out this interpretation.

And for each component of our language we also write some mock interpreters:

```haskell
-- Mocked reads and writes
mockKeyValIO :: KeyValF ~> IO
mockKeyValIO = ...

-- Real reads but mock writes
mockWritesKeyValValIO :: KeyValF ~> IO
mockWritesKeyValValIO = ...

mockConsoleIO :: ConsoleF ~> IO
mockConsoleIO = ...
```

Finally, we interpret our business logic into a free monad representing all the
functionality we need: `Console` and `KeyVal`. This takes care of translating
our high-level API into the nitty-gritty of which keys are used in our Redis
system. Structuring the system in this way guarantees that *such details are
banished from the rest of the code, and there is a single function where these
conventions may be changed*.

```haskell
clubI :: ClubF ~> (Free (Sum ConsoleF KeyValF))
clubI (GetClubMembers clubId next) = do
  r <- right $ getKey ("clubs." ++ clubId ++ ".members")
  pure $ next (words <$> r)
clubI (GetMemberClubs memberId next) = do
  r <- right $ getKey ("users." ++ memberId ++ ".clubs")
  pure $ next (words <$> r)
clubI (GetInput next) = do
  r <- left Free.getLine
  pure $ next r
clubI (Display o next) = do
  left $ Free.putStrLn o
  pure next
```

## Solving our initial problem

Now combining interpreters is easy, we can just use `(.)` and `sumNat`. What's
more, we can mock certain aspects of our system selectively, and in varying
degrees, with great flexibility. It's this flexibility which gives us the
ability to create a spectrum of mock interpreters which we use to filter the
large set of computations we need to test.

Our computations are expressed by the `CompF` functor, and we could capture all
the data-requirements of our domain in a `DataF` functor, which interprets into
various database models:

```haskell
data :: CompF ~> DataF

keyVal :: DataF ~> KevValF
relational :: DataF ~> RelationalF
graph :: DataF ~> GraphF
```

And each of `KeyValF`, `RelationalF` and `GraphF` might point to several
specific implementations, each with their own mocking strategies.

We create several sorts of mocking interpreters: `DataF ~> IO` with varying
accuracy and speed:

- `mockDataCached`: uses the lower level mocks, reading data from files which
  have cached the responses.
- `mockDataGen`: a cheaper direct interpretation into `IO` which randomly
  generates plausible looking data of the right shape (à la `QuickCheck`).

Using this sort of composability of interpreters, we can create our final set of
interpreters:

```haskell
fastButCrude :: CompF ~> IO
mediumPlausible :: CompF ~> IO
slowButAccurate :: CompF ~> IO
```

and use them in succession on the large list of possible computations we need to
check, filtering out the dud ones in stages. In this way we filter out the
easily detectable duds using `fastButCrude`, so that `slowButAccurate` is only
used for those remaining harder-to-detect duds.
