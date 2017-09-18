---
title: Free monads for cheap interpreters
author: James Haydon
featured: yes
---

## What is a free monad?

In this post we'll talk about free monads and how they can help structure a
codebase. The problem I came across in a particular project was the following:

- We have a system which produces many possible *effectful* computations, many
  of which will fail.
- You want to run only one of these (or none at all if they all fail).

So now we need to be able to run things without running them. Free monads help
here because if you target a free monad, you know it'll be easy to interpret
it in many target environments. This is because they are *free*, and the
definition of a free object (in a category) says that they are easy to map
*from*.

Specifically, a free object is *generated* by something less powerful, and then
the idea is that to map to something we now only need to provide a definition
over the generating object (which is easier).

To give an example, in high-school you may have been asked to manipulate lots of
maps `f :: R^n -> R^m`. Now `R^n` happens to be a free object over any set of
vectors that form a *basis*. So instead of defining the function `f` over *all*
the points of `R^n`, which would be tedious, we just define it over the `n`
points `(1,0,..,0)`, `(0,1,0,..,0)`, etc. These get mapped to vectors in `R^m`,
which we stick together to form a grid of numbers: now you have a matrix. The
matrices are much more economical and much easier to manipulate.

This is the essence of the advantage of free monads: *morphisms between free
monads are economical and easy to manipulate.*

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
well know are what makes up a monad. Here `Free` says that you can squash down
any `f`-structure of monadic values. When you apply `Free` to a functor, you get
another functor back:

```haskell
instance Functor f => Functor (Free f) where
  fmap g (Free fx) = Free (fmap g <$> fx)
  fmap g (Pure x)  = Pure (g x)
```

But more is true, `Free f` is a monad:
```haskell
instance Functor f => Monad (Free f) where
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

Happily, this law is automatic in Haskell (i.e. it's impossible to define a
natural transformation between functors that breaks this law).

One important piece of structure is that not only does `Free` take functors to functors, it also maps natural transformations to naturaral transformations:

```haskell
freeM :: (Functor f, Functor g) => f ~> g -> Free f ~> Free g
freeM phi (Pure x) = Pure x
freeM phi (Free fx) = Free $ phi (freeM phi <$> fx)
```

This makes `Free` a *functor*, not a haskell-functor, but a functor of categories: from the category of functors and natural transformations to itself.

So now the first important remark is that `Free` doesn't do anything if we apply it to something that is already a monad, and this is neatly expressed by the natural transformation:

```haskell
monad :: Monad m => Free m ~> m
monad (Pure x) = pure x
monad (Free mfx) = do
  fx <- mfx
  monad fx
```

If you have two monads, `m` and `n`, then the proper notion of morphism between them is a
_monad morphism_. This is a natural transformation `phi :: m ~> n` with a bunch of extra
properties that make sure you aren't doing something insane.

First of all: `phi . pure = pure`. That is, it should take pure values to pure
values. Second, if we have something of type `m (m a)` there are now several
ways we can get and `n a`:
- Sequence in `m`, and then translate: `phi . join`.
- Or, translate the two parts independently, and then sequence in `n`: `join .
(fmap phi) . phi`. If you want to translate between monads in a sensible way,
these should obviously produce the same thing!

Monad morphisms are often called _interpretations_ because they interpret,
sensibly, computations into other computations.

The neat thing about free monads is that interpretations are _cheap_;
interpreting them in other monads is easy. The idea is that all we need is a
natural transformation of functors, and then we get a morphism of monads, for
free.

```haskell
-- | Buy one natural transformation, and get this monad morphism for free!
interp :: (Functor f, Monad m) => f ~> m -> Free f ~> m
interp phi = monad . freeM phi
```

Great! So lets recap: `Free` is actually a functor mapping haskell-functors to
haskell-monads. What's more, this functor is left-adjoint to the "forgetful
functor", the one that takes a monad `m` and considers it as just a plain old
haskell-functor.

What does that mouthful of abstract nonsense mean? It means that morphisms of
monads `Free f ~> m` are the same as natural transformations of functors `f ~>
m`. And that means that that **ALL** interpretations of `Free f` can be obtained
by using the `interp` function. So you don't need to ever worry about some
complicated interpretation not being definable with `interp`.

The functor `Free` (being the left side of an adjunction) itself defines a monad
(of the categorical sort) on the category of haskell-functors. And the algebras
of this monad are.. _monads_! (the haskell ones).

So in fact `Free` is so essential to the concept of monad that it contains the
definition of what a monad is within itself, and so, we could redefine haskell's
monad typeclass (we'll call our new class `Monad'`) as just being algebras for `Free`:

```haskell
class Functor m => Monad' m where
  monad :: Free m ~> m
```

And we can see that it's just as powerful as `Monad`:
```haskell
pure' :: Monad' m => a -> m a
pure' = monad . Pure

join' :: Monad' m => m (m a) -> m a
join' = monad . Free . fmap (Free . fmap Pure)
```

## Free monads in the real world

Okay, so how do we use free monads in your codebase?

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
logic. In this case, we are making some software for people who want to organise
social clubs.

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
sumNat :: NatTrans f t -> NatTrans g t -> NatTrans (Sum f g) t
sumNat phi _   (InL x) = phi x
sumNat _   psi (InR x) = psi x
```
because `Sum` is the coproduct in the category of functors.

Using some helper functions:
```haskell
left :: (Functor f, Functor g) => Free f a -> Free (Sum f g) a
left = interp (Free . InL . fmap pure)

right :: (Functor f, Functor g) => Free g a -> Free (Sum f g) a
right = interp (Free . InR . fmap pure)
```
We can (finally!) start writing some interpretations:
```haskell
-- Console in IO:
consoleIO :: NatTrans ConsoleF IO
consoleIO (PutStrLn s v) = do
  Prelude.putStrLn s
  pure v
consoleIO (GetLine cb) = do
  s <- Prelude.getLine
  pure (cb s)

-- KeyValue in IO via Redis.
keyValIO :: NatTrans KeyValF IO
keyValIO (GetKey k cb) = do
  r <- Redis.lookupKey k
  pure (cb r)
keyValIO (PutKey k v n) = do
  Redis.putKeyVal k v
  pure n
```

If we wanted to use a different key-value store one day, all we'd have to do is
swap out this interpretation.

We have a mock one too:
```haskell
-- This one could read from serialised data on disk, or
-- some other clever thing.
mockKeyValIO :: NatTrans KeyValF IO
mockKeyValIO = ...
```

Finally, we interpret our business logic into a free monad representing all the
functionality we need: `Console` and `KeyVal`. This takes care of translating
our high-level API into the nitty gritty of which keys are used in our Redis
system.

```haskell
clubI :: NatTrans ClubF (Free (Sum ConsoleF KeyValF))
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

Now combining interpreters is easy:
```haskell
real, mock :: Free ClubF a -> IO a
real = interp $ interp (sumNat consoleIO keyValIO)     . clubI
mock = interp $ interp (sumNat consoleIO mockKeyValIO) . clubI
```
And switching between a real computation and a mock one is as easy as calling
`real` or `mock`:

```haskell
main :: IO ()
main =
  real showClubSiblings
  -- mock showClubSiblings
```
