---
title: "The Shrinks Applicative"
author: "Arnaud Spiwack"
tags: [haskell]
description: "An applicative functor to define shrinkers"
---

One of the key ingredients of randomised property testing is the
shrinker. The shrinker turns the output of a failed property test from
"your function has a bug" to "here is a small actionable example where
your function fails to meet the specification". Specifically, after
a randomised test has found a counterexample, the shrinker will kick
in and recursively try smaller potential counterexamples until it
can't find a way to reduce the counterexample anymore.

## Roll your own shrinker

When it comes to writing a shrinker for a particular generator, my
advice is:

- If you are using [QuickCheck] and you can use
  [`genericShrink`](https://hackage.haskell.org/package/QuickCheck-2.14.1/docs/Test-QuickCheck.html#v:genericShrink),
  do so.
- Otherwise, give [Hedgehog] a try

Hedgehog will automatically generate shrinkers for you, even for the
most complex types. They are [far from perfect][edsko-shrinkers], but
in most cases, writing a shrinker manually is too hard to be worth it.

Nevertheless, there are some exceptions to everything. And you may
find yourself in a situation where you have to write something which
is much like a QuickCheck shrinker, but not quite. I have. If it
happens to you, this blog post provides a tool to add to your
tool belt.

## Applicative functors

I really like applicative functors. If only because of how easy they
make it to write traversals.

```haskell
data T a
  = MkT1 a
  | MkT2 a (T a)
  | MkT3 a (T a) a

class Traversable T where
  traverse f (MkT1 a) = MkT1 <$> f a
  traverse f (MkT2 a as) = MkT2 <$> f a <*> traverse f as
  traverse f (MkT3 a1 as a2) = MkT3 <$> f a1 <*> traverse f as <*> f a2
```

There is a zen to it, really: we're just repeating the
definition. Just slightly accented.

So when defining a shrinker, I want to reach for an applicative
functor.

Let's look at the type of `shrink`: from a counterexample, `shrink`
proposes a list of smaller candidate counterexample to check:

```haskell
shrink :: a -> [a]
```

Ah, great! `[]` is already an applicative functor. So we can go and
define

```haskell
shrink :: (a, b) -> [(a, b)]
shrink = (,) <$> shrink a <*> shrink b
-- Which expands to:
shrink = [(a, b) | a <- shrink a, b <- shrink b]
```

But if I compare this definition with the actual shrinker for `(a, b)`
in Quickcheck:

```haskell
shrink :: (a, b) -> [(a, b)]
shrink (x, y) =
     [ (x', y) | x' <- shrink x ]
  ++ [ (x, y') | y' <- shrink y ]
```

I can see that it's a bit different. My list-applicative based
implementation shrinks too fast: it shrinks both components of the pair
at the same time, while Quickcheck's hand-written shrinker is more
prudent and shrinks in one component at a time.

## The Shrinks applicative

At this point I could say that it's good enough: I will miss some
shrinks, but it's a price I'm willing to pay. Yet, I can have my cake
and eat it too.

The problem of using the list applicative is that I can't construct
all the valid shrinks of `(x, y)` based solely on `shrink x` and
`shrink y`: I also need `x` and `y`. The solution is simply to carry
the original `x` and `y` around.

Let's define our `Shrinks` applicative:

```haskell
newtype Shrinks a = Shrinks { original :: a, shrinks :: [a] }
  deriving (Functor)

-- | Class laws:
-- * `original . shrinkA = id`
-- * `shrinks . shrinkA = shrink`
class Shrinkable a where
  shrinkA :: a -> Shrinks a
  shrinkA x = Shrinks { original=x, shrinks=shrink x}

  shrink :: a -> [a]
  shrink x = shrinks (shrinkA x)
  {-# MINIMAL shrinkA | shrink #-}
```

All we need to do is to give to `Shrinks` an `Applicative`
instance. Which we can base on the Quickcheck implementation of
`shrink` on pairs:

```haskell
instance Applicative Shrinks where
  pure x = Shrinks { original=x, shrinks=[] }

  fs <*> xs = Shrinks
    { original = (original fs) (original xs)
    , shrinks = [f (original x) | f <- shrinks fs] ++ [(original f) x | x <- shrinks xs]
    }
```

It is a simple exercise to verify the applicative laws. In the process
you will prove that

```haskell
shrinkA :: (a, b, c) -> Shrinks (a, b, c)
shrinkA (x, y, z) = (,,) <$> shrinkA x <*> shrinkA y <*> shrinkA z
```

does indeed shrink one component at a time.

## A word of caution

Using a traversal-style definition is precisely what we want for
fixed-shaped data types. But, in general, shrinkers require a bit more
thought to maximise their usefulness. For instance, in a list, you
will typically want to reduce the size of the list. Here is a possible
shrinker for lists:

```haskell
instance Shrinkable a => Shrinkable [a] where
  shrink xs =
    -- Remove one element
    [ take k xs ++ drop (k+1) xs | k <- [0 .. length xs]]
    -- or, shrink one element
    ++ shrinks (traverse shrinkA xs)
```

[quickcheck]: https://hackage.haskell.org/package/QuickCheck
[hedgehog]: https://hackage.haskell.org/package/hedgehog
[edsko-shrinkers]: http://www.well-typed.com/blog/2019/05/integrated-shrinking/
