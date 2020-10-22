---
title: "The Shrinks Applicative"
author: "Arnaud Spiwack"
tags: [haskell]
description: "An applicative functor to define shrinkers"
---

One of the key ingredient of randomised property testing is the
shrinker. The shrinker turns the output of a failed property test from
"your function has a bug" to "here is a small actionable example where
your function fails to meet the specification".

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
toolbelt.

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
  traverse f (MkT3 a1 as a2) = MkT3 <$> f a1 <*> traverses f as <*> f a2
```

There is a zen to it, really: we're just repeating the
definition. Just slightly accented.

## Shrinkers

So when defining a shrinker, I want to reach for an applicative
functor.

Let's look at the type of shrink

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
implementation shrinks too fast: it shrinks both component of the pair
at the same time, while Quickcheck's hand-written shrinker is more
prudent and shrinks in one component at a time.

## The Shrinks applicative

At this point I could say that it's good enough: I will miss some
shrinks, but it's a price I'm willing to pay. Yet, I can have my cake
and eat it too.

The problem of using the list applicative is that I can't construct
all the valids shrinks of `(x, y)` based solely on the `shrink x` and
`shrink y`: I also need `x` and `y`. The solution is simply to carry
the original `x` and `y` around.

Let's define our `Shrinks` applicative:

```haskell
newtype Shrinks a = Shrinks (NonEmpty a)
  deriving (Functor)

class Shrinkable a where
  -- Invariant: `head . shrinkA = id`
  shrinkA :: a -> Shrinkable a
  
shrink :: Shrinkable a => a -> [a]
shrink x = case shrinkA x of
  Shrinks xs -> tail xs
```

All we need to do is to give to `Shrinks` an `Applicative`
instance. Which we can base on the Quickcheck implementation of
`shrink` on pairs:

```haskell
instance Applicative Shrinks where
  pure x = Shrinks [x]
  
  (Shrink (f :| fs)) <*> (Shrinks (x :| xs)) = Shrinks $
      (f x) :| [f' x | f' <- fs] ++ [f x' | x' <- xs]
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
  shrinkA xs = 
    -- Remove one element
    [ take k xs ++ drop (k+1) xs | k <- [0 .. length xs]]
    -- or, shrink one element
    ++ traverse shrinkA xs
```

[QuickCheck]: https://hackage.haskell.org/package/QuickCheck
[Hedgehog]: https://hackage.haskell.org/package/hedgehog
[edkso-shrinkers]: http://www.well-typed.com/blog/2019/05/integrated-shrinking/
