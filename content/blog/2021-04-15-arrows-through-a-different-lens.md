---
title: "Arrows, through a different lens"
author: Juan Raphael Diaz Simões
tags: [haskell]
description: "How to use lenses and other optics within an Arrow-based framework."
---

Our previous posts on computational pipelines, such as those introducing [Funflow][funflow] and [Porcupine][porcupine], show that Arrows are very useful for data science workflows.
They allow the construction of effectful and composable pipelines whose structure is known at compile time, which is not possible when using `Monad`s.
However, Arrows may seem awkward to work with at first. For instance,
it's not obvious how to use _lenses_ to access record fields in Arrows.

My goal in this post is to show how lenses and other optics can be used in Arrow-based workflows.
Doing so is greatly simplified thanks to [Profunctor
optics][profunctor-optics] and some utilities that I helped add to the latest version of the [lens][lens] library.

## Optics on functions

We're used to think of lenses in terms of getters and setters, but I'm
more interested today in the functions `over` and `traverseOf`.

```haskell
-- We will use this prefix for the remaining of the post.
-- VL stands for Van Laarhoven lenses.
import qualified Control.Lens as VL

-- Transform a pure function.
over :: VL.Lens s t a b -> (a -> b) -> (s -> t)

-- Transform an effectful function.
traverseOf :: VL.Traversal s t a b -> (a -> m b) -> (s -> m t)
```

We would like to use similar functions on Arrow-based workflows,
something like

```haskell
overArrow :: VL.Lens s t a b -> Task a b -> Task s t
```

However, the type of lenses:

```haskell
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
```

doesn't make it very obvious how to define `overArrow`.

On the other hand, Arrows come equipped with functions `first` and `second`:

```haskell
first :: Task b c -> Task (b, d) (c, d)
second :: Task b c -> Task (d, b) (d, c)
```

which very much feel like specialised versions of `overArrow` for the
lenses

```haskell
_1 :: VL.Lens (b, d) (c, d) b c
_2 :: VL.Lens (d, b) (d, c) b c
```

so maybe there is a common framework that can take both of these into account?
The answer is yes, and the solution is lenses -- but lenses of a different type.

## Profunctor optics

There is an alternative and equivalent formulation of optics, called [Profunctor optics][profunctor-optics], that works very well with `Arrows`.
Optics in the `Profunctor` framework have the following shape:

```haskell
type Optic p s t a b = p a b -> p s t
```

with more precise optics such as `Lens` being obtained by imposing constraints to `p` coming from the different [Profunctor][profunctor] classes.
In other words, an `Optic` is _precisely_ a higher-order function acting on some profunctor.
Because every Arrow is also a Profunctor[^arrow-profunctor], the shape
of an `Optic` is precisely what is needed to act on Arrows! Moreover, like the optics of the `lens` library, profunctor optics can be composed like regular functions, with `(.)`.

The `lens` library now includes a [module][profunctor-module] containing functions that convert between standard and profunctor optics, which makes using them very convenient.

In the following sections, we will go through the use and the intuition of the most common optics: `Lens`, `Prism` and `Traversal`.
But first, let's import the compatibility module for profunctor optics:

```
-- PL for Profunctor Lenses
import Control.Lens.Profunctor as PL
```

## Lenses

Standard lenses are all about products -- `view`, for example, is used to deconstruct records:

```
view _fst :: (a, b) -> a
```

Therefore, it makes sense for Profunctor lenses to also talk about products.
Indeed, that is exactly what happens, through the `Strong` type class:

```haskell
class Profunctor p => Strong p where
  first' :: p a b -> p (a, c) (b, c)
  second' :: p a b -> p (c, a) (c, b)
```

With profunctor optics, a `Lens` is defined as follows:

```haskell
type Lens s t a b = forall p. Strong p => p a b -> p s t
```

Every `Arrow` satisfies the `Strong` class.
If we squint, we can rewrite the type of these functions as:

```haskell
first' :: Lens' (a,c) (b,c) a b
second' :: Lens' (c,a) (c,b) a b
```

That is, a `Strong` profunctor is equipped with lenses to reach
inside products.
One can always convert a record into nested pairs and act on them using `Strong` -- the `Lens` just makes this much more convenient.

But how do we build a `Lens`?
Besides writing them manually, we can also use all `Lens`es from `lens`:

```haskell
PL.fromLens :: VL.Lens s t a b -> Lens s t a b
```

which means we can still use all the lenses we know and love.
For example, one can apply a task to a tuple of arbitrary size:

```haskell
PL.fromLens _1 :: Task a b -> Task (a,x,y) (b,x,y)
```

Summarizing, a `Strong` profunctor is one we can apply lenses to.
Since every `Arrow` is also a `Strong` profunctor, one can use `Lens`es with them.

## Prisms

Standard prisms are all about sums -- `preview`, for example, is used to deconstruct sum-types:

```
view _Left :: Either a b -> Maybe a
```

Therefore, it makes sense for Profunctor prisms to also talk about sums.
Indeed, that is exactly what happens, through the `Choice` type class:

```haskell
class Profunctor p => Choice p where
  left' :: p a b -> p (Either a c) (Either b c)
  right' :: p a b -> p (Either c a) (Either c b)
```

With profunctor optics, a `Prism` is defined as follows:

```haskell
type Prism s t a b = forall p. Choice p => p a b -> p s t
```

Every `ArrowChoice` satisfies the `Choice` class.
Once more, we can rewrite the type of these functions as:

```haskell
left' :: Prism (Either a c) (Either b c) a b
right' :: Prism (Either c a) (Either c b) a b
```

That is, a `Choice` profunctor is equipped with prisms to discriminate sums.
One can always convert a sum into nested `Either`s and act on them using `Choice` -- the `Prism` just makes this much more convenient.

But how do we build a `Prism`?
We can also use any prisms from `lens` with a simple conversion:

```haskell
PL.fromPrism :: VL.Prism s t a b -> Prism s t a b
```

For example, one can execute a task conditionally, depending on the existence of the input:

```haskell
PL.fromPrism _Just :: Action a b -> Action (Maybe a) (Maybe b)
```

Summarizing, a `Choice` profunctor is one we can apply prisms to.
Since every `ArrowChoice` can be a `Choice` profunctor, one can uses prisms with them.

## Traversals

Standard traversals are all about `Traversable` structures -- `mapMOf`, for example, is used to execute effectful functions:

```haskell
mapMOf traverse readFile :: [FilePath] -> IO [String]
```

Therefore, it makes sense for Profunctor traversals to also talk about these traversable structures.
Indeed, that is exactly what happens, through the `Traversing` type class:

```haskell
class (Choice p, Strong p) => Traversing p where
  traverse' :: Traversable f => p a b -> p (f a) (f b)
```

With profunctor optics, a `Traversal` is defined as follows:

```haskell
type Traversal s t a b = forall p. Traversing p => p a b -> p s t
```

There is no associated `Arrow` class that corresponds to this class, but many `Arrow`s, such as `Kleisli`, satisfy it.
We can rewrite the type of this functions as:

```haskell
traverse' :: Traversable f => Traversal (f a) (f b) a b
```

That is, a `Traversing` profunctor can be lifted through `Traversable`
functors.

But how do we build a `Traversal`?
We can also use any `Traversal` from `lens` with a simple conversion:

```haskell
PL.fromTraversal :: VL.Traversal s t a b -> Traversal s t a b
```

For example, one can have a task and apply it to a list of inputs:

```haskell
PL.fromTraversal traverse :: Action a b -> Action [a] [b]
```

## Conclusion

Using Arrows does not stop us from taking advantage of the Haskell ecosystem.
In particular, optics interact very naturally with Arrows, both in their classical and profunctor formulations.
For the moment, the ecosystem is still lacking a standard library for Profunctor optics, but this is not a show stopper — the `lens` library itself has most of the tools we need.
So the next time you are trying out [Funflow][funflow] or [Porcupine][porcupine], don't shy away from using `lens`!

[funflow]: ./2018-04-25-funflow.html
[porcupine]: ./2019-10-30-porcupine.html
[funflow-src]: https://github.com/tweag/funflow/
[porcupine-src]: https://github.com/tweag/porcupine
[arrow]: https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Arrow.html
[lens]: https://hackage.haskell.org/package/lens-5.0.1
[profunctor-optics]: https://arxiv.org/abs/1703.10857
[profunctor]: https://hackage.haskell.org/package/profunctors-5.5.2/docs/Data-Profunctor.html
[profunctor-module]: https://hackage.haskell.org/package/lens-5/docs/Control-Lens-Profunctor.html

[^arrow-profunctor]: The fact that these hierarchies are separated is due to historical reasons.
