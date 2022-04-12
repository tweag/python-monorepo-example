---
title: "Existential optics"
description: "Encoding optics with existential types"
author: Marco Perone
tags: [haskell, optics]
---

Optics are one of those concepts which always caught quite a lot of attention from the functional programming community, thanks to their composability. Still, if someone tries to understand how they work, just by looking at some data declarations and type definitions, he will probably have a hard time understanding what is actually going on.

In this post, I will try to present a different way of encoding optics, which could make understanding them easier. These ideas are not new and are well known in the category theory academic circles. Still, they do not seem to appear in libraries for languages like Haskell, Purescript, or Scala.

The most well-known type of optics are lenses, which were also the first to be analyzed and used. We will use them as our recurring example to compare the several ways we have to encode optics.

## Lenses 101

A `Lens` could be seen as the immutable version of a `getter`/`setter` pair you will often find in object-oriented programming, and especially in ORMs. It allows focusing a component inside a container data type.

For example, consider an `Address` record that contains a `street` field. A `Lens Address Street` allows us to retrieve the `Street` from the address

```haskell
streetLens :: Lens Address Street

view streetLens { street = "Baker Street", number = "221B" }
-- "Baker Street"
```

We could reuse the same lens to update the street inside the address

```haskell
over streetLens toUpper { street = "Baker Street", number = "221B" }
-- { street = "BAKER STREET", number = "221B" }
```

Lenses, and optics in general, compose extremely well. For example, if we also had a `User` record containing an `address` field of type `Address`, we could easily access and modify the `street` field of the address

```haskell
addressLens :: Lens User Address

view (streetLens . addressLens)
  { address = { street = "Baker Street", number = "221B" }, name = "Sherlock" }
-- "Baker Street"

over (streetLens . addressLens) toUpper
  { address = { street = "Baker Street", number = "221B" }, name = "Sherlock" }
-- { address = { street = "BAKER STREET", number = "221B" }, name = "Sherlock" }
```

Now it is time to ask ourselves how a `Lens s a` could be encoded. Next, I will present some possible alternatives for lenses and compare them with respect to the following aspects:

- how easy it is to understand what the encoding actually describes;
- how easily composition works;
- how easily we can generalize the encoding to other optics;

### Explicit encoding

The easiest option is to encode a `Lens` just as a getter and a setter

```haskell
data Lens s a = Lens
  { get :: s -> a
  , set :: s -> a -> s
  }
```

This encoding is extremely easy to grasp, packing together exactly the API we would like to use.

On the other hand, it is not immediate to understand how a `Lens s a` and a `Lens a b` could compose to return a `Lens s b`.

Also, this encoding is very ad-hoc, and it is not immediately clear how to generalize it to other optics.

### Van Laarhoven encoding

In 2009 Twan Van Laarhoven came up with a [new encoding for lenses](https://www.twanvl.nl/blog/haskell/cps-functional-references) which is the one still used by the [`lens`](https://hackage.haskell.org/package/lens) library.

```haskell
data Lens s a
  =  forall f. Functor f
  => (a -> f a) -> s -> f s
```

This says that a `Lens s a` allows us to lift a function `a -> f a` to a function `s -> f s` for any possible functor `f`.

I would argue that it is harder to understand what the encoding is describing. How is this equivalent to the previous one?

What we gain, though, is a massive improvement with respect to composability. Now, we can use just function composition, i.e. `.`, to compose lenses.

It also generalizes quite well to some other types of optics, but not as well as one might hope; see for example how the definition of [`Prism`](https://hackage.haskell.org/package/lens-5.1/docs/Control-Lens-Prism.html#t:Prism) in the [`lens`](https://hackage.haskell.org/package/lens) library differs.

### Profunctor encoding

The encoding which is commonly used for new optics libraries is the so-called profunctor encoding. The main idea is to quantify the encoding, not over functors, but profunctors instead.

```haskell
data Lens s a
  =  forall p. Strong p
  => p a a -> p s s
```

In these terms, a `Lens` is a way to lift a value of type `p a a` to a value of type `p s s` for any [`Strong`](https://hackage.haskell.org/package/profunctors-5.6.2/docs/Data-Profunctor.html#t:Strong) profunctor `p`.

I would argue that this encoding is even less immediate to understand than Van Laarhoven's one, but, it turns out, it is even easier to compose!

We are still dealing with simple functions, so we are still able to compose optics just with function composition.

It becomes also extremely easy to generalize this encoding to other types of optics. The type of optic is determined by the constraint we have on the `p` type variable. For example, if we use just `Profunctor`, we get [`Iso`s](https://hackage.haskell.org/package/profunctor-optics-0.0.2/docs/Data-Profunctor-Optic-Iso.html#t:Iso); if we use `Choice`, we get [`Prism`s](https://hackage.haskell.org/package/profunctor-optics-0.0.2/docs/Data-Profunctor-Optic-Prism.html#t:Prism).

In short, the profunctor encoding works extremely well with regard to compositionality, but it constrains us with an encoding that is not easy to manipulate.

So the question now is: can we encode optics in another way, that is more expressive and easy to manipulate, possibly giving up a little bit of composability?

## Existential encoding

An equivalent way of expressing what a lens is, uses the so-called existential encoding.

```haskell
data Lens s a
  = forall c. Lens (s -> (c, a)) ((c, a) -> s)
```

This says that a `Lens s a` is constituted by functions from `s` to `(c, a)` and back, where we can choose `c` arbitrarily. Since `c` appears only in the constructor and not in the type definition, it is called _existential_.

### Easy to grasp

Another way to read this definition is:

> A `Lens s a` is a proof that there exists a `c` such that `s` is isomorphic to `(c, a)`.

This is an extremely explicit way to think about a `Lens`; it says that whenever we have a `Lens`, we could actually think about a tuple.

### Easy to use

Thanks to the fact that we can easily understand what a `Lens` is with the existential encoding, it is also easy to understand how to use defining combinators for it. The idea is that we can deconstruct `s` into a pair `(c, a)` and then build it back.

For example, if we want to extract `a` from `s`, we simply deconstruct `s` into the pair `(c, a)`, and then we project it into the second component.

```haskell
view :: Lens s a -> s -> a
view (Lens f _) = snd . f
```

Similarly, is we want to lift a function `h :: a -> a` to a function `s -> s`, we can decompose `s` into `(c, a)`, map `h` over the pair and then reconstruct `s` back.

```haskell
over :: Lens s a -> (a -> a) -> s -> s
over (Lens f g) h = g . fmap h . f
```

### Easy to generalize to other optics

When it comes to generalizing the existential encoding to other optics, it turns out that it is enough to switch the `(,)` data type with another construct of the same shape.

#### Prisms

For example, if we use `Either` instead of `(,)` in the definition of a `Lens`, what we get is a `Prism`.

```haskell
data Prism s a
  = forall c. Prism (s -> Either c a) (Either c a -> s)
```

This definition tells us that a `Prism s a` is just a proof that there exists a `c` such that `s` is isomorphic to `Either c a`.

#### General optics

Generally, we can say that an optic has the following shape:

```haskell
data Optic f s a
  = forall c. Optic (s -> f c a) (f c a -> s)
```

This amounts to saying that `Optic f s a` is a proof that there exists a `c` such that `s` is isomorphic to `f c a`.

I find this a really clear explanation of what an optic is and how to think about it.

It is enough then to plug a concrete data type instead of `f` to get a concrete family of optics. For example:

```haskell
type Lens = Optic (,)

type Prism = Optic Either

type Iso = Optic Tagged

type Grate = Optic (->)

type AffineTraversal = Optic Affine

type Traversal = Affine PowerSeries
```

## Composing existential optics

Lenses and optics are well known for how well they compose. Let's see how the existential encoding behaves with respect to composition.

We can immediately observe that function composition does not work with the existential encoding, since we are not dealing with functions anymore.

Let's consider first the case where we are composing two optics of the same type

```haskell
compose :: Optic f s u -> Optic f u a -> Optic f s a
```

If we try to implement `compose` just following the types, we will probably arrive at

```haskell
compose
  :: (forall x. Functor (f x))
  => Optic f s u -> Optic f u a -> Optic f s a
compose (Optic f g) (Optic h l)
  = Optic (_a . fmap h . f) (g . fmap l . _b)
```

where `_a` and `_b` are typed holes with types

```haskell
_a :: f c (f c1 a) -> f c0 a
_b :: f c0 a -> f c (f c1 a)
```

We are deconstructing first `s` into `f c u` and then `u` into `f c1 a`.

### Existential associativity

To fill the holes we left, we can introduce a type class. Notice that `c0` in `_a` and `_b` is existential, so we have the freedom to choose it however we prefer.

```haskell
class ExistentiallyAssociative f where

  type E f a b

  existentialAssociateL :: f a (f b c) -> f (E f a b) c
  existentialAssociateR :: f (E f a b) c -> f a (f b c)
```

We use an associated type family `E` to be able to choose what `c0` should be for the given `f`. Notice also how, when `E f = f`, this type class is saying that `f` is associative. And this is in fact what happens with data types as `(,)` and `Either`. You can find instances for other data types [here](https://github.com/marcosh/existential-optics/blob/main/src/Associativity.hs).

Now, thanks to this new type class, we can fill the holes we left and conclude the definition of `compose`

```haskell
compose
  :: ( forall x. Functor (f x)
     , ExistentiallyAssociative f )
  => Optic f s u -> Optic f u a -> Optic f s a
```

### Changing optic type

To compose optics of different types (i.e. defined for different `f`s), we need first to be able to change the type of an optic. This makes sense because, for example, an `Iso` can always be seen both as a `Lens` and as a `Prism`, and `Lens`es and `Prism`s can be seen as `AffineTraversals`.

What we would like to do is convert an `Optic f s a` into an `Optic g s a`.

If we try to follow the types, we will probably end up with something like the following:

```haskell
morph :: Optic f s a -> Optic g s a
morph (Optic h l) = Optic (_a . h) (l . _b)

_a :: f c a -> g c0 a
_b :: g c0 a -> f c a
```

where `_a` and `_b` are type holes and `c0` is existential, meaning that we can choose what it is.

### Embedding

As we did for `ExistentiallyAssociative`, we are going to fill the holes by introducing a type class that provides exactly what we need. Similar to the previous case, we will use an associated type family `M` to be able to choose `c0`.

```haskell
class Morph f g where

  type M f g c

  f2g :: f c a -> g (M f g c) a
  g2f :: g (M f g c) a -> f c a
```

This class describes how we can embed `f` into `g` choosing `c0` appropriately.

For example, we can see the identity functor `Id` as a pair `(,)` choosing the first component of the pair to be `()`. Similarly, we can see `Id` as an `Either` choosing the left component to be `Void`. For more instances, take a look [here](https://github.com/marcosh/existential-optics/blob/main/src/Morph.hs).

This new type class allows us to complete the definition of `morph` we started above.

### Composing optics of different type

To compose existential optics of different types, we now need to connect all the pieces we have. To compose an `Optic f s u` with an `Optic g u a` we first need to morph them both to a common optic type where we can compose them.

```haskell
compose'
  :: ( ExistentiallyAssociative h
     , forall x. Functor (h x)
     , Morph f h
     , Morph g h )
  => Optic f s u
  -> Optic g u a
  -> Optic h s a
compose' opticF opticG
  = compose (morph opticF) (morph opticG)
```

## Conclusion

The existential encoding for optics can not compete with profunctor optics in terms of composability. On the other hand, it scores better on other aspects, in particular:

- optics definition is easy to understand. This is a two-fold perk. On one side, it makes teaching and learning optics easier. On the other hand, it makes the implementation of combinators and consuming the library easier, since the implementer is left to use simple data types.

- it clarifies what an optic is. Being able to express an optic as proof of the existence of an isomorphism, allows having a clear picture in mind about what an optic is. This helps discriminate optics from other constructs and possibly also to discover new optics.

- the hierarchy between optics is explicit. It is completely described by the `Morph` instances we described above, which can be understood in terms of embedding one data type into another.

This blog post would not exist without the precious work of many other programmers and category theorists. Reading [Bartosz Milewski](https://bartoszmilewski.com/category/lens/) optics-related work was a particular inspiration.

If you want to dive deeper into the code, you can find a sketch of the ideas explained in this post in [this repository](https://github.com/marcosh/existential-optics).
