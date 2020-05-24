---
redirect_from: [/posts/2020-01-16-data-vs-control.html]
title: "A Tale of Two Functors  or: How I learned to Stop Worrying and Love Data and Control"
shortTitle: A Tale of Two Functors
author: Arnaud Spiwack
tags: [haskell, linear-types]
description: "Haskell's Data and Control
  module hierarchies have always bugged me. Now, I understand that the intuition
  behind the Data/Control separation is rooted in a deep technical justification."
---

Haskell's `Data` and `Control` module
hierarchies have always bugged me. They feel arbitrary. There's `Data.Functor` _and_
`Control.Monad`—why? Monads are, after all, functors. They should
belong to the same hierarchy!

I'm not that person anymore. Now, I understand that the intuition
behind the Data/Control separation is rooted in a deep
technical justification. But—you rightly insist—monads are
_still_ functors! So what's happening here? Well, the truth is that there
_are_ two different kinds of functors. But you could never tell them
apart because they coincide in regular Haskell.

But they are different—so let's split them into two kinds: _data_ functors and _control_
functors. We can use [linear-types][linear-types]
to show why they are different. Let's get started.

## Data functors

If you haven't read about linear types, you may want to check out Tweag's other [posts on the topic][linear-list].
Notwithstanding, here's a quick summary: linear
types introduce a new type `a ⊸ b` of _linear functions_. A linear
function is a function that, roughly, uses its argument exactly
once.

With that in mind, let's consider a prototypical functor: lists.

```haskell
instance Functor [] where
  fmap :: (a -> b) -> [a] -> [b]
  fmap f [] = []
  fmap f (a:l) = (f a) : (fmap f l)
```

How could we give it a linear type?

- Surely, it's ok to take a linear function as an argument (if `fmap`
  works on any function, it will work on functions which happen to be
  linear).
- The `f` function is, on the other hand, not used linearly: it's used once per
  element of a list (of which there can be many!). So the second
  arrow must be a regular arrow.
- However, we are calling `f` on each element of the list exactly
  once. So it makes sense to make the rightmost arrow linear—exactly
  once.

So we get the following alternative type for list's `fmap`:

```haskell
fmap :: (a ⊸ b) -> [a] ⊸ [b]
```

List is a functor because it is a container of data. It is a _data functor_.

```haskell
class Data.Functor f where
  fmap :: (a ⊸ b) -> [a] ⊸ [b]
```

Some data functors can be extended to applicatives:

```haskell
class Data.Applicative f where
  pure :: a -> f a
  (<*>) :: f (a ⊸ b) ⊸ f a ⊸ f b
```

That means that containers of type `f a` can be zipped together. It
also constrains the type of `pure`: I typically need more than one
occurrence of my element to make a container that can be
zipped with something else. Therefore `pure` can't be linear.

As an example, vectors of size 2 are data applicatives:

```haskell
data V2 a = V2 a a

instance Data.Functor f where
  fmap f (V2 x y) = V2 (f x) (f y)

instance Data.Applicative f where
  pure x = V2 x x
  (V2 f g) <*> (V2 x y) = V2 (f x) (g y)
```

Lists would almost work, too, but there is no linear way to zip
together two lists of different sizes. Note: such an instance would correspond to
the `Applicative` instance of `ZipList` in `base`, the `Applicative`
instance for `[]`, in `base` is definitely not linear (left as an
exercise to the reader).

## Control functors

The story takes an interesting turn when considering monads. There is
only one reasonable type for a linear monadic bind:

```haskell
(>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b
```

Any other choice of linearization and you will either get no linear
values at all (if the continuation is given type `a -> m b`), or you
can't use linear values anywhere (if the two other arrows are
non-linear). In short: if you want the do-notation to work, you need
monads to have this precise type.

Now, you may remember that, from `(>>=)` alone, it is possible to
derive `fmap`:

```haskell
fmap :: (a ⊸ b) ⊸ m a ⊸ m b
fmap f x = x >>= (\a -> return (f a))
```

But wait! Something happened here: all the arrows are linear! We've
just discovered a new kind of functor! Rather than containing data,
we see them as
wrapping a result value with an effect. They are _control functors_.

```haskell
class Control.Functor m where
  fmap :: (a ⊸ b) ⊸ m a ⊸ m b

class Control.Applicative m where
  pure :: a ⊸ m a  -- notice how pure is linear, but Data.pure wasn't
  (<*>) :: m (a ⊸ b) ⊸ m a ⊸ m b

class Control.Monad m where
  (>>=) :: (>>=) :: m a ⊸ (a ⊸ m b) ⊸ m b
```

Lists are _not_ one of these. Why? Because you cannot map over a list with
a single use of the function! (Neither is `Maybe` because you may drop
the function altogether, which is not permitted either.)

The prototypical example of a control functor is linear `State`

```haskell
newtype State s a = State (s ⊸ (s, a))

instance Control.Functor (State s) where
  fmap f (State act) = \s -> on2 f (act s)
    where
      on2 :: (a ⊸ b) ⊸ (s, a) ⊸ (s, b)
      on2 g (s, a) = (s, g b)
```

## Conclusion

There you have it. There indeed are two kinds of functors: _data_ and _control_.

- **Data functors** are containers: they contain many values; some are data
  applicatives that let you zip containers together.
- **Control functors**
  contain a single value and are all about effects; some are monads that
  the do-notation can chain.

That is all you need to know. Really.

But if you want to delve deeper,
follow me to the next section because there is, actually, a solid mathematical
foundation behind it all. It involves a branch of category theory
called [enriched category theory][enriched-cat-wiki].

Either way, I hope you enjoyed the post and learned lots. Thanks for reading!

## Appendix: The maths behind it all

Briefly, in a category, you have a collection of objects and sets
of morphisms between them. Then, the game of category theory is to
replace sets in some part of mathematics, by objects in some
category. For example, one can substitute “set” in the definition of
group by topological space ([topological group][topological-group-link]) or by smooth manifold
([Lie group][lie-group-link]).

[Enriched category theory][enriched-cat-wiki] is about playing this game on the definition
of categories itself: a category enriched in \\(\mathcal{C}\\) has a collection of
objects and objects-of-\\(\mathcal{C}\\) of morphisms between them.

For instance, we can consider categories enriched in [abelian groups][abelian-group-link]:
between each pair of objects there is an abelian group of morphisms. In
particular, there is at least one morphism, 0, between each pair of objects. The category
of vector spaces over a given field (and, more generally, of modules over a given ring)
is enriched in abelian groups. Categories enriched in abelian groups are
relevant, for instance, to homology theory.

There is a theorem that all [symmetric monoidal
closed][monoidal-closed-nlab] categories (of which the category of
abelian groups is an example) are enriched in themselves. Therefore,
the category of abelian groups itself is another example of a category
enriched in abelian groups. Crucially for us, the category of types
and linear functions is also symmetric monoidal closed. Hence is
enriched in itself!

Functors can either respect this enrichment (in which case we say that
they are enriched functors) or not. In the category [Hask][hask-link] (seen as a
proxy for the category of sets), this theorem is just saying that all
functors are enriched because “Set-enriched functor” means the same as
“regular functor”. That's why Haskell without linear types doesn't
need a separate enriched functor type class.

In the category of abelian groups, the functor which maps \\(A\\) to
\\(A\otimes A\\) is an example of a functor which is not enriched: the
map from \\(A → B\\) to \\(A\otimes A → B\otimes B\\), which maps \\(f\\) to
\\(f\otimes f\\) is not a group morphism. But the functor from \\(A\\) to
\\(A\oplus A\\) is.

Control functors are the enriched functors of the category of linear
functions, while data functors are the regular functors.

Here's the last bit of insight: why isn't there a `Data.Monad`? The
mathematical notion of a monad does apply perfectly well to data
functors—it just wouldn't be especially useful in Haskell. We need the monad to be
[strong][strong-monad-wiki] for things like the do-notation
to work correctly. But, as it happens, [a strong functor is
the same as an enriched functor][strength-nlab], so data monads aren't
strong. Except in Hask, of course, where data monads and control
monads, being the same, are, in particular, strong.

[functor-link]: https://wiki.haskell.org/Functor
[hask-link]: https://wiki.haskell.org/Hask
[lie-group-link]: https://en.wikipedia.org/wiki/Lie_group
[topological-group-link]: https://en.wikipedia.org/wiki/Topological_group
[abelian-group-link]: https://en.wikipedia.org/wiki/Abelian_group
[linear-types]: https://www.tweag.io/posts/2017-03-13-linear-types.html
[linear-list]: https://www.tweag.io/tag/linear-types.html
[enriched-cat-wiki]: https://en.wikipedia.org/wiki/Enriched_category
[monoidal-closed-nlab]: https://ncatlab.org/nlab/show/closed+monoidal+category
[strong-monad-wiki]: https://en.wikipedia.org/wiki/Strong_monad
[strength-nlab]: https://ncatlab.org/nlab/show/tensorial+strength
