---
title: "Denotational Homomorphic Testing"
author: Divesh Otwani
tags: [haskell]
description: "Property test a denotational model of a data structure."
---

Almost a million years ago, I was dealing with some sinister bugs inside the
data structures in [`linear-base`] and to stop the nightmares I decided to just
test the specification of the data structures themselves. I ended up using
something that I've been calling _denotational homomorphic testing_. In this
post, I'll walk through how I ended up with this and why this is legitimately helpful.

## Our Toy Example

Let's consider a toy example which corresponds to one of the data structures I
was working with; see PR [#263][263]. How shall we test a specification of a simple `Set`
implementation shown below?

```haskell
-- Constructors / Modifers
empty :: Set a
insert :: Keyed a => a -> Set a -> Set a
delete :: Keyed a => a -> Set a -> Set a
intersect :: Keyed a => Set a -> Set a -> Set a
union :: Keyed a => Set a -> Set a -> Set a

-- Accessors
member :: Keyed a => a -> Set a -> Bool
size :: Keyed a => Set a -> Int
```

## Specification via Axioms

My first idea was to create a specification for my data structures via axioms,
and then property test those axioms.

I actually got this idea from my introductory computer science course. Our
professor chose to introduce us to data structures by understanding
axioms that functionally specify their behavior. Together, these axioms
specified "simplification" rules that would define the value of the accessors
on an arbitrary instance of that data structure. Think of it as defining rules
to evaluate any well typed expression whose head is an accessor. The idea being
that if you define the interface for an arbitrary _use_ of the data structure
by an external program, then you've defined what you want (functionally) from
the data structure[^2].

In this system, we need at least one axiom for each accessor applied to each
constructor or modifier. If we had $m$ accessors and $n$ constructors or
modifiers, we would need at least $mn$ axioms. For instance, for the accessor
`member`, we would need at least 5 axioms, one for each of the 5 constructors
or modifiers, and these are some of the axioms we would need:

```haskell
-- For all x, y != x,
member empty x == False
member (insert x s) x == True
member (insert x s) y == member s y
```

Intuitively, the axioms provide a way to evaluate any well typed expression starting
with `member` to either `True` or `False`.

With this specification by axioms, I thought I could just property test each axiom
in [`hedgehog`] and provided my samples were large enough and random enough, we'd have
a high degree of confidence in each axiom, and hence in the specification and everyone would
live happily ever after. It was a nice dream while it lasted.

It turns out that there are just too many axioms to test. For instance,
consider the axioms for `size` on `intersection`.

```haskell
size (intersect s1 s2) = ...
```

You would effectively need axioms that perform the intersection on two terms
`s1` and `s2`. A few of simple axioms would include the following.

```haskell
size (intersect empty s2) = 0
-- For x ∉ s1
size (intersect (insert x s1) s2) = boolToInt (member x s2) + size (intersect s1 s2)
```

So, if we can't come up with a specification that's easy to specify, much less
easy to test, what are we to do?

## Specification via Denotational Semantics

I looked at a test written by my colleague Utku Demir and it sparked an idea.
Utku wrote some tests, actually for arrays, that translated operations on
arrays to analogous operations on lists:

```haskell
f <$> fromList xs == fromList (f <$> xs)
fromList :: [a] -> Array a
```

I asked myself whether we could test sets by testing whether they map onto lists
in a sensible way. I didn't understand this at the time, but I was really testing
a semantic function that was itself a specification for `Set`.

### Denotational Semantics

Concretely, consider the following equations.

```haskell
-- toList :: Set a -> [a]
-- toList (1) does not produce repeats and (2) is injective

-- Semantic Function
--------------------

toList empty == []
toList (insert x s) == listInsert x (toList s)
  -- listInsert x l = nub (x : l)
toList (delete x s) == listDelete x (toList s)
  -- listDelete x = filter (/= x)
toList (union s1 s2) == listUnion (toList s1) (toList s2)
  -- listUnion a b = nub (a ++ b)
toList (intersect s1 s2) == listIntersect (toList s1) (toList s2)
  -- listIntersect a b = filter (`elem` a) b

-- Accessor Axioms
------------------

member x s == elem x (toList s)
size s == length (toList s)
```

These effectively model a set as a list with no repeats. **This model is a
specification for `Set`**. In other words, we can say our `Set` implementation
is correct if and only if it corresponds to this model of lists without
repeats.

### Consequences of Denotational Semantics

Why is this so, you ask me? Well, I'll tell you. The first five equations cover
each constructor or modifier and hence fully characterise `toList`. We
say that `toList` is
a **semantic function**. Any expression using `Set` either is a
`Set` or is composed of operations that use the accessors. Thus, any
expression using `Set`s can be converted to one using lists.

Moreover, since
`toList` is injective, we can go back and forth between expressions in
the list model and expressions using `Set`s.
For instance, we can prove that for any sets `s1` and `s2`,
`insert x (union s1 s2) = union (insert x s1) s2`. First we have:

```haskell
toList (insert x (union s1 s2)) =                 -- Function inverses
listInsert x (toList (union s1 s2)) =             -- Semantic function
listInsert x (nub ((toList s1) ++ (toList s2))) = -- Semantic function
nub (listInsert x (toList s1) ++ (toList s2)) =   -- List properties
nub (toList (insert x s1) ++ (toList s2)) =       -- Semantic function
toList (union (insert x s1) s2) =                 -- Semantic function
```

Then by injectivity of `toList` we deduced, as expected:

```haskell
insert x (union s1 s2) =
union (insert x s1) s2
```

This actually means that our denotational specification subsumes
the axiomatic specification that we were reaching for at the beginning
of the post.

Now, there's an interesting remark to make here. Our axiomatic system had axioms
that all began with an accessor. The accessors' denotations
only have `toList` on the right-hand side, removing the need for injectivity
of the semantic function.
For instance, `member (insert s x) x = elem x (toList (insert s x))`
which simplifies to `elem x (nub (x:toList s))` which is clearly `True`.
The injectivity of the semantic function is only needed
for equalities between sets like the one in the proof above.

### Denotational Homomorphic Testing

In summary, we now have an equivalent way to give a specification of `Set` in
terms of laws that we can property test. Namely, we could property test all the
laws that define the semantic function, like `toList empty = []`, and
the accessors axioms, like `member x s = elem x (toList s)`, and this would
test that our implementation of `Set` met our specification. And to boot,
unlike before, we'd only write one property for each constructor, accessor or
modifier. If we had $m$ accessors and $n$ constructors and modifiers, we'd need
to test about $m+n$ laws.

Stated abstractly, we have the following technique. Provide a specification (à
la _denotational semantics_) of a data structure `D` by modeling it by
another simpler data structure `T`. The model consists of a semantic function
`sem :: D -> T` and axioms that correspond constructors on `D` to
constructors of `T` and accessors on `D` to accessors on
`T`, e.g. `acc someD = acc' (sem someD)`. Now, test the specification by
property testing each of the `sem` axioms. These
axioms _look like homomorphisms_ in that operations on `D`s are recursively
translated to related operations on `T`s, which preserves some semblance of
structure (more on this below). If our property testing goes well, we have a
strong way to test the implementation of `D`.

## Related Work

It's critical that I note that providing and testing a specification for data
structures is not at all a novel problem. It has been well studied in a variety
of contexts. Two of the most relevant approaches to this technique are in Conal
Elliot's paper [_Denotational design with type class morphisms_] and the
textbook [_Algebra Driven Design_].

In Elliot's paper, he outlines a design principle: provide denotational semantics
to data structures such that the semantic function is a homomorphism over any
type classes shared by the data structure at hand and the one mapped to.
Applying this to our example, this would be like creating an `IsSet` type
class, having an instance for `Ord a => IsSet [a]` and `Ord a => IsSet (Set a)`, and verifying that if we changed each equation in the former to use
`toList`, it would be the corresponding equation in the latter. So, for example

```haskell
class IsSet s where
  member :: a -> s a -> Bool
  -- ... etc

-- Actual homomorphism property:
--
-- >   member x (toList s) = member x s
--
```

where the property to test is actually a homomorphism.

This has the same underlying structure to our approach and the type class laws
that hold on the list instance of `IsSet` prove the laws hold on the `Set`
instance, analogous to how facts on lists prove facts about `Set`s.

In _Algebra Driven Design_, the approach is similar. First generate axioms on
a reference implementation `RefSet a` (which might be backed by lists) via
[`quickspec`] and test those. Then, property test the homomorphism laws from
`Set` to `RefSet`.

## Concluding Thoughts

To conclude, it seems to me that denotational homomorphic testing is a pretty
cool way to think about and test data structures. It gives us a way to abstract
a complex data structure by making a precise correspondence to a simpler one.
Facts we know about the simpler one prove facts about the complex one, and yet,
the correspondence is itself the specification. Hence proving the
correspondence would be a rigorous formal verification. Falling a few inches
below that, we can property test the correspondence because it's in the form of
a small number of laws and this in turn gives us a high degree of confidence in
our implementation.

[^2]: Think about the batman line: _"It's not who I am underneath, but what I do that defines me"_.

[263]: https://github.com/tweag/linear-base/pull/263
[`hedgehog`]: https://hackage.haskell.org/package/hedgehog
[`quickspec`]: https://hackage.haskell.org/package/quickspec
[`quickcheck`]: https://hackage.haskell.org/package/QuickCheck
[axioms]: https://www.cs.unc.edu/~stotts/COMP410/adt/
[`linear-base`]: https://github.com/tweag/linear-base
[_algebra driven design_]: https://algebradriven.design/#about
[_denotational design with type class morphisms_]: http://conal.net/papers/type-class-morphisms/
