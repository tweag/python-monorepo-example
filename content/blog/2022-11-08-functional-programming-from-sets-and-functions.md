---
title: Functional programming from sets and functions
author: Marco Perone
tags: [haskell]
description: Understanding the basic ideas of functional programming by writing a program using only sets and functions
---

Introductions to functional programming are usually targeted at people who are already accustomed to programming, and they typically present the perks of the paradigm by comparing it to imperative or object-oriented programming. This often leaves the reader with the impression that they have to unlearn practices and concepts they already know to adopt this new paradigm.

In this post I would like to try a different approach, not requiring any programming knowledge, but only the most basic intuition. We will build on high school mathematics, in the form of sets and functions, to provide a learning path to understanding functional programming[^1].

## Introduction

Functional programming is a paradigm for writing computer programs based on, well, you can guess: functions!

These [functions](<https://en.wikipedia.org/wiki/Function_(mathematics)>) are exactly the same ones that are taught in high school.

To recall the definition of a function, and specify the notation and some terminology used in this post, a function $f : X \to Y$ is a [relation](<https://en.wikipedia.org/wiki/Relation_(mathematics)>) between a set $X$ (called the [domain](https://en.wikipedia.org/wiki/Domain_of_a_function) of $f$) and a set $Y$ (called the [codomain](https://en.wikipedia.org/wiki/Codomain) of $f$) such that every $x \in X$ is related to exactly one element in $Y$. We write $f \, x$ to describe this element in $Y$.

In mathematics, sets and functions are used to study the properties of some mathematical object or to prove some theorem. We, however, would like to use them to instruct a computer to perform certain specified operations. Functions, given their nature of associating outputs to inputs, provide a natural model for computations.

Nonetheless, from their definition it is not obvious how functions could be used to write full-fledged programs, implement complex algorithms, and handle interactions with the users and the world.

Let's explore together how we could implement programs using only functions.

## The <span style="text-transform: none">$\mathit{max}$</span> between two integers

As a starting point, let's try to define a program that, given two integers, returns the greatest one.

To implement it as a function, we need to think first about its domain, which describes the possible inputs, and its codomain, describing the possible outputs. Since we want to receive _two_ integers as input, we need to choose the set which contains all the _pairs_ of integers. This set of pairs is called the [Cartesian product](https://en.wikipedia.org/wiki/Cartesian_product) and is denoted, for example, by $\mathbb{Z} \times \mathbb{Z}$. Given that we want a single integer as output, the codomain will be the set of integers.

Hence, the [signature](https://en.wikipedia.org/wiki/Type_signature) of our function will be:

$$
\mathit{max} : \mathbb{Z} \times \mathbb{Z} \to \mathbb{Z}
$$

Next, we would like to implement it along these lines: if we call $x$ and $y$ the two inputs, if $x$ is greater than or equal to $y$ then we output $x$, otherwise we output $y$.
Notice that to describe our implementation, we are referring to "if" and "greater than". And, guess what? They are functions themselves!

$\mathit{if}$ could be seen as a function that receives three arguments: a Boolean (i.e., from the set $\{\mathit{true},\mathit{false}\}$); an integer to be used as output when the Boolean is $\mathit{true}$; and another integer which should be the output when the Boolean is $\mathit{false}$:

$$
\mathit{if} : \mathit{Bool} \times \mathbb{Z} \times \mathbb{Z} \to \mathbb{Z}
$$

Similarly, $\mathit{greater\_than}$ could be seen as a function that receives two integers, and outputs $\mathit{true}$ if the first is greater than or equal to the second, or $\mathit{false}$ otherwise:

$$
\mathit{greater\_than} : \mathbb{Z} \times \mathbb{Z} \to \mathit{Bool}
$$

At this point, it becomes clear that we can implement $\mathit{max}$ by carefully composing these two functions:

$$
\mathit{max} \, (x, y) = \mathit{if} \, (\mathit{greater\_than} \, (x, y), x, y)
$$

## The <span style="text-transform: none">$\mathit{max}$</span> among a set of integers

We are now able to compute the maximum of two integers. It should be easy enough now to generalize this to compute the maximum of an arbitrary non-empty finite set of integers.

First, as always, let's try to think about the domain and the codomain of the function we want to define. Nothing changes for the codomain; it still is just an integer. The domain is now a bit more involved since its elements should be non-empty finite [subsets](https://en.wikipedia.org/wiki/Subset) of $\mathbb{Z}$. Whenever the elements of a set are sets themselves, we can be sure that we need to use the [power set](https://en.wikipedia.org/wiki/Power_set). Since we want only non-empty finite subsets, we restrict ourselves to the set of all non-empty finite subsets of a given set $S$, which we denote by $\mathscr{F} S$.

Hence, what we would like to define is the following function:

$$
\mathit{multi\_max} : \mathscr{F} \mathbb{Z} \to \mathbb{Z}
$$

Let's try to think about the implementation. If you had to do such a computation in your head, you would probably do something like the following:

- If our input set contains just one element, then that element is already the maximum of the set, and we are done.
- On the other hand, if our set contains at least two elements, we could compute their $\mathit{max}$ and keep it in memory.
- Then we take another element of our input set, if it exists, and we compute the $\mathit{max}$ between the value we kept in memory and this new element, and then we replace the value in memory with the result.
- We continue in this fashion until we are left with no other elements to compare. At that point, the value we have in memory is the maximum of the set.

Functions have no memory, though. They can rely only on their inputs to compute their outputs. Therefore, we need to shuffle things around a bit to be able to provide the result of every step as an input for the next one. We can do this by observing that the maximum of a set $X$ with at least two elements, containing an element $x$, is just the $max$ between $x$ and $\mathit{multi\_max} \, (X \setminus \{x\})$, where $X \setminus \{x\}$ is the set $X$ with the element $x$ [removed](<https://en.wikipedia.org/wiki/Complement_(set_theory)#/Relative_complement>):

$$
\begin{array}{lccl}
\mathit{multi\_max} & \{x\} & = & x \\
\mathit{multi\_max} & (\{x\} \cup (X \setminus \{x\})) & = & \mathit{max} \, (x, \mathit{multi\_max}\, (X \setminus \{x\}))
\end{array}
$$

With $A \cup B$ we denote the [union](<https://en.wikipedia.org/wiki/Union_(set_theory)>) of two sets $A$ and $B$, which is the set that contains all the elements from both $A$ and $B$.

This definition is [recursive](https://en.wikipedia.org/wiki/Recursion) since $\mathit{multi\_max}$ is defined in terms of $\mathit{multi\_max}$ itself. This works because we are calling $\mathit{multi\_max}$ on a smaller set and, since $X$ is finite, we can be certain that if we continue to do so, sooner or later we will arrive at a set containing a single element, and the recursion will stop.

Recursion is a typical tool that is often used in functional programming since iterations (in the form of [$\mathit{for}$ loops](https://en.wikipedia.org/wiki/For_loop), for example) need to resort to things that could not be directly implemented as functions, like mutation of an iterator.

## How does execution work?

Suppose we want to compute the maximum of the set $\{5, 2, 7, 4\}$ using our $\mathit{multi\_max}$ function.

The only thing we need to use is the equality provided by the definition of $\mathit{multi\_max}$:

$$
\begin{array}{lcl}
\mathit{multi\_max} \, \{5, 2, 7, 4\} & = & \mathit{max} \, (5, \mathit{multi\_max} \, \{2, 7, 4\}) \\
& = & \mathit{max} \, (5, \mathit{max} \, (2, \mathit{multi\_max} \, \{7, 4\})) \\
& = & \mathit{max} \, (5, \mathit{max} \, (2, \mathit{max} \, (7, \mathit{multi\_max} \, \{4\}))) \\
& = & \mathit{max} \, (5, \mathit{max} \, (2, \mathit{max} \, (7, 4))) \\
& = & \mathit{max} \, (5, \mathit{max} \, (2, 7)) \\
& = & \mathit{max} \, (5, 7) \\
& = & 7
\end{array}
$$

All of these expressions are equivalent to one another. It just so happens that $7$ is the simplest of them.

From this perspective, computing is the same thing as simplifying.

## What if we wanted the <span style="text-transform: none">$\mathit{min}$</span> instead?

Now, for a twist, suppose we want to compute the minimum of a non-empty finite set of integers. We could just copy and paste our definition of $\mathit{multi\_max}$ and replace $\mathit{max}$ with $\mathit{min}$ everywhere:

$$
\begin{array}{lccl}
\mathit{multi\_min} & \{x\} & = & x \\
\mathit{multi\_min} & (\{x\} \cup (X \setminus \{x\})) & = & \mathit{min} (x, \mathit{multi\_min} \, (X \setminus \{x\}))
\end{array}
$$

Et voilà! Done!

However, we might not be extremely happy about this. A lot of the definition is just duplicated. It would be nicer if we could generalize this to remove the duplication.

The main trick to removing duplication when writing in this style is to keep the common parts and extract the different ones as inputs.

In this case, the only difference is in the usage of either the $\mathit{max}$ or the $\mathit{min}$ functions. They are both members of the [set of functions](<https://en.wikipedia.org/wiki/Function_(mathematics)#Set_exponentiation>) from $\mathbb{Z} \times \mathbb{Z}$ to $\mathbb{Z}$, which we denote by $\mathbb{Z}^{\mathbb{Z} \times \mathbb{Z}}$.

Using this insight, we can now define

$$
\mathit{multi} : \mathbb{Z}^{\mathbb{Z} \times \mathbb{Z}} \times \mathscr{F} \mathbb{Z} \to \mathbb{Z}
$$

which takes as its inputs a function to combine two integers and a non-empty finite set of integers, and returns an integer as its result.

Its implementation is similar to our previous $\mathit{multi\_max}$ and $\mathit{multi\_min}$[^2]:

$$
\begin{array}{lccl}
\mathit{multi} & (f, \{x\}) & = & x \\
\mathit{multi} & (f, (\{x\} \cup (X \setminus \{x\}))) & = & f (x, \mathit{multi} \, (f, X \setminus \{x\}))
\end{array}
$$

Now we can redefine $\mathit{multi\_max}$ and $\mathit{multi\_min}$ by using $\mathit{max}$ and $\mathit{min}$, respectively, as the first argument $f$:

$$
\mathit{multi\_min} \, X = \mathit{multi} \, (\mathit{min}, X) \\
\mathit{multi\_max} \, X = \mathit{multi} \, (\mathit{max}, X)
$$

Our newly defined $\mathit{multi}$ is much more general and opens up new possibilities. For example, we could define a function to compute the sum or the product of all the elements in a set just by plugging in $+$ or $*$ as the first argument.

## Conclusion

Programming using only mathematical functions is certainly doable, and it also has many perks, since it allows us to reuse all the knowledge we acquired about functions during our math studies, and build on concepts which are typically learned in high school.

Moreover, functions are by definition deterministic – a characteristic which generally does not hold for [imperative statements](https://www.haskellforall.com/2013/07/statements-vs-expressions.html) – and this allows to keep things more explicit and more easily testable.

Even though the conceptual model is extremely simple, it is nonetheless [as powerful as other programming paradigms could be](https://en.wikipedia.org/wiki/Lambda_calculus#Explanation_and_applications). In particular, it is possible to model with functions every possible interaction with the external world, including [IO actions](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html#t:IO) and [mutable variables](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-IORef.html).

The trick to achieve this consists in appropriately enlarging the domain and the codomain of our functions. Does the function need to access some data? Enlarge its domain to add an input parameter. Does the function need to interact with some external component? Enlarge the codomain to return all the data necessary to describe the required interaction.

Once you have modeled your computation as a function, you can hand it over to a machine which is able to execute it way faster that you could possibly do on your own.

If you have an interesting algorithm (or even a boring one!) to implement, it is always a valuable exercise to think how it could be implemented just in terms of sets and functions. It will help you to understand and to create a more precise mental model of the flow of the data throughout your implementation, possibly uncovering some hidden assumptions you were making which would be better treated explicitly.

[^1]: I am referring to [pure](https://en.wikipedia.org/wiki/Pure_function) functional programming, specifically. Other programming languages exist which are referred to as functional, in which the functions do not necessarily adhere to this kind of mathematical definition.
[^2]: For the attentive reader: since a set has no natural ordering, this definition works only for an associative and commutative binary function `f`. Working with naturally ordered structures as [lists](<https://en.wikipedia.org/wiki/List_(abstract_data_type)>) instead of sets makes the issue disappear.
