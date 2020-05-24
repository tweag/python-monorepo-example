---
title: "On linear types and <br/>exceptions"
shortTitle: "On linear types and exceptions"
author: Arnaud Spiwack
tags: haskell, linear-types
description: "A primer on the interaction between linear types and exceptions in Haskell."
---

Haskell has exceptions. Therefore, any design for linear types in
Haskell will have to deal with exceptions. It might seem
impossible to square with this requirement:
how can linear types, which require that values be used
exactly once, accommodate exceptions, which
interrupt my computation?

The mantra to remember, to dispel this feeling, is that “a function
`f` is linear when: _if_ its result is consumed exactly once, _then_ its
argument is consumed exactly once”. If the result is consumed multiple
times, then the argument is consumed multiple times, if the result is
consumed only partially (_e.g._ because an exception was thrown), then
the argument may be consumed only partially (including not at all).

This may, at first, read as a carefully worded misdirection. But I
assure you that it isn't. This conditional statement was already a
property of [linear logic][linear-logic] when it was introduced
in 1987. And it is indeed one of the key intuitions for how [Linear
Haskell][linear-haskell-paper] interacts with exceptions.

## Back to basics with monads

The design of Linear Haskell was deliberately chosen to closely follow linear
logic. This is due to an observation called the
[Curry-Howard correspondence][curry-howard], by which types in
some programming languages can be read like propositions in logic.
This observation
has served programming language design well, and is in fact one of the
original design principles of Haskell (by the way, did you know what
[Curry's][curry-wiki] first name was?). Haskell corresponds to
intuitionistic logic, while Linear Haskell corresponds to
linear logic.

If we were to design exceptions for a linearly typed language from
first principles, we would start from linear logic and add exceptions on
top. The methodology to do so was given to us by [Eugenio
Moggi][moggi-wiki]: apply a monad. Indeed neither intuitionistic logic
nor linear logic have a native notion of exceptions. In fact, when
viewed as programming languages using the Curry-Howard correspondence,
they only have terminating computations. But
Moggi showed how to use monads to model effectful computations.

This has since become routine in Haskell programming, and you probably
guessed where this is going: the simplest model of exception, for
intuitionistic logic, is the `Maybe` monad.

However, in linear logic, there are several refinements of the `Maybe`
type. Which one models exceptions? Interestingly enough, not the one
called `Maybe` in Linear Haskell (`_⊕1` in linear logic). In fact, if
you read my [previous blog post][data-vs-control], you will
know already that it isn't the right kind of monad to apply here. Instead we
need to use the `_⊕⊤` monad from linear logic, where `⊤` can be
defined in Linear Haskell as

```haskell
newtype Top = Top (forall b. Void #-> b)
```

The difference between `Top` and `()` (aka `1` in the linear logic
literature) is that there is a *linear* function `a #-> Top` for every
type `a`:

```haskell
swallow :: a #-> Top
swallow x = Top $ \v -> case v of {}
```

But, `swallow` doesn't use its argument! How can it be a linear
function? Remember the mantra: “a function `f` is linear when _if_ its
result is consumed exactly one, _then_ its argument is consumed
exactly once”. The trick is that there is no way, in linear logic, to
consume a value of type `⊤` exactly once. Indeed neither in
Linear Haskell is there a way to consume a value of type `Top` exactly once
(since there is no way to apply the wrapped function exactly once).
So, vacuously, `swallow` will consume its argument exactly once when
its result is consumed exactly once.

We can use the type `Either Top a` to model in Haskell potentially
failing computations that return a value of type `a`.
Using `swallow`, we can write a computation that errors out,
ignoring all the linearity requirements which we would otherwise need to honour.
Since we can't consume values of type `Top` exactly once,
we can't `catch` exceptions in a linear
computation. We will need an unrestricted computation instead:

```haskell
catch :: Either Top a -> a -> a
catch (Left _) handler = handler
catch (Right x) _ = x
```

So the Curry-Howard correspondence, the bridge between logic and programming,
compels us to have a `catch`
without linear arguments. Intuitively, this prevents linear variables
from escaping outside of the `catch`.

## Resource management

Now that we know how to model exceptions in linear functions,
let us turn to
how exceptions interact with applications of linear types. More
specifically, how do exceptions interact with resource management?

Resource management is one of the original motivations for Linear
Haskell. In a recent [blog post][linear-inline-java], we described,
for instance, how we use linear types to manage references across two
different garbage collectors to avoid memory leaks in
[inline-java][inline-java] applications.

The point of linear types for resource management is that types
force us to call the `release` function on our resources to free them,
allowing for
precise, yet safe, management of the resource. We just can't forget to
call `release`.
But it may seem that
exceptions throw a wrench in this plan: since we can interrupt the
computation at any time, we can entirely bypass the call to `release`.

However, Linear Haskell doesn't have any notion of resource or
resource management. Linear Haskell is only a type system, and doesn't
extend the compiler with new concepts. Quite the contrary: the
philosophy of Linear Haskell is to empower the programmer to add new
abstractions _in user space_ (i.e. without requiring additional
compiler support).

So the question isn't, does Linear Haskell correctly manage resources,
but instead: is it possible to write a resource management
abstraction in Linear Haskell? And to this, the answer is an emphatic
_yes_.

I can make such an unabashed claim because, quite simply, [I wrote
one][resource-io]. The high-level view is:

- there is *the resource monad* `RIO` in which resources are managed,
- each resource type has acquire and release functions,
- resources are linear values,
- the function `run :: RIO (Unrestricted a) -> IO a` (notice the
  unrestricted arrow), is responsible for
  itself releasing all remaining resources if an exception occurs.

This way, resource management is entirely under the programmer's control:
resources are released when the appropriate function is called. The
programmer can't forget to release a resource, or use it after
releasing, since it is prevented by the type system. If an exception
occurs, then all the resources are cleaned immediately.

To reiterate, saying that linear types are exception-safe, or on the
contrary exception-oblivious, doesn't make sense. Whether system
resources are always released in a timely manner even in the face of
exceptions is a property of the abstraction you create using linear
types to enable programmers to have full but safe control over
resources. The resource monad is one such abstraction, just like the
`Either Top` monad is an abstraction, with different properties, for
modeling potentially failing computations. The same is true of
virtually any other type system feature like higher-ranked types,
GADTs, dependent types: they are tools to build abstractions with
desirable properties.

## Thoughts about affine types

It is tempting to say that since computations can be interrupted by
exceptions, this system is an affine type system. This is
misleading. An affine function is such that if its result is used
_exactly once_ then its argument is used _at most once_.

It's a very different system. For instance, for resource management,
there is no guarantee that a resource is released before the entire
computation has ended. We could be waiting a long time. It's harder to
create an abstraction that provides the same properties as above.

There is indeed some connection between affine types and
exceptions. Most notably, `catch` can be affine in an affine type
system.

But wrapping linear logic in the `Either Top` monad doesn't make it
affine. In fact, affinity is not a monadic effect: it's a _comonadic
coeffect_. But this is a story for another time. If you grab me
over tea, you can easily get me to talk about it.

## Conclusion

I hope to have convinced you that the interaction of linearity and
exceptions in Linear Haskell, as it is currently designed, is not only
reasonable: it is natural and necessary.

It doesn't mean that exceptions are free: when writing a new linear
abstraction using unsafe functions (such as the FFI), it is your
responsibility to ensure that the functions you write are indeed linear—just as
when using `unsafePerformIO`, you need to make sure that the
computation really is pure. And when doing so, you need to be mindful
of exceptions, which can complicate the implementation. But Haskell has
exceptions, and this complication is unavoidable.

[linear-logic]: https://en.wikipedia.org/wiki/Linear_logic
[linear-haskell-paper]: https://arxiv.org/abs/1710.09756
[curry-howard]: https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence
[curry-wiki]: https://en.wikipedia.org/wiki/Haskell_Curry
[moggi-wiki]: https://en.wikipedia.org/wiki/Eugenio_Moggi
[data-vs-control]: https://www.tweag.io/posts/2020-01-16-data-vs-control.html
[linear-inline-java]: https://www.tweag.io/posts/2020-02-06-safe-inline-java.html
[inline-java]: https://github.com/tweag/inline-java
[resource-io]: https://github.com/tweag/linear-base/blob/0b6a6c59491c854581027df160b1703af673deef/src/System/IO/Resource.hs
