---
title: "Functional Python, Part II: Dial M for Monoid"
shortTitle: Dial M for Monoid

author: Christopher Harrison

description: |
  Commandeering techniques from richly typed, functional languages into Python for fun and profit.

  In this episode: Typeclasses and continuation-passing style.

tags:
  - python
  - programming-languages
  - tutorial
---

Tweagers have an engineering mantra -- Functional. Typed. Immutable. --
that begets composable software which can be reasoned about and avails
itself to static analysis. These are all "good things" for building
robust software, which inevitably lead us to using languages such as
Haskell, OCaml and Rust. However, it would be remiss of us to snub
languages that don't enforce the same disciplines, but are nonetheless
popular choices in industry. Ivory towers are lonely places, after all.

[Last time][blog-fp1] I wrote about how Python's[^et-al] type system
and syntax is now flexible enough to represent and utilise algebraic
data types ergonomically. Here, I'll develop that idea further by way of
a motivating example, for which I shall make use of some functional
programming "tricks" to arrive at an efficient Python implementation.

## Typeclass ABCs

[Typeclasses][wiki-typeclass] are famously a feature from Haskell, which
have since spawned analogous features elsewhere (e.g., "traits" in Rust
and "concepts" in C++20). They allow the definition of "interfaces for
types", where values of types which conform to that specification can
be freely swapped out. This enables what's known as ["ad hoc
polymorphism"][wiki-ahp]... but perhaps an example would be more
illuminating!

It is very common to want to encode and decode some value back and forth
from a serialised representation (e.g., reading and writing to a byte
stream). Say we have some arbitrary Python class, a first approximation
may be to define `encode` and `decode` methods:

```python
from typing import Self

class MyClass:
    ...

    def encode(self) -> bytes:
        # Implementation goes here
        ...

    @classmethod
    def decode(cls, data: bytes) -> Self:
        # Implementation goes here
        ...
```

We could then use this in, say, a function that writes an object's
representation to a file handler:

```python
import io

def write_to_file(fd: io.BufferedWriter, obj: MyClass) -> int:
    return fd.write(obj.encode())
```

A reason for not implementing this as a method of `MyClass` could be
because we may also want to write other types of objects to the file
handler; let's say the `OtherClass` and `YetAnotherThing` classes also
conform to the interface. The naïve approach would be to use a
`typing.Union` type annotation:

```python
def write_to_file(
    fd: io.BufferedWriter,
    obj: MyClass | OtherClass | YetAnotherThing,
) -> int:
```

This will quickly get out of hand!

Instead, we can define an interface -- what Python calls an ["abstract
base class"][py3-abc] -- that all our classes must provide
implementations for, lest they fail at instantiation time. Then we can
use that in the type annotation for our file-writing function:

```python
from abc import ABC, abstractmethod
from typing import Self

class Codec(ABC):
    # NOTE Abstract base classes don't need implementations,
    # just method stubs to define their signatures

    @abstractmethod
    def encode(self) -> bytes: ...

    @classmethod
    @abstractmethod
    def decode(cls, data: bytes) -> Self: ...

# NOTE You may inherit from *many* abstract base classes
class MyClass(Codec):
    def encode(self) -> bytes:
        # Implementation goes here
        ...

    @classmethod
    def encode(cls, data: bytes) -> bytes:
        # Implementation goes here
        ...

def write_to_file(fd: io.BufferedWriter, obj: Codec) -> int:
    return fd.write(obj.encode())
```

We can thus make our code much easier to annotate by defining abstract
base classes that outline related groups of capabilities. Indeed, as
we'll see next, some capabilities are so pervasive, it can be useful to
consider them in their own right.

## Monoidial Soup

In mathematics, a [monoid][wiki-monoid], $(M, \star)$, is a set $M$,
with a binary operation $\star : M\times M \to M$ that is
associative[^associativity] and has an identity element.[^identity] For
non-mathematicians this probably sounds completely opaque, bordering on
absurd. What possible application does this have in software
engineering?

Well, it turns out that this occurs _all_ the time! To offer a few
examples:

- Addition over arbitrary-precision integers, with $0$ as the identity
  element. (Likewise for products, with $1$ as the identity.)

- String concatenation, with the empty string as the identity element.

- [Null coalescing][wiki-nullcoalesce] over any type augmented with a
  null-value, which also serves as the identity element.

- "Any" and "all" predication, over Boolean values, with `False` and
  `True` being the respective identities.

Combining values in such a way is very common and, provided the
definition is satisfied, you have a monoid. Given the frequency at which
monoids naturally occur, it makes sense to define an interface for them.

In Haskell, the [`Monoid` typeclass][haskell-monoid][^semigroup] defines
three functions:

- `mempty`\
  A parameter-less function that returns the monoid's identity element.

- `mappend`\
  A function that defines the monoid's binary operation.

- `mconcat`\
  A convenience for applying the binary operation over a list of values.
  (A default implementation exists, but it can be overridden if there is
  opportunity for optimisation.)

If you [recall our previous discussion][blog-fp1-list] -- in which we
define a `List` type[^py-list] and a right [fold][wiki-fold] over it
-- we can follow Haskell's lead and implement a monoidial abstract base
class in Python as follows:

```python
# This postpones type annotation evaluation until after the source is
# parsed, allowing structural type definitions that can return types of
# themself and references to types that are defined later in the code.
# As of Python 3.11, this is not yet the default.
from __future__ import annotations

from abc import ABC, abstractmethod
from typing import Generic, TypeVar

M = TypeVar("M")

class Monoid(Generic[M], ABC):
    @staticmethod
    @abstractmethod
    def mempty() -> Monoid[M]: ...

    @abstractmethod
    def mappend(self, rhs: Monoid[M]) -> Monoid[M]: ...

    @classmethod
    def mconcat(cls, values: List[Monoid[M]]) -> Monoid[M]:
        folder = lambda x, y: x.mappend(y)
        return foldr(folder, cls.mempty(), values)
```

Guess what? Lists also form a monoid over concatenation, with the empty
list as the identity element. As such, we can update the `List` class to
inherit from our `Monoid` abstract base class and plug in the
implementations:[^circular]

```python
from typing import TypeVar

T = TypeVar("T")

class List(Monoid[T]):
    @staticmethod
    def mempty() -> List[T]:
        return Nil()

    # WARNING mypy has trouble type checking this signature
    def mappend(self, rhs: List[T]) -> List[T]:
        return foldr(Cons, rhs, self)
```

Let's test this out:[^custom-str]

    >>> a = Cons(1, Cons(2, Cons(3, Nil())))
    >>> b = Cons(4, Cons(5, Cons(6, Nil())))
    >>> c = Cons(7, Cons(8, Cons(9, Nil())))

    >>> a.mappend(b)
    Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Nil()))))))

    >>> List.mconcat(Cons(a, Cons(b, Cons(c, Nil()))))
    Cons(1, Cons(2, Cons(3, Cons(4, Cons(5, Cons(6, Cons(7, Cons(8, Cons(9, Nil())))))))))

Perfect!

## What the Thunk?

Now allow me to take you on a little digression. It'll be worth it, I
promise.

Consider a textbook recursive function. I'll ignore type hints for now,
so you can focus on what's going on:

```python
def factorial(n):
    if n == 0:
        return 1
    else:
        return n * factorial(n - 1)
```

This executes as follows:

    factorial(3)
    → 3 * factorial(2)
    → 3 * (2 * factorial(1))
    → 3 * (2 * (1 * factorial(0)))
    → 3 * (2 * (1 * 1))

The problem with this is that each recursion increases the depth of the
call stack. In Python, by default, the call stack is limited to a depth
of 1,000. We can [increase that][py3-setreclimit], but only insofar as
the machine's memory allows.

    >>> factorial(4)
    24

    >>> factorial(1000)
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      File "<stdin>", line 5, in factorial
      File "<stdin>", line 5, in factorial
      File "<stdin>", line 5, in factorial
      [Previous line repeated 995 more times]
      File "<stdin>", line 2, in factorial
    RecursionError: maximum recursion depth exceeded in comparison

Let's rewrite our function in so-called [continuation-passing
style][wiki-cps], wherein the function takes an additional argument
`k`,[^continuation-k] which represents the "continuation of the
computation". This is represented as a single-valued function and we
return it _embedded_ into the next step of the sequence:

```python
def factorial(n, k):
    if n == 0:
        return k(1)
    else:
        return factorial(n - 1, lambda x: k(n * x))
```

Now the execution is like this:

    factorial(3, k)
    → factorial(2, lambda x: k(3 * x))
    → factorial(1, lambda x: k(3 * (2 * x)))
    → factorial(0, lambda x: k(3 * (2 * (1 * x)))
    → k(3 * (2 * (1 * 1)))

We can retrieve the result from the continuation by setting `k` to be
the identity function (`lambda x: x`). However, we see that this still
fills up the stack. We can even make this explicit by adding some
debugging output to our identity function:

```python
import traceback

def identity_with_callstack_depth(x):
    print(f"Stack Depth: {len(traceback.extract_stack())}")
    return x
```

Now:

    >>> factorial(3, identity_with_callstack_depth)
    Stack Depth: 9
    6

    >>> factorial(123, identity_with_callstack_depth)
    Stack Depth: 249
    << Number with 206 digits >>

    >>> factorial(1000, identity_with_callstack_depth)
    Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      File "<stdin>", line 5, in factorial
      File "<stdin>", line 5, in factorial
      File "<stdin>", line 5, in factorial
      [Previous line repeated 995 more times]
      File "<stdin>", line 2, in factorial
    RecursionError: maximum recursion depth exceeded in comparison

The problem is that Python is [strictly evaluated][blog-strict-v-lazy]
and so will still perform function evaluation on every recursion. So
what if we didn't give it functions to evaluate? What if instead of
evaluating functions whenever we see them, we wrap them up in a
[thunk][wiki-thunk]; that is, a "to-be-evaluated function"? That sounds
complicated, but all it involves in Python is an argument-less lambda
function, or `functools.partial`:

```python
def factorial(n, k):
    if n == 0:
        return k(1)
    else:
        # Equivalently: partial(factorial, n - 1, lambda x: partial(k, n * x))
        return lambda: factorial(n - 1, lambda x: lambda: k(n * x))
```

So we return a thunk and the continuation returns a thunk. By doing so,
the execution now looks like this:

    factorial(3, k)
    → lambda: factorial(2, lambda x: lambda: k(3 * x))

That's where it stops. There's no recursion. What we get in return is a
thunk that represents the next sequence of the iteration, the length of
which will be twice the number of original recursive steps as we
ping-pong between the thunk and its evaluation. Following from the
above, you can convince yourself that the following execution holds:

    factorial(3, k)()
    → factorial(2, lambda x: lambda: k(3 * x))

    factorial(3, k)()()
    → lambda: factorial(1, lambda x: lambda: k(3 * (2 * x)))

    factorial(3, k)()()()
    → factorial(1, lambda x: lambda: k(3 * (2 * x)))

    factorial(3, k)()()()()
    → lambda: factorial(0, lambda x: lambda: k(3 * (2 * (1 * x))))

    factorial(3, k)()()()()()
    → factorial(0, lambda x: lambda: k(3 * (2 * (1 * x))))

    factorial(3, k)()()()()()()
    → k(3 * (2 * (1 * 1)))

This can be unrolled deterministically with a
[trampoline][wiki-trampoline],[^trampoline-gotcha] which is just a
simple loop:

```python
def trampoline(thunk):
    while callable(thunk): thunk = thunk()
    return thunk
```

A loop will not blow up the stack:

    >>> trampoline(factorial(6, identity_with_callstack_depth))
    Stack Depth: 3
    720

    >>> trampoline(factorial(1000, identity_with_callstack_depth))
    Stack Depth: 3
    << Number with 2,568 digits >>

What we've achieved by doing this transformation is to implement
[tail-call][wiki-tailcall] optimisation in Python; a language whose
reference implementation [does not support][gvr-2009] TCO. This allows
us to implement recursive functions _without_ $O(n)$ space usage as we
fill up the stack, while maintaining the same asymptotic time complexity
as the original recursive version.

## Origami

Now let's jump back (pun intended) to folds. We can see that our
recursive right fold will grow the stack linearly with respect to its
depth, as any recursive function in Python will. As I've shown, this can
be avoided by rewriting the function in continuation-passing style and
using thunks to delay evaluation.

The conversion is quite a mechanical process:

```python
def foldr(fn, acc, lst, k):
    match lst:
        case Nil():
            return k(acc)

        case Cons(x, xs):
            return lambda: foldr(fn, acc, xs, lambda v: lambda: k(fn(x, v)))
```

There we have it: an efficient right fold, which can be unrolled with a
trampoline! That said, at this point it behooves me to point out that
this technique is not always necessary and should be used judiciously.
For example, a strictly evaluated left fold can easily be rewritten from
a recursive version into a simple loop with an accumulator; no
continuation-passing required.

Anyway, let's see our right fold in action:

    >>> trampoline(foldr(lambda x, y: x * y, 1, [1, 2, 3], identity_with_callstack_depth))
    Stack Depth: 4
    6

    >>> trampoline(foldr(lambda x, y: x * y, 1, range(1, 1001), identity_with_callstack_depth))
    Stack Depth: 4
    << Number with 2,568 digits >>

It remains an exercise for the reader to supplant this efficient fold
implementation into our monoid abstract base class and `List`.

### What About Types?

I've deliberately avoided adding type hints in the above discussion for
clarity's sake. (You're welcome.) However, our code should be annotated.
We already know the signature for recursive `foldr`, where `S` and `T`
are [type variables][wiki-typevar]:

```python
def foldr(fn: Callable[[S, T], T], acc: T, lst: List[S]) -> T: ...
```

The continuation-passing style version returns a thunk that will
ultimately return a `T`; let's call this type `Thunk[T]`. The
continuation itself is a single-valued function that takes a `T` and
returns a thunk that returns a `T`; that is, `Callable[[T], Thunk[T]]`.
So how should we define the `Thunk` type?

The definition of a thunk that we're using is a zero-valued function
that returns either another thunk or a value of some type. Ideally,
therefore, we'd like to write something like this:

```python
Thunk[T] = Callable[[], Thunk[T] | T]
```

The problem is, we can't; this is not valid Python syntax. Generics --
to account for the type variable -- are not used like that, but the
bigger problem is that mypy, for example, does not currently support
this kind of recursive type definition. To get around both of these
problems, we have to write a recursive callback
[protocol][mypy-protocols] type:

```python
from __future__ import annotations

from typing import TypeVar, Protocol

# The type variable has to be marked as covariant to type check
T_co = T.TypeVar("T_co", covariant=True)

class Thunk(T.Protocol[T_co]):
    def __call__(self) -> Thunk[T_co] | T_co: ...
```

We have to be explicit about the [covariance][wiki-variance] of the type
variables, which are expected to take a `_co` suffix. So putting this
altogether gives the following signatures for our continuation-passing
style version of `foldr` and `trampoline`:

```python
def foldr(
    fn: Callable[[S, T_co], T_co],
    acc: T_co,
    lst: List[S],
    k: Callable[[T_co], Thunk[T_co]]
) -> Thunk[T_co]: ...

def trampoline(thunk: Thunk[T_co]) -> T_co: ...
```

Phew!

## Conclusion

Continuing our theme of building software from composable parts, the
concept of typeclasses, from Haskell, can be simulated in Python using
abstract base classes. This allows classes to be categorised by their
capabilities, simply by virtue of inheritance, enabling you to build
more generic functions that utilise your object models, without
upsetting the type checker.

One such typeclass is the "monoid", which is a pattern that is regularly
seen in day-to-day software engineering. By abstracting this interface
with a typeclass, we can ensure consistency amongst all our monoid
implementations and arrive at familiar patterns throughout our code.
This is a boon for reasoning about the software we write.

Finally, I have shown how using continuation-passing style can improve
the performance of traditional functional programming patterns in a more
imperative and strictly evaluated context. While the indirection breeds
a certain complexity -- and it is not something that needs to be done on
the regular -- this has its place in, for example, the depths of library
code, where performance matters.

In the next and final episode, I'll cover testing strategies that can be
learnt from functional programming and applied to Python.

_Thanks to Connor Baker, Guillaume Desforges, Johann Eicher, Johan
Herland and Maria Knorps for their reviews of this article._

<!-- Footnotes -->

[^et-al]:
    We are not limited to Python; these techniques can be applied in any
    language with suitable support, libraries and tooling.

[^associativity]: For all $a,b,c \in M$, $(a\star b)\star c = a\star(b\star c)$.
[^identity]:
    There exists an $e\in M$ such that, for all $a\in M$,
    $e\star a = a\star e = a$.

[^semigroup]:
    In Haskell, a monoid is actually defined in terms of a
    [semigroup][wiki-semigroup]. This is a further abstraction that we
    could also implement in Python as an abstract base class, leveraging
    object inheritance in our monoid ABC.

[^py-list]:
    We can do the same with regular Python lists, which would be more
    efficient, but we use our definition for consistency (and
    dog-fooding purposes!)

[^circular]:
    The type annotations are circular, here: `Monoid.mconcat` is
    dependent on `List`, which is dependent on `Monoid`. Enabling
    [postponed type annotation evaluation][pep-563] resolves this.
    However, I _suspect_ this may be the reason why mypy cannot (as of
    writing) validate the annotation for `List.mappend`.

[^custom-str]:
    I have not documented the custom `__str__` implementations for `Nil`
    and `Cons`, which render this output, for sake of brevity.

[^continuation-k]:
    Conventionally, the continuation is denoted by the letter `k`. I
    cannot find from where this convention originated; even when the
    concept was [introduced][aim-349], by Sussman and Steele, they used
    the letter `C`.

[^trampoline-gotcha]:
    Care must be taken with a trampoline when you want to return a
    function. In our case, it won't work -- a function is callable and
    so the trampoline will try to evaluate it -- but this can be
    resolved easily by wrapping it in some kind of container type, for
    example.

<!-- Links -->

[aim-349]: ftp://publications.ai.mit.edu/ai-publications/pdf/AIM-349.pdf
[blog-fp1-list]: ../2022-09-08-fp1-typopaedia-pythonica#recursive-types-and-pattern-matching
[blog-fp1]: ../2022-09-08-fp1-typopaedia-pythonica
[blog-strict-v-lazy]: ../2022-05-12-strict-vs-lazy
[gvr-2009]: https://neopythonic.blogspot.com/2009/04/final-words-on-tail-calls.html
[haskell-monoid]: https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-Monoid.html
[mypy-protocols]: https://mypy.readthedocs.io/en/stable/protocols.html
[pep-563]: https://peps.python.org/pep-0563
[py3-abc]: https://docs.python.org/3/library/abc.html
[py3-setreclimit]: https://docs.python.org/3/library/sys.html#sys.setrecursionlimit
[wiki-ahp]: https://en.wikipedia.org/wiki/Ad_hoc_polymorphism
[wiki-cps]: https://en.wikipedia.org/wiki/Continuation-passing_style
[wiki-fold]: https://en.wikipedia.org/wiki/Fold_(higher-order_function)
[wiki-monoid]: https://en.wikipedia.org/wiki/Monoid
[wiki-nullcoalesce]: https://en.wikipedia.org/wiki/Null_coalescing_operator
[wiki-semigroup]: https://en.wikipedia.org/wiki/Semigroup
[wiki-tailcall]: https://en.wikipedia.org/wiki/Tail_call
[wiki-thunk]: https://en.wikipedia.org/wiki/Thunk
[wiki-trampoline]: https://en.wikipedia.org/wiki/Trampoline_(computing)
[wiki-typeclass]: https://en.wikipedia.org/wiki/Type_class
[wiki-typevar]: https://en.wikipedia.org/wiki/Type_variable
[wiki-variance]: https://en.wikipedia.org/wiki/Covariance_and_contravariance_(computer_science)
