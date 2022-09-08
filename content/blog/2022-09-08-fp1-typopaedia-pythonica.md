---
title: "Functional Python, Part I: Typopædia Pythonica"
shortTitle: Typopædia Pythonica

author: Christopher Harrison

description: |
  Commandeering techniques from richly typed, functional languages into
  Python for fun and profit.

  In this episode: Gradual typing and algebraic data types.

tags:
  - python
  - programming-languages
  - tutorial
---

Tweagers have an engineering mantra -- Functional. Typed. Immutable. --
that begets composable software which can be reasoned about and avails
itself to static analysis. These are all "good things" for helping us
build robust software, which inevitably lead us to using languages such
as Haskell, OCaml and Rust. However, it would be remiss of us to snub
languages that don't enforce the same disciplines, but are nonetheless
popular choices in industry. Ivory towers are lonely places, after all.

In this series of articles, I will discuss how some of the principles
and techniques learnt from typed functional languages can be applied to
Python[^1] to achieve the same results. We'll see that, while Python
[famously eschews][gvr-2005] the functional paradigm, we can build
pythonic(ish) equivalents relatively easily.

First up, I'll look at typing discipline in Python.

## Gradual Typing

Quickly. Tell me the type signature of the following function:

```python
def mystery(a, b):
    return a / b
```

You may be tempted to say that `a` and `b` are numeric... and that's
probably true. However, Python is dynamically typed, so without further
documentation, the _only_ thing we can actually say for sure is that `a`
must implement the [`__truediv__` interface][py3-truediv] _at runtime_
(or `b` implements `__rtruediv__`). For instance:

```python
>>> mystery(pathlib.Path("/foo/bar"), "quux")
PosixPath("/foo/bar/quux")
```

This is a fairly well-behaved (if not contrived) example. However, I'm
sure you can relate to the shenanigans that some Python engineers get up
to; constructing opaque dictionaries where we're lucky if we know the
keys without mentally running through the code in our heads.
Impenetrable. Unmaintainable. Chaos.

Fortunately for our (and future maintainers') sanity, since Python 3.5,
syntax for [type hinting][pep-484] has been introduced and continues to
improve in each subsequent release. We can thus annotate our function as
a form of standardised documentation:

```python
def not_so_mysterious(a: float, b: float) -> float:
    return a / b
```

Python is _still_ dynamically typed; these are just annotations and you
are free to flaunt them with reckless abandon. Presuming, however, that
you're an upstanding citizen, this is an approach known as "gradual
typing", which is a pragmatic solution to allow engineers to annotate
their code piecemeal, without the friction that comes from the
expectations of "all-or-nothing".

So what's the point, if it's transparent to the Python interpreter?

Well, type checkers exist, such as:

- [mypy][mypy], from the Python Software Foundation
- [Pyright][pyright], from Microsoft
- [Pyre][pyre], from Meta
- [pytype][pytype], from Google

Any of these can (and should) be incorporated into an engineer's setup,
or integrated into a project in much the same way as formatters, linters
and test runners. The more you annotate your code, the more leverage you
gain; type checkers can infer the type of code that isn't annotated when
they have enough information. Having your editor scream at you, or your
CI fail if your code doesn't type check is an easy win in the fight
against bugs.

Now, much ink has been spilt on the merits of typing -- and I would
encourage you to read [Python's][py3-typing] and [mypy's][mypy-docs]
documentation -- so I won't go into any more detail about the specifics.
Instead, we'll see how we can use this to implement a keystone of
functional programming languages while reassuring ourselves of its
correctness.

## Algebraic Data Types

Algebraic data types are structures formed by composition in
predictable, well-defined ways.[^2] Moreover, they can be nested,
allowing you to build up complex data structures from well-understood
parts. Why is this useful? While the concept may seem abstract,
modelling everyday data structures -- both algorithmic and for business
logic -- in this way turns out to be rather natural, where the same
arguments that favour modular code, which can be put together into a
more-meaningful whole, apply.

Data structures are at the heart of the software we write. For
functional programmers, it's common to start implementing new
functionality by thinking about what types will be needed and how they
will interact with each other.

With that in mind, rather than waxing poetic, let's get down to the
business of applying the same practice to our Python code. I'll start
with "product types" as they're simpler, both conceptually and in terms
of their Python implementation.

### Product Types

A point on a plane can be represented, for example, by its $x$ and $y$
coordinates relative to some origin. These two values can range
independently to cover the entire plane. This is known as the [Cartesian
product][wiki-cartesian], which directly corresponds to a "product
type". In the plane example, this would be expressed by its two numeric
components. However, nothing precludes you from having more components,
fewer components, or even heterogeneous components.

At its simplest, this can be represented by the juxtaposition of (zero
or more) types. In Python, this is a tuple, which is straightforwardly
annotated[^3] by its component types. For example:

```python
point: tuple[float, float] = (3, 4)

sql_statement: tuple[str, tuple] = (
    "select * from foo where bar > ? and quux = ?",
    (0, True)
)
```

Furthermore, tuples are immutable in Python, which ensures that
unexpected changes -- that can cause bugs and are hard to track down --
cannot happen. Less favourably, however, is that their component access
is positional, which elides useful information from the engineer. This
can lead to meaningless code:

```python
do_something(point[0])

# Destructuring helps, but is at best just a proxy
stmt, params = sql_statement
```

To resolve this, the fields can be named; a so-called "record type", or
a `struct` in C and Rust. Before Python 3.7, a record type could be
implemented using a [named tuple][py3-named-tuple]; which gained type
annotation support in Python 3.6. Since Python 3.7, [data
classes][pep-557][^4] were introduced which supplant this role. At first
blush, the two may seem equivalent, but an important distinction -- that
will become necessary in the next episode -- is that named tuples don't
support multiple inheritance.

The implementation is facilitated through a class
[decorator][py3-decorator], which computes the type's interface (e.g.,
constructor, equality matching, etc.) at runtime. It therefore offers a
lot of flexibility, such as specifying default values and setting
immutability; instantiation is like a normal Python class and field
access is through the familiar "dotted-attribute" pattern:

```python
from dataclasses import dataclass

@dataclass(frozen=True)
class ProgrammingLanguage:
    name: str
    strongly_typed: bool
    statically_typed: bool

haskell = ProgrammingLanguage("Haskell", True, True)
python = ProgrammingLanguage("Python", True, False)

assert haskell.statically_typed != python.statically_typed
```

In this example, I use the anti-pattern of Boolean arguments, which
gives no clue as to what each argument refers. From Python 3.10 this
situation can be improved slightly -- at the expense of verbosity -- by
forcing the use of keyword arguments. However, perhaps there is a better
way...

### Sum Types

Sum types can express a variety of types, but only one at a time. The
most common example of this is probably a Boolean: it's either `True` or
`False`. That can be extended to an arbitrary number of simple branches
with an [`Enum`][py3-enum], introduced in [Python 3.4][pep-435]. While
somewhat pointless[^5] beyond its illustrative power, we could thus
amend our previous example like so:

```python
from dataclasses import dataclass
from enum import Enum, auto

class TypingDiscipline(Enum):
    StrongStatic = auto()
    StrongDynamic = auto()
    WeakStatic = auto()
    WeakDynamic = auto()

@dataclass(frozen=True)
class ProgrammingLanguage:
    name: str
    type_discipline: TypingDiscipline

javascript = ProgrammingLanguage("JavaScript", TypingDiscipline.WeakDynamic)
```

While this is now clearer, it has limited utility. What _would_ be
useful is a mixture of branch types, where individual components can
carry information besides their name. For example, say we wanted a
`Shape` type which encoded different metrics for different types of
shape (e.g., side lengths for rectangles, radius for circles, etc.).
That can't be done with an `Enum`.

In object orientated programming languages, like Python, this pattern is
a class hierarchy: a root superclass, with any number of child classes
representing the branches. From Python 3.8, we can also make use of the
`typing.final` decorator to get the type checker involved in enforcing
the correct structure. As for carrying data, we've already covered that
with product types:

```python
from dataclasses import dataclass
from typing import final

class Shape: ...

@final
@dataclass(frozen=True)
class Rectangle(Shape):
    width: float
    height: float

@final
@dataclass(frozen=True)
class Circle(Shape):
    radius: float
```

This is approximately equivalent to a true sum type that can be defined
in functional languages. We've constructed it from smaller,
well-understood types to enable us to express data that is more than the
sum of its parts (pun intended). However, there are some caveats which
must be observed:

- The `Shape` parent class is a bona fide type. This is perfect for type
  annotating, but care should be taken not to mistakenly instantiate it.
  Conversely, the subclasses are also bona fide types, but they're less
  useful for type annotation, yet _should_ be instantiated.

- Nothing prevents further subclassing of `Rectangle` or `Circle`
  besides the `final` decorator, which would only fail the type checker.
  Deeper subclassing is meaningless with respect to the simulated sum
  type, but will still type check against the root class.

The instantiation and subclassing problems _could_ be enforced with some
[metaclass trickery][gist-sumtype-metaclass], but this does more to
paint you into a corner than provide value. Not recommended. Instead,
this is where the judgement of the engineer comes in to play.

### Recursive Types and Pattern Matching

When learning Haskell, the [classic algebraic data type
example][lyah-list] is a cons-list: a homogeneous, singly-linked list
made up of [cons][wiki-cons] cells. This can be expressed as:

```haskell
data List a = Cons a (List a) | Nil
```

Following the protocol I've outlined above, this can be translated
mechanically into Python:

```python
from typing import Generic, TypeVar, final
from dataclasses import dataclass

T = TypeVar("T")

class List(Generic[T]): ...

@final
@dataclass(frozen=True)
class Cons(List[T]):
    car: T
    cdr: List[T]

@final
class Nil(List): ...
```

Granted, it's not one line of code any more and it may look a bit alien,
as far as Python code goes, but it is readable. It's also worth
emphasising that this is again more illustrative than useful: Don't
implement your Python lists like this! So let's get on and illustrate by
defining some linked lists and showing how the type checker reacts:

```python
# The type checker approves of this
correct: List[int] = Cons(1, Cons(2, Cons(3, Nil())))

# The type checker doesn't like these
# mypy: Argument 1 to "Cons" has incompatible type "str"; expected "int"
mistyped: List[int] = Cons("foo", Nil())
hetero: List[int] = Cons(1, Cons("2", Nil()))
```

"This seems like a lot of effort to go to," I hear you cry. "Can we do
anything cool with this?"

The great thing about structures that follow a predictable pattern is
that they can be deconstructed in a predictable way. You see this all
the time in idiomatic Python code, with destructuring of tuples and
dictionaries to get at their parts. With our sum and product types, let
me give an example which allows you to go one step further:

```python
from typing import Callable, TypeVar

S = TypeVar("S")
T = TypeVar("T")

def foldr(fn: Callable[[S, T], T], acc: T, lst: List[S]) -> T:
    match lst:
        case Nil():
            return acc

        case Cons(x, xs):
            return fn(x, foldr(fn, acc, xs))
```

[Structural pattern matching][pep-634] landed in Python 3.10 and this is
where algebraic data types really shine. It allows us to leverage the
structural patterns that are naturally embedded within them without
having to write a lot of tiresome boilerplate. The above is a recursive
(strictly evaluated) implementation of a right [fold][wiki-fold],[^6]
which is about as close to a perfect translation of the [Haskell
equivalent][lyah-folds] as you can get:

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _  acc []     = acc
foldr fn acc (x:xs) = fn x (foldr fn acc xs)
```

Let's give it a spin:

```python
>>> foldr(lambda x, y: x + y, 0, Cons(1, Cons(2, Cons(3, Nil()))))
6

>>> foldr(lambda x, y: f"{x}{y}", "", Cons("Hello", Cons("World", Nil())))
'HelloWorld'
```

In functional programming languages, many such abstractions exist --
so-called ["higher-order functions"][wiki-hof] -- and are used as
component building blocks to compose useful software. As we can reason
about each component, we can reason about their composition. Functional
programming is all about composition and now we can do the same in our
Python code.

Of course, not all is sunshine and rainbows. There are some caveats:

- As mentioned previously, the root class (`List`, here) is a genuine
  type. The type checker realises that the `match` clause is therefore
  inexhaustive and so it (correctly) concludes that there's a non-return
  path through the function; which is a type error. It's not as pretty,
  but we can fix this easily by adding a dummy `match` branch that is
  never taken.

- Pythonistas may baulk at the recursion... and for good reason: Python
  does not do [tail-call][wiki-tailcall] optimisation -- again, by
  [decree][gvr-2009] -- and so deep recursion can blow the stack. In the
  next episode, I'll show how we can resolve this.

## Conclusion

Functional programming takes many of its cues from mathematics, where
larger, more-specific structures (programs) are built from smaller,
more-general ones. You start from the bottom and work your way up. This
ensures the correctness at each level of abstraction and is a robust
philosophy for writing well-behaved software.

I have shown that the axiomatic level -- types -- can now be imitated in
the Python ecosystem to achieve the same end. In the next episode, I'll
generalise this further and implement some nifty computer science
concepts to assuage Python's "mechanical sympathies".

_Thanks to Gala Camacho, Simeon Carstens, Guillaume Desforges, Clément
Hurlin, Steve Purcell and Noon van der Silk for their reviews of this
article._

<!-- Footnotes -->

[^1]:
  We are not limited to Python; these techniques can be applied in
  any language with suitable support, libraries and tooling.

[^2]:
  Why they're "algebraic" is because algebraic data types form a
  [semiring][wiki-semiring]; a structure from abstract algebra. Bartosz
  Milewski [goes into depth][milewski-2015] on this, with a slight
  Haskell bent.

[^3]:
  Since Python 3.9, standard collections [can be used][pep-585] as
  their own type annotations. Prior to this, collection types could be
  found in the [`typing`][py3-typing] standard library module.

[^4]:
  The [`attrs`][pypi-attrs] library is a common dependency that
  achieves a similar goal, while `dataclasses` fulfils the ["Pareto
  principle"][wiki-pareto] from within the Python standard library.

[^5]:
  It would be more useful here to split the `TypingDiscipline`
  enumeration into two `Enums`: one for strong/weak and the other for
  static/dynamic. As I say, it's just an example!

[^6]:
  You may be familiar with Python's [`functools.reduce`][py3-reduce],
  which is a _left_ fold.

<!-- Links -->

[gist-sumtype-metaclass]: https://gist.github.com/Xophmeister/8d5ed64058e0191378d7f32a88e79564
[gvr-2005]: https://www.artima.com/weblogs/viewpost.jsp?thread=98196
[gvr-2009]: https://neopythonic.blogspot.com/2009/04/final-words-on-tail-calls.html
[lyah-list]: http://learnyouahaskell.com/making-our-own-types-and-typeclasses#recursive-data-structures
[lyah-folds]: http://learnyouahaskell.com/higher-order-functions#folds
[milewski-2015]: https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types
[mypy-docs]: https://mypy.readthedocs.io/en/stable/
[mypy]: http://mypy-lang.org/
[pep-435]: https://peps.python.org/pep-0435
[pep-484]: https://peps.python.org/pep-0484
[pep-557]: https://peps.python.org/pep-0557
[pep-585]: https://peps.python.org/pep-0585
[pep-634]: https://peps.python.org/pep-0634
[py3-decorator]: https://docs.python.org/3/glossary.html#term-decorator
[py3-enum]: https://docs.python.org/3/library/enum.html
[py3-named-tuple]: https://docs.python.org/3/glossary.html#term-named-tuple
[py3-reduce]: https://docs.python.org/3/library/functools.html#functools.reduce
[py3-truediv]: https://docs.python.org/3/reference/datamodel.html#object.__truediv__
[py3-typing]: https://docs.python.org/3/library/typing.html
[pypi-attrs]: https://www.attrs.org
[pyre]: https://pyre-check.org
[pyright]: https://github.com/Microsoft/pyright
[pytype]: https://google.github.io/pytype
[wiki-cartesian]: https://en.wikipedia.org/wiki/Cartesian_product
[wiki-cons]: https://en.wikipedia.org/wiki/Cons
[wiki-fold]: https://en.wikipedia.org/wiki/Fold_(higher-order_function)
[wiki-hof]: https://en.wikipedia.org/wiki/Higher-order_function
[wiki-pareto]: https://en.wikipedia.org/wiki/Pareto_principle
[wiki-semiring]: https://en.wikipedia.org/wiki/Semiring
[wiki-tailcall]: https://en.wikipedia.org/wiki/Tail_call
