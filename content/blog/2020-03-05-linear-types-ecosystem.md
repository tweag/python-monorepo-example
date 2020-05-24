---
title: Linear types, backward and forward compatibility
author: Mathieu Boespflug, Krzysztof Gogolewski
tags: [haskell, linear-types]
---

The headline feature of the [first Linear Haskell
paper][linear-haskell-paper] was backward compatibility. This
contribution was deemed so important that is was made to be the title
of the paper. Linear types are everything but a new idea. Jean-Yves
Girard's first publication on the topic came in 1987. By now,
a [search][linear-logic-search] on Google Scholar yields close to 30k
results, including one by Phil Wadler about their application to
Haskell, with the title ["Linear types can change the
world!"][linear-types-wadler], published... exactly 30 years ago.
What's new about Linear Haskell is that it bakes in linear types in
a way that is unintrusive. In this post we'll explore what this means
in practice.

## Pure linear functions

Linear Haskell is a refinement to Haskell's type system. Like levity
polymorphism, it in effect adds more parameters to Haskell's function
type constructor `(->)`. We now have as parameters:

- the type of the argument to a function and its result type,
- the levity of each type,
- the multiplicity of the argument type.

Years ago, argument types could only be lifted. Now they can be lifted
or unlifted. Until recently, argument types always had multiplicty
`Many`. With Linear Haskell they can also have multiplicity `1`. The
added expressivity means that even simple functions like `\x -> x` can
be given more types than before:

```haskell
\x -> x :: a -> a
\x -> x :: a #-> a
```

The latter type says that `x` has multiplicity `1`. It should appear
exactly once in the body of the function, or the compiler will
complain. You might recognize `\x -> x` as the definition of `id` in
the Prelude. Other Prelude functions can also been given linear types:

```haskell
const :: a #1 -> b -> a
foldr :: (a #1 -> b #1 -> b) -> b #1 -> [a] #1 -> b
```

_Could_ we change the types of `id`, `const` and `foldr` in the
Prelude to read as above? Doing so wouldn't break any existing code.
Intuitively, anywhere a non-linear function suffices, a linear
function can be provided. So strengthening the contract of a function
by specifying in the type which arguments must be handled linearly in
the body of the function, is _backward compatible_: all code
downstream of you that worked before linear types will still work with
linear types.

_Should_ we change the Prelude? Possibly. But we can afford to wait.

[linear-haskell-paper]: ee
[linear-types-wadler]: http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.31.5002
[linear-logic-search]: https://scholar.google.com/scholar?q=%22linear+logic%22
