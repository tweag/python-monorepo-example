---
redirect_from: [/posts/2020-02-06-safe-inline-java.html]
title: "Safe memory management in inline-java  using linear types"
shortTitle: "A safer inline-java"
author: Facundo Dominguez
tags: [haskell, linear-types, inline-java]
description: "In this post about inline-java I aim to walk you through the upcoming safe interface of the library, which allows detecting memory management mistakes at compile time using linear types."
---

In [an earlier post][two-gcs] about [inline-java][inline-java], a Haskell
library for interoperating with Java, we discussed the hardships of
coordinating the Haskell and Java runtimes and their respective garbage
collectors. In this post I aim to walk you through the upcoming
[_safe_ interface][safe-inline-java] of the library, which allows detecting
memory management mistakes at compile time using [linear types][linear-types-tag].

## Managing references in two languages

Recapping the discussion from the previous post, the most ubiquitous source of
mistakes when using `inline-java` has been the management of Java references.

```Haskell
f :: IO Int32
f = do
  it <- [java| java.util.Arrays.asList(0, 1, 2).iterator() |]
  x0 <- [java| $it.next() |]
  deleteLocalRef it
  return x0
```

In this program, `it` is a reference to a Java iterator, created
by the first quasiquotation. The reference `it` needs to be destroyed
eventually or the garbage collector in the Java Virtual Machine (JVM)
won't be able to reclaim the memory it points to.
This requires a bit of care.
If the reference is collected prematurely, the behavior of the program
will become undefined when the reference is used after it is destroyed.
If the reference is collected too late, the garbage collector in
Java might not be able to prevent memory from being exhausted in some
executions of the program.

In general, the more resources a program has to manage, the more
opportunities there are to get it wrong. Memory is a case of a resource
which is used very often, and managing it is so difficult
that we reach for garbage collectors, or tools like Valgrind's
`memcheck` to cope with it. But no such tool exists for the case of
`inline-java`.

A shared garbage collector for the Haskell and the Java runtimes would
solve the problem. But alas, we have two garbage collectors instead.
Therefore, we are reduced to request and destroy references
explicitly from the JVM runtime.

## A language solution

The appearance, on the horizon, of
[linear types for Haskell][linear-types-proposal] brings
the opportunity to use the compiler as a tool for checking resource
management. When a function has an argument declared as
linear, the compiler will check and demand that the argument is used
exactly once. It turns out that this restriction can be leveraged to
enforce some operations to occur in a specific order.

I've added a new memory-safe interface to `inline-java` based on linear
types. With this new interface, our example would become

```Haskell
f :: Linear.IO Int32
f = do
  it <- [java| java.util.Arrays.asList(0, 1, 2).iterator() |]
  [java| $it.next() |]
```

In a [linear setting][lineario], the monad in which these operations
run declares
that the bound variables are linear and therefore should be used
exactly once. This is the case of the reference `it`, which we
originally used twice: once for reading the next element and once to
destroy it. Because the compiler doesn't allow this anymore, we adjust
the meaning of the `java` quasiquotation so it
deletes any references that it gets from Haskell before returning.

In this way, it is not possible to write a program that neglects to
destroy a reference. Nor is it possible to write a program that
destroys the reference prematurely.

```Haskell
f :: Linear.IO Int32
f = do
  it <- [java| java.util.Arrays.asList(0, 1, 2).iterator() |]
  deleteLocalRef it
  x0 <- [java| $it.next() |] -- compilation error: 'it' is used more than once
  it2 <- [java| java.util.Arrays.asList(0, 1, 2).iterator() |]
  return x0 -- compilation error: 'it2' is used less than once
```

But what if we wanted to read the next element of an iterator a second
time? The compiler would reject the following program.

```Haskell
f :: Linear.IO Int32
f = do
  it <- [java| java.util.Arrays.asList(0, 1, 2).iterator() |]
  x0 <- [java| $it.next() |]
  x1 <- [java| $it.next() |] -- compilation error: 'it' is used more than once
  return (x0 + x1)
```

To placate the compiler, we can duplicate the reference ahead of
using it.

```Haskell
f :: Linear.IO Int32
f = do
  it1 <- [java| java.util.Arrays.asList(0, 1, 2).iterator() |]
  (it2, it3) <- newLocalRef it1
  x0 <- [java| $it2.next() |]
  x1 <- [java| $it3.next() |]
  return (x0 + x1)
```

Now `it1` is used exactly once to produce two other references `it2`
and `it3`. They all refer to the same iterator, and each of `it2` and
`it3` can be used to read an element.

Duplicating references like this is, arguably, undesirable bookkeeping
to please the compiler. Eventually, I hope to take this overhead away
from the hands of the programmer, either via GHC plugins or further
extensions to the language.

## Escaping linearity

A nuance of using a linear monad in this setting is that demanding
every bound variable to be used exactly once means that even the
integers that we are reading from the iterator need to comply.
Integers certainly take some memory, but they don't affect the
JVM runtime, and they are managed by the garbage collector in
Haskell.

```Haskell
f :: Linear.IO Int32
f = do
  it1 <- [java| java.util.Arrays.asList(0, 1, 2).iterator() |]
  x0 <- [java| $it1.next() |]
  return (x0 + x0) -- compilation error: 'x0' must be used only once
```

The fix here is to use the `Unrestricted` type, which allows the
integer to escape the linear restriction.

```Haskell
f :: Linear.IO Int32
f = do
  it1 <- [java| java.util.Arrays.asList(0, 1, 2).iterator() |]
  Unrestricted x0 <- [java| $it1.next() |]
  return (x0 + x0)
```

The `java` quasiquotations can both take unrestricted values as inputs
and produce unrestricted values as output.

## Exceptions

Special provisions need to be taken to clean up resources in the
presence of exceptions. Consider the following program, which uses
an iterator with no elements to yield.

```Haskell
f :: Linear.IO (Unrestricted Int32)
f = do
  it1 <- [java| java.util.Arrays.asList().iterator() |]
  (it2, it3) <- newLocalRef it1
  Unrestricted x0 <- [java| $it2.next() |]
  Unrestricted x1 <- [java| $it3.next() |]
  return $ Unrestricted (x0 + x1)
```

The first call to `next` produces an exception complaining that
there are no more elements in the iterator. The last
quasiquotation will not execute. Even though it is responsible for cleaning
up the reference `it3`! Without further measures, if the program
recovers from the exception, `it3` won't be destroyed, and yet
it will be unreachable to the Haskell runtime.

The current solution is to surround manually all uses of the linear
monad with a function `withLocalFrame` coming from the `jni`
package.

```Haskell
import Foreign.JNI.Safe
  (withLocalFrame) -- :: Linear.IO (Unrestricted a) -> IO a

main :: IO
main = withLocalFrame f >>= print
```

This function runs the argument in a local frame, a concept of the
JNI interface. A local frame is a scope in a thread of the program.
All local references created during the scope belong to the frame,
and are destroyed automatically when the scope ends if they haven't
been destroyed yet.

In our running example, this means that `it3` will stay alive until
the exception reaches `withLocalFrame`, which will have `it3` destroyed
before propagating the exception.

There are safer and more flexible solutions to deal with exceptions,
like [resourcet][resourcet] or
[type-level monadic regions][monadic-regions]. Let me defer refining this
aspect, though, until we gain some more experience with the safe
interface.

## Final remarks

In this post I have shown how the safe interface of `inline-java`
helps avoiding the most common mistakes when managing references to
Java objects.

We depend on the implementation of the
[LinearTypes proposal][linear-types-proposal] to
be merged into GHC. But the safe `inline-java` can be used already
with the [forked linear-types-enabled GHC][linear-types-ghc].

Linear types are not the first solution proposed in Haskell for
resource management, but as we have discussed [in the past][two-gcs],
we still believe it is the most effective solution for integrating
Haskell with the JVM currently.

[inline-java]: https://github.com/tweag/inline-java
[lineario]: https://github.com/tweag/linear-base/blob/dd65d1381de03fb567e52b8b8b6b7f9bf693544f/src/System/IO/Linear.hs
[linear-types-tag]: https://www.tweag.io/blog/tags/linear-types
[linear-types-ghc]: https://github.com/tweag/ghc/tree/linear-types#ghc-branch-with-linear-types
[linear-types-proposal]: https://github.com/tweag/ghc-proposals/blob/linear-types2/proposals/0000-linear-types.rst
[monadic-regions]: http://okmij.org/ftp/Haskell/regions.html#light-weight
[resourcet]: http://hackage.haskell.org/package/resourcet
[safe-inline-java]: https://github.com/tweag/inline-java/blob/master/src/linear-types/Language/Java/Inline/Safe.hs
[techempower]: https://www.techempower.com/benchmarks
[two-gcs]: https://www.tweag.io/posts/2017-11-29-linear-jvm.html
[wizzardo-inline]: https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Haskell/wizzardo-inline
