---
title: Making two garbage collectors be good neighbours <br/> (using linear types)
shortTitle: Making two garbage collectors be good neighbours
author: Facundo Domínguez and Mathieu Boespflug
tags: haskell, linear-types
---

Foreign function interfaces (FFI) allow fast interop between
languages. Unlike other approaches, like performing RPC calls between
different components written in different languages, using the FFI
allows for all manner of data to be shared between each language
runtime, in the same address space. This reduces memory consumption
and obviates marshalling costs. But when two garbaged-collected
languages share references to the same values, each garbage collector
(GC) needs to be careful to not collect these values while the other
language has references to them. This is a problem we ran into when
building both [inline-r][inline-r] and [inline-java][inline-java]. In
this post, we'll survey this very generic problem in all fast language
interop, using Java interop as a case study.

Bonus: we'll show you how linear types can help solve the problem
safely.

## Unsafe bindings to Java

The Java Virtual Machine (JVM) offers a foreign interface to
manipulate Java objects, known as the Java Native Interface (JNI).
This is a C interface, which we can readily bind in Haskell
using [inline-c][inline-c] or similar. This is what the [jni][jni]
package does.

The JNI is a low-level interface that is painful to use. No programmer
wants to invoke Java methods through the JNI using stringly typed
class names, method names and argument types. Doing so is very
error-prone and verbose. So we built higher-level abstractions on
top, [jvm][jvm] and [inline-java][inline-java], that run every method
invocation through the Java type checker as well as the Haskell type
checker. Think of `inline-java` as a pretty good typo detector.

[jni]: https://www.stackage.org/package/jni
[jvm]: https://www.stackage.org/package/jvm
[inline-c]: https://www.stackage.org/package/inline-c
[inline-java]: https://www.stackage.org/package/inline-java
[inline-r]: https://www.stackage.org/package/inline-r

In fact, `inline-java` does even more than that. It checks that
Haskell types and Java types line up. It catches at compile time many
common bugs that could cause the program to crash or fail, but a few
remain. Notably,

* it is possible to use references to Java objects by mistake after
they have been collected, and
* it is possible to accidentally retain large amounts of memory in the
Java heap with references that live in the memory managed by Haskell.

Here's a case study: the conversion of Java `Iterator`s to Haskell
`Stream`s (as defined in the [streaming][streaming] package).

[streaming]: https://www.stackage.org/package/streaming

``` haskell
import Foreign.JNI
import Language.Java as Java
import Language.Java.Inline
import Streaming

iteratorToStream
  :: Reify a
  => J ('Iface "java.util.Iterator")
  -> IO (Stream (Of a) IO ())
iteratorToStream it = do
    return $ Streaming.untilRight $ do
      [Inline.java| $it.hasNext() |] >>= \case
        False -> return (Right ())
        True -> do
          obj <- [Inline.java| $it.next() |]
          Left <$> Java.reify obj
```

See [previous posts][inline-java-blog-post] for an intro to
`inline-java`, but here's the gist. The input to this function is any
Java object that conforms to the `java.util.Iterator` interface. The
output is a `Stream` yielding values of some type `a`. The Java
objects are pulled from the iterator as the stream is consumed. The
constraint `Reify a` states that we know how to convert Java objects
to Haskell values of type `a`. We do this on the last line by calling
`reify`.

[inline-java-blog-post]: http://www.tweag.io/posts/2017-09-15-inline-java-tutorial.html

Like in Java, `it` and `obj` above are actually *references* to
objects. But it's a special type of reference provided by the JNI,
which can be used by foreign code (such as C or Haskell). These JNI
references need to be deleted explicitly once they are no longer
needed, otherwise JVM objects cannot be reclaimed by the JVM GC.

The above implementation of `iteratorToStream` is not deleting the
references to Java objects. That's a leak! Indeed, an object reference
acts as a root in the graph of all objects in the heap, as far as the
JVM garbage collector is concerned. Adding to the problem, the JVM
can't deal very well with large and unknown amounts of references. The
JNI expects native calls to use only a few references and expects the
programmer to say in advance how many references will be needed.
Failing to do so affects performance and can lead to failures.

A straightforward fix to this situation is to delete the reference
after the Haskell value has been obtained.

``` haskell
    ...
    bracket [Inline.java| $it.next() |]
	        JNI.deleteLocalRef
            (\jNext -> Left <$> Java.reify jNext)
```

There are two problems with this approach:

* this puts the burden on the programmer to remember to delete the
  reference and to be careful not to use it afterwards (or risk
  a segfault). Moreover,
* JNI references are usually *local*, meaning that they are only valid
  on the thread that created them. So the programmer has to be careful
  to not share them with other threads.

Could we possibly ask the compiler to perform these checks?

## Garbage Collector Finalizers

One way to avoid needing these checks in the first place is to just
let the Haskell GC delete Java references automatically when they
become unreachable. We attach to each reference a finalizer that
deletes it, which is going to be called by the Haskell GC. Such
references are no longer *local* references, but *global* references.
Unlike local references, a global reference can be used in any thread
and it is not destroyed when control returns to Java. Since the JNI
provides a facility to promote any local reference to a global one,
couldn't we just turn all local references into global ones and then
have them be managed by the GC? A global reference is more expensive
than a local one, so performance suffers. But it mostly works. Until
you run out of memory...

A major problem with letting the GC run the show completely is that
counter intuitively, sometimes memory might never be reclaimed, even
when many objects are long dead. Suppose that the Java heap is
crowded, the Garbage Collector of the JVM is desperate to kick some
objects out of existence, and yet there is a good chunk of references
from Haskell-land to the Java Heap. The Haskell portion of the
application is already done with the references, but since there is
plenty of space in the Haskell heap, the Haskell's Garbage Collector
is basking in the sun, with no pressure to run the finalizers that
would delete the unused references.

Sometimes, the application is lucky and the Haskell GC runs the
finalizers just in time, which lets the Java GC clean
the Java heap. Unfortunately, sometimes, the Haskell GC won't run and
the JVM will fail with an `OutOfMemory` exception.

## Dynamic scopes

Another solution is to define dynamic scopes. When a program's control
flow enters a scope, we open a new buffer. We keep track of all newly
created references in the buffer, until the control flow leaves the
scope, at which point we discard all recorded references all at once.
In general, scopes are not allowed to overlap arbitrarily, but they
can be nested.

In Haskell,
the [resourcet](https://www.stackage.org/package/resourcet) package
neatly encapsulates this idea. The JNI natively supports a similar
idea with
using
[`pushLocalFrame`](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html#push_local_frame) and
[`popLocalFrame`](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html#pop_local_frame).
`pushLocalFrame (n :: Int)` creates a new scope in which at least `n`
local references can be created. Exceeding the given capacity might
cause performance issues or errors. `popLocalFrame j` copies the
reference `j` to the parent frame and deletes the current frame, which
causes all references of the frame to be deleted.

We are still running the risk of accidentally using a local reference
after deletion, and to use it in threads where it is invalid. But
programmers no longer need to remember to delete *individual* local
references. Still, in practice we found difficult finding a hierarchy
of nested scopes that keeps the counts of local references low.
It is
a problem that worsens with the size of the application. When building
a complex server application that made many invocations to Java, we
started with a scope per client request, and then a scope per test,
and then we added scopes within the scopes when we were creating more
local references than anticipated. Eventually, it did get very
difficult for multiple teams of programmers of varying experience
levels to be sure that the number of extant references stayed bounded
for all possible code paths and inputs.

## Linear Types

We would really prefer to delete a reference exactly
when we know it to be no longer useful. In this way, memory becomes
reclaimable by Java GC immediately. The problem is: it's easy to
forget doing so at all, leading to multiple leaks in an application.
The key invariant we want checked by the
compiler is that once we have a reference, it should be deleted
*exactly once*, and never referred to after that. That is, we want to
use references *linearly*.

What if we used the GHC proposal for
[linear types](https://github.com/ghc-proposals/ghc-proposals/pull/91)
to treat our local references linearly? It would look something like this:

``` haskell
import Foreign.JNI
import Language.Java as Java
import Language.Java.Inline as Inline.
import Streaming

iteratorToStream
  :: Reify a
  => J ('Iface "java.util.Iterator" <> [Interp a])
  ->. IOL (Stream (Of a) IOL ())
iteratorToStream itLocal = do
    return $ Streaming.untilRight $ do
      [Inline.java| $it.hasNext() |] >>= \case
        False -> return (Right ())
        True -> do
          obj0 <- [Inline.java| $it.next() |]
          (obj1, Unrestricted a) <- Java.reify obj0
          JNI.deleteLocalRef obj1
          return a

Java.reify :: J (Interp a) ->. IOL (J (Interp a), Unrestricted a)

-- | A linear value of type `Unrestricted a` holds a value of
-- type `a` which can be used non-linearly or unrestrictly.
data Unrestricted a where
  Unrestricted :: a -> Unrestricted a
```

We are assuming that we have a restricted form of the `IO` monad,
called `IOL`, with the following operations.

```
return :: a ->. IOL a
(>>=) :: IOL a ->. (a ->. IOL b) ->. IOL b

liftIO :: IO a -> IOL a

data IOL a where
  IOL :: IO a -> IOL a

runIOL :: IOL (Unrestricted a) -> IO a
runIOL (IOL io) =
    Unrestricted a <-
      bracket_ (JNI.pushLocalFrame capacity)
               (JNI.popLocalFrame JNI.jnull)
               io
    return a
  where
    capacity = ...
```

Compared to dynamic scopes, the major feature of `IOL` is that
programmers can delete local references promptly, inside a single
global scope, when they are no longer needed. The programmer doesn't
have to be concerned with guessing a scope hierarchy anymore.

`IOL` introduces local references as linear values. Operations that do
not delete the reference, like `reify`, now have to return a copy of
it, and the operations that delete the value, like `deleteLocalRef`,
produce no copy. This means both that references cannot be used after
they are deleted (since they can't be used more than once), and that
the compiler will require them to be deleted eventually (they must be
used at least once). Finally, local references cannot be allowed to
escape the scope of `runIOL`, as they become invalid before `runIOL`
returns. This is achieved by constraining its argument to yield an
unrestricted value `Unrestricted a`. Local references are released
promptly even if an exception arises, thanks to the `bracket` inside
`runIOL` and the fact that there is no way to catch exceptions in `IOL`.

Admittedly, if exceptions need to be caught, it has to be done by the
caller of `runIOL`. In our experience, many applications need
to catch exceptions in a few places only, so this is a modest price to
pay.

## Summary

Each the local and global references we create via the JNI is
effectively a GC root for the Java GC. The JNI was designed with the
assumption that programmers ensure that very few such roots are in
flight at any one time. The R native interface and others make similar
assumptions. In this post, we discussed the tension that arises
between releasing early and frequently, and doing so safely without
increasing the risk of use-after-free bugs. With linear types, we can
get both.

A competing approach that we haven't discussed is the lightweight
monadic regions of
[Kiselyov and Shan](http://okmij.org/ftp/Haskell/regions.html#light-weight).
This is an incarnation of dynamic scopes that, like linear types, have
the type checker guarantee that resources aren't used after released
and that they aren't used in other threads. However, they still demand
from the programmer to not insert too many or too few scopes.

Some have suggested introducing affine types instead of linear types
in Haskell. But for the particular use case discussed in this post,
affine types would do no better than these monadic regions. That's
because affine types provide a weaker guarantee to the caller: we can
return to the caller having used the argument at most once, but also
never at all. We'd need nested scopes all over again to ensure that
references *do* get disposed of in a timely fashion.

In our discussion of linear types, we brought streams to a linear
monad without delving into the details of whether it is possible and how
it would work. This will be the topic for a future post.
