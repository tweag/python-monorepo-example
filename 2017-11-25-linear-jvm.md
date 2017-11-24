---
title: Binding to Java with linear types
author: Facundo Domínguez and Mathieu Boespflug
---

Foreign function interfaces (FFI) allow fast interop between
languages. Unlike other approaches, like performing RPC calls between
parts of a program in each language, using the FFI allows for all
manner of data to be shared between each language runtime, hence
reducing memory consumption and obviating marshalling costs. But when
two garbaged-collected languages share references to the same values,
each garbage collector needs to be careful to not collect these values
while the other language has references to them. This is a problem we
ran into both when building [HaskellR][haskellr]
and [inline-java][inline-java]. In this post, we'll survey this very
generic problem in all fast language interop. Bonus: we'll show you
how linear types can help solve the problem safely.

## Unsafe bindings to Java

The Java Virtual Machine (JVM) offers a foreign interface to
manipulate Java objects, known as the Java Native Interface (JNI).
This is a C interface, which we can readily bind in Haskell
using [inline-c][inline-c] or similar. This is what the
 [jni](https://www.stackage.org/package/jni) package does.

The JNI is a low-level interface that is painful to use. No programmer
wants to invoke Java methods through the JNI using stringly typed
class names, method names and argument types. Doing so is very
error-prone and verbose. So we built higher-level abstractions on
top, [jvm][jvm] and [inline-java][inlin-java], that run every method
invocation through the Java type checker as well as the Haskell type
checker. Think of `inline-java` as a pretty good typo detector.

[jvm]: https://www.stackage.org/package/jvm
[inline-java]: https://www.stackage.org/package/inline-java

In fact, `inline-java` does even more than that. It checks that
Haskell types and Java types line up. It catches at compile time many
interfacing bugs that could cause the program to crash or fail, but
a few remain. Notably,

* it is possible to use references to Java objects after they have
been collected, and
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
  => J ('Iface "java.util.Iterator" <> [Interp a])
  -> IO (Stream (Of a) IO ())
iteratorToStream itLocal = do
    -- We make sure the iterator remains valid while we reference it.
    it <- JNI.newGlobalRef itLocal
    return $ Streaming.untilRight $ do
      [Inline.java| $it.hasNext() |] >>= \case
        False -> return (Right ())
        True -> do
          obj <- [Inline.java| $it.next() |]
          Left <$> Java.reify obj
```

The input to this function is a reference to a Java iterator that
produces an object of some Java type described by `Interp a`. The output
is a `Stream` yielding values of some type `a`. The Java objects are
pulled from the iterator as the stream is consumed. The constraint
`Reify a` states that we know how to convert Java objects of type
`Interp a` to Haskell values of type `a`, and it enables our function
to use the `reify` method to do the conversion.

The above implementation of `iteratorToStream` is not deleting the
references to Java objects that it obtains from the iterator. Does
this constitute a leak? It depends. References in JNI can be local
or global, and in this case the references we get from the iterator are
local. A local reference is only valid in the thread in which it is
created, and only for as long as the thread does not return from
an ongoing native call (i.e. a call from Java to C or Haskell).
If Java called into Haskell, and then Haskell invoked
`iteratorToStream`, being local implies that the references will be
deleted as soon as the control returns back to Java. If this doesn't
happen for a while, then the local references can accumulate, causing
the live data on the heap to grow. Adding to the problem, the JVM
can't deal very well with large and unknown amounts of local
references. The JNI expects native calls to use only a few local
references and expect the programmer to say in advance how many
references will be needed. Failing to do so affects performance and
can lead to failures.

A straightforward fix to this situation is to delete the reference
after the Haskell value has been obtained.

``` haskell
    ...
    bracket [Inline.java| $it.next() |]
	        JNI.deleteLocalRef
            (\next -> Left <$> Java.reify next)
```

There are two problems with this approach:

* this puts the burden on the programmer to remember to delete the
  reference and to be careful not to use it afterwards (or risk
  a segfault). Moreover,
* local references are only valid on the thread that created them, so
  the programmer has to be careful to not exchange them with other
  threads.

Could we possibly ask the compiler to perform these checks?

## Garbage Collector Finalizers

The solution that the `jni` package adopts for global references is
to attach finalizers to Java references, and make the GC
in Haskell-land responsible for deleting them.
Unlike local references, a global reference can be used in any thread
and it is not destroyed when control returns to Java. In
`iteratorToStream` we make the reference to the iterator global with
`JNI.newGlobalRef`. This way, we know that the reference to the iterator
remains valid while the stream is producing values. In principle, we
don't know how many times the control flow will cross language
boundaries before the stream is fully consumed in the calling context.
Again, using a global reference is a practical way to ensure our
reference remains valid throughout.

Could we not deal with local references in the same way? We could. But
this doesn't protect against the perils of using the local reference
in a different thread, or using it in the same thread after they
become invalid. It looks like progress though, because at least the
programmer wouldn't have to remember to delete the references anymore.

A major problem of using finalizers so profusely, is that they introduce
non-deterministic failures in the presence of two garbage collectors.
Suppose that the Java heap is crowded, the Garbage Collector of the JVM
is desperate to kick some objects out of existence, and yet there is a
good chunk of references from Haskell-land to the Java Heap. The Haskell
portion of the application is already done with the references, but
there is plenty of space in the Haskell heap, and the Haskell's Garbage
Collector is basking in the sun with no pressure to run the finalizers
that would delete the unused references.

Sometimes, the application is lucky and the Haskell's GC runs just in
time to delete the unused references, which lets the Java's GC clean
the Java heap. Unfortunately, sometimes, the Haskell's GC won't run and
the JVM will fail with an `OutOfMemory` exception.

## Dynamic scopes

Another solution is to define dynamic scopes where references are valid.
A dynamic scope is a piece of the program traversed by the control flow
from a well defined beginning to a well defined ending point. At the
beginning of the scope we inject some code that declares that we are
starting a new scope, and any local references created during the
execution of the scope is associated to it. At the end of the scope,
we inject some code to delete all the associated local references.
In general, scopes are not allowed to overlap arbitrarily, but they can
be nested.

The package [resourcet](https://www.stackage.org/package/resourcet)
provides facilities for defining scopes, and JNI offers a couple of
functions
[pushLocalFrame](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html#push_local_frame)
and
[popLocalFrame](https://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html#pop_local_frame)
to implement this idea
as well. `pushLocalFrame (n :: Int)` creates a new scope in which
at least `n` local references can be created. It may produce performance
issues or errors to exceed the given capacity. `popLocalFrame j` copies
the reference `j` to the parent frame and deletes the current frame,
which causes all references of the frame to be deleted.

We are still running the risk of accidentally using a local references after deletion, and
to use them in threads where they are invalid. But the programmer no
longer needs to remember to delete *individual* local references.  She
does need to be careful of other things, though:

* she has to remember to introduce enough scopes to always keep
  bounded the retained portion of the Java heap. And
* she has to make sure to not blow up the stack with too many nested
  scopes, as the following code could do.

``` haskell
sumIterator
  :: J ('Iface "java.util.Iterator" <> [ 'Class "java.lang.Integer" ])
  -> IO Int
sumIterator it =
    iteratorToStream it >>= go 0
  where
    go :: Int32 -> Stream (Of Int32) IO () -> IO Int32
    go !acc s =
      -- We create a new nested scope on every recursive call.
      bracket_ (JNI.pushLocalFrame capacity) (JNI.popLocalFrame JNI.jnull) $ do
        e <- Streaming.next s
        case e of
          Left () -> return acc
          Right (i, s) -> go (acc + i) s

    capacity = ...
```

## Linear Types

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
    Unrestricted it <- JNI.newGlobalRef itLocal
    return $ Streaming.untilRight $ do
      [Inline.java| $it.hasNext() |] >>= \case
        False -> return (Right ())
        True -> do
          obj0 <- [Inline.java| $it.next() |]
          (obj1, Unrestricted a) <- Java.reify obj0
          JNI.deleteLocalRef obj1
          return a

Java.reify :: J (Interp a) ->. IO (J (Interp a), Unrestricted a)
JNI.newGlobalRef :: J ty ->. IOL (Unrestricted (J ty))
```

We are assuming that we have available a restricted form of the `IO`
monad, called `IOL`, with the following operations.

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

The major feature of `IOL` when compared to the dynamic scopes is that
it releases local references promptly when they are no longer needed,
and it does this by introducing a single scope. The programmer no
longer has to be concerned with introducing too many or too few scopes.

Let us analyze this claims from closer. `IOL` introduces local
references as linear values. Operations that do not delete the value,
like `reify`, now have to return a copy of it, and the operations which
delete the value, like `deleteLocalRef`, produce no copy. This means
both that references cannot be used after deleted (they can't be used
more than once), and that the compiler will require them to be deleted
eventually (they must be used at least once) when exceptions do not
occur. When exceptions occur, `IOL` offers no means to catch them, which
causes the exception to propagate to `runIO` and above, which runs the
cleanup in `runIOL`. Finally, local references cannot be allowed to
escape the scope of `runIOL`, as they become invalid before `runIOL`
returns. This is achieved by constraining its argument to yield an
unrestricted value `Unrestricted a`. If needed, it could be possible to
come up with a version of `runIOL` that uses the argument of
`popLocalFrame` to have a selected reference survive.

Admittedly, if exceptions need to be caught, it has to be done by the
caller of `runIOL`, outside the safety of `IOL`. In our experience,
many applications need to catch exceptions at a few places only, and
this becomes a modest
price to pay.

## Summary

Each the local and global references we create via the JNI is
effectively a GC root for the Java GC. The JNI was designed with the
assumption that programmers ensure that very few such roots are in
flight at any one time. In this post, we discussed the tension that
arises between releasing early and frequently, and doing so safely
without increasing the risk of use-after-free bugs. With linear types,
we can get both.

A competing approach that we haven't discussed is the lightweight
monadic regions of
[Kiselyov and Shan](http://okmij.org/ftp/Haskell/regions.html#light-weight).
This is an incarnation of dynamic scopes that, like linear types, have
the type checker guarantee that resources aren't used after released
and that they aren't used in other threads. However, they still demand
from the programmer to not insert too many or too few scopes.

In our discussion of linear types, we brought streams to a linear
monad without delving into the details of whether it is possible and how
it would work. This will be the topic for a future post.
