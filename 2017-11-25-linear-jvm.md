---
title: Binding to Java with linear types
author: Facundo Domínguez and Mathieu Boespflug
---

When two garbaged-collected languages share references to the same
values, the garbage collectors need to be careful to not collect
these values while the other language has references to it.

In this post we will survey the situation for writing Haskell
bindings to Java, and we will show how linear types enable a new
solution to the problem.

## Unsafe bindings to Java

The Java Virtual Machine (JVM) offers a foreign interface to manipulate
Java objects, known as the Java Native Interface (JNI). This interface
is available in C and is feasible to bind from Haskell as done in the
[jni](https://www.stackage.org/package/jni) package.

Because JNI makes possible to write a few mistakes that can be avoided
with proper use of the type checker, a higher level wrapper interface
is offered in the [jvm](https://www.stackage.org/package/jvm) package
and even higher in
[inline-java](https://www.stackage.org/package/inline-java).

With `inline-java` many of the interfacing errors that could cause the
program to crash or fail are caught at build time, but a few remain.
Notably, it is possible to use references to Java objects after they
have been collected, and it is possible to accidentally retain large
amounts of memory in the Java heap with references that live in the
memory managed by Haskell.

As a case of study, we offer the conversion of Java iterators to
Haskell streams from the
[streaming](https://www.stackage.org/package/streaming) package.

``` haskell
import Foreign.JNI
import Language.Java as Java
import Language.Java.Inline as Inline.
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
produces object of some Java type described by `Interp a`. The output
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
happen for a while, then the local references can accumulate causing
the live data on the heap to grow.

A straightforward fix to this situation is to delete the reference
after the Haskell value has been obtained.

``` haskell
    ...
    bracket [Inline.java| $it.next() |]
            JNI.deleteLocalRef
            (fmap Left . Java.reify)
```

This puts the burden on the programmer to remember to delete the
reference and to take care of not using it afterwards. Moreover,
local references are only valid on the thread that created them,
and therefore the programmer has to be careful to not exchange
them with other threads.
We endeavor next to find a way to have the compiler do these
checks.

## Garbage Collector Finalizers

The solution that the `jni` package adopts for global references is
to attach finalizers to Java references, and make the Garbage Collector
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

Could we not deal with local references in the same way?
This doesn't protect against the perils of using the local reference
in a different thread, or using it in the same thread after they become
invalid. But at least the programmer wouldn't have to remember to delete
the references anymore.

A major problem of using finalizers so profusely, is that they introduce
undefined behavior in the presence of two garbage collectors.
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
functions `pushLocalFrame` and `popLocalFrame` to implement this idea
as well. `pushLocalFrame (n :: Int)` creates a new scope in which
at least `n` local references can be created. It may produce performance
issues or errors to exceed the given capacity. `popLocalFrame j` copies
the reference `j` to the parent frame and deletes the current frame,
which causes all references of the frame to be deleted.

We are still exposed to use local references after deleted, and
to use them in threads where they are invalid, but the programmer no
longer needs to remember to delete *individual* local references.  She
does need to be careful of other things, though. Firstly, she has to
remember to introduce enough scopes to always keep bounded the retained
portion of the Java heap. And secondly, she has to make sure to not blow
up the stack with too many nested scopes, as the following code could
do.

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
to treat our local references linearly? We restate our example with this
approach.

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

We are assuming that we have available a restricted form of monad
`IOL` with the following operations.

```
return :: a ->. IOL a
(>>=) :: IOL a ->. (a ->. IOL b) ->. IOL b

runIOL :: IOL (Unrestricted a) -> IO a
liftIO :: IO a -> IOL a
```

Provided that there are no exceptions, we get the compiler to check that
the references to objects returned by the iterator are eventually
deleted. Given that `obj1` must be used exactly once, it is not possible
to forget deleting it. Using the reference after it has been deleted is
equally forbidden by the type checker. And using a linear local
reference in another thread is not possible either, while the operations
on IOL do not provide a way to fork threads or send linear local
references to other threads.

When there are exceptions involved, we complement the linear approach
with a simpler form of dynamic scopes. We use a `jcatch` primitive to
define a scope.

``` haskell
sumIterator2
  :: J ('Iface "java.util.Iterator" <> [ 'Class "java.lang.Integer" ])
  ->. IOL Int32
sumIterator2 it =
   jcatch
     (unrestrictIn32 <$> iteratorToStream it >>= Streaming.fold_ (+) 0 id)
     someHandler

jcatch
  :: Exception e
  => IOL (Unrestricted a)
  -> (e -> IOL (Unrestricted a))
  -> IOL (Unrestricted a)
jcatch io handler = do
    JNI.pushLocalFrame capacity
    liftIO $ do
      a <- catch
        (runIOL $ do
           ua <- io
           -- We dispose of the result of popLocalFrame or the
           -- compiler would complain. In this case it will return
           -- jnull which is safe to pass to deleteLocalRef.
           JNI.popLocalFrame JNI.jnull >>= JNI.deleteLocalRef
           return ua
        )
        (\e -> runIOL $ do
          JNI.popLocalFrame JNI.jnull >>= JNI.deleteLocalRef
          handler e
        )
      return $ Unrestricted a

-- | A primitive possibly coming from a base library
unrestrictInt32 :: Int32 ->. Unrestricted Int32
```

These dynamic scopes are simpler because the programmer
doesn't need to worry about inserting too little or too many scopes.
Usually, clean ups are written to deal with both normal exit and
exceptions. We are departing from that by using dynamic scopes to
clean up on exceptions only. Moreover, when an exception is thrown,
cleaning up local references is needed only if the exception is ever
caught. If the exception ends up killing the thread, the local
references will be deleted anyway by the JVM.

Some restrictions apply when using `jcatch`, though. Because the
multiplicities of the arguments are not linear, only non-linear values
can be used from outside the scope. If we need to bring a linear
resource from outside the scope, this code wouldn't typecheck.

``` haskell
f :: J ty ->. IOL a
f j = jcatch (... j ...) (\e -> ... j ...)
```

We either make the argument of `f` non-linear, perhaps making it also a
global reference, or we implement some form of borrowing.

``` haskell
f :: J ty ->. IOL a
f j0 = do
    (j1, a) <- borrowJ j0 $ \j -> jcatch (... j ...) (\e -> ... j ...)
    JNI.deleteLocalRef j1
    return a

borrowJ :: J ty ->. (J ty -> IOL a) -> IOL (J ty, a)
```

The function `borrowJ` defines a scope where the reference is unrestricted.
It is unsafe as the reference could be leaked in the returned value of
type `a`, but we would be willing to tolerate this unsafety as long as
`jcatch` is only used sparingly in an application.

Another restriction of `jcatch` is that the scope can only produce
unrestricted values. If we allowed linear local references to be returned,
they would be deleted by the call to `popLocalFrame` before the caller has
a chance to use them. If we really needed to return a local reference,
we could generalize a bit the type of `jcatch` to make use of the argument
of `popLocalFrame`.

``` haskell
jcatch
  :: Exception e
  => IOL (Maybe (J ty), Unrestricted a)
  -> (e -> IOL (Maybe (J ty), Unrestricted a))
  -> IOL (Maybe (J ty), Unrestricted a)
```

## Summary

In this post we've compared some of the approaches to manage references
to Java objects. We have reviewed the problems of relying on GC
finalizers and dynamic scopes, and we have shown how linear types are
an interesting alternative.

A competing approach that we haven't discussed is the lightweight
monadic regions of
[Kiselyov and Shan](http://okmij.org/ftp/Haskell/regions.html#light-weight).
This is an incarnation of dynamic scopes that, like linear types, have
the type checker guarantee that resources aren't used after released
and that they aren't used in other threads. However, they still demand
from the programmer to not insert too much or too little scopes.
In our setting, monadic regions could still be interesting to use for
implementing functions like `borrowJ`.

In our discussion of linear types, we brought streams to a linear monad
without giving much consideration to whether it is possible and how it
would work. This is topic for a future post, but we speculate that it is
possible to work with linear streams, and to attach finalizers that run
promptly when the streams are no longer used. In this scenario, it would
make sense to have a stream finalizer delete the global reference to the
iterator in `iteratorToStream`.
