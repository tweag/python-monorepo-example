---
title: Linear bridges to Java
author: Facundo Domínguez and Mathieu Boespflug
---

When two garbaged-collected languages share references to the same
values, the garbage collectors need to be careful to not collect
these values while the other language has references to it.

In this post we will survey the situation for writing Haskell
bindings to Java, and we will show how linear types enable a new
solution to the problem.

# Unsafe bindings to Java

The Java Virtual Machine offers an interface to manipulate Java objects,
known as the Java Native Interface (JNI). This interface is available in
C and is feasible to bind from Haskell as done in the
[jni](https://www.stackage.org/package/jni) package.

Because JNI makes possible to write a few mistakes that can be avoidable
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

# Garbage Collector Finalizers

Here we will discuss how finalizers do release references managed by
Haskell, but since the Haskell GC has no pressure to collect when the
Java Heap is full, this leads to sporadic OutOfMemory failures. 

# Dynamic scopes

Here we will discuss how we can force finalizers to run earlier by
using the ResourceT monad transformer or local frames. The drawback
of this approach is that the programmer is responsible for keeping
scopes small enough to prevent large areas to be retained in the Java
heap.

# Linear Types

Here we will devote dynamic scopes to cleanup in case of exceptions,
and will use linear types to do prompt clean ups when exceptions do not
occur.

# Summary


