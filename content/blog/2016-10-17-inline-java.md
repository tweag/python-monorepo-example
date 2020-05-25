---
redirect_from: [/posts/2016-10-17-inline-java.html]
title: "A new ecosystem for Haskell:  the JVM"
author: Mathieu Boespflug, Alp Mestanogullari
featured: yes
tags: [haskell]
---

By now, Haskell has first class support for seamlessly embedding
foreign code into source files and casually call anything in C (via
[inline-c][inline-c]) or R (via [inline-r][inline-r]), let alone that
whole programs can also be compiled down to in-browser JavaScript,
thanks to [GHCJS][ghcjs]. Today the interoperability story for Haskell
is getting better still: we're announcing the addition of a new _set
of languages_ into the mix. With [jvm][haskell-jvm], you can call any
method known to the JVM from Haskell. With [inline-java][inline-java],
you can moreover call these methods in Java syntax, embedded in your
source files.

<!--more--> Not that this was particularly our intention - promise!

`inline-java` and friends just fell out naturally from our work on
[sparkle][sparkle]...

To give you a taste of what it's like to program this way, here's an
obligatory "Hello World!" in Haskell, but with a twist: we call the
Swing GUI toolkit to display our message to the world in a graphical
dialog box.

![A Swing GUI application in Haskell](../img/posts/swing-helloworld.png)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Int
import Language.Java
import Language.Java.Inline

main :: IO Int32
main = withJVM [] $ do
    message <- reflect "Hello World!"
    [java| { javax.swing.JOptionPane.showMessageDialog(null, $message);
             return 0; } |]
```

In short, it's now possible to **write Java programs that call into
Haskell with near trivial overhead**, as we demonstrated previously
with [sparkle][sparkle-blog-post], or indeed **Haskell programs that
call into any of the hundreds of thousands of JVM packages** available
publicly and boundless custom enterprise developments.

[haskell-jvm]: https://github.com/tweag/inline-java/tree/master/jvm
[inline-java]: https://github.com/tweag/inline-java/
[sparkle]: https://github.com/tweag/sparkle/
[sparkle-blog-post]: http://blog.tweag.io/posts/2016-02-25-hello-sparkle.html
[inline-c]: https://www.stackage.org/package/inline-c
[inline-r]: http://tweag.github.io/HaskellR/
[ghcjs]: https://github.com/ghcjs/ghcjs

## How it works

The key enabler to talking to Java and all the other JVM languages
from Haskell is that Haskell speaks C and the JVM speaks C. That is,
both Haskell and the JVM make it possible to call C functions and have
C functions call them. In both cases, this is done as part of their
respective foreign function interfaces (FFI). So we have a _lingua
franca_ for both languages: to move from Haskell to Java or _vice
versa_, go through C first. Each language has their own custom calling
convention anyways, so some small amount of glue code to mediate
between the two is inevitable.

In fact, in the case of the JVM, bytecode is compiled "just-in-time"
or perhaps even not at all. Fortunately, that's not something we have
to worry about: the JVM's standard interface from C, called the
[Java Native Interface (JNI)][jni], encapsulates all the nitty-gritty
detail of invoking methods behind a simple interface. As a first step,
we wrote near complete bindings to all of the JNI, using
[inline-c][inline-c] under the hood for better safety.

[jni]: https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html

### Calling into the JVM, the hard way

We could just expose the raw JNI API in Haskell and call it a day.
Using raw JNI calls to invoke say a static Java method called `foo`,
which takes an `int` and a `boolean` and returns some object, goes
something like this:

```haskell
import Foreign.JNI

callFoo = do
  klass <- findClass "some/Java/Class"    -- JNI uses '/' instead of '.'...
  method <- getStaticMethodID klass "foo" "(IZ)Ljava/lang/Object;"
  callStaticObjectMethod klass method [JInt 0, JBoolean 1]
```

Because the JVM allows overloaded method names, when grabbing a handle
to invoke a method, you'll need to specify a _type signature_ to
disambiguate which method you really want to call. But the JNI was
purposefully designed independently of Java's syntax, to the point
where even class names are written differently. The
[JNI syntax for type signatures][jni-typesigs] is optimized for speed
of parsing and compactness, not legibility. So constructing these type
signatures by hand to invoke JVM methods via raw JNI calls is rather
error prone. That's why we wrote the [jvm][haskell-jvm] package,
a toolkit for invoking JVM methods more conveniently and robustly.

[jni-typesigs]: http://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/types.html#type_signatures

### Using Haskell types for safer JVM calls

There are two downsides to the raw JNI calls we saw above:

- **performance:** getting class and method handles is expensive.
  Ideally, we'd only ever lookup classes and methods by name _at most
  once_ throughout the lifetime of the program, assuming that loaded
  classes exist for all time and are never redefined.
- **stringly typing:** we pass signatures explicitly, but these are
  literally strings, typos and all. If you mistype the signature, no
  compiler will call that out. Ideally ill-formed signatures would be
  caught at compile-time, rather than at runtime when it's far too
  late and your program will simply crash.

The performance issue is easily dispensed with. The trick is to write
wrappers that tell Haskell that `findClass` and `getStaticMethodID`
are really pure, in the sense that calling either of them multiple
times and in any context always yields equivalent results. So we could
in principle ascribe pure types to them. The argument goes something
like the following. Compare the following snippet with the one above:

```haskell
callFoo = do
  let pureStuff@(klass, method) = unsafePerformIO $ do
      (,) <$> findClass "some/Java/Class"
          <*> getStaticMethodID klass "foo" "(IZ)Ljava/lang/Object;"
  callStaticObjectMethod klass method [JInt 0, JBoolean 1]
```

The expression for `pureStuff` is a closed expression (no free
variables occur). And because its type is not `IO`, the compiler is
free to float it to top-level, effectively turning it into a CAF,
which are always evaluated at most once thanks to laziness:

```haskell
(klass, method) = unsafePerformIO $ do
  (,) <$> findClass "some/Java/Class"
      <*> getStaticMethodID klass "foo" "(IZ)Ljava/lang/Object;"

callFoo = do
  callStaticObjectMethod klass method [JInt 0, JBoolean 1]
```

As for the stringly typing problem, we'll need some tools first.
First, we need to reflect in Haskell enough type information. To that
end, we'll index the type of Java objects by their Java type:

```haskell
newtype J (a :: JType) = J (Ptr (J a))
```

Java types can either be primitives (`int`,`boolean`, etc) or
reference types (classes, arrays, interfaces, generics etc). So our definition
of `JType` goes something like this:

```haskell
data JType
  = Prim Symbol
  | Class Symbol
  | Array JType
  | ...

genSingletons ['JType]
```

Thus equipped, we can write types like,

- the type of Swing option panes, `J ('Class "javax.swing.JOptionPane")`
- the type of boxed Java integers, `J ('Class "java.lang.Integer")`,
- the type of primitive integer arrays, `J ('Array ('Prim "int"))`,
- etc.

What's more, thanks to the family of singleton types and instances
created by `genSingletons` above, we can reflect on the type of any
Java object at runtime to get a representation of the type at the
value level. This is helpful to _auto compute_ JNI type signatures
from the types alone. No more stringly typing will all those typos in
tow: JNI type signatures are now correct by construction.

In particular, we can define a family of variants of
`callStaticObjectMethod`:

```haskell
module Language.Java where

callStatic1
  :: (SingI ty1, SingI tyr)
  => Sing (klass :: Symbol) -> JNI.String -> J ty1 -> IO (J tyr)
callStatic2
  :: (SingI ty1, SingI ty2, SingI tyr)
  => Sing (klass :: Symbol) -> JNI.String -> J ty1 -> J ty2 -> IO (J tyr)
callStatic3
  :: (SingI ty1, SingI ty2, SingI ty3, SingI tyr)
  => Sing (klass :: Symbol) -> JNI.String -> J ty1 -> J ty2 -> J ty3 -> IO (J tyr)
...
```

The types of these functions are expressive enough to infer a type
signature for the called method. Thanks to the type reflection
provided by the `singletons` package, we can reify types as values and
produce JNI type signatures from that. Of course, a fixed number of
`callStatic*` functions, one per arity, is rather limiting (what
about arbitrary arities?), so in reality the `Language.Java` module
provides a single such function, to whom arguments are passed packed
into a homogeneous list:

```haskell
callStatic
  :: SingI tyr
  => Sing (klass :: Symbol) -> JNI.String -> [JValue] -> IO (J tyr)
```

where `JValue` is defined as

```haskell
data JValue
  = forall a. SingI a => JObject (J a)
  | JBoolean Word8
  | JInt Int32
  | ...
```

In this way, values of primitive type can be passed to Java without
penalty: no need to box them into tiny objects first. It turns out we
can extend the same idea to obtain unboxed _return_ values, but the
technical details get a bit more intricate, so we'll have to defer
that to the [module's documentation][haskell-jvm-docs].

Calling a non-static method is achieved in much the same way:

```haskell
call :: (SingI ty, SingI tyr)
     => J ty -> JNI.String -> [JValue] -> IO (J tyr)
```

[haskell-jvm-docs]: https://stackage.org/package/jvm

### JVM calls the Java way

`call` and `callStatic` are surprisingly versatile facilities for
calling arbitrary JVM methods with an arbitrary number of boxed or
unboxed arguments and return values, but sometimes one might still get
the types wrong. For example, there's nothing stopping us from
attempting to `call` a `java.lang.Integer` constructor with a boolean
typed argument. No such constructor exists, so we'll get a method
lookup exception at runtime. After all, we don't know in Haskell what
methods really do exist, and what their signatures are. But if we
call the `java.lang.Integer` constructor using Java syntax, we can
hope to get the Java compiler to perform full scope checking and type
checking, thus ruling out common errors such as calling non-existent
methods are supplying arguments of the wrong type.

To achieve that, we use GHC's
[quasiquotation extension][quasiquotation]. This extension allows us
to embed syntax from arbitrary foreign languages in Haskell source
files, in between special brackets. Better yet, we are free to extend
the foreign syntax to express antiquotation variables, _i.e._
variables that refer to the enclosing context in Haskell. Take for
example our earlier "hello world" code snippet, simplified:

```haskell
do message <- reflect "Hello World!"
   [java| javax.swing.JOptionPane.showMessageDialog(null, $message) |]
```

Using `reflect`, also provided by `inline-java`, we create a `J "java.lang.String"` from a Haskell `String`. We can then refer to this
Java object, bound to a Haskell variable, from inside the Java code
snippet. The `$` sigil is there to disambiguate between variables
bound in the Haskell context (aka antiquotation) and in the Java
context.

[quasiquotation]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell-quasi-quotation

You might have noticed a difference with `inline-c`: in `inline-java`
we don't need to annotate quasiquotations with the return type nor
each antiquote with their types, which can get quite verbose. Instead,
we just about manage to infer which foreign types are intended based
on the types of the Haskell variables. To pull this off required
a journey in [compiler hacking][addmodfinalizer-local],
[ghc-heap-view][ghc-heap-view] and a novel use of
[static pointers][static-pointers]. A journey best told next time...

[addmodfinalizer-local]: https://phabricator.haskell.org/rGHC8d63419478074728eb03082787ea51d498b3e62e
[ghc-heap-view]: https://www.stackage.org/package/ghc-heap-view
[static-pointers]: https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#static-pointers

## The road ahead

There are plenty of solutions out there for lightweight interop across
languages. You can start by swapping JSON messages between separate
processes and take it from there. But for a truly universal solution
fit for all situations, our experience is that keeping any overheads
low or perhaps even nonexistent is the key enabler to seamlessly
mixing multiple languages and blithely crossing language boundaries
without guilt. In this post we presented a suite of packages for
high-speed Java/Haskell interop, which together ensure:

- **box-free foreign calls:** because we infer precise JVM types from
  Haskell types, arguments passed to JVM methods are boxed only if
  they need to be. Small values of primitive type can be passed
  to/from the JVM with no allocation at all on the heap.
- **marshalling-free argument passing:** Java objects can be
  manipulated as easily from Haskell as from Java. This means that you
  can stick to representing all your data as Java objects if you
  find yourself calling into Java very frequently, hence avoiding
  any marshalling costs when transferring control to/from the JVM.
- **type safe Java calls:** when calls are made in Java syntax, this
  syntax is supplied to an embedded instance of `javac` at
  compile-time for scope checking and type checking. That way we have
  a static guarantee that the types on the Haskell side match up with
  the types on the Java side, without having to resort to FFI stub
  generators and preprocessors.

We were fortunate enough to be able to stand on excellent libraries to
get here. Take parsing of Java syntax: that came straight from Niklas
Broberg and Vincent Hanquez's venerable [language-java][language-java]
library.

What we haven't addressed yet with `inline-java` is the perennial
issue when interoperating two garbage collected languages of automatic
memory management. Since we have two heaps (the GHC heap and the JVM
heap), with two garbage collectors, neither of which able to traverse
objects in the other heap, we are forced to pin in memory objects
shared across the language boundary. In the case of JVM objects, the
JNI does this for us implicitly, provided object references are kept
thread-local. It would be nice if we could make these object
references safe across threads and get both garbage collectors to
agree to dispose of them safely when dead. You can get a fair amount
of mileage the way things are: we managed to run
[topic analysis on all of Wikipedia][lda-wikipedia] concurrently on 16
machines and hours of machine time without tinkering with object
lifetimes and GC's.

So plenty more to do still! Make sure to check out the project's
[GitHub repository][inline-java] to follow progress and contribute.

[lda-wikipedia]: http://blog.tweag.io/posts/2016-06-20-haskell-compute-paas-with-sparkle.html
[language-java]: https://www.stackage.org/package/language-java
