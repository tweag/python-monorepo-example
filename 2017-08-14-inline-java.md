---
title: Painless Java for the Functional Programmer
author: Facundo Domínguez
featured: yes
---

In our
[introductory post](http://www.tweag.io/posts/2016-10-17-inline-java.html)
for
[inline-java](https://github.com/tweag/inline-java)
we showed how calling methods written in Java from Haskell was possible.
In addition, it was shown how Haskell's type system was leveraged to
polish rough edges of using low level interfaces like the
[Java Native Interface (JNI)](https://docs.oracle.com/javase/8/docs/technotes/guides/jni/spec/jniTOC.html).

In this post we bring a tutorial with practical aspects of using
`inline-java`, from marshalling values between Haskell and Java to
type checking that both sides exchange values of the types they
are expected to.

# Invoking java methods

Let's start with a simple program.

```Haskell
-- hello.hs
{-# LANGUAGE QuasiQuotes #-}
import Foreign.JNI (withJVM)
import Language.Java.Inline

main :: IO ()
main = withJVM [] [java| { System.out.println("Hello Java!"); } |]
```

The function `withJVM` starts an instance of the Java Virtual Machine (JVM),
and the `java` quasiquotation executes the java code given to it as a block
of statements. The program can be built and executed with

```
$ ghc hello.hs
$ ./hello
Hello Java!
```

GHC doesn't parse or generate any Java. Neither does `inline-java`.
So how can this program possibly work? The answer is that `inline-java`
feeds the quasiquotation to the `javac` compiler, which generates some
bytecode that is stored in the object file of the module. At runtime,
`inline-java` arranges the bytecode to be given to the JVM using the
package [jni](https://github.com/tweag/jni).
Finally, inline-java makes use of the package
[jvm](https://github.com/tweag/jvm)
to have the bytecode executed.

# Marshalling values

Now, suppose we have some value in Haskell that we want to provide as
argument to a java method. 

```Haskell
main :: IO ()
main = withJVM [] $ do
    let d = 1 :: Double
    [java| { System.out.println($d); } |]
```

We are coercing a Haskell value of type `Double` into a Java value of
the primitive type `double`, which is then used in the quasiquotation.
When `inline-java` gives this quasiquotation to `javac`, it feeds if a
method of the form

```Java
void fresh_name(double $d) { System.out.println($d); }
```

At runtime, `inline-java` passes the result of the coercion as
the argument `$d`. Any instance of `Language.Java.Coercible a ty` can be
used in the same way, where `a` stands for the Haskell type and `ty`
stands for an encoding of the Java type.
The package `jvm` defines a few instances, and the user can
define its own.

```Haskell
instance Coercible Bool ('Prim "boolean")
instance Coercible CChar ('Prim "byte")
instance Coercible Char ('Prim "char")
instance Coercible Word16 ('Prim "char")
instance Coercible Int16 ('Prim "short")
instance Coercible Int32 ('Prim "int")
instance Coercible Int64 ('Prim "long")
instance Coercible Float ('Prim "float")
instance Coercible Double ('Prim "double")
instance Coercible () 'Void
instance Coercible (J ty) ty
```

In the following program we get an integer value from Java.

```Haskell
...
import Data.Int (Int32)

main :: IO ()
main = withJVM [] $ do
    x <- [java| new Object[5].length |]
    print (x :: Int32)
```

Here we have dropped the braces surrounding the Java code in order to
hint to `inline-java` that we are giving an expression rather than a
block of statements.
We are coercing a Java value of type `int` into a Haskell
value of type `Int32`. The quasiquoter arranges for the coercion to
happen after the JVM finishes evaluating the Java expression.

As it was the case for using Haskell values in quasiquotations, the
return type of the quasiquotation needs to be an instance of 
`Language.Java.Coercible a ty`.

# Marshalling Java objects

Coercing values is useful enough until we consider how to marshal values
which do not have an obvious counterpart in Java. For instance, what do
we coerce a Haskell list or a vector to?
For these types, which would deserve a more elaborate representation in
Java, we use the classes `Reflect` and `Reify` from the package `jvm`.

```Haskell
type family Interp a

class Reify a where
  reify :: J (Interp a) -> IO a

class Reflect a where
  reflect :: a -> IO (J (Interp a))
```

The type family `Interp a` stands for the Java type that
corresponds to the Haskell type `a`. A value of type `J (Interp a)`
is a reference to a Java object of type `Interp a`.
With `reify` we can convert a java object to a Haskell value.
With `reflect` we can convert a Haskell value back into a Java object.

As with the type class `Coercible`, the package `jvm` already provides a
few instances of `Reify` and `Reflect`. For example

```Haskell
type instance Interp ByteString = 'Array ('Prim "byte")
instance Reify ByteString
type instance Interp Text = 'Class "java.lang.String"
instance Reify Text
type instance Interp Double = 'Class "java.lang.Double"
instance Reify Double
type instance Interp [a] = 'Array (Interp a)
instance Reify a => Reify [a]
```

There is an instance of `Coercible (J ty) ty`. So we can use
references produced with `reflect` in `java` quasiquotations.

```Haskell
...
import qualified Data.Text
import Language.Java (reflect)

main :: IO ()
main = withJVM [] $ do
    text <- reflect (Data.Text.pack "Hello Java!")
    [java| { System.out.println($text); } |]
```

In this example, `text` has type `J ('Class "java.lang.String")` and
the antiquotation variable `$text` is expected to have type
`java.lang.String`.

Conversely, we can use `reify`, to create a Haskell value from the
reference produced by a quasiquotation.

```Haskell
main :: IO ()
main = withJVM [] $ do
    jarray <- [java| new String[] {"a", "b"} |]
    xs <- reify jarray
    print (xs :: [Text])
```

# Type checking

One of the strengths of `inline-java` is that it makes it difficult to
get an ill-typed interaction between Haskell and Java. What if a
quasiquotation returned a value of a type unexpected to the Haskell
side? What if any of the methods used in the quasiquotation are used
with arguments of the wrong type?

The short answer to both questions is that most of the time, GHC and
`javac` will catch the type mistakes.

```Haskell
main :: IO ()
main = withJVM [] $ do
    jarray <- [java| new String[] {"a", "b"} |]
    xs <- reify jarray
    print (xs :: [Double])
```

Based on the instances of `Reify` and `Coercible` that are in scope,
Haskell is able to pin with precision what Java type the quasiquotation
should return. In this program, the Haskell side expects Java to return
an array of doubles (`java.lang.Double[]` when the Java side is
returning an array of strings (`java.lang.String[]`). The `javac`
compiler complains.

```
$ ghc hello.hs
[1 of 1] Compiling Main             ( Main.hs, Main.o )
.../Inline__main_Main.java:5: error: incompatible types: String[] cannot be converted to Double[]
{ return  new String[] {"a", "b"} ; } // .hs:10
          ^
```

When making calls with wrong argument or return types to Java methods,
the functions in the package `jvm` would produce a runtime error.
Usually the programmer would get an exception called `NoSuchMethodError`
and the name of the offending method. The error produced by
`inline-java` improves this in two aspects. Firstly, we get the error at
build time. Secondly, the error message points precisely at either the
return type or the argument with the mismatched type. 

Is there any type error that cannot be caught at build time? There is,
indeed. For instance, a quasiquotation can yield or use objects of type
`java.lang.Object`. The Haskell or the Java side may need then to
downcast these objects, which can fail if the objects are downcasted to
the wrong type. In this sense, Haskell + Java is no safer than Java
alone.

# Summary

In this blogpost we have covered the basics of marshalling values
between Haskell and Java, and using `inline-java` for invoking Java
methods.
The package `inline-java` not only makes Java code convenient to embed
in Haskell programs, it also prevents coding mistakes which could
otherwise occur when relying on the lower-level packages `jni` and
`jvm`.

You might have noticed that in our examples we are creating references
to java objects, yet this references are never explicitly destroyed.
In a future blogpost we will see how to manage these java references.
