---
title: GHC plugins for language interoperation
author: Facundo Domínguez, Mathieu Boespflug
featured: yes
---

Language interoperation requires solving how to convert values from one
language to another. In addition, it requires a mechanism for
invoking code written in one of the languages from the other.
There are different flavours of the problem according to factors like
whether the languages are compiled or interpreted, the foreign interfaces
they offer and whether they use garbage collectors to manage memory.
In this post we present a technique to invoke code in a
statically-typed language from Haskell. To be concrete, we will
be summarizing our experience when writing
[inline-java](https://github.com/tweag/inline-java).

# Calling to Java

The package `inline-java` allows to invoke code written in Java using
a Haskell language feature known as quasiquotes.

```Haskell
-- hello.hs
{-# LANGUAGE QuasiQuotes #-}
import Foreign.JNI (withJVM)
import Language.Java.Inline

main :: IO ()
main = withJVM [] $ do
    let x = 1.5 :: Double
    y <- [java| { System.out.println($x);
	              return $x + 1;
	            } |]
    print (y :: Double)
```

The function `withJVM` starts an instance of the Java Virtual Machine (JVM),
and the `java` quasiquotation executes the java code given to it as a block
of statements.
In this example, the Haskell value `x` of type `Double` is coerced into
a Java value of primitive type `double`, which is then used whenever the
antiquoted variable `$x` appears inside the quasiquotation.
When the quasiquotation finishes executing, the Java value resulting
from evaluating `$x + 1` is coerced back to a Haskell value of type
`Double`. The program can be built and executed with

```
$ ghc hello.hs
$ ./hello
1.5
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

# Type-safety

A notable characteristic of this approach, is that we know at compile time
if types are correct. The Java quasiquotation can't return a Java object
when the Haskell side expects it to return a primitive `double`. Even if
the Haskell side expected an object, say of type `java.util.List`, the
Java quasiquotation can't return an object of type `java.lang.String` either.
And conversely for arguments, Java and Haskell need to agree on what the
type of arguments are, or a compile-time error ensues.

Type checking across language boundaries works as follows. First, GHC
infers the types of the antiquoted variables and the return type which
is expected of the quasiquotation. Then, these types are translated to
Java types. The translation is conducted by a machinery of type classes
living in the package [jvm](https://github.com/tweag/inline-java/jvm).
The particular innards of this is not interesting to us now, but what
matters is that it allows to translates types across languages. For
instance,
| Haskell type | Java type         |
|:-------------|------------------:|
| Double       |            double |
| [Double]     |          double[] |
| ByteString   |            byte[] |
| Text         |  java.lang.String |

The `javac` compiler gets the translated type with the quasiquotation.
In our running example this would be

```Java
double fresh_name(double $x) {
    System.out.println($x);
    return $x + 1;
}
```

Finally, the `javac` compiler type-checks the quasiquotation. Type
mismatches would be discovered and reported at this stage.

By far, the first step has been the trickiest to pull off in this process.
Namely, figuring out which types GHC has inferred for the antiquoted
variables and which type is expected of the quasiquotation.

# Looking for the types

At first, finding the types looked trivial. There is this Template
Haskell primitive called
[reify](https://www.stackage.org/haddock/lts-9.0/template-haskell-2.11.1.0/Language-Haskell-TH.html#v:reify).
```Haskell
reify :: Name -> Q Info

data Info =
      ...
    | VarI Name Type (Maybe Dec)	
      ...
```

If we have an antiquoted variable `$x`, surely we can use `reify 'x` to
learn the Haskell type. Well, this doesn't quite work, because type checking
is not finished when `reify` is evaluated. Thenceforth, we went
downhill proposing patches to Template Haskell in order to get our hands
on the inferred types.
If you want to check it, here there are the related issues for
your amusement
([1](https://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/Reify),
[2](https://ghc.haskell.org/trac/ghc/ticket/12777),
[3](https://ghc.haskell.org/trac/ghc/ticket/12778),
[4](https://ghc.haskell.org/trac/ghc/ticket/13608)).

After many discussions with Simon Peyton Jones, and some deal of
creative hacking, we could kind of get the inferred types for antiquoted
variables, but only for as long as the java quasiquotation didn't appear
inside Template Haskell brackets (`[| ... |]`). Moreover, we made no
progress getting the expected type of the quasiquotation.
Every idea we came up with, required difficult compromises in the design.
In the meantime, we had to choose between checking at runtime the type
of the values returned by quasiquotations or using unsafe coercions.

Eventually, it came to our attention that Template Haskell was not the
only way to query the output of the type checker.

# Enter GHC Core plugins

In essence, a
[Core plugin](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/extending_ghc.html#core-plugins-in-more-detail)
is a set of Core-to-Core passes that we can
ask GHC to add to the compilation pipeline. The passes can be inserted
anywhere in the Core pipeline, and in particular, they can be inserted
right at the beginning, just after typechecking and desugaring have
occurred.
Terms in the internal Core language carry the types that have been
inferred at the typechecking stage, making possible to see these types
by walking over the Core tree.

Quasiquotations dissapear from the abstract syntax tree when Template
Haskell is executed. This happens well before the plugin passes.
In order to enable the plugin to find the location of the
quasiquotations, the quasiquoter can insert some artificial
function call as a beacon or marker. In `inline-java`, the program
looks something as follows after Template Haskell runs.

```Haskell
main :: IO ()
main = withJVM [] $ do
    let x = 1.5 :: Double
    y <- qqMarker
	   (Proxy :: Proxy "{ System.out.println($x); return $x + 1; }")
	   x
    print (y :: Double)

qqMarker :: forall input args r. Proxy input -> args -> IO r
qqMarker = error "inline-java: The Plugin is not enabled."
```

The GHC Plugin is supposed to replace the call to qqMarker with an
appropriate call to the generated Java method. The all-important
point, however, is that the calls to `qqMarker` are annotated with the
types we want to learn in Core.

```Haskell
main :: IO ()
main = ...
       qqMarker
         @ "{ System.out.println($x); return $x + 1; }"
         @ Double
         @ Double
	   ...
```

The second and third type parameter give us the types of the antiquoted
variable and the expected type of the quasiquotation. From here, the
plugin has all the information it needs to generate the Java code to
feed to `javac`. In addition, the plugin can inject the generated bytecode
in the object file of the module, and it arranges for this bytecode to
be located at runtime so it can be loaded in the JVM.

Now the user needs to remember to tell GHC to use the plugin by passing it
the option `-fplugin=Language.Java.Inline.Plugin`. But this is only until
Template Haskell earns
[the ability to tell GHC which plugins to use](https://phabricator.haskell.org/D3821).

# Summary

By using a GHC plugin, we have simplified `inline-java` from a
complicated spaghetti which sprung from attempting to use 
Template Haskell's `reify` and didn't quite addressed the type lookup
problem in a robust way. Now we have a linear story which starts by
introducing the `qqMarker` beacons, attaches the Java bytecode in the
plugin phase and ends by loading it at runtime into the JVM.

Writing a compiler plugin is similar to writing Template Haskell code.
Both approaches need to manipulate abstract syntax trees. The plugin
approach can be regarded as more coupled with a particular version of
the compiler, since it relies on the internal Core language. However,
Core changes relatively little over the years, and anyway, a pass that
looks for some markers is hardly going to change a lot even if Core
did change.

Many thanks to Simon Peyton Jones for its patience to walk with us over
our attempts to _fix_ Template Haskell. Without a dialog with the
compiler implementors it would have been difficult to scout as wider a
range of the design space.
