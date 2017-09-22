---
title: "GHC compiler plugins in the wild:<br/> typing Java"
author: Facundo Domínguez, Mathieu Boespflug
featured: yes
---

[Previously][inline-java-tutorial], we discussed how to
*use* [inline-java][inline-java-stackage] to call any Java function
from Haskell. The reverse is also possible, though that will be a topic
for a future post. In this post, we'll dive underneath the hood to
talk a little about *how* inline-java does its deed.

You might find it an interesting read for at least the following
reason: since the latest v0.7 release of inline-java, it's an example
use of a recent feature of GHC called compiler plugins. These allow
you to introspect and transform types and the abstract syntax tree
before handing them off to later stages of the compiler pipeline. We
use this to good effect in order to check that argument and return
types on the Java side line up with those on the Haskell side (and
*vice versa*).

[inline-java-tutorial]: ./2017-09-15-inline-java-tutorial.html
[inline-java-stackage]: https://www.stackage.org/package/inline-java

# Calling Java

`inline-java` makes it possible to invoke code written in Java using
a Haskell language feature known
as
[quasiquotes](https://scholar.google.com/citations?view_op=view_citation&citation_for_view=jjWDm9wAAAAJ:2osOgNQ5qMEC).

```Haskell
{-# LANGUAGE QuasiQuotes #-}
import Language.Java (withJVM)
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
and the `java` quasiquotation executes the Java code passed to it as a block
of statements.

In this example, the Haskell value `x` of type `Double` is coerced into
a Java value of primitive type `double`, which is then used whenever the
antiquoted variable `$x` appears inside the quasiquotation.
When the quasiquotation finishes executing, the Java value resulting
from evaluating `$x + 1` is coerced back to a Haskell value of type
`Double`.

GHC doesn't parse or generate any Java. Neither does `inline-java`.
So how can this program possibly work? The answer is that `inline-java`
feeds the quasiquotation to the `javac` compiler, which generates some
bytecode that is stored in the object file of the module. At runtime,
`inline-java` arranges for the bytecode to be handed to the JVM using
the [jni](https://www.stackage.org/package/jni) package.
Finally, `inline-java` makes use of the
[`jvm`](https://www.stackage.org/package/jvm) package
to have the bytecode executed.

# Type safety

A notable characteristic of this approach is that we know at compile
time if types are correct. We know that Java won't return an object if
on the Haskell side we expect a double, because the Java side knows
it's on the hook for handing us a double. `javac` will raise a compile
time error if the Java code doesn't do that. Even if the Haskell side
expected an object, say of type `java.util.List`, the Java
quasiquotation can't return an object of type `java.lang.String`
either. And conversely for arguments, Java and Haskell need to agree
on the type of arguments, or a compile-time error ensues.

Given that no one compiler analyses both languages, how can
type-checking work across language boundaries? Fortunately, both
compilers can be put to cooperate on the task. First, GHC
infers the types of the antiquoted variables and the return type which
is expected of the quasiquotation. Then, these types are translated to
Java types. The translation is conducted by a machinery of type classes
living in the [jvm](https://github.com/tweag/inline-java/jvm) package.
The details of this process are not important at this point. What
matters is that it enables us to translate types across languages. For
instance,

| Haskell type | Java type         |
|:-------------|------------------:|
| Double       |            double |
| [Double]     |          double[] |
| ByteString   |            byte[] |
| Text         |  java.lang.String |

The translated types are passed to `javac` together with the rest of the
quasiquoted Java code. In our running example this would be

```Java
double fresh_name(double $x) {
    System.out.println($x);
    return $x + 1;
}
```

Finally, the `javac` compiler type checks the quasiquotation. Type
mismatches will be discovered and reported at this stage.

It turns out that the first step is by far the most intricate.
Specifically, for inline-java to query the types that GHC inferred for
the antiquoted variables, and also query the type of the entire
quasiquotation.

# Looking for the types

At first, it appears as if determining these types is trivial. There is
a Template Haskell primitive called
[reify](https://www.stackage.org/haddock/lts-9.0/template-haskell-2.11.1.0/Language-Haskell-TH.html#v:reify).
```Haskell
reify :: Name -> Q Info

data Info =
      ...
    | VarI Name Type (Maybe Dec)	
      ...
```

Given an antiquoted variable `$x`, we ought to be able to use `reify 'x`
to determine its Haskell type. Well, except that this doesn't quite
work, because type checking is not finished when `reify` gets evaluated. From
there, we went down a rabbit hole of trying to propose patches to
Template Haskell to reliably get our hands on the inferred types.
If you want to follow the intricacies of our journey, here are the
related GHC issues for your amusement:
[initial discussion](https://ghc.haskell.org/trac/ghc/wiki/TemplateHaskell/Reify),
[12777](https://ghc.haskell.org/trac/ghc/ticket/12777),
[12778](https://ghc.haskell.org/trac/ghc/ticket/12778),
[13608](https://ghc.haskell.org/trac/ghc/ticket/13608).

After many discussions with Simon Peyton Jones, and some deal of
creative hacking, we could kind of get the inferred types for antiquoted
variables, but only for as long as the java quasiquotation didn't appear
inside Template Haskell brackets (`[| ... |]`). Moreover, we made no
progress getting the expected type of the quasiquotation.
Every idea we came up with required difficult compromises in the design.
In the meantime, we had to choose between checking the type of the
values returned by quasiquotations at runtime or using unsafe coercions,
neither of which is an attractive option.

Eventually, we learnt that Template Haskell was not the only way to
query the output of the type checker.

# Enter GHC Core plugins

The GHC compiler uses an explicitly typed intermediate language known as
Core. All type applications of terms in Core are explicit, making it
possible to learn the types inferred at the type checking phase
by inspecting Core terms.
In order to get our hands on Core terms, we can use
[Core plugins](https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/extending_ghc.html#core-plugins-in-more-detail).
We could think of a Core plugin as a set of Core-to-Core passes that we
can
ask GHC to add to the compilation pipeline. The passes can be inserted
anywhere in the Core pipeline, and in particular, they can be inserted
right after desugaring, the phase which generates Core from the abstract
syntax tree of a Haskell program.

Quasiquotations disapear from the abstract syntax tree when Template
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
	   "{ System.out.println($x); return $x + 1; }"
	   x
    print (y :: Double)

qqMarker :: forall args r. String -> args -> IO r
qqMarker = error "inline-java: The Plugin is not enabled."
```

The GHC Plugin is supposed to replace the call to `qqMarker` with an
appropriate call to the generated Java method. The all-important
point, however, is that the calls to `qqMarker` are annotated with the
types we want to determine in Core.

```Haskell
main :: IO ()
main = ...
       qqMarker
         @ Double
         @ Double
         "{ System.out.println($x); return $x + 1; }"
	   ...
```

The type parameters provide us with the type of the antiquoted
variable and the expected type of the quasiquotation. From here, the
plugin has all the information it needs to generate the Java code to
feed to `javac`. In addition, the plugin can inject the generated bytecode
in the object file of the module, and it arranges for this bytecode to
be located at runtime so it can be loaded in the JVM.

Now the user needs to remember to tell GHC to use the plugin by passing it
the option `-fplugin=Language.Java.Inline.Plugin`. But this is only until
Template Haskell learns
[the ability to tell GHC which plugins to use](https://phabricator.haskell.org/D3821).

# Summary

By using a GHC plugin, we have simplified `inline-java` from a
complicated spaghetti which sprung from attempting to use 
Template Haskell's `reify` and didn't fully addressed the type lookup
problem in a robust way. Now we have a straight forward story which
starts by
introducing the `qqMarker` beacons, attaches the Java bytecode in the
plugin phase and ends by loading it at runtime into the JVM.

Writing a compiler plugin is similar to writing Template Haskell code.
Both approaches need to manipulate abstract syntax trees. The plugin
approach can be regarded as more coupled with a particular version of
the compiler, since it relies on the internal Core language. However,
Core changes relatively little over the years, and anyway, a pass that
looks for some markers is hardly going to change a lot even if Core
did change.

Many thanks to Simon Peyton Jones for his patience to walk with us over
our attempts to _fix_ Template Haskell. Without this dialog with the
compiler implementors, it would have been difficult for us to explore as
much of the design space as we needed to.
