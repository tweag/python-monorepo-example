---
title: Streaming with linear types
author: Edvard Hübinette and Facundo Domínguez
---

In
[an earlier blog post](http://www.tweag.io/posts/2017-07-27-streaming-programs.html)
we have discussed how streaming libraries
help writing composable programs without lazy I/O. We showed, for
a simple program, how using a streaming library helps reducing the
amount of unaided bookkeeping that the programmer needs to conduct
to make it correct. In this post we delve further in that direction
by considering
[linear types](http://www.tweag.io/posts/2017-03-13-linear-types.html)
and uncover their potential to have the compiler do more checks.

# Streaming today

Let us recap our simple streaming example. We implemented a
function that yields the first line of input. Both the input and
the output are effectful bytestrings as defined in the
[streaming-bytestring](http://www.stackage.org/package/streaming-bytestring)
package.

```Haskell
import qualified Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming.Char8 as SB
import qualified Streaming

headLineStream :: Monad m => ByteString m r -> ByteString m ()
headLineStream = SB.unlines . Streaming.takes 1 . SB.lines
```

This function transforms an effectful bytestring. It might not reside
fully in memory, but it may be produced in chunks as the bytestring is
consumed. In contrast to lazy `ByteString`s, the effectful bytestring
produces the chunks in the monad `m` rather than through lazy
evaluation. 

There are some side conditions to ensure that the resulting program is
streaming and correct.
 1. The output `ByteString` is not fed to any function that loads all of
   the output into memory like
   [SB.toStrict](https://www.stackage.org/haddock/lts-8.22/streaming-bytestring-0.1.4.6/Data-ByteString-Streaming-Char8.html#v:toStrict),
 2. the source of the input `ByteString` needs to be closed *soon enough* to
   prevent open handles from accumulating,
 3. the output `ByteString` shall not be used after the source of the
   input `ByteString` is closed, and
 4. the input bytestring shall not be given to any other function.
 
We neglected to include the last condition in our earlier post,
but it is necessary because otherwise the effects producing the
contents of the input would be replayed yielding an undefined outcome.
This is left implicit in the documentation of most streaming libraries.
Suppose we used
[fromHandle](https://www.stackage.org/haddock/lts-9.4/streaming-0.1.4.5/Streaming-Prelude.html#v:fromHandle).

``` haskell
-- | Read Strings from a Handle using hGetLine. Terminates on end of input.
fromHandle :: MonadIO m => Handle -> Stream (Of String) m ()
```

In the following code, `s` is a `Stream` producing the strings `"a"`,
`"b"` and `"c"`. We want to accumulate the stream elements in two lists
and compare them, expecting them to be the same. However, the two
evaluations of `toList_ s` yield different results actually, because
the meaning of `s` depends on whether it has been evaluated before.
In this case, extracting elements from `s` causes the source handle
to read a file. The first evaluation of `toList_ s` reads the whole
file. When we reach the second evaluation of `toList_ s`, the
source handle has reached the end of the file, and we get the empty
list.

``` haskell
writeFile "stream.txt" "a\nb\nc\n"
h <- openFile "stream.txt" ReadMode
s <- fromHandle h
xs1 <- Streaming.Prelude.toList_ s
xs2 <- Streaming.Prelude.toList_ s
return (xs1 == xs2)
```

This is not a problem of streaming libraries exclusively. If using lazy
`String`s or `ByteString`s, passing the input to another
function would mean that its contents might be retained until all
functions are evaluated, making the program non-streaming.

Argueably, we could refine the meaning of `s` so we say instead that
it is some sort of state machine that reads lines from a file. And
then the functions we write on streams need to document in which state
the streams are left when they return. But in practice, the norm is to
regard a stream reference as invalid once it has been inspected, and if
one element has been extracted, processing should continue on the tail
of the stream. Could the compiler help ensuring that we don't use
invalid stream references?

# Linear streams

With linear types we introduce _multiplicity_ of values; not only does
the type system track what sort of values we have, but also to which
extent they are used. An argument of a linear type has to be used — or
_consumed_ — exactly once in a function, or the program will not
compile. Both forgetting to use a linear value or using it
more than once will lead to a compile time error. This simple concept 
help us move a new category of programming errors from the
responsibility of the programmer to the compiler, making it easier
to write correct code with stronger guarantees about the implementation.

Linearity is a contract with the implementer of a function — never the
caller. This means that even if a function has linearity constraints it
can always be called without knowledge of this, with linear or
unconstrained types of arguments. The converse is of course not true:
if a function type does not guarantee linearity in an argument, it is
an error to pass it a linear value as such because it might be duplicated
or discarded in the function.

There are two aspects where linear types can help managing streams.
Firstly, ensuring that the effects of a stream are performed at most
once. Secondly, ensuring that linear values contained in the stream are
eventually used.

The type of streams is kept as
[Stream f m r](https://www.stackage.org/haddock/lts-9.17/streaming-0.1.4.5/Streaming-Internal.html#t:Stream)
were the type parameters
have almost the same meaning as for unrestricted streams. Most functions
in the new interface expect `m` to be an instance of [LMonad](https://github.com/m0ar/safe-streaming/blob/master/src/Control/Monad/LMonad.hs) and `f`
to be an instance of [LFunctor](https://github.com/m0ar/safe-streaming/blob/master/src/Data/Functor/LFunctor.hs).
With an `LMonad` we can introduce new streams as linear values in our 
programs.

`LMonad` is a class offering methods `return` and `>>=` similar to those of
the `Monad` class, but where arguments have been changed to have a
linear multiplicity.

```haskell
class LMonad m where
  return :: a ->. m a
  (>>=) :: m a ->. (a ->. m b) ->. m b
```

For streams, the type of `>>=` enforces that a stream reference is
used linearly in the continuation. The consequence is that when
we use a stream reference more than once, like we use `s` in the `fromHandle`
example above, this breaks the linearity constraints from `>>=`, and the
error is caught at compile time!

# Linear vs affine streams

Instead of using linear types, we could have used affine types. That is,
an argument of an affine type can be used or consumed at most once in the
body of a function. Unlike linear types, arguments of affine types can be
ignored without the compiler flagging an error.

After all, in the previous discussion, we are only concerned with our
stream references not being used more than once. A consequence of using
linear types is that we cannot implement

```haskell
take :: LMonad m => Stream (Of a) m r -> Stream (Of a) m ()
```

If we had a stream `s` which produces a sequence of linear values, the
expression `take 0 s` would cause all the values to be dropped, which the
compiler would not stand. This is a compelling reason to use affine streams
instead of linear streams. However, then we can not store or produce linear
values in our affine streams. And we have at least one case where we would
like to do this: marshaling Java iterators from/to Haskell streams.

Suppose we are writting a program which is written both in Java and
Haskell. Let us suppose further that, on the Java side, we have an iterator
of type `java.util.Iterator<Object>`. To use it on the Haskell side, we
would like to marshal it to Haskell as a stream of type
`Stream (Of JObject) m ()`, where `m` is a linear monad and `JObject` is
the Haskell type of references to Java objects.
As we explain in
[an earlier post](https://www.tweag.io/posts/2017-11-29-linear-jvm.html),
we want to treat Java references linearly to make sure that they are
promptly deleted when they are no longer used, hence the requirement
to have the parameter `m` be a linear monad.

```Haskell
-- IOL is a linear version of the IO monad.
instance LMonad IOL where
  ...

-- JIterator stands for a reference to an iterator on the Java side.
mapIterator :: (JObject ->. JObject) -> JIterator ->. IOL JIterator
mapIterator f jiterator =
    iteratorToStream jiterator >>= streamToIterator . linearMap f
  where
    iteratorToStream :: JIterator ->. Stream (Of JObject) IOL ()
    streamToIterator :: Stream (Of JObject) IOL () ->. IOL JIterator

linearMap :: LMonad m => (a ->. b) -> Stream (Of a) m r ->. Stream (Of b) m r
```

Thanks to linear types, the compiler could check that the `jiterator`
reference is deleted (inside `iteratorToStream`), and it can check
that the references that the intermediate stream produces are
eventually deleted too.

# Summary

We have shown how linear types can prevent some forms of
mistakes when writing streaming programs. This translates in simpler
side conditions for the programmer to check. In our example of the
`headLineStream` function, conditions (3) and (4) are discharged by
the type checker. Moreover, linear streams allow to produce linear
values from the stream which allows using them in combination with
other resources that need to be treated linearly.

A few questions remain open. For instance, it has to be seen if it
is practical to have a single implementation of streams that can be
used as linear, affine or unrestricted depending on the context.
Otherwise we might end up with three similar implementations where
we would prefer to avoid the code duplication.
Another question is in which use cases affine streams would be a
good fit, whereas linear streams would be not.

At this time, the GHC proposal for linear types is still under
discussion and a strong implementation of linear streams has a long way
to go before becoming a reality. But we hope that this
post gives a perspective on how a future with linear types could be for
streaming programs.

For those interested, there is a
[prototype of GHC](https://github.com/tweag/ghc/tree/linear-types)
which implements linear types. Related to linear streams, this summer
Tweag sponsored a Summer of Haskell project where Edvard
Hübinette worked with this prototype and the
[streaming package](https://github.com/m0ar/safe-streaming).
Several interesting findings are documented
in the [tech blog](https://m0ar.github.io/safe-streaming/).
