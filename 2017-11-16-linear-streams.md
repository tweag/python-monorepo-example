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
by introducing
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

In the following code we could regard `s` as a `Stream` which produces
the strings `"a"`, `"b"` and `"c"`. But this depends on the context. The
second use of `s` produces an empty list actually.

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

Streams retain their intended meaning when they are used at most once.
Could the compiler help ensuring this?

# Linear types

With linear types we introduce _multiplicity_ of values; not only do
the type system track what sort of values we have, but also to which
extent they are used. An argument of a linear type has to be used — or
_consumed_ — exactly once in a function, or the program will not
compile. Both forgetting to use a linear value or using it
more than once will lead to a compile time error. This simple concept 
help us move a new category of programming errors from the
responsibility of the programmer to the compiler, making it easier
to write correct code with stronger guarantees about the implementation.

The following two simple functions take linear arguments, as indicated
by the special function arrows. However, the implementation treats them
in an unrestricted fashion. This mistake is caught by the compiler, and
the program is refused with a linearity error.

```haskell
frugal :: a ->. (a, a)
frugal x = (x, x)

wasteful :: a ->. ()
wasteful _ = ()
```

Linearity is a contract with the implementer of a function — never the
caller. This means that even if a function has linearity constraints it
can always be called without knowledge of this, with linear or
unconstrained types of arguments. The converse is of course not true:
if a function type does not guarantee linearity in an argument, it is
an error to pass it a linear value as such because it might be duplicated
or discarded in the function.

There are two aspects where linear types can help managing streams.
Firstly, ensuring that the effects of a stream are performed at most
once. Secondly, ensuring that resources associated to the stream,
such as file handles, are promptly released when the stream is no longer
needed.

# Linear streams

The type of streams is kept as `Stream f m r` were the type parameters
have almost the same meaning as for unrestricted streams. Most functions
in the new interface expect `m` to be an instance of[LMonad](https://github.com/m0ar/safe-streaming/blob/master/src/Control/Monad/LMonad.hs) and `f`
to be an instance of [LFunctor](https://github.com/m0ar/safe-streaming/blob/master/src/Data/Functor/LFunctor.hs).
With an `LMonad` we can introduce new streams as linear values in our 
programs.

`LMonad` is exactly what the name hints suggests, a _linear_ monad. The
type signatures of return and bind are familiar, but with the linear
arrow in place of the usual one:

```haskell
class LApplicative m => LMonad m where
  return :: a ->. m a
  (>>=) :: m a ->. (a ->. m b) ->. m b
```

For streams, this means the type of `>>=` enforces that a stream reference
has to be used linearly in the continuation. The consequence is that when 
we use a stream reference more than once, like we use `s` in the `fromHandle`
example above, this breaks the linearity constraints from bind, and the
error is caught at compile time! 


# Prompt finalization

To release resources associated to streams, the `streaming` package
relies on the
[MonadResource](https://www.stackage.org/haddock/lts-9.4/resourcet-1.1.9/Control-Monad-Trans-Resource.html#t:MonadResource)
class of the
[resourcet](https://www.stackage.org/lts-9.4/package/resourcet) package.
The resources are usually released when the end of the respective stream
is reached, but if the streams are not completely consumed, they are
released at the end of
[runResourceT](https://www.stackage.org/haddock/lts-9.4/resourcet-1.1.9/Control-Monad-Trans-Resource.html#v:runResourceT).

The [conduit](https://www.stackage.org/lts-9.4/package/conduit) package
goes a step beyond to have resources released immediately after a
`conduit` is used even if it has not been fully consumed. For this sake,
`conduit`s have finalizers which are executed immediately after a
`conduit` is used.

TODO: motivate the need to cleanup stream resources promptly 

In a similar way to `conduit`s, it is possible to attach finalizers to a
stream and have them called when the stream is last used. But we would
have to make the programmer responsible for remembering to run the
finalizers promptly. If the streams are linear values, then the type
checker can signal when a stream is not being disposed of.

TODO: discuss code of takes

``` haskell
takes :: (LMonad m, LFunctor f) => Int -> Stream f m r ->. Stream f m ()
takes n = splitsAt n >>= \s -> effect (fmap Return $ dropStream s)

dropStream :: Stream f m r ->. m ()
dropStream (Return _) = return ()
dropStream (Step fin fs) = fin >> return (unsafeDrop fs)
dropStream (Effect fin fs) = fin >> return (unsafeDrop fs)

unsafeDrop :: a ->. ()
unsafeDrop = unsafeCoerce (const ())

splitsAt :: forall f m r. (LMonad m, LFunctor f)
         => Int -> Stream f m r ->. Stream f m (Stream f m r)
```

# Summary

In this post we have shown how linear types can prevent some forms of
mistakes when writing streaming programs. This translates in simpler
side conditions for the programmer to check. In our example of the
`headLineStream` function, conditions (3) and (4) are discharged by
the type checker. And condition (2) is further approached by having
a finalizer of the input stream close the source right after the end
of the output stream is reached.

At this time, the GHC proposal for linear types is still under
discussion and an implementation of linear streams ready for production
has a long way ahead before becoming a reality. But we hope that this
post gives a perspective on how a future with linear types could be for
streaming programs.

This summer Tweag sponsored a Summer of Haskell project where Edvard
Hübinette worked with the [streaming library](https://github.com/m0ar/safe-streaming).
Here, linear types was leveraged to eliminate issues arising from
repeated monadic effects when accessing old stream references.
Linearity helped since it allowed the stream references to be used
only once. There were several interesting findings which are documented
in the [tech blog](https://m0ar.github.io/safe-streaming/). Compilation
of this project requires the [GHC prototype](https://github.com/tweag/ghc/tree/linear-types)
with linear types; installation instructions can be found in the
repository README.
