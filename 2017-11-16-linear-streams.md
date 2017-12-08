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
once. Secondly, ensuring that resources associated to the stream,
such as file handles, are promptly released when the stream is no longer
needed.

The type of streams is kept as
[Stream f m r](https://www.stackage.org/haddock/lts-9.17/streaming-0.1.4.5/Streaming-Internal.html#t:Stream)
were the type parameters
have almost the same meaning as for unrestricted streams. Most functions
in the new interface expect `m` to be an instance of [LMonad](https://github.com/m0ar/safe-streaming/blob/master/src/Control/Monad/LMonad.hs) and `f`
to be an instance of [LFunctor](https://github.com/m0ar/safe-streaming/blob/master/src/Data/Functor/LFunctor.hs).
With an `LMonad` we can introduce new streams as linear values in our 
programs.

`LMonad` is class offering methods `return` and `>>=` similar to those of
the `Monad` class, but where arguments have been changed to have a
linear multiplicity.

```haskell
class LApplicative m => LMonad m where
  return :: a ->. m a
  (>>=) :: m a ->. (a ->. m b) ->. m b
```

For streams, the type of `>>=` enforces that a stream reference is
used linearly in the continuation. The consequence is that when
we use a stream reference more than once, like we use `s` in the `fromHandle`
example above, this breaks the linearity constraints from `>>=`, and the
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
`conduit` is used. In the absence of linear types, it is still the
responsibility of the programmer to make sure that a conduit is ever
used. Otherwise, its finalizers won't be executed until leaving the
scope of `runResourceT`.
If an application handles many files or connections using streams,
closing them soon after they are last used is essential to keep it
within the limits for open files and connections.

In a similar way to `conduit`s, it is possible to attach finalizers to a
stream and have them called when the stream is last used. If the streams
are linear values, then the type checker can signal when a stream is not
being disposed of.
Here is a modification of the `Stream` data type from the
[streaming](http://www.stackage.org/package/streaming)
package.

``` haskell
data Stream f m r where
  Step :: m () -> !(f (Stream f m r)) ->. Stream f m r
  Effect :: m () -> m (Stream f m r) ->. Stream f m r
  Return :: r ->. Stream f m r

instance (LFunctor f, LMonad m) => LMonad (Stream f m r) where
  ...
```

`Step` gives data from the stream in a structure of type
`f (Stream f m r)` and a finalizer of type `m ()`.
`Effect` gives a monadic computation that produces more of the stream
and also a finalizer of type `m ()`.
`Return` terminates a stream and yields a final value of type `r`.
When a stream reads values from a file handle, all the `Step` and `Effect`
constructors have finalizers which close the handle.

``` haskell
import Streaming (Of(:>))
import System.IO (Handle, hClose, hGetLine, hIsEOF)

fromHandle :: MonadIO m => Handle -> Stream (Of String) m ()
fromHandle h = Effect (hClose h) $ do
    eof <- hIsEOF h
    if eof then do
      hClose h
      return (Return ())
    else do
      s <- hGetLine h
      return $ Step (hClose h) (s :> fromHandle h)
```

We provide a function to finalize a stream, which purpose is to
be able to close the handle without being forced to read all of the file
contents.

``` haskell
dropStream :: LMonad m => Monad Stream f m r ->. m ()
dropStream (Return _) = return ()
dropStream (Step fin fs) = fin >> return (unsafeDrop fs)
dropStream (Effect fin fs) = fin >> return (unsafeDrop fs)

unsafeDrop :: a ->. ()
unsafeDrop = unsafeCoerce (const ())
```

There is a price to have `dropStream`, though. We no longer guarantee
that linear values placed in a linear stream will be used exactly once.
This guarantee is lost when resorting to `unsafeDrop`. Thus, we can
write the following function which doesn't use its argument despite of
having multiplicity 1.

``` haskell
dropLinear :: a ->. m ()
dropLinear a = dropStream (Step (return ()) (a :> Return ()))
```

TODO: Can we somehow avoid `dropLinear` from being accepted by the compiler?
      (so the programmer is forced to use an unrestricted a)

We can still ensure, though, that all finalizers will run promptly and
that no value in a linear stream will be consumed more than once. We
illustrate the first point with the implementation of `takes`.
We use `splitAt` to get a stream with the first `n` elements` of the
input. Then we drop the reminder with dropStream. Note that we can't
forget to drop the reminder as we are in a linear monad.

``` haskell
takes :: (LMonad m, LFunctor f) => Int -> Stream f m r ->. Stream f m ()
takes n s = splitsAt n s >>= \rem -> effect (fmap Return $ dropStream rem)

splitsAt :: forall f m r. (LMonad m, LFunctor f)
         => Int -> Stream f m r ->. Stream f m (Stream f m r)
```

# Summary

We have shown how linear types can prevent some forms of
mistakes when writing streaming programs. This translates in simpler
side conditions for the programmer to check. In our example of the
`headLineStream` function, conditions (3) and (4) are discharged by
the type checker. And condition (2) is further approached by having
a finalizer of the input stream close the source right after the end
of the output stream is reached.

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
