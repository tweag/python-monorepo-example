---
title: Streaming programs for the layman
author: Facundo Domínguez and Mathieu Boespflug
---

In school, we're taught that I/O is a simple affair: read a bunch of
data in, spit some data out. Rinse, repeat. But then as practitioners
we realize things are often more complicated. For one, I/O is slow, so
we probably want operations to overlap (i.e. be processed
asynchronously), if there's lots of them that we want to perform. In
this post, we'll talk about another topic that will tickle the ear of
any functional programmer at some point along the infinite path to
enlightenment: streaming resources. Ever wondered what that's about?
This post is an attempt at explaining why you'd want to think about
it.

# Streaming programs

Let's say we want to write a small utility that truncates any input to
its topmost line. We can start with a pure function from input to
output:

```Haskell
headLine :: String -> String
headLine = unlines . take 1 . lines
```

Simple enough. We could hook up this function to an input source
somewhere on disk and some output sink like an output file.
This program would use resources like memory, CPU time, file
descriptors and disk space.

If the amount of memory does not grow beyond a finite bound, for all
possible inputs, we say that the program runs in bounded memory. And
more generally, we will say that **a program is streaming** if the
usage of some resources considered *scarce* is bounded.

In our example we care of memory and file handles. It's
important to tame RAM usage, because the amount of fast volatile
memory available on a computer is typically far smaller than the size
of the program's input. Likewise, file descriptors aren't a commodity
in infinite supply: operating systems impose aggressive
per-process limits by default.

Disk space, and sometimes even CPU time, are comparatively far less
scarce, so we won't worry about those here.

It can be hard to reconcile the constraints of resource scarcity with
another imperative: don't give up on writing programs from composable
pieces that can be well understood in isolation from each other, lest
you end up with unmaintainable spaghetti. This is where streaming
libraries can help: the idea is to define once and for all common
patterns that enable building streaming *and* compositional programs.


# Writing a streaming program

Resuming our running example, we could make streaming program from
function `headLine` provided that we satisfy the following conditions:

* evaluation of the output string should not be forced into memory all
  at once by any callers of `headLine`, and
* the source of the input string needs to be closed *soon enough* to
  prevent open handles from accumulating.
  
Additionally, for the program to be a *correct* program,

* the source of the input string should not be closed before the
  output string has been fully evaluated.

These conditions embody the amount of thinking that the programmer
needs to do without help from the compiler. They are as many
opportunities for the programmer or the language's runtime system
to screw up and end up with a non-streaming
or an incorrect program. In Haskell, traditionally people have been
exploiting lazy evaluation to build streaming programs: if we can
somehow produce a string that represents the entire contents of
a file, we could plug that string as an input to `headLine` and hope
that only the first line will ever be evaluated and loaded in memory.
But this is a dangerous proposition. The type system is no longer
distinguishing whether a `String` is a list of values, a computation
which will produce the values on demand, or a computation which
requires a file handle to complete successfully.

Consider this attempt at a full program that uses `headLine`:

```Haskell
import Control.Exception (bracket)
import System.IO (hGetContents, hClose, openFile, IOMode(ReadMode))

printHeadLine1 :: FilePath -> IO ()
printHeadLine1 path = do
    contents <- bracket (openFile path ReadMode) hClose hGetContents
    putStr $ headLine contents
```

The type checker is happy to let it go through. However, it always
produces an empty output. This is because what `hGetContents` returns
(something of type `String`) is really a *computation that performs
I/O as a side effect*, not a regular value, despite what the type
says. As soon as we evaluate `contents`, or any part of it, those side
effects will have to occur. But in the example above, due to laziness,
any evaluation of `contents` will happen as part of the evaluation of
`headLine`, and by the time that happens, the file handle is already
closed, thus violating our third condition above. Here's a fix:

```Haskell
import Control.DeepSeq (($!!))
import Control.Exception (bracket)
import System.IO (hGetContents, hClose, openFile, IOMode(ReadMode))

printHeadLine2 :: FilePath -> IO ()
printHeadLine2 path = do
    str <- bracket (openFile path ReadMode) hClose \h -> do
      contents <- hGetContents h
      return $!! contents
    putStr $ headLine str
```

Now, evaluation of the `contents` side-effecting computation is forced
to happen before the file handle is closed by `($!!)`. The result
`str` is a string available at the time it is consumed. Problem
solved? Not quite, because this time the whole file contents is loaded
into memory at once. What we really want is for the input of
`headLine` to be a computation that produces the values on demand.
A streaming version follows.

```Haskell
import Control.Exception (bracket)
import System.IO (hGetContents, hClose, openFile, IOMode(ReadMode))

printHeadLine :: FilePath -> IO ()
printHeadLine path = do
    bracket (openFile path ReadMode) hClose $ \h ->
      hGetContents h >>= putStr . headLine
```

So it turns out that we *can* write a correct and streaming program.
But it would be great if the type checker could helps us verifying
the three conditions above.


## Streaming with a streaming library

We turn now to show how streaming libraries help writing streaming
programs. We will develop our discussion making use of the package
[streaming](http://www.stackage.org/package/streaming),
but the argument would work as well with other streaming libraries
like
[conduit](http://www.stackage.org/package/conduit),
[pipes](http://www.stackage.org/package/pipes),
[enumerator](http://www.stackage.org/package/enumerator) or
[io-streams](http://www.stackage.org/package/io-streams).

The package `streaming`,
as other streaming libraries, helps discerning whether a value
is a list or a computation. It offers an effectful `Stream` abstraction
as a sequence of computations on some parametric monad `m`, and each
computation can produce a part of a potentially long list of values.

This `streaming` package has a companion package called
[streaming-bytestring](http://www.stackage.org/package/streaming-bytestring),
which provides an effectful ByteString abstraction. Similar to
`Stream`s, a `ByteString` is a sequence of computations, each of which
yields a part of a potentially long bytestring.

To fix ideas, let us consider the function `headLine` implemented
with these abstractions.
```Haskell
import qualified Data.ByteString.Streaming (ByteString)
import qualified Data.ByteString.Streaming.Char8 as SB
import qualified Streaming

headLineStream :: Monad m => ByteString m r -> ByteString m ()
headLineStream = SB.unlines . Streaming.takes 1 . SB.lines
```
This function transforms an effectful bytestring. It might not reside
fully in memory, but it might be produced in chunks as the bytestring is
consumed. In contrast to lazy `ByteString`s, the effectful bytestring
produces the chunks through the monad `m` rather than through lazy
evaluation. Thus the type makes explicit that some computation happens
as the bytestring is consumed. And it becomes possible to reason about
the order in which resources are acquired, used and released in terms
of the monad operations.

```Haskell
SB.lines :: Monad m => ByteString m r -> Stream (ByteString m) m r
Streaming.takes :: Monad m => Int -> Stream (ByteString m) m r -> Stream (ByteString m) m ()
SB.unlines :: Monad m => Stream (ByteString m) m r -> ByteString m r
```
The function starts by creating a stream of
[lines](https://www.stackage.org/haddock/lts-8.22/streaming-bytestring-0.1.4.6/Data-ByteString-Streaming-Char8.html#v:lines).
Each line is itself
an effectful bytestring. When the first bytestring is fully consumed,
the bytestring for the second line becomes available.

Then the function discards all the input except for the first bytestring
([takes](https://www.stackage.org/haddock/lts-8.21/streaming-0.1.4.5/Streaming.html#v:takes)),
and finally it assembles a bytestring from the resulting stream
([unlines](https://www.stackage.org/haddock/lts-8.22/streaming-bytestring-0.1.4.6/Data-ByteString-Streaming-Char8.html#v:unlines)).

The conditions to ensure that the resulting program is streaming and
correct follow.
 * The output `ByteString` is not fed to any function that loads all of
   the output into memory like
   [SB.toStrict](https://www.stackage.org/haddock/lts-8.22/streaming-bytestring-0.1.4.6/Data-ByteString-Streaming-Char8.html#v:toStrict),
 * the source of the input `ByteString` needs to be closed *soon enough* to
   prevent open handles from accumulating and
 * the output `ByteString` shall not be used after the source of the
   input `ByteString` is closed.

Now these conditions do not refer to some *evaluation* that may happen
lazily. The programmer is no longer responsible for discerning
values from effectful computations, the compiler will do it for her.
Thus, by using a streaming library, we are reducing the amount of unaided
thinking that the programmer needs to invest.

Let us consider the full program for the sake of completeness.
```Haskell
printHeadLineStream :: FilePath -> IO ()
printHeadLineStream fp =
    runResourceT $ SB.stdout $ headLineStream $ SB.readFile fp
```

`printHeadLineStream` calls
[SB.readFile](https://www.stackage.org/haddock/lts-8.22/streaming-bytestring-0.1.4.6/Data-ByteString-Streaming-Char8.html#v:readFile)
which produces an effectful
stream with the contents of the file. The file is created using the
[MonadResource](https://www.stackage.org/haddock/lts-8.21/resourcet-1.1.9/Control-Monad-Trans-Resource.html#t:MonadResource)
class to ensures the file is closed before
[runResourceT](https://www.stackage.org/haddock/lts-8.21/resourcet-1.1.9/Control-Monad-Trans-Resource.html#v:runResourceT)
completes.

```Haskell
SB.stdout :: MonadIO m => ByteString m r -> m r
```
The call to
[SB.stdout](https://www.stackage.org/haddock/lts-8.22/streaming-bytestring-0.1.4.6/Data-ByteString-Streaming-Char8.html#v:stdout)
will consume the effectful `ByteString` returned by `headLineStream`
by printing it to the standard output.

# Summary

Streaming libraries allow writing composable streaming programs
without relying on lazy IO. This simplifies reasoning on the order in
which resources are acquired, used and released. However, no streaming
library ensures that well-typed programs are always streaming. The
programmer is still responsible for getting resource management right.

In the next post, we'll delve in more detail on the features that
streaming libraries provide and how they allow writing composable
programs while keeping lazy IO out of the equation.
