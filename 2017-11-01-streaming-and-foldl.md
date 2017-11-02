---
title: The Exodus to Streamgard,<br/> an epic poem
author: Yves Parès
featured: yes
---

*<span class="dropcap">I</span><span style="font-variant: small-caps;">f</span>
Haskell was a god, often would he be depicted with the ravens Modularity and
Abstraction flying above him, hovering the world and reporting to him every
detail of our whereabouts. Haskell would sit on the Throne of Purity and look
upon the world with <span class="tooltip" title="Yes, of course Haskell would be one-eyed. And he'd have a list of like 200 awe-inspiring nicknames, like 'The Monadbringer' or 'The Father of all things pure', but that's another story.">an eye</span> full of wisdom. And in his hand, the mighty Haskell would
wield the Spear of Lazy Lists, which is said to have the power to tackle each
and every problem the world might have to face. And to honour him, we would
code and abstract everything with lazy lists. For millenia would lists be used
to map, filter, separate, merge, group, <span class="tooltip" title="Full cosmogony in the religion of Haskell is left as an exercise to the reader.">and
so forth</span>.*

*<span class="dropcap">B</span><span style="font-variant:
small-caps;">ut</span>, one day, the <span class="tooltip" title="Yes, all that buildup for a lousy pun">
Real-[World Serpent](https://en.wikipedia.org/wiki/J%C3%B6rmungandr)</span>, son of the wicked <span class="tooltip" title="Also seen written as 'Folður'">Foldr</span>, would come. And the Real-World Serpent
carries an eternal hatred towards lazy lists. Oh, that dreaded Serpent, that
will throw everything it can muster to prevent us from staying within the warm
comfort of abstraction and laziness. The Serpent will assemble its minions,
[_Early-close_ and _Strictness of
effects_](http://www.tweag.io/posts/2017-07-27-streaming-programs.html), and
unleash its wrath upon our world. Foldl, son of Haskell and brother of Foldr,
would lead humanity to its last bastion, Streamgard, and organize the final
fight...*

So, long story short,
[`streaming`](http://hackage.haskell.org/package/streaming) is a library that
allows you to leverage the insights you have gained while manipulating lazy
lists in Haskell to handle effectful streams of data. We already talked about
`streaming` on this blog, with
[this post](http://www.tweag.io/posts/2017-07-27-streaming-programs.html)
discussing the IO part and
[this one](http://www.tweag.io/posts/2017-10-05-streaming2.html) comparing it to
[pipes](http://hackage.haskell.org/package/pipes) and
[conduit](http://hackage.haskell.org/package/conduit). Here, we will be using
`streaming` for highly efficient data processing and filtering. To this effect, we will use
it conjointly with another library,
[`foldl`](http://hackage.haskell.org/package/foldl), which gives us an
`Applicative` interface to the usual list functions. In this blog post we will
apply them to the task of computing some statistics about a distribution of
data. We want to be able to:

- process the input data stream _only once (aka in one pass)_,
- never repeat the effects that were used to produce that data stream,
- maintain the possibility to use the input stream as if it were a list, for
  instance by splitting it into two subparts, sending each subpart to be
  processed by a specific function.

So lets imagine that the statistics I want to compute on my input data
distributions take the shape of a simple summary. This is what I want to obtain
in the end:

```haskell
data Summary v a = Summary
  { summaryLength :: Int
  , summaryMins :: [a]
  , summaryMaxes :: [a]
  , summaryMean :: v
  , summaryStdDev :: v
  }
  deriving (Show)
```

Nothing too fancy here, I just want to be able to compute the length, the `n`
smallest elements, the `n'` biggest elements, the mean and the standard deviation
of my distribution. We distinguish the types `a` and `v` here because our input
distribution does not have to be numerical, as long as we have a projection `a
-> v` available. This way, we can compute a summary of a stream of `(Double,
String)` tuples, for instance, if the projection is just `fst`.

So let's have a little reminder of our conditions. We want to be able to read
the input data only once. But, we still want modularity and reusability. We do
not want to have to recode our `Summary`-computing function every time we want
to add a new field, and we would like to reuse already existing functions
computing these statistics. And this is where the `foldl` package comes in.

This package defines a type `Fold` as follows:

```haskell
data Fold a b = forall acc. Fold (acc -> a -> acc) acc (acc -> b)
```

You might recognize here the typical arguments of the classical `foldl` function
of the `Prelude`: `a` is the type of each element of the input stream we
consume, the first field `(acc -> a -> acc)` is an accumulation function and the
second field `acc` is the initial value of the accumulator.  The new component
is the `b` type parameter and the last field `(acc -> b)`. This one is called
_extract_. It is used to extract the final value out of the accumulator. This is
necessary so that `Fold a` can be a `Functor` and therefore an
`Applicative`. See the
[original blog post](http://www.haskellforall.com/2013/08/composable-streaming-folds.html)
and [this talk](https://www.youtube.com/watch?v=6a5Ti0r8Q2s) by Gabriel Gonzalez
for more detail, though be aware that `Fold` had a different shape back then.

One of the central ideas of the `foldl` library is that `Fold` implements the
`Applicative` type class:

```haskell
instance Applicative (Fold a)
```

Crucially, this instance combines two `Fold`s, into a guaranteed one-pass
traversal of the data. Therefore we can safely decompose the computation of a
`Summary` as follows:

```haskell
import qualified Control.Foldl as L
import Data.Function (on)

summarizeBy :: (Floating v, Ord v)
            => (a -> v) -> Int -> Int -> L.Fold a (Summary v a)
summarizeBy f nMins nMaxes = Summary
  <$> L.length
  <*> collect ((>=) `on` f) nMins
  <*> collect ((<=) `on` f) nMaxes
  <*> L.premap f L.mean
  <*> L.premap f L.std
```

What's happening here? We are using a few of the functions already present in
the `foldl` package and a new one, so let's delve into it a bit. The function
`summarizeBy` takes a projection `f`, which we talked about earlier, the number
of smallest elements we want to collect and the number of biggest elements. Then
our five statistics are computed:

- `L.length :: L.Fold a Int` gives us the number of elements in the input.
- `collect`, which we will define a bit later, accumulates either the mins or
  the maxes given a comparison function.
- `L.mean` gives us the average. We use `L.premap f` to turn it into a fold that
  will work on our projection `f`.
- `L.std` gives us the standard deviation.

The combination of the above gives us a `Fold a (Summary v a)`, something that
will consume a stream of `a`'s and output a summary. At this point, nothing is
consumed, we have only composed folds together, and a `Fold` is agnostic of the
exact nature of the input. Running it on any `Foldable` datatype for instance is
just a matter of calling:

```haskell
L.fold (summarizeBy id 3 3) [1..100]
```

The only function <span class="tooltip" title="The foldl package provides 'minimum' and 'maximum', but here we want more than that.">not provided by the `foldl` package</span> is the `collect` function. Defining it as a brand new
`Fold` is simple:

```haskell
import Data.Sequence as Seq

collect :: (a -> a -> Bool) -> Int -> L.Fold a [a]
collect skipPred n = L.Fold insertPop Seq.empty (L.fold L.list)
  where
    insertPop acc x
      | Seq.length acc < n = insert x acc
      | otherwise          = pop (insert x acc)
    insert x s = let (before, after) = Seq.spanl (skipPred x) s
                 in before <> Seq.singleton x <> after
    pop s = case viewr s of
              s' :> _ -> s'
              _ -> s
```

Here we manually defined a new `Fold` from the three elements we mentioned
earlier: an accumulation function (`insertPop`), an initial accumulator value
(`Seq.empty`) and an _extract_ function (`(L.fold L.list)`, which also uses a
`Fold` to turn the final sequence into a plain list).

Now, the astute reader will notice we left `streaming` aside. Let's get back to
it. Let's use as an input the classic
[Titanic dataset](https://github.com/caesar0301/awesome-public-datasets/blob/master/Datasets/titanic.csv.zip):

```csv
PassengerId,Survived,Pclass,Name,Sex,Age,SibSp,Parch,Ticket,Fare,Cabin,Embarked
1,0,3,"Braund, Mr. Owen Harris",male,22,1,0,A/5 21171,7.25,,S
2,1,1,"Cumings, Mrs. John Bradley (Florence Briggs Thayer)",female,38,1,0,PC 17599,71.2833,C85,C
3,1,3,"Heikkinen, Miss. Laina",female,26,0,0,STON/O2. 3101282,7.925,,S
4,1,1,"Futrelle, Mrs. Jacques Heath (Lily May Peel)",female,35,1,0,113803,53.1,C123,S
...
```

We want to get two different summaries for the fares: one for the passengers
that survived and one for those who did not. First, let's load the CSV into a
`stream` by using the
[`streaming-cassava`](http://hackage.haskell.org/package/streaming-cassava) and
[`streaming-bytestring`](https://hackage.haskell.org/package/streaming-bytestring)
packages:

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (mzero)
import qualified Data.ByteString.Streaming as BS
import Streaming
import Streaming.Cassava

data Passenger { name :: !String, fare :: !Double, survived :: !Bool }
  deriving (Show)

instance FromNamedRecord Passenger where
  parsedNamedRecord m =
    Person <$> m .: "Name" <*> m .: "Fare" <*> (toBool =<< (m .: "Survived"))
  where toBool 0 = return False
        toBool 1 = return True
        toBool _ = mzero

streamCsv :: (MonadResource m) => Stream (Of Passenger) m ()
streamCsv = decodeByName (BS.readFile ".../titanic.csv")
```

Nothing too fancy here, just a bit of required boilerplate to be able to read
`Passenger`s from the CSV file. `MonadResource` is necessary to track the files
opened by our program. The type `Stream (Of Passenger) m ()` means that we will
be manipulating a stream whose elements are `Passenger`s, that will run some
effects in a monad `m` and return no result in the end.

Now, lets split that input in two different substreams:

```haskell
import qualified Streaming.Prelude as S

aliveDead :: Stream (Of Passenger) (Stream (Of Passenger) m) ()
aliveDead = S.partition survived streamCsv
```

Let's look at the type of `aliveDead`: it is a `Stream` over another
`Stream`. `Stream (Of a)` is actually a monad transformer, the way the
partitioning happens is by creating two layers: one for the live passengers and
one for the dead ones. It's not exactly a tuple of two streams (as it would be
with `Data.List.partition`), but is has the same advantages: each layer can be
processed by different functions which don't have to know where the stream they
process lies in the monad stack. Therefore, each one of these functions can be
expressed as:

```haskell
summarizePassengers
  :: (Monad m) => Stream (Of Passenger) m a -> m (Of (Summary Double Passenger) a)
summarizePassengers = L.purely S.fold (summarizeBy fare 3 3)
```

where `m` can be any monad. This can be the bottom MonadResource or another
`Stream`, `summarizePassengers` does not mind and does not have to! `Of` behaves
like a tuple, so it simply means that we return both the newly computed
`Summary` and an `a` (`a` may just be `()`, but here we have to be a little more
general). `S.fold` is the basic folding function for streams. `L.purely fn f`
"unpacks" a `Fold` `f` and calls a folding function `fn`. So now, getting our
summaries is just a matter of

```haskell
runAll = runResourceT $ do
  (summaryAlive :> summaryDead :> ()) <-
    summarizePassengers $ summarizePassengers aliveDead
  ...
```

So in the end, we splitted the input file in two substreams, we computed various
statistics twice, and despite all this `streaming` and `foldl` guarantee that
the input will be read _only once_ in bounded memory.

These techniques are currently being applied by Tweag I/O in the context of a
project with [Novadiscovery](http://www.novadiscovery.com). Novadiscovery is a
consulting company for _in silico_ clinical trials, namely simulation of virtual
patients through biomodeling. Parts of this blog post are actual code from the
tools we develop with them.
