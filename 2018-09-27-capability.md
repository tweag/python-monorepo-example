---
title: "capability: the ReaderT pattern without boilerplate"
shortTitle: Announcing capability
author: Andreas Herrmann, Arnaud Spiwack
---

About a year ago, Michael Snoyman made a blog post about
the [ReaderT pattern][readert]. Ostensibly, it's a blog post about
preferring to encode state as `IORef`-s when you can. But that's not
how _we_ read it. What we saw first and foremost is a story about
using extensional type classes describing specifically the effects
that functions use, but not how the effects are implemented (see the
`MonadBalance` story in [Michael's blog post][readert]). We call these
extensional type classes *capabilities*. Here is an
excellent [blog post][three-layer-cake] from Matt Parsons which takes
this aspect to heart.

[Capability][capability] is a library to replace [the MTL][mtl] with
capabilities. In this post, we'll argue why capabalities are
important, why you should use them, and tell you about what it took to
design a library of capabilities with good ergonomics. It turns out
that a brand new language extension that shipped with GHC 8.6,
`-XDerivingVia`, has a crucial role in this story.

[readert]: https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
[three-layer-cake]: http://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
[capability]: https://github.com/tweag/capability
[mtl]: http://hackage.haskell.org/package/mtl

## The difference with the MTL

How is using capabilities, like in Snoyman's and Parsons's blog posts
any different from using the well-trodden MTL? Quite a bit! The MTL's
classes, like `MonadReader`, `MonadState` and their close relatives,
are _intensional_: they reflect *how* the monad has been constructed.
A monad `MyM` is a `MonadState` because it is a stack of monad
transformers, one of which is a `StateT`.

This is because of what Haskell instances mean: if I have an instance,

```haskell
instance MonadState s m => MonadState s (ReaderT r m)
```

while it may look like we're saying that it _suffices_ to have
`MonadState s m` for `ReaderT r m` to be `MonadState s`, what we are
really saying is that `MonadState s (ReaderT r m)` _means_ that
`MonadState s m`. It defines a computation, rather than a deduction.
In particular, we are not permitted to add the following instance:

```haskell
data St = ...
instance MonadState St (ReaderT (IORef St) IO)
```

You may want to work around this issue using `{-# OVERLAPPING #-}`
instances. However, in doing so, you are acting against the semantics
of instances, and heading for trouble. For an example of issues with
overlapping instances, see the warning at the end of the [Overlapping
instances
section](https://downloads.haskell.org/~ghc/8.4.3/docs/html/users_guide/glasgow_exts.html#overlapping-instances)
of the GHC manual.

In contrast, what the ReaderT pattern is advertising is extensional
classes, indicating *what* an individual function is allowed to do
(hence the name "capability"), regardless of *how* the monad is
implemented.

## The problem

Irrespective of whether you are willing to twist the arm of the MTL
with `{-# OVERLAPPING #-}` instances, when programming with
capabilities you will run into two kind of issues. The first is
probably the lesser of the two, but has been a known pain point with
the MTL for a while: it's that the MTL uses types (e.g. the state
type) to discriminate layers. In other words: **with the MTL, you
can't have two states of the same type in your capabilities**.

This, of course, is not a problem if you are writing your own type
classes: you can simply use a different class for each piece of state
(e.g. `MonadBalance`). This leads us to the second, more serious
issue: lack of inference. With all its faults, the MTL gives a very
comfortable environment: it defines plenty of generic instances, so
that when we give a concrete monad stack, then all the instances are
automatically computed for us. In contrast, with capabilities and the
ReaderT pattern, we collect all the capabilities, and assemble
a bespoke type to handle all of these instances. Haskell's instance
resolution is simply not equipped for this.

The bottom line is: an insane amount of boilerplate. Custom type class
definitions. A slew of instances at each main entry point (where a
concrete type is defined for the monad).

## Deriving Via

What we would really like is a way to use type class instances the
other way around, compared to instance resolution. Instead of reading

```haskell
instance MonadState s m => MonadState s (ReaderT r m)
```

as saying that `MonadState`, on `ReaderT` means that `MonadState s m`, and
fixing the implementation, we would like to read it as: if I have an
implementation of `MonadState s m`, then this is a _possible_
implementation of `MonadState` on a `ReaderT`.

This is made possible by a new language extension available in the
freshly released GHC 8.6: [`-XDerivingVia`][user-guide].

In short, `-XDerivingVia` is a generalisation of
`-XGeneralizedNewtypeDeriving` that allows you not only to derive an
instance _for_ a newtype, but also _from_ a newtype, or, and this is
most relevant for us, from a combination of newtypes. For example:

``` haskell
{-# LANGUAGE DerivingVia #-}

import Data.Monoid (Sum (..))

newtype MyInt = MyInt Int
  deriving (Monoid, Semigroup) via Sum Int
  deriving Num via Int
```

In the above snippet we define `MyInt` which wraps an `Int`, and
derive two instances for it. The `Monoid` instance is taken from `Sum
Int`, and the `Num` instance is taken directly from `Int`. Note the
`via` keyword in the deriving clauses. (In this example we could also
have derived the `Num` instance using `-XGeneralizedNewtypeDeriving`.)

You will find a more complete introduction in [Baldur Blondal's
talk][stolen-instances] on the subject. If you want all the details,
head to the [proposal][proposal] or the [paper][paper].

[stolen-instances]: https://skillsmatter.com/skillscasts/10934-lightning-talk-stolen-instances-taste-just-fine
[proposal]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0023-deriving-via.rst
[paper]: https://www.kosmikus.org/DerivingVia/deriving-via-paper.pdf
[user-guide]: https://downloads.haskell.org/~ghc/8.6.1/docs/html/users_guide/glasgow_exts.html#deriving-via

## Enter capability

With the above piece of kit in hand, we can write
the [`capability` library][capability]. This library provides
strategies that can be composed to derive capabilities using the
`-XDerivingVia` language extension.

`capability` defines a set of standard, reusable capability type
classes, such as `HasReader`, or `HasState`. Contrary to the MTL type
classes these are parameterized by a name (aka *tag*), which makes it
possible to refer to, say, multiple different states in the same
computation, even if they correspond to the same type.

``` haskell
getAB :: (HasState "a" Int m, HasState "b" Int m) => m (Int, Int)
getAB = do
  a <- get @"a"  -- get state under tag "a"
  b <- get @"b"  -- get state under tag "b"
  pure (a, b)
```

The library then provides newtypes
to derive instances of these capability type-classes
in deriving via clauses, similar to `Sum` in the `MyInt` example above.

For example, given an MTL `MonadReader` instance, we can derive
a `HasReader` capability as follows:

``` haskell
data AppData = ...

newtype AppM a = AppM (ReaderT AppData IO a)
  deriving (HasReader "appData" AppData) via
    MonadReader (ReaderT AppData IO)
```

We can also combine multiple newtypes to derive capability instances.
Building on the above example,
we can pick a field within `AppData` as follows:

``` haskell
data AppData = AppData { intRef :: IORef Int, ... }
  deriving Generic

newtype AppM a = AppM (ReaderT AppData IO a)
  deriving (HasReader "intRef" (IORef Int)) via
    Field "intRef" "appData" (MonadReader (ReaderT AppData IO))
```

The `Field` combinator takes two tags as arguments.
The first specifies the field name, and also the new tag.
The second specifies the old tag,
which provides the record with the requested field.
Note, that the `MonadReader` newtype can provide an instance for any tag.
The `Field` combinator uses [generic-lens][generic-lens] under the hood,
which is why `AppData` needs to have a `Generic` instance.

[generic-lens]: http://hackage.haskell.org/package/generic-lens

## A worked example: combining writers without guilt

Let's consider a complete example to demonstrate how you could use `capability`
in your own projects.
The code is available in the [capability repository][capability]
if you want to follow along.

In this example we will receive a text as input
and want to count occurrences of words and letters in the text, ignoring white space.
To that end we will use a writer monad.
Recall, that writer has the method `tell :: w -> m ()`,
which will `mappend` the given `w` to the current tally, starting from `mempty`.
This requires a `Monoid` instance on `w`.

We start with counting single letters and words.

``` haskell
-- | Count the occurrence of a single letter.
countLetter ::
  HasWriter "letterCount" (Occurrences Char) m
  => Char -> m ()
countLetter letter = tell @"letterCount" (oneOccurrence letter)

-- | Count the occurrence of a single word.
countWord ::
  HasWriter "wordCount" (Occurrences Text) m
  => Text -> m ()
countWord word = tell @"wordCount" (oneOccurrence word)
```

The type `Occurrences k` is a newtype around a `Map` from values `k` to their count.
Its `Monoid` instance will add occurrences in the expected fashion.

``` haskell
newtype Occurrences k = Occurrences (Map k Int)

instance Ord k => Monoid (Occurrences k)
  -- See repository for instance implementation.

-- | A single occurrence of the given value.
oneOccurrence :: k -> Occurrences k
oneOccurrence k = Occurrences $ Map.singleton k 1
```

Next, we combine `countLetter` and `countWord` to handle one word in the input text.
 
``` haskell
-- | Count the occurrence of a single word and all the letters in it.
countWordAndLetters ::
  ( HasWriter "letterCount" (Occurrences Char) m
  , HasWriter "wordCount" (Occurrences Text) m )
  => Text -> m ()
countWordAndLetters word = do
  countWord word
  mapM_ countLetter (Text.unpack word)
```

Finally, we can handle the full input text by first splitting it into its words
and then applying the above function to each word.

``` haskell
-- | Count the occurrences of words and letters in a text,
-- excluding white space.
countWordsAndLettersInText ::
  ( HasWriter "letterCount" (Occurrences Char) m
  , HasWriter "wordCount" (Occurrences Text) m )
  => Text -> m ()
countWordsAndLettersInText text =
  mapM_ countWordAndLetters (Text.words text)
```

In a production setting we might prefer to stream the input,
instead of holding he whole text in memory.
For simplicity's sake we will omit this here.

With that we have written a program that demands two `HasWriter` capabilities.

Before we can execute this program we need to define a concrete implementation
that provides these capabilities.
This is where we make use of the deriving-via strategies that the library offers.

It is well-known that the writer monad provided by MTL [has a space leak][writert-space-leak].
In `capability`, **we can derive a writer capability from a state capability instead**,
to avoid this issue.
In fact, we don't even provide a way to derive a writer capability from a writer monad.
Following the ReaderT pattern we derive the state capabilities
from reader capabilities on `IORef`s.

First, we define the application context.
A record holding two `IORef`s - one for each counter.

``` haskell
-- | Counter application context.
data CounterCtx = CounterCtx
  { letterCount :: IORef (Occurrences Char)
    -- ^ Counting letter occurrences.
  , wordCount :: IORef (Occurrences Text)
    -- ^ Counting word occurrences.
  } deriving Generic
```

Next, we define our application monad.

``` haskell
-- | Counter application monad.
newtype Counter a = Counter { runCounter :: CounterCtx -> IO a }
  deriving (Functor, Applicative, Monad) via (ReaderT CounterCtx IO)
```

Note that we use `ReaderT` in the deriving via clause as a strategy
to derive the basic `Functor`, `Applicative`, and `Monad` instances.

Deriving the writer capabilities makes use of a large set of newtypes
provided by the capability library.
Each line after the `via` keyword corresponds to one newtype.
Comments explain the purpose of the respective newtype.
Read these from bottom to top.

``` haskell
  deriving (HasWriter "letterCount" (Occurrences Char)) via
    (WriterLog  -- Generate HasWriter using HasState of Monoid
    (ReaderIORef  -- Generate HasState from HasReader of IORef
    (Field "letterCount" "ctx"  -- Focus on the field letterCount
    (MonadReader  -- Generate HasReader using mtl MonadReader
    (ReaderT CounterCtx IO)))))  -- Use mtl ReaderT newtype
```

The `"wordCount"` writer is derived in the same way:

``` haskell
  deriving (HasWriter "wordCount" (Occurrences Text)) via
    WriterLog (ReaderIORef
    (Field "wordCount" "ctx" (MonadReader (ReaderT CounterCtx IO))))
```

The only thing left is to combine all these pieces into an executable program.
We will take the text as an argument, and return an `IO` action
that executes `countWordsAndLettersInText` using our `Counter` monad,
and prints the resulting word and letter counts to standard output.

``` haskell
-- | Given a text count the occurrences of all words and letters in it,
-- excluding white space, and print the outcome to standard output.
wordAndLetterCount :: Text -> IO ()
wordAndLetterCount text = do
```

First, we setup the required `IORef`s and the counter context:

``` haskell
  lettersRef <- newIORef Map.empty
  wordsRef <- newIORef Map.empty
  let ctx = CounterCtx
        { letterCount = lettersRef
        , wordCount = wordsRef
        }
```

Then, we call `countWordsAndLettersInText` on the input text,
and instantiate it using our `Counter` application monad:

``` haskell 
  let counter :: Counter ()
      counter = countWordsAndLettersInText text
```

Finally, we run `counter` and print the results:

``` haskell
  runCounter counter ctx
  let printOccurrencesOf name ref = do
        putStrLn name
        occurrences <- readIORef ref
        ifor_ occurrences $ \item num ->
          putStrLn $ show item ++ ": " ++ show num
  printOccurrencesOf "Letters" lettersRef
  printOccurrencesOf "Words" wordsRef
```

Executing this program in GHCi should produce the following output:

``` haskell
>>> wordAndLetterCount "ab ba"
Letters
'a': 2
'b': 2
Words
"ab": 1
"ba": 1
```

This concludes the example.
We invite you to experiment with this library.
It is still in an early stage and the API is subject to change.
However, your feedback will help to evolve it in a better direction.

[writert-space-leak]: https://blog.infinitenegativeutility.com/2016/7/writer-monads-and-space-leaks

## A word on free monads

Another solution to many of the same problems has been known for a while:
[free monads and extensible effects][extensible-effects]. As it
happens, capability and free monads can be formally compared. In
[this paper][vlfm], Mauro Jaskelioff and Russell O'Connor, prove that
free monads are a special case of capabilities (it's not phrased in
these terms, of course, but that's what the paper amounts to).

So another way of looking at capability is that it is a library
of extensible effects. It makes it possible to write effects which are
not available to free monads: free monads can only model algebraic
effects, while capabilities do not have such a restriction. For instance the
`HasCatch` capability, giving a function the ability to catch errors,
is not algebraic, hence not available to free monads.

However, the most important reason for us to develop capability
is that we find this style of programming quite manageable and
idiomatic, whereas free-monad programming quickly becomes
unwieldy. This is an entirely subjective judgement of course, but we
believe that it has slowed the adoption of extensible effects.
Absolutely wonderful though they are! As a bonus, capabilities should
be more efficient than free-monad-style programming because it doesn't
rely on a tagged encoding.

[extensible-effects]: https://hackage.haskell.org/package/extensible-effects
[vlfm]: http://r6.ca/blog/20140210T181244Z.html

<!--  LocalWords:  intensional monads monad mtl GHC composable
 -->
<!--  LocalWords:  newtypes
 -->
