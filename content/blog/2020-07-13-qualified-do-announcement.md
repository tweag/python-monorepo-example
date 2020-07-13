---
title: "Qualified do: rebind your do-notation the right way"
shortTitle: "Announcing qualified do"
author: Matthías Páll Gissurarson, Facundo Domínguez, Arnaud Spiwack
tags: [haskell, linear-types]
description: "Announcement of the upcoming QualifiedDo language extension."
---

Since the early 2000s, the `RebindableSyntax` language extension
was our only choice if we wanted to use do-notation with anything
other than instances of the `Monad` type class. This design became
particularly unwieldy a few years ago, when we started introducing
linear monads in our experiments with
[-XLinearTypes][linear-types-tag]. In this post
we will discuss how `QualifiedDo`, a new language extension
in the upcoming 8.12 release of GHC, improves the experience of
writing do-notation with _monad-like_ types.

## Do-notation

All Haskellers are accustomed to do-notation. Mark Jones introduced
it in the Gofer compiler [in 1994][a-history-of-haskell], and from
there it made it into the
Haskell language report [in 1996][haskell-language-report-do].
Since then it has become popular, and for a good reason: it makes
easy to read a sequence of statements describing an effectful computation.

```Haskell
f :: T String
f = do h <- openAFile
       s <- readTheFile h
       sendAMessage s
       closeTheFile h
       waitForAMessage
```

One just reads it top-to-bottom. There are no parentheses and no
operator precedences to figure out where statements begin and end.

Because monads are the main abstraction for dealing with effectful
computations in Haskell, it made sense to support do-notation for
instances of the `Monad` type class, which means that the type `T`
above needs to have a `Monad` instance.

```Haskell
class Applicative m => Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b

instance Monad T where
  . . .
```

## Monad-like

In the new century, new ideas appeared about how to represent
effectful programs. A growing collection of monad-like types
accumulated for which the `Monad` type class was an inadequate
abstraction. Such has been the case of [indexed monads][super-monads], graded
monads, constrained monads and, lately, [linear monads][data-control].

In the case of linear monads, the operations use [linear arrows][linear-types-doc],
which prevents us from using them to implement the standard
`Monad` type class.

```Haskell
{-# LANGUAGE LinearTypes #-}
module Control.Monad.Linear where

-- constraints elided for the sake of discussion
class (...) => Monad m where
  return :: a #-> m a
  (>>=) :: m a #-> (a #-> m b) -> m b

(>>) :: Monad m => m () #-> m b #-> m b
m0 >> m1 = m0 >>= \() -> m1
```

Until now, the way to use do-notation with
_not-quite-monads_ has been to use the `RebindableSyntax` language
extension. With `RebindableSyntax`, we need only to bring into scope
the operations that do-notation should use.

```Haskell
{-# LANGUAGE RebindableSyntax #-}
import Control.Monad.Linear as Linear

instance Linear.Monad LinearT where
  . . .

f :: LinearT String
f = do h1 <- linearOpenAFile
       (h2, Unrestricted s) <- linearReadTheFile h1
       linearLiftIO (sendAMessage s)
       linearCloseTheFile h2
       linearWaitForAMessage
```

Now the compiler will desugar the `do` block as we want.

```Haskell
f :: LinearT String
f = linearOpenAFile Control.Monad.Linear.>>= \h1 ->
    linearReadTheFile h1 Control.Monad.Linear.>>= \(h2, Unrestricted s) ->
    linearLiftIO (sendAMessage s) Control.Monad.Linear.>>
    linearCloseTheFile h2 Control.Monad.Linear.>>
    linearWaitForAMessage
```

`RebindableSyntax`, however, has other effects over the whole
module. Does your module have another `do` block on a regular monad?

```Haskell
sendAMessage s =
  do putStr "Sending message..."
     r <- post "https://tweag.io/post" s
     if statusCode r == 200
     then putStrLn "Done!"
     else putStrLn "Request failed!"
```

The compiler will complain that there is no instance of `Linear.Monad IO`.
And indeed, there is not supposed to be. We want the operations of `Monad IO`
here! But aren't `Prelude.>>` and `Prelude.>>=` in scope anymore? No, they're
not in scope, because `RebindableSyntax` also has the effect of not importing
the `Prelude` module implicitly.

If function `sendAMessage` had a type signature, GHC would complain that
`IO` is not in scope.

```Haskell
sendAMessage :: String -> IO ()
```

It gets worse:

```Haskell
. . .
  if statusCode r == 200
  then putStrLn "Done!"
  else putStrLn "Request failed!"
. . .
```

There would be:

- no `putStrLn` in scope
- no `fromInteger` to interpret the literal `200`
- no `ifThenElse` function that `-XRebindableSyntax` mandates to
  desugar an `if` expression

To add insult to injury, in the particular case of linear types, there
is not even a correct way to define an `ifThenElse` function to desugar `if`
expressions. So enabling `-XRebindableSyntax` together with
`-XLinearTypes` deprives us from `if` expressions in linear contexts.

The list of problems does not end here, but the remaining issues are already
illustrated. Each issue has a simple fix, all of which add up to an annoying bunch
the next time we are faced with the decision to introduce `RebindableSyntax`
or do away with do-notation.

But despair no more, dear reader. The days of agony are a thing of the past.

## Qualified do

By enabling the `QualifiedDo` language extension, we can qualify the
`do` keyword with a module alias to tell which operations to use in
the desugaring.

```Haskell
{-# LANGUAGE QualifiedDo #-}
import qualified Control.Monad.Linear as Linear

instance Linear.Monad LinearT where
  . . .

f :: LinearT String
f = Linear.do                                      -- Desugars to:
      h1 <- linearOpenAFile                        -- Linear.>>=
      (h2, Unrestricted s) <- linearReadTheFile h1 -- Linear.>>=
      linearLiftIO (sendAMessage s)                -- Linear.>>
      linearCloseTheFile h2                        -- Linear.>>
      linearWaitForAMessage                        -- Linear.>>

sendAMessage :: String -> IO ()
sendAMessage s = do                       -- Desugars to:
    putStr "Sending message..."           -- Prelude.>>
    r <- post "https://tweag.io/post" s   -- Prelude.>>=
    if statusCode r == 200
    then putStrLn "Done!"
    else putStrLn "Something went wrong!"
```

This has the compiler desugar the `Linear.do` block with any
operations `Linear.>>=` and `Linear.>>` which happen to be in scope.
The net result is that it ends up using `Control.Monad.Linear.>>=`
and `Control.Monad.Linear.>>`. The unqualified `do` still uses the
`>>=` and `>>` operations from the `Prelude`, allowing different
desugarings of do-notation in a module with ease.

Is that it? Yes! Really? No extra effects on the module!
One can combine `QualifiedDo` with `ApplicativeDo`, or `RecursiveDo` and
even `RebindableSyntax`. Only the qualified `do` blocks will be affected.

Every time that a `do` block would need to reach out for `mfix`, `fail`,
`<$>`, `<*>`, `join`, `(>>)` or `(>>=)`, `Linear.do` will use
`Linear.mfix`, `Linear.fail`, `Linear.<$>`, etc.

## The proposal

An extension being this short to explain is the merit of more than
a year-long GHC [proposal][qualified-do-proposal] to nail each aspect
that could be changed in the design.

We had to consider all sorts of things we could possibly use to qualify
a `do` block. How about qualifying with a type class name? Or a value
of a type defined with record syntax? Or an expression? And expressions
of what types anyway? And what names should be used for operations in the
desugaring? And should the operations really be imported? And what if
we wanted to pass additional parameters to all the operations introduced
by the desugaring of a given `do` block?

There is an answer to each of these questions and more in the proposal.
We thank our reviewers, with a special mention to Iavor Diatchki, Joachim
Breitner, fellow Tweager Richard Eisenberg, and Simon Peyton Jones, who devoted their energy
and insights to refine the design.

## What's next?

Reviewers have suggested during the discussion that other syntactic sugar
could be isolated with qualified syntax as well: list comprehensions, monad
comprehensions, literals, `if` expressions or arrow notation are candidates
for this.

For now, though, we are eager to see how much adoption `QualifiedDo` has. The
[implementation][qualified-do-implementation] has recently been merged into GHC,
and we will be able to assess
how well it does in practice. The extension is now yours to criticize or improve.
Happy hacking!

[a-history-of-haskell]: https://dl.acm.org/doi/10.1145/1238844.1238856
[data-control]: https://www.tweag.io/blog/2020-01-16-data-vs-control
[haskell-language-report-do]: https://www.haskell.org/definition/from12to13.html#do
[linear-types-doc]: https://gitlab.haskell.org/ghc/ghc/-/blob/40fa237e1daab7a76b9871bb6c50b953a1addf23/docs/users_guide/exts/linear_types.rst
[linear-types-tag]: https://www.tweag.io/blog/tags/linear-types
[qualified-do-implementation]: https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3436
[qualified-do-proposal]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0216-qualified-do.rst
[super-monads]: http://www.cs.nott.ac.uk/~psxjb5/publications/2017-BrackerNilsson-SupermonadsAndSuperapplicatives-UnderConsideration.pdf
