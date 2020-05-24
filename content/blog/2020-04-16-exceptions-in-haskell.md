---
redirect_from: [/posts/2020-04-16-exceptions-in-haskell.html]
title: "The three kinds of Haskell exceptions  and how to use them"
shortTitle: "The three kinds of Haskell exceptions"
author: Arnaud Spiwack
tags: [haskell]
description: "Imprecise, synchronous, asynchronous exceptions: what do they mean, and what to do with them."
---

Exceptions, in Haskell, are surprising, sometimes puzzling, always
difficult. But exceptions, in Haskell, are an unavoidable fact of
life. And, as such, you need to learn to love them and to care for
them, despite their flaws.

In this blog post, I'd like to explain how I recommend understanding
and using Haskell's exceptions. The fact that these recommendations
are somewhat personal, rather than best practices that permeate the
community, is a good indication that exceptions really are
difficult. For instance the [safe-exceptions][safe-exceptions] package
makes different recommendations.

## Three flavours of exceptions

There are three different kinds of exceptions in Haskell: one is
imprecise; two are precise, of which one is synchronous, and one
asynchronous. These were all identified in foundational Haskell
papers[^exception-papers]. I'd like to go over what these different kinds of exceptions
mean.

[^exception-papers]: For instance [_A semantics for imprecise exceptions_][imprecise-exceptions], [_Asynchronous exceptions in Haskell_][asynchronous-exceptions], [_Tackling the awkward squad_][awkward-squad].

### Imprecise exceptions

Imprecise exceptions are exceptions like `error "foo"`, which can be
used in pure code. They are so named because it is not determined
which exception will be thrown by `(error "foo") + (error "bar")`. And, in fact, the compiler is very much allowed to change
which error will be thrown. You'll find plenty more details
in the original paper: [_A semantics for imprecise
exceptions_][imprecise-exceptions].

Imprecise exceptions are known to cause distress to Haskell newcomers:
if Haskell is a pure language, how come there are exceptions, which
are impure? The truth of the matter is that imprecise exceptions are
both necessary and unavoidable.

Consider integer division: what should `` 42 `div` 0 `` return? There are
basically three choices:

- An (imprecise) exception
- `Nothing`
- An arbitrary value. Say `0`.

It would be pretty painful to have to do all arithmetic computation in
a `Maybe` monad. The cost on performance would also be
unacceptable. Returning `0` is an option, but you probably didn't want
to divide by 0 to begin with, so it's just going to obscure your
error: in a compound computation, it will just return a nonsense
answer. Throwing an exception is the sane thing to do.

It's also useful to realise that imprecise exceptions don't add any
expressiveness to the language: instead of throwing an exception, I
could just return an infinite loop (such as `let bot = bot in bot`). An imprecise exception is just a more readable
infinite loop. The truth of the matter is that throwing
exceptions was never a problem for purity: the problem was catching
exceptions[^no-pure-catch]. Accordingly, Haskell doesn't provide a
_pure_ `catch`.

[^no-pure-catch]: See [A semantics for imprecise exceptions][imprecise-exceptions] §3.5.

So what are imprecise exceptions for? They denote _programming
errors_. These are situations which are not supposed to occur: someone wrote
some code which they were not supposed to write. This could be a
division by 0, taking the head of an empty list, de-referencing an
array index out of bounds, etc… Basically, whenever a function has a
precondition which cannot be checked by the type checker, violating it
would ideally throw an imprecise exception (sometimes such a function
will return an unspecified value for performance reasons, but it makes
contract violation really non-obvious: avoid it as much as possible).

### Synchronous exceptions

Synchronous exceptions are thrown with functions such as `throwIO`:
throwing a synchronous exception is an `IO` action; the exception
interrupts the current computation.

Synchronous exceptions are very much like exceptions in most other
programming languages. But contrary to what is common in other
programming languages, you shouldn't use them to denote programming
errors: we have imprecise exceptions for that.

Synchronous exceptions denote _exceptional behaviour_, a disk being
full, a file being corrupted, etc… Something that you don't really
want to think about when you are writing the body of a function, but
you may want to have some exceptional logic to handle these cases (such
as a retry logic when a connection fails).

### Asynchronous exceptions

Asynchronous exceptions are thrown with functions such as
`throwTo`. Just like synchronous exceptions, throwing an asynchronous
exception is an `IO` action, but an asynchronous exception interrupts
the computation in another thread.

The dual of this is that you have to be ready to receive asynchronous
exceptions anywhere in `IO` code, since these exceptions are thrown by
someone else. This is especially true of library code, which cannot
know whether it will be used in a multi-threaded program or not.

Asynchronous exceptions denote _thread cancellation_; they are the
Haskell equivalent of Unix signals.

Asynchronous exceptions are a common source of problems in Haskell
code, which is probably why the [safe-exceptions][safe-exceptions]
library puts a particular emphasis on asynchronous exceptions. An
alternative to supporting asynchronous exceptions would have been to let the
thread cancel itself by polling a mailbox, but that approach comes with its
own set of problems (_e.g._ it's impossible to interrupt a long pure
computation). At any rate, Haskell chose the path of asynchronous
exceptions, so we have to deal with it.

## How to use Haskell's exceptions

It's pretty nice that Haskell has given us three different kinds of
exceptions to denote three different kinds of behaviours. But the
truth is that it didn't: there is really one kind of exception, and
three ways to throw them. It's really not that easy to distinguish
between the three kinds of exceptions in a `catch`: the intention has
been lost.

This is why, I think, exceptions in Haskell are difficult. But here is
my methodology to recover a bit of the lost intention.

### Imprecise exceptions

</br>

#### Catch

There was a programming mistake, what can you do? Not much. It is time
to let the program crash. Maybe you're running a long-lived
application, like a server, then it's a bad idea to crash the program
altogether, instead you only want to let the current task or request
to crash.

Concretely, it means that if you are catching an imprecise exception
with `try` or `catch`, you want to re-throw it, except in a toplevel
`catch` which is responsible for cleanly exiting the current task and
reporting an error. Such a toplevel catch should be a blanket catch:
`catch @SomeException` (I find that `catch` works really well with
visible type applications).

In most cases, you are better off not using `catch` or `try`, and use
`bracket`, or `ResourceT` instead: these will properly clean your
resources, and re-throw the exceptions for you.

#### Throw

Since we are not going to catch the error, we don't need a structured
type for imprecise exceptions, so imprecise exceptions might as well be
strings. Therefore, simply use `error` to throw imprecise exceptions.

This is true even in monadic code:

```haskell
do
  x <- error "foo"
  …

case x of
  Good -> return …
  Bad -> error "bar"
```

both work quite fine. As long as you are reporting a programming
error.

There are really two situation where `error` shows up:

- There is an impossible case in my function that the typechecker
  doesn't recognise, something like
  ```haskell
  case prime_number_sieve of
    a:as -> …
    [] -> error "There are infinitely many prime numbers"
  ```
- I'm writing a function which has a precondition that the typechecker
  can't express
  ```haskell
  head (a:_) = a
  head [] = error "Head on an empty list"
  ```

In the latter case: when there is a precondition that users of my function
will need to think about, always add a `HasCallStack` constraint to
the function

```haskell
head :: HasCallStack => [a] -> a
```

This will ensure that if the function is misused and the precondition
is not respected, you will have a stack trace pointing you to the
function which doesn't honour the contract. In other words, use
`HasCallStack` to mark partial functions.

### Asynchronous exceptions

</br>

#### Catch

When you receive an asynchronous exception, your thread _has to_
crash. It is the semantics of asynchronous violation. If you don't let
the thread crash, then you will have broken the expectation of the
sender.

So, like imprecise exceptions, make sure that if you `catch` an
asynchronous exception, you rethrow it. The best way to achieve this
is still to avoid `catch` and use `bracket` or `ResourceT`.

Additionally, you should always be prepared for asynchronous
exceptions. The best advice I can give you is: always allocate your
resources with `bracket` or `ResourceT`, otherwise they may not be
properly released.

#### Throw

The best way to throw an asynchronous exception is by using the
`cancel` function of the [async][async] package, which implies that
you should not use `forkIO` to create threads, but one of the functions
of the async package as well.

### Synchronous exceptions

</br>

#### Catch

You often do want to catch synchronous exceptions with `catch` or
`try`. But since we want to avoid accidentally catching imprecise or
asynchronous exceptions, you need to make sure that you always call
`catch` at a specific exception type (that is: do not use
`SomeException`), which you know can be thrown synchronously

```haskell
catch @SpecificExceptionType
  body
  handler
```

If you make sure to always throw imprecise exceptions with `error`
and asynchronous exception with `cancel`, then you won't catch any of
them. If you are writing library code, you don't fully control what
asynchronous exceptions you can receive, but hopefully, client code
won't go and raise a disk-is-full exception asynchronously.

You can also use `HasCatch` from the [capability][capability] library:
it will ensure that you only catch exceptions thrown by a
corresponding `HasThrow` instance.

#### Throw

Because you want to be able to write `catch @SpecificExceptionType`,
make sure that you use specific (not `SomeException`) types for your synchronous
exceptions. In particular: don't use `IO`'s `fail` method.

Defining a new exception type is easy

```haskell
{-# LANGUAGE DeriveAnyClass #-}

import Control.Exception

data MyException
  = Exception Int Int String
  deriving (Show, Exception)
```

Throw synchronous exceptions with `throwIO` or with capability's
`HasThrow`.

## On the safe-exceptions classification

A well-known framework to classify Haskell exception is the [framework
of the safe-exceptions library][safe-exceptions-announcement]. For the
most part, my recommendations share the same philosophy as the
safe-exceptions framework. We only differ on the handling of imprecise
exceptions.

In the safe-exception framework, imprecise exceptions are
classified together with synchronous exceptions as the group of
exceptions which you want to recover from.
I recommend, on the contrary, to consider that imprecise exceptions
are more similar to asynchronous exceptions. There is no exceptional
behaviour in pure code: pure code is deterministic. And, like asynchronous
exceptions, you should expect imprecise exceptions anywhere: imprecise
exceptions are bugs.

A small point on terminology: in the safe-exceptions framework, imprecise exceptions are called
“impure exceptions”. I am not fond of this terminology: as I've argued
at the beginning of this post, imprecise exceptions are no more impure
than infinite loops. They are just more useful.

## In conclusion

Haskell exceptions require a bit of care. But when used carefully they
can be very effective.

- The three kinds of exceptions correspond to how exceptions are
  thrown, but can't be fully identified when they are
  caught. Therefore they require some discipline.
- Use imprecise exceptions for programming errors. Throw with `error`,
  catch with `bracket`. Use `HasCallStack` for partial functions.
- Use synchronous exceptions for exceptional behaviour. Throw specific
  error types with `throwIO`, catch specific error types with `catch`.
- Asynchronous exceptions cancel threads. Throw with `cancel`, catch
  with `bracket`.

[safe-exceptions]: https://hackage.haskell.org/package/safe-exceptions
[safe-exceptions-announcement]: https://tech.fpcomplete.com/haskell/tutorial/exceptions
[imprecise-exceptions]: https://dl.acm.org/doi/abs/10.1145/301618.301637
[awkward-squad]: https://www.microsoft.com/en-us/research/publication/tackling-awkward-squad-monadic-inputoutput-concurrency-exceptions-foreign-language-calls-haskell/
[asynchronous-exceptions]: https://dl.acm.org/doi/abs/10.1145/378795.378858
[async]: https://hackage.haskell.org/package/async
[capability]: https://hackage.haskell.org/package/capability
