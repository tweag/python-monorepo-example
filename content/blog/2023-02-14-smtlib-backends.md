---
title: "smtlib-backends: faster SMT-LIB-based Haskell interface to SMT solvers"
author: Quentin Aristote
tags: [internship, haskell, formal-methods, liquidhaskell]
description: "Announcement of smtlib-backends, a Haskell library providing a generic interface for interacting with SMT solvers using SMT-LIB"
---

[SMT][wiki-smt] solvers are tools for solving [satisfiability][wiki-satisfiability] problems over logical formulae
involving data types such as real numbers, arrays or strings. They use various
heuristics in an attempt to assess whether the [free variables][wiki-freevars] of a given formula
may be assigned some values such that the formula holds.

[wiki-smt]: https://en.wikipedia.org/wiki/Satisfiability_modulo_theories
[wiki-satisfiability]: https://en.wikipedia.org/wiki/Satisfiability
[wiki-freevars]: https://en.wikipedia.org/wiki/Free_variables_and_bound_variables

SMT solvers are particularly good at what they do, and are thus often used as
dependencies in proof assistants and similar tools: [Liquid
Haskell][liquid-haskell], which provides formal verification of Haskell
programs[^1], is one such example. As a means of interaction, these components
often rely on [SMT-LIB][smtlib], a language that allows the expression of the basic
operations supported by SMT solvers. The available solvers are multitudinous,
but most of them understand this language, which is thus particularly convenient
as it provides a single, common interface to them.

In this post I'll share some of the outcomes of my internship at Tweag[^2],
during which I worked on optimizing [Pirouette][pirouette] (a Haskell tool for
finding counterexamples to properties specified about programs[^3]) and its
SMT-LIB-based interaction with SMT solvers. Pirouette was unfortunately not fast
enough to be used on advanced examples and, while this limitation still remains,
my work has contributed to improve the runtime performance significantly.

The main improvement was to evaluate SMT-LIB commands through the solver's
bindings instead of calling it through an external process, which halved the time spent interacting with [Z3][wiki-z3] in our measurements.
This led to the creation of [`smtlib-backends`][smtlib-backends], a library exposing these optimizations, which, after speeding-up Pirouette, was also [integrated into Liquid Haskell][lh-integration].

[lh-integration]: https://github.com/ucsd-progsys/liquid-fixpoint/pull/641
[wiki-z3]: https://en.wikipedia.org/wiki/Z3_Theorem_Prover

I'll review the pros and cons of different methods for interacting with
SMT solvers, before introducing `smtlib-backends` and showing how it can improve
these interactions for Haskell programmers.

[^1]: See also [why Liquid Haskell matters][blog-liquidhaskell]
[^2]: Special thanks to Facundo Dominguez and the High Assurance Team for mentoring me during these great 6 months :)
[^3]: Read more about Pirouette [here][blog-pirouette]

# SMT-LIB-based interaction with SMT solvers through external processes

The most generic way to interact with an SMT solver is to use the
[SMT-LIB][smtlib] language. For instance, we may tell the solver to define a
boolean variable `p` with the command `(declare-const p Bool)`, assert that `p`
and its negation hold simultaneously with `(assert (and p (not p)))`, and
check whether this is consistent with `(check-sat)`.

This interaction is usually done by running an SMT solver as an external
process, writing SMT-LIB commands on the process' input channel and reading the
solver's response on its output channel. When using [the Z3 SMT solver][z3] as
an external process in Haskell's GHCi, this looks like:

```haskell
> import System.Process
> import System.IO
> (hIn, hOut, _, _) <- runInteractiveCommand "z3 -in -smt2"
> hPutStrLn hIn "(declare-const p Bool)"
> hPutStrLn hIn "(assert (and p (not p)))"
> hPutStrLn hIn "(check-sat)"
> hFlush hIn
> hGetLine hOut
"unsat"
```

Using SMT-LIB is convenient because it is a universal language and thus isn't
tied to a specific solver: it is common to want to use several solvers in the
same project, as different solvers have different strengths. For instance, a
solver may fare particularly well on [existential formulae][wiki-existential] yet be worse than
average on less complex problems. Another upside of this method is that
co-opting the input and output channels of the solver make for a very informative
debug log that's also trivial to implement.

[wiki-existential]: https://en.wikipedia.org/wiki/Existential_quantification

However convenient this method is, it still has drawbacks. The main one is
that it is especially slow: running solvers as external processes forces the
operating system to constantly switch contexts when running your code.

# Interaction with SMT solvers through their bindings

When the speed of interacting with the SMT solver is a concern, the
alternative method is to use it as a library, through its bindings.
For instance, Z3 provides a [C library][z3-c] that is also exposed in several
other languages. Using the (unofficial) [Haskell bindings][z3-haskell] in GHCi,
our previous example becomes:

```haskell
> :set +m
> import Z3.Monad
> evalZ3 $ do
|   p <- mkFreshBoolVar "p"
|   notP <- mkNot p
|   assert =<< mkAnd [ p, notP ]
|   solverCheck
|
Unsat
```

The upsides and downsides of this method are dual to those of the
previous one: you won't get anything faster than this (as the solver's library
is directly linked inside your compiled code), but it's not as nice
debugging-wise, and switching to another solver's API is very tedious.

# The best of both worlds: SMT-LIB-based interaction with SMT solvers through their bindings

As Pirouette used SMT-LIB-based interaction through external processes, there was
scope for runtime improvement. We didn't want to use interaction
through bindings because we liked the upsides of SMT-LIB-based interaction and
didn't deem the time investment of rewriting the whole interface with bindings
worth our while.

Instead, a good compromise was to use bindings to evaluate the SMT-LIB commands:
instead of running the solver as an external process and writing the commands on
its input channel, we settled on feeding the SMT-LIB commands to Z3's
[`Z3_eval_smtlib2_string`][z3-eval-smtlib2-string] C function which then
directly outputs the solver's response. This ended up being almost twice as fast
as using external processes.

Unfortunately, this solution isn't as straightforward as it sounds, as it seems
SMT solvers aren't designed to be used in such a way. At the time of writing,
[the CVC5 SMT solver][cvc5]'s library simply does not provide a binding for
evaluating SMT-LIB commands and yet the CVC5 executable understands SMT-LIB.
Similarly, Z3 does provide the binding above, but this binding comes with a
constant overhead (that we reduced in a subsequent [pull request to
Z3][z3-pull-request]): it was only designed to be called once
on the concatenated string of SMT-LIB commands, instead of once
for every command. A change that helped mitigate this overhead was to send
commands in batches, as oftentimes the solver's response isn't immediately
needed (e.g. when sending assertions or declarations).

# Introducing `smtlib-backends`

Since having one well-optimized and safe library is more efficient than having
the same code be spread out between different projects, we set out on developing
and maintaining [`smtlib-backends`][smtlib-backends], a new minimal library for
SMT-LIB-based interaction with SMT solvers that implements the optimizations
described above.

Using `smtlib-backends-0.3` inside GHCi, our previous example now unfolds as follows:

```haskell
> -- enable OverloadedStrings to write literal ByteStrings
> :set -XOverloadedStrings
> import SMT-LIB.Backends
> import SMT-LIB.Backends.Process as Process
> let cfg = Process.defaultConfig { Process.exe = "z3", Process.args = [ "-in", "-smt2" ] }
> backend <- Process.toBackend <$> Process.new cfg
> -- 'Queuing' enables sending commands to the SMT solver in batches
> solver <- initSolver Queuing backend
> command_ solver "(declare-const p Bool)"
> command_ solver "(assert (and p (not p)))"
> command  solver "(check-sat)"
"unsat"
```

`smtlib-backends` is designed to provide a generic interface to different
backends, i.e. the different SMT-LIB-based ways to interact with an SMT solver
we've looked at before. We first instantiate the `backend` and only then create
the `solver` wrapper which provides the interface to the `backend`. In the above
example we use [the backend that runs solvers as external
processes][smtlib-backends-process]), but evaluating SMT-LIB commands through
Z3's bindings instead is now as simple as switching to the [corresponding
backend][smtlib-backends-z3]:

```haskell
> import SMT-LIB.Backends.Z3 as Z3
> backend <- Z3.toBackend <$> Z3.new Z3.defaultConfig
> ...
```

`smtlib-backends` has already been plugged to Pirouette, simplifying the
codebase and improving the runtime performance by 12% (on our biggest test file). It was then integrated in [Liquid Fixpoint][liquid-fixpoint], Liquid Haskell's constraint solver. No significant speed-up was observed, but this integration simplified the interface to the solvers and could start making a difference when other parts of the codebase are optimized and become less dominant.

# Conclusion

There are two main ways to interact with an SMT solver: running it as an
external process and sending it SMT-LIB commands through its input channel, or
directly using the solver's bindings when they are provided. Both these methods
have pros and cons, but a good compromise can be reached by evaluating the
SMT-LIB commands using the solver's bindings.

`smtlib-backends` is a new Haskell library that offers an SMT-LIB-based
interface to SMT solvers, yet doesn't force the user to have them run as
external processes. It is well-documented, and the interface is reasonably
universal and abstract. We hope this library will help many projects replace
redundant code: it has already done so for Pirouette and Liquid Haskell!

[blog-liquidhaskell]: https://www.tweag.io/blog/2022-01-19-why-liquid-haskell/
[blog-pirouette]: https://www.tweag.io/blog/2022-07-01-pirouette-2/
[cvc5]: https://cvc5.github.io/
[liquid-fixpoint]: https://github.com/ucsd-progsys/liquid-fixpoint
[liquid-haskell]: https://ucsd-progsys.github.io/liquidhaskell/
[pirouette]: https://github.com/tweag/pirouette/
[smtlib]: https://smtlib.cs.uiowa.edu/
[smtlib-backends]: https://hackage.haskell.org/package/smtlib-backends
[smtlib-backends-github]: https://github.com/tweag/smtlib-backends/
[smtlib-backends-process]: https://hackage.haskell.org/package/smtlib-backends-process
[smtlib-backends-z3]: https://hackage.haskell.org/package/smtlib-backends-z3
[z3]: https://github.com/Z3Prover/Z3/
[z3-c]: https://z3prover.github.io/api/html/group__capi.html
[z3-eval-smtlib2-string]: https://z3prover.github.io/api/html/group__capi.html#ga1bf14e02d5594bf0eb3bf01524e3a0c7
[z3-haskell]: https://github.com/IagoAbal/haskell-z3/
[z3-pull-request]: https://github.com/Z3Prover/z3/pull/6422/
