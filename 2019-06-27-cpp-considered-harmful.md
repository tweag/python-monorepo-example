---
title: "CPP<br/> considered harmful"
author: Mathieu Boespflug
tags: haskell
---

Edsger Dijkstra [took issue][goto-considered-harmful] with the
"unbridled" use of the `goto` programming construct in 1968. He noted
that our already limited ability to reason about the dynamic behaviour
of imperative programs was nigh impossible in the presence of
`goto`-jumps to arbitrary labels. With `goto`, control flow is
permitted to be entirely disconnected from the structure of the code
as written, so it becomes very hard to guess at the value of variables
without actually running the program. Just like `goto` for *dynamic
behaviour*, the unbridled use of the C preprocessor (CPP) to
conditionally compile code is hampering our ability to analyse and
manipulate *code*. In fact the commonality in the arguments made it
difficult to resist the temptation to reuse the already overused title
of Dijkstra's paper. In this post, I want to [rehash][ifdef-harmful]
the argument that CPP should be dispensed with because it makes bad
code too tempting to write, like Dijkstra did for `goto`.

The idea that *unrestricted* conditional compilation should be avoided
is old news. While it is extremely common in programming languages of
the 70's (like C), it is nonexistent in popular programming languages
of the 00's (like Rust or Go). Haskell, a language born in the late
80's, punted on difficult problems like multi-platform support and
handling breaking changes in upstream dependencies. Using CPP to deal
with these issues was a quick fix, and a historical accident that
somehow survived to this day.

Say I'm writing innocent enough code:

```haskell
module Main where

main = do
  name <- getLine
  putStrLn $ "Hello " <> name
```

This works fine with the latest GHC. But using GHC circa 2016, this
won't compile, because `(<>)` is not part of the `Prelude`. At this
point I have four choices:

1. Decide that I don't care about old GHC (or in general any old
   version of a package dependency) and move on.
2. Use `(++)` instead of `(<>)` (they happen to have the same meaning
   in this particular case), or otherwise rewrite my code, e.g. by
   adding `import Data.Semigroup ((<>))`.
3. Write two versions of my module: one that works for newer GHC, and
   one that works for older GHC, and letting the build system decide
   which one to pick.
4. Use conditional compilation.

This last solution might look like this:

```haskell
module Main where

main = do
  name <- getLine
#if MIN_VERSION_base(X, X, X)
  putStrLn $ "Hello " <> name
#else
  putStrLn $ "Hello " ++ name
```

Clearly Option 1 or Option 2 would work out better than this already
unreadable mess, which might only get worse when other
incompatibilities arise, requiring *nested* conditional compilation.
Some might argue that Option 1 (dropping support) isn't used nearly
often enough. I agree, all the more so given the success and broad
adoption of [Stackage][stackage], but it's a debate for another
day. Option 2 is about resisting the temptation to use new functions
not previously available. But how do we deal with breaking changes in
dependencies? In such a case only Option 3 and Option 4 are available.

In version 2.1, the [singletons][singletons] library exposed two
datatype definitions:


```haskell
data Proxy t :: * -> *
data KProxy t :: * -> *
```

From version 2.2 onwards, only one, more general datatype is exposed:

```haskell
data Proxy k (t :: k) :: * -> *
```

In user code, `KProxy` now needs to be replaced everywhere with
`Proxy`. Unlike in our previous example, Option 2 is not available:
there is no way to change the code in such a way that it compiles with
*both* singletons-2.1 and singletons-2.2. Option 3 doesn't look
terribly appealing at first blush
because [Don't Repeat Yourself (DRY)][dry].

The temptation is high to introduce conditional compilation
everywhere. The common way to do so is with CPP at each use site:

```haskell
#if MIN_VERSION_singletons(2,2,0)
  ... KProxy ...
#else
  ... Proxy ...
#endif
```

The problem is that with conditional compilation using CPP, we lose
a great deal. It is no longer possible to do any syntactic analysis of
your modules, say to lint check or to apply code formatters. The
source is no longer syntactically valid Haskell: it needs
a preprocessor to defang it first. Which would be fine, except that if
you want to analyze every branch of every `#if` and `#ifdef`, you need
to run the preprocessor with every combination of every predicate
(true/false or defined/undefined for every macro), leading to an
exponential blow up in the number of times you need to
run [HLint][hlint], and other automatic tools like code formatters
thrown out the window entirely.

What if we taught these tools CPP syntax, to obviate having to
evaluate each branch of the preprocessor? Like unrestricted `goto`
allowing labels pretty much anywhere, CPP conditionals can appear
anywhere at all: module headers, import declarations, in the middle of
an expression, or indeed arbitrary *different* combinations of these in
each branch of a conditional. Each branch need not be syntactically
correct, with balanced parentheses and well-scoped names. Parsing
becomes a very complicated problem.

The vexing issue is that we seldom need CPP for conditional
compilation in the first place, if at all. There is no silver bullet
for avoiding CPP. Giving up `goto` means turning to structured control
flow constructs (e.g. `while`-loops or `try`/`catch`), neither of
which completely replacing `goto`, but together covering most use
cases for `goto`. Giving up CPP means turning to any of the following
strategies to achieve broader compatibility:

* **Push all configuration to the build system:** if you're writing
  a cross-platform network library, put all Win32 code in separate
  files from the Linux code. Let the build system choose what modules
  to build depending on the target platform. No CPP required.
* **Designing for extensibility:** the [network][network] library has
  a datatype of socket address domains. Since not all platforms
  support all domains, this forces conditional compilation in socket
  address primitives. By contrast, the [socket][socket] library has an
  open type family of domains. Support for each domain can be kept in
  a dedicated source file, as above.
* **Abstract away compatibility concerns:** if you really need to
  target multiple versions of a dependency, create a small module that
  abstracts away the differences and depend on that.
* **Use structured conditional compilation:** if you really have to
  use conditional compilation within a source file, prefer
  *structured* conditional compilation. Template Haskell can
  conditionally define a function. Unlike CPP, using Template Haskell
  is still syntactically correct Haskell.

We should challenge the idea that CPP is unavoidable. After all,
multi-platform support and backwards compatibility are universal
concerns for all programming languages. Unstructured conditional
compilation is highly unusual in many of these, even for
multi-platform code. Like some of the pioneers of software did with
the `goto` of old, we overestimate the need for the power of
non-structure, while forgetting about the benefits of structure.
Foregoing CPP entirely should rid us of our illusions.

[dry]: https://en.wikipedia.org/wiki/Don%27t_repeat_yourself
[hlint]: https://hackage.haskell.org/package/hlint
[network]: https://hackage.haskell.org/package/network
[singletons]: http://hackage.haskell.org/package/singletons
[socket]: https://hackage.haskell.org/package/socket
[stackage]: https://www.stackage.org/
[goto-considered-harmful]: https://homepages.cwi.nl/~storm/teaching/reader/Dijkstra68.pdf
[ifdef-harmful]: http://www.literateprogramming.com/ifdefs.pdf
