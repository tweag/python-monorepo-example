---
title: A dialogue with Liquid Haskell
author: Facundo Domínguez
tags: [haskell, liquidhaskell, formal-methods]
description: "New features to understand old verification failures"
---

[Liquid Haskell][liquidhaskell] (LH) is a tool that gives the Haskell
programmer the ability to express what a program is meant to compute
and to verify that the program does meet this expectation.

One aspect that was not easy to deal with was understanding verification
failures. When a failure happened, we could get a summary of the condition that
LH was trying to verify, but we wouldn't be able to say exactly which facts LH
was using to check it or which facts were missing.

In this post I will present some recent contributions of mine to LH, which give
the user better access to the facts used during verification. We will go through
an example of how these facts can be used to help LH prove a property in a more
informed way than previously possible.

## Relating programs to logical conditions

In programming, we can use verification tools by relating our programs
to logical formulas that these tools can reason about. Let us bring an
example from an [earlier post][whylh]. Suppose that we wanted to ensure
that a list indexing function is only called with indices smaller than
the length of the list. Using LH, we can express this expectation in a
specification of the indexing function, which goes in special comments
marked with `{-@ ... @-}`.

```Haskell
{-@ at :: xs:[a] -> { i:Int | i >= 0 && i < length xs } -> a @-}
at :: [a] -> Int -> a
at (x:_) 0 = x
at (_:xs) i = at xs (i - 1)
```

The arguments of the `at` function get refinement types in the specification.
A refinement type is a subtype of another type, defined by a predicate
characterizing the elements in the subtype.
The language in which the refinement predicate is written is not Haskell, but
a simpler language with constants, function symbols, application, logical
connectives, and arithmetic and comparison operators.

In the function `at`, the predicate of the index argument states
both the expectation that the index is non-negative and that it is strictly
smaller than the length of the list. LH will check that these preconditions
rule out receiving an empty list, and therefore the definition doesn't need
more equations. In addition, at compile time, LH will systematically check that
every invocation of the `at` function satisfies these expectations.

In the last equation LH can assume that `i > 0` because the
preceding equation already checked for `0` and its refinement type says it is
non-negative. LH can also assume that `i < length (_:xs)`
because the refinement type guarantees it. Now LH will verify that
`i - 1 >= 0 && i - 1 < length xs` which is the precondition of the
recursive call.

## Diagnosing verification failures

If a fact can't be verified by LH, but we are sure it really holds, chances
are that extra knowledge must be given to the tool. To do so, it is crucial to
know how close the tool is to verifying it, which in turn requires retrieving
some information from the tool. This is where LH got a
boost recently.

To illustrate how we can now interact with LH's diagnostics, let us prove
a lemma. In LH, lemmas are just functions whose return type is `()`: such a
function doesn't compute any information at runtime, the only information
is that the refinement specification holds (statically).

Here is what we will be proving: if `i` is smaller than the length of `xs`,
then the `i`-th element of `xs` is the same as the `i`-th element of
`append xs ys`.

```Haskell
{-@
atFromAppend
  :: { i:Int | i >= 0 }
  -> { xs:[a] | i < length xs }
  -> ys:[a]
  -> { _:() | at xs i == at (append xs ys) i }
@-}
atFromAppend :: Int -> [a] -> [a] -> ()
```

We will also need the traditional definitions for `length` and `append`.

```Haskell
{-@ reflect length @-}
length :: [a] -> Int
length [] = 0
length (_:xs) = 1 + length xs

{-@ reflect append @-}
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
```

The [`reflect` directive][reflection] allows `length` and `append` to
appear in the predicates of refinement types. This is accomplished by
translating their definitions to the logic language that LH uses. We also
need to reflect the `at` function.

```Haskell
{-@ reflect at @-}
```

As a first attempt, we could implement `atFromAppend` with a single equation.

```Haskell
atFromAppend :: Int -> [a] -> [a] -> ()
atFromAppend _i _xs _ys = ()
```

Since `atFromAppend` is expected to return a value of the `()` type, this
definition is satisfactory to the Haskell compiler.

On the other hand, LH analyses the program and constructs a list of goals to
verify, also known as constraints, which are associated to various
sub-expressions in the program. These constraints are fed to [Liquid
Fixpoint][liquid-fixpoint] (LF), an SMT solver wrapper with some extra
enhancements for LH. When LH reports an error, it usually means that LF failed
to verify some of the constraints.

In the equation above, LH is going to produce an error when checking that the
returned `()` has the expected refinement type.

```
The inferred type
  {v : () | v == ()}

is not a subtype of the required type
  {v : () | at _xs _i == at (append _xs _ys) _i}

...

Constraint id 39
   |
67 | atFromAppend _i _xs _ys = ()
   | ^
```

We can ask LH to dump constraints to a file `.liquid/<module>.fq.prettified`
with the directive `{-@ LIQUID "--save" @-}`. By searching for the
constraint identifier at the end of the message (39 in this case), we can find
the failing constraint:

```
constraint:
  lhs {v : () | v = ()}
  rhs {_ : () | at _xs _i = at (append _xs _ys) _i}
  // META constraint id 39 : test1.hs:67:1-28
  environment:
    _i : {_i : int | _i >= 0}
    _xs : {xs : [a] | _i < length xs}
    _ys : [a]
```

Constraints have a left-hand side with the inferred type of the expression, while the
right-hand side has the expected type. As in most interesting errors from LH,
the inferred and expected refinement types only differ in the predicate part.

Constraints also have an environment with the facts or hypotheses that LF can use to
verify the right-hand side. In this case, it only contains the preconditions
that we would expect from the refinement type of `atFromAppend`, which is
insufficient for the SMT solver to establish the right-hand side of the
constraint.

These explicit environments are a [recent feature][environment-reduction] -- before, all one could do
was to guess them from the source code[^1] or read other dumps that are not
suitable for human consumption really[^2].

Returning to our example, the SMT solver still needs to be informed about
how to unfold the applications of `at` and `append` to have a better chance to
see that the preconditions do ensure the expected property. Though LH has
translated the definitions of these functions to the logic, it cannot use them
yet because each one of the definitional equalities requires the arguments to be
more specific than we know them to be. Let's contemplate the right-hand side
once more.

```
  rhs {_ : () | at _xs _i = at (append _xs _ys) _i}
```

We don't know, for instance, if `_i` is zero or positive, which is necessary
for deciding which equation of `at` can be applied. Furthermore, we don't know
if `_xs` is a non-empty list, which is also a necessary condition to make the
equations applicable.

In order to move forward, we can split the verification task by cases.

```Haskell
atFromAppend 0 (_x:_xs) _ys = ()
atFromAppend _i (_x:_xs) _ys = ()
```

We don't need equations for the case of the empty list because LH checks that
the preconditions rule it out. However, LH still rejects both equations with
different failing constraints. The constraint for the first equation is:

```
constraint:
  lhs {v : () | v = () }
  rhs {_ : () | at (_x:_xs) 0 = at (append (_x:_xs) _ys) 0 }
  // META constraint id 4 : test1.hs:67:31-32
  environment:
    _x : a
    _xs : {_xs : [a] | length (_x:_xs) >= 0}
    _ys : [a]
```

Note that the arguments of `at` in the right-hand side are sufficiently specific, as
they are for `append`. The following equalities from the definition of `at` and
`append` become applicable now.

```Haskell
at (_x:_xs) 0 = _x
append (_x:_xs) _ys = _x : append _xs _ys
```

In order to have LH generate these equalities and feed them to the SMT solver, we
need to enable PLE with `{-@ LIQUID "--ple" @-}`.
PLE stands for [Proof by Logical Evaluation][reflection], which is one of the
LF algorithms enhancing the SMT solver capabilities.

After applying the equality for `append`, the right-hand side becomes

```
  rhs {_ : () | at _xs 0 = at (_x : append _xs _ys) 0 }
```

Which will prompt PLE to produce yet another equality for the `at` function.

```Haskell
at (_x : append _xs _ys) 0 = _x
```

With all of these equalities, LH finally accepts the first equation of
`atFromAppend`.

The second equation is rejected with another constraint.

```
constraint:
  lhs {v : () | v = () }
  rhs {_ : () | at (_x:_xs) _i = at (append (_x:_xs) _ys) _i }
  // META constraint id 8 : test1.hs:68:31-32
  environment:
    _i : {_i:int | _i > 0}
    _x : a
    _xs : {_xs : [a] | length (_x:_xs) >= 0}
    _ys : [a]
```

We have that `_i > 0` because otherwise the control flow would have entered the
first equation instead. The equations that PLE generated are missing because
they go in a separate dump file called `<module>.fq.ple`. If we look for them
there, we can find:

```Haskell
at (_x:_xs) _i = at _xs (_i - 1)
append (_x:_xs) _ys = _x : append _xs _ys
at (_x : append _xs _ys) _i = at (append _xs _ys) (_i - 1)
```

These are informative equalities, but something is still missing to
convince the SMT solver. Let us apply these equalities to the right-hand
side of the constraint.

```Haskell
  at (_x:_xs) _i = at (append (_x:_xs) _ys) _i
  -- apply the first equality to get
  at _xs (_i - 1) = at (append (_x:_xs) _ys) _i
  -- apply the second equality to get
  at _xs (_i - 1) = at (_x : append _xs _ys) _i
  -- apply the third equality to get
  at _xs (_i - 1) = at (append _xs _ys) (_i - 1)
```

To finish, we need the fact at the last line. In a pen-and-paper proof, this
would be the inductive hypothesis of an induction on `_i`. PLE can't help with
discovering that, because it only knows how to unfold definitions. But it turns
out there is a natural way to bring the missing fact into the environment:
calling `atFromAppend` recursively.

```Haskell
{-@
atFromAppend
  :: { i:Int | i >= 0 }
  -> { xs:[a] | i < length xs }
  -> ys:[a]
  -> { ():() | at xs i == at (append xs ys) i }
@-}
atFromAppend :: Int -> [a] -> [a] -> ()
atFromAppend 0 (_x:_xs) _ys = ()
atFromAppend i (_x:xs) ys = atFromAppend (i - 1) xs ys
```

And now LH accepts both equations of `atFromAppend`, which is sound to
do since LH also checks that the recursion of `atFromAppend` is terminating.

## Chatty PLE dumps

Having access to the list of equalities generated by PLE is another feature
that I implemented recently. Before, one needed to edit the LH source code and
activate tracing messages. Until [very recently][optimization-plan], however,
the list of equations would be too large even for moderately complex goals, and
equalities would also be larger than necessary.

To cite a couple of examples, PLE would include the expansion of a
function like

```Haskell
at _xs 0 = if 0 == 0 then head _xs else at (tail _xs) (0 - 1)
at (_x:_xs) _i = if _i == 0 then head (_x:_xs) else at (tail (_x:_xs)) (_i - 1)
append (_x:_xs) _ys = if null (_x:_xs) then _ys else head (_x:_xs) : append (tail (_x:_xs)) _ys
```

The function `at` would be expanded like this for every invocation encountered
in a constraint or the expansion of invocations in a constraint. This is for
`at`, which has only two equations, but functions with more equations are not
uncommon and would lead to further nested `if` statements.

Another difficulty was that PLE would also include the context in which
invocations appear, which is irrelevant most of the time. For instance,
whenever PLE encounters the expression `f (g (at (append (_x:_xs) _ys) _i))`,
one would hope that telling the SMT solver how to expand `append` would
suffice.

```Haskell
append (_x:_xs) _ys = _x : append _xs _ys
```

However, PLE would generate instead the equality

```Haskell
f (g (at (append (_x:_xs) _ys) _i)) = f (g (at (_x : append _xs _ys) _i))
```

The same `append` equality would apply in multiple contexts, and PLE would
produce one such equality for every context. More cases where the generated
equalities were more chatty than expected are discussed in the
[LF repo][optimization-plan].

## PLE present and future

Improving the introspection capabilities of Liquid Haskell is important
for user experience. More often than not, LH will need additional
help from the user to verify a property, and it is vital that the
communication is fluent for applying LH at large scale.

There are many corners to improve. Integrating the information of the
various dumps with cross references and consistent presentation of names
still remains to be done. There is extra noise and verbosity that I have filtered
when presenting the dumps in this post that could be eliminated. There
are some examples where PLE still reports more equations than
necessary. And beyond PLE, there are other algorithms in LF that still
offer little feedback to the user when verification fails[^3].

However, PLE already yields vastly smaller sets of equations
than it used to. This not only influences the feedback the user
receives but also the amount of work that is demanded from the SMT solver,
with time reductions in benchmarks that range between 10% and 50%. On the other
hand, the experience of revising and reformulating the assumptions on the
implementation of PLE was enlightening and rewarding on all kinds of
technical details that constitute the integration of PLE in the larger
context of LH[^4]. Please, join me in celebrating this milestone. I look
forward to hearing from your experience with the latest LH.

[^1]: In truth, the error message that LH offers also contains a summary of hypotheses that we have elided in our error snippet. In this example, the `.prettified` file doesn't add any new hypotheses, but this is not always the case. In an ideal future, the `.prettified` dump and the hypotheses in the reported error would always match, rendering the `.prettiffied` dump obsolete.
[^2]: These other dumps are produced with `--save` as well, and have suffix `.fq`. They are still text files, but have so many cross references that it is hard to grasp their meaning for more than the smallest examples.
[^3]: Algorithms like [Fusion][fusion] for local refinement typing and [REST][rest].
[^4]: Special thanks to Niki Vazou and Ranjit Jhala who helped me navigate the network of design decisions at crucial times.

[environment-reduction]: https://github.com/ucsd-progsys/liquidhaskell/issues/1848
[fusion]: https://ranjitjhala.github.io/static/local_refinement_typing.pdf
[liquidhaskell]: https://github.com/ucsd-progsys/liquidhaskell
[liquid-fixpoint]: https://github.com/ucsd-progsys/liquid-fixpoint
[reflection]: https://ranjitjhala.github.io/static/refinement_reflection.pdf
[rest]: https://github.com/zgrannan/rest
[optimization-plan]: https://github.com/ucsd-progsys/liquid-fixpoint/issues/500
[whylh]: https://www.tweag.io/blog/2022-01-19-why-liquid-haskell/
