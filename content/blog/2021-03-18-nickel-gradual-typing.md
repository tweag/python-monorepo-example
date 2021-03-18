---
title: "Types à la carte in Nickel"
shortTitle: "Types à la carte in Nickel"
author: Yann Hamdaoui
description: "A dive into Nickel's gradual type system, which mixes dynamic and
static typing"
tags: [nix]
---

This post is the third one of a series on [Nickel][nickel-repo], a new
configuration language we are developing at Tweag. In this post, I explore the
design of Nickel's type system, which mixes static and dynamic typing, and the
reasons behind this choice.

1. [Presenting Nickel: better configuration for less][nickel-intro]
2. [Programming with contracts in Nickel][nickel-contracts]
3. Types à la carte in Nickel

Tweag's core engineering mantras are "functional, typed, immutable". When
other constraints allow it (the right tool for the job and all that), I
personally go for a statically typed language whenever I can.

But the Nickel language is a tad different, for it is a configuration language.
You usually run a terminating program once on fixed inputs to generate a static
text file. In this context, any type error will most likely either be triggered
at evaluation anyway, typechecker or not, or be irrelevant (dead code). Even
more if you add data validation, which typing can seldom totally replace:
statically enforcing that a string is a valid URL, for example, would require a
powerful type system. If you have to validate anyway, checking that a value is a
number at run-time on the other hand is trivial.

Nickel also aims at being as interoperable with JSON as possible, and dealing with
JSON values in a typed manner may be painful. For all these reasons, being
untyped[^1] in configuration code is appealing.

But this is not true of all code. Library code is written to be reused many
times in many different settings. Although specialised in configuration, Nickel
is a proper programming language, and one of its value propositions is precisely
to provide abstractions to avoid repeating yourself. For reusable code, static
typing sounds like the natural choice, bringing in all the usual benefits.

How to get out of this dilemma?

[^1]:
  I will use typed to mean statically typed, and untyped to mean dynamically
  typed.

## Gradual typing

[Gradual typing][gradual-typing] reconciles the two belligerents by allowing
both typed code and untyped code to coexist. Not only to coexist, but most
importantly, to _interact_.

One common use-case of gradual typing is to retrofit static typing on top of an
existing dynamically typed language, allowing to gradually -- hence the name --
type an existing codebase. In the case of Nickel, gradual typing is used on its
own, because optional typing makes sense. In both situations, gradual typing
provides a formal and principled framework to have both typed and untyped code
living in a relative harmony.

## Promises, promises!

Since configuration code is to be untyped, and make for the majority of Nickel
code, untyped is the default. A basic configuration looks like JSON, up to
minor syntactic differences:

```
{
  host = "google.com",
  port = 80,
  protocol = "http",
}
```

Typechecking is triggered by a type annotation, introduced by `:`. Annotations
can either be apposed to a variable name or to an expression:

```
let makePort : Str -> Num = fun protocol =>
  if protocol == "http" then
    80
  else if protocol == "ftp" then
    21
  else
    null in
let unusedBad = 10 ++ "a" in
{
  port = makePort protocol,
  protocol = ("ht" ++ "tp" : Str),
}
```

In this example, `makePort` is a function taking a string and returning a
number. It is annotated, causing the typechecker to kick in. It makes sure that
each sub-expression is well-typed. Notice that subterms don't need any other
annotation: Nickel is able to guess most of the types using unification-based
type inference.

Such a static type annotation is also called a **promise**, as you make a firm
promise to the typechecker about the type of an expression.

Static typechecking ends with `makePort`, and although `unusedBad` is clearly
ill-typed (concatenating a number and a string), it won't cause any typechecking
error.

Can you guess the result of trying to run this program?

```
error: Incompatible types
  ┌─ repl-input-1:7:5
  │
7 │     null in
  │     ^^^^ this expression
  │
  = The type of the expression was expected to be `Num`
  = The type of the expression was inferred to be `Dyn`
  = These types are not compatible
```

The typechecker rightly complains than `null` is not a number. If we fix this
(for now, substituting it with `-1`), the programs runs correctly:

```
$nickel export <<< ...
{
  "port": 80,
  "protocol": "http"
}
```

`unusedBad` doesn't cause any error at run-time. Due to [laziness][laziness], it is never
evaluated. If we were to add a type annotation for it though, the typechecker
would reject our program.

To recap, the typechecker is a lurker by default, letting us do pretty much what
we want. It is triggered by a type annotation `exp : Type`, in which case it
switches on and statically typechecks the expression.

## Who's to be blamed

So far, so good. Now, consider the following example:

```
let add : Num -> Num -> Num = fun x y => x + y in
add "a" 0
```

As far as typechecking goes, only the body of `add` is to be checked, and it is
well-typed. However, `add` is called with a parameter of the wrong type by an
untyped chunk. Without an additional safety mechanism, one would get this
runtime type error:

```
error: Type error
  ┌─ repl-input-0:1:26
  │
1 │ let add : Num -> Num -> Num = fun x y => x + y
  │                                              ^ This expression has type Str, but Num was expected
  │
[..]
```

The error first points to a location inside the body of `add`. It doesn't feel
right, and kinda defeats the purpose of typing: while our function should be
guaranteed to be well-behaved, any untyped code calling to it can sneak in
ill-typed terms via the parameters. In turn, this raises errors that are located
in well behaving code. In this case, which is deliberately trivial, the end of
the error message elided as `[..]` turns out to give us enough information to
diagnose the actual issue. This is not necessarily the case for more complex
real life functions.

There's not much we can do statically. The whole point of gradual typing being
to accommodate for untyped code, we can't require the call site to be statically
typed. Otherwise, types would contaminate everything and we might as well make
our language fully statically typed.

We can do something at run-time, though. Assuming [type soundness][type-soundness],
no type error should arise in the body of a well-typed function at evaluation.
The only sources of type errors are the parameters provided by the caller.

Thus, we just have to control the boundary between typed and untyped blocks by
checking the validity of the parameters provided by the caller. If we actually
input the previous example in the Nickel REPL, we don't get the above error
message, but this one instead:

```
nickel> let add : Num -> Num = fun x y => x + y in
add 5 "a"

error: Blame error: contract broken by the caller.
  ┌─ :1:8
  │
1 │ Num -> Num -> Num
  │        --- expected type of the argument provided by the caller
  │
  ┌─ repl-input-6:1:31
  │
1 │ let add : Num -> Num -> Num = fun x y => x + y
  │                               ^^^^^^^^^^^^^^^^ applied to this expression
  │
[..]
note:
  ┌─ repl-input-7:1:1
  │
1 │ add 5 "a"
  │ --------- (1) calling <func>
[..]
```

This error happens before the body of `add` is even entered. The Nickel
interpreter wraps `add` in a function that first checks the parameters to be of
the required type before actually calling `add`. Sounds familiar? This is
exactly what we described in [the post on contracts][nickel-contracts]. That is,
typed functions are automatically guarded by a _higher-order contract_. This
ensures that type errors are caught before entering well-typed land, which
greatly improves error locality.

In summary, to avoid sneaking ill-typed value in well-typed blocks, the Nickel
interpreter automatically protects typed functions by inserting appropriate
contracts.

## A contract with the devil

We have dealt with the issue of calling typed code from untyped code. A natural
follow-up is to examine the dual case: how can one use definitions living in
untyped code inside a statically typed context? Consider the following example:

```
// this example does NOT typecheck
let f = fun x => if x then 10 else "a" in
let doStuffToNum: Num -> Num = fun arg =>
  arg + (f true) in

doStuffToNum 1
```

The typed function `doStuffToNum` calls to an untyped function `f`. `f true` turns out to be a
number indeed, but `f` itself is not well-typed, because the types of the `if`
and the `else` branch don't match. No amount of additional type annotations can
make this program accepted.

See what happens in practice:

```
error: Incompatible types
  ┌─ repl-input-1:3:10
  │
3 │   arg + (f true) in
  │          ^ this expression
  │
  = The type of the expression was expected to be `_a -> Num`
  = The type of the expression was inferred to be `Dyn`
  = These types are not compatible
```

`f` not being annotated, the typechecker can't do much better than to give `f`
the dynamic type `Dyn` (although in some trivial cases, it can infer a better
type). Since it was expecting a function returning `Num`, it complains. It seems
we are doomed to restrict our usage of untyped variables to trivial expressions,
or to type them all.

Or are we? One more time, contracts come to the rescue. Going back to the [post on contracts][nickel-contracts]
again, contracts are enforced similarly to types, but using `|` instead of `:`. Let us fix our example:

```
let doStuffToNum: Num -> Num = fun arg =>
  arg + (f true | Num) in
```

We just applied a `Num` contract to `f true`, and surprise, this code
typechecks! Our typechecker didn't get magically smarter. By adding this
contract check, we ensure the fundamental property that _`f true` will either
evaluate to a number or fail at run-time with an appropriate contract error_. In
particular, no value of type `Str`, for example, can ever enter our well-typed
paradise `add`. When writing `exp | Type` in a typed block, two things happen:

1. The typechecker switches back to the default mode inside `exp`, where it
   doesn't enforce anything until the next promise (annotation).
2. The typechecker blindly assumes that the type of `exp | Type` is `Type`.
   Hence, contract checks are also called **assume**.

Put differently, a contract check is considered a **type cast** by the static
type system, whose correctness check is **delayed to run-time**.

Behold: this implies that something like `(5 | Bool) : Bool` typechecks. How
outrageous, for a typed functional language proponent. But even languages like
Haskell have some side conditions to type soundness: `b :: Bool` doesn't
guarantee that `b` evaluate to a boolean, for it can loop, or raise an
exception. Minimizing the amount of such possibilities is surely for the
better, but the important point remains that `b` never silently evaluates to a
string.

To conclude, we can use contracts as delayed type casts, to make the typechecker
accept untyped terms in a typed context. This is useful to import values from
untyped code, or to write expressions that we know are correct but that the
typechecker wouldn't accept.

## Conclusion

There is more to say about Nickel's type system, that features parametric
polymorphism or structural typing with row polymorphism for records, to cite
some interesting aspects. The purpose of this post is rather to explain the
essence of gradual typing, why it makes sense in the context of Nickel, and how
it is implemented. We've seen that contracts are a fundamental ingredient for
the interaction between typed and untyped code in both ways.

[nickel-intro]: https://www.tweag.io/blog/2020-10-22-nickel-open-sourcing/
[nickel-contracts]: https://www.tweag.io/blog/2021-01-22-nickel-contracts/
[ml-type-inference]: https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
[gradual-typing]: https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/
[nickel-repo]: https://github.com/tweag/nickel/
[type-soundness]: https://papl.cs.brown.edu/2014/safety-soundness.html
[laziness]: https://en.wikipedia.org/wiki/Lazy_evaluation
