---
title: "Programming with contracts in Nickel"
shortTitle: "Programming with contracts in Nickel"
author: Yann Hamdaoui
tags: [nix, nickel]
---

1. [Presenting Nickel: better configuration for less][nickel-1]
2. Programming with contracts in Nickel
3. [Types à la carte in Nickel][nickel-gradual-typing]

In a [previous post][nickel-1], I gave a taste of [Nickel][nickel-repo], a configuration
language we are developing at Tweag. One cool feature of Nickel is the ability
to validate data and enforce program invariants using so-called contracts. In
this post, I introduce the general concept of programming with contracts and
illustrate it in Nickel.

## Contracts are everywhere

> You go to your favorite bakery and buy a croissant. Is there a contract binding you to the baker?

A long time ago, I was puzzled by this very first question of a law class exam.
It looked really simple, yet I had absolutely no clue.

> "Ehm..No?"

A contract should write down terms and conditions, and be signed by both
parties. How could buying a croissant involve such a daunting liability?

Well, I have to confess that this exam didn't go very well.

It turns out the sheer act of selling something implicitly and automatically
establishes a legally binding contract between both parties (at least in
[France][contract-vente]). For once, the programming world is not that different
from the physical world: if I see a `ConcurrentHashmap` class in a Java library,
given the context of Java's naming conventions, I rightfully expect it to be a
thread-safe implementation of a hashmap. This is a form of contract. If a
programmer uses `ConcurrentHashmap` to name a class that implements a non-thread
safe linked list, they should probably be sent to court.

Contracts may take multiple forms. A contract can be _explicit_, such as in a
formal specification, or _implicit_, as in the `ConcurrentHashMap` example. They
can be _enforced_ or not, such as a type signature in a statically typed
language versus an invariant written as a comment in a dynamically typed
language. Here are a few examples:

<center>

| Contract             | Explicitness                             | Enforced                         |
| -------------------- | ---------------------------------------- | -------------------------------- |
| Static types         | Implicit if inferred, explicit otherwise | Yes, at compile time             |
| Dynamic types        | Implicit                                 | Yes, at run-time                 |
| Documentation        | Explicit                                 | No                               |
| Naming               | Implicit                                 | No                               |
| `assert()` primitive | Explicit                                 | Yes, at run-time                 |
| pre/post conditions  | Explicit                                 | Yes, at run-time or compile time |

</center>

As often, explicit is better than implicit: it leaves no room for
misunderstanding. Enforced is better than not, because I would rather be
protected by a proper legal system in case of contract violation.

## Programming with Contracts

Until now, I've been using the word contract in a wide sense. It turns out
contracts also refer to a particular programming paradigm which embodies the
general notion pretty well. Such contracts are _explicit_ and _enforced_,
following our terminology. They are most notably used in
[Racket][racket-contracts]. From now on, I shall use contract in this more
specific sense.

To first approximation, contracts are assertions. They check that a value
satisfies some property at run-time. If the test passes, the execution can go on
normally. Otherwise, an error is raised.

In Nickel, one can enforce a contract using the `|` operator:

```
let x = (1 + 1 | Num) in 2*x
```

Here, `x` is bound to a `Num` contract. When evaluating `x`, the following steps
are performed:

1. evaluate `1 + 1`
2. check that the result is a number
3. if it is, return the expression unchanged. Otherwise, raise an error that
   halts the program.

Let's see it in action:

```
$ nickel repl
nickel> 1 + 1 | Num
2

nickel> false | Num
error: contract broken by a value
  ┌─ :1:1
  │
1 │ Num
  │ --- expected type
  │
  ┌─ repl-input-1:1:1
  │
1 │ false | Num
  │ ^^^^^ applied to this expression

note:
  ┌─ repl-input-1:1:9
  │
1 │ false | Num
  │         ^^^ bound here
```

## Contracts versus types

I've described contracts as assertions, but the above snippet suspiciously
resembles a type annotation. How do contracts compare to types? First of all,
contracts are checked at run-time, so they would correspond to dynamic typing
rather than static typing. Secondly, contracts can check more than just the
membership to a type:

```
nickel> let GreaterThan2 = fun label x =>
  if builtin.is_num x then
    if x > 2 then
      x
    else
      contract.blame_with "smaller or equals" label
  else
    contract.blame_with "not a number" label

nickel> 3 | GreaterThan2
3

nickel> 1 | GreaterThan2
error: contract broken by a value: smaller or equals
[..]

nickel> "a" | GreaterThan2
error: contract broken by a value: not a number
[..]
```

Here, we just built a _custom contract_. A custom contract is a function of two
arguments:

- the label `label`, carrying information for error reporting.
- the value `x` to be tested.

If the value satisfies the condition, it is returned. Otherwise, a call to
`blame_with` signals rejection with an error message. When evaluating
`value | Contract`, the interpreter calls `Contract` with an appropriate label and
`value` as arguments.

Such custom contracts can check arbitrary properties. Enforcing the property of
being greater than two using static types is rather hard, requiring a fancy type
system such as [refinement types][refinement-types] , while the role of dynamic
types generally stops at distinguishing basic datatypes and functions.

Back to our first example `1 + 1 | Num`, we could have written instead:

```
let MyNum = fun label x =>
  if builtin.is_num x then x else contract.blame label in
(1 + 1 | MyNum)
```

This is in fact pretty much what `1 + 1 | Num` evaluates to. While a contract is
not the same entity as a type, one can derive a contract from any type. Writing
`1 + 1 | Num` asks the interpreter to derive a contract from the type `Num` and
to check `1 + 1` against it. This is just a convenient syntax to specify common
contracts.

To sum up, contracts are just glorified assertions. Also, there is this
incredibly convenient syntax that spares us a whole two characters by writing
`Num` instead of `MyNum`. So... is that all the fuss is about?

## Function contracts

Until now, we have only considered what are called _flat_ contracts, which
operate on data. But Nickel is a functional programming language: so what about
function contracts? They exist too!

```
let f | Str -> Num = fun x => if x == "a" then 0 else 1 in ...
```

Here again, we ask Nickel to derive a contract for us, from the type `Str -> Num`
of functions sending strings to numbers. To find out how this contract could
work, we must understand what is the defining property of a function of type
`Str -> Num` that the contract should enforce.

A function of type `Str -> Num` has a _duty_: it must produce a number. But what
if I call `f` on a boolean? That's unfair, because the function has also a
_right_: the argument must be a string. The full contract is thus: if you give
me a string, I give you a number. If you give me something else, you broke the
contract, so I can't guarantee anything. Another way of viewing it is that the
left side of the arrow represents **preconditions** on the input while the right
side represents **postconditions** on the output.

More than flat contracts, function contracts show similarities with traditional
legal contracts. We have two parties: the **caller**, `f "b"`, and the
**callee**, `f`. Both must meet conditions: the caller must provide a string
while the callee must return a number.

In practice, inspecting the term `f` can tell us if it is a function at most.
This is because a function is inert, waiting for an argument to hand back a
result. In consequence, the contract is doomed to fire only when `f` is applied
to an argument, in which case it checks that:

1. The argument satisfies the `Str` contract
2. The return value satisfies the `Num` contract

The interpreter performs additional bookkeeping to be able to correctly blame
the offending code in case of a higher-order contract violation:

```
nickel> let f | Str -> Num = fun x => if x == "a" then 0 else 1
nickel> f "a"
0

nickel> f 0
error: contract broken by the caller
  ┌─ :1:1
  │
1 │ Str -> Num
  │ --- expected type of the argument provided by the caller
  │
  ┌─ repl-input-8:1:3
  │
1 │ f 0
  │   - evaluated to this expression
  │
[..]

note:
  ┌─ repl-input-6:1:9
  │
1 │ let f | Str -> Num = fun x => if x == "a" then 0 else 1
  │         ^^^^^^^^^^ bound here


nickel> let f | Str -> Num = fun x => x in f "a"
error: contract broken by a function
  ┌─ :1:8
  │
1 │ Str -> Num
  │        --- expected return type
  │
  ┌─ repl-input-9:1:38
  │
1 │ let f | Str -> Num = fun x => x in f "a"
  │                                      --- evaluated to this expression
  │
  ┌─ <unknown> (generated by evaluation):1:1
  │
1 │ "a"
  │ --- evaluated to this value
  │
[..]

note:
  ┌─ repl-input-9:1:9
  │
1 │ let f | Str -> Num = fun x => x in f "a"
  │         ^^^^^^^^^^ bound here
```

These examples illustrate three possible situations:

1. The contract is honored by both parties.
2. The contract is broken by the caller, which provides a number instead of a
   string.
3. The contract is broken by the function (callee), which rightfully got a
   string but returned a string instead of a number.

Combined with custom contracts, function contracts make it possible to express
succinctly non-trivial invariants:

```
let f | GreaterThan2 -> GreaterThan2 = fun x => x + 1 in ..
```

## Conclusion

In this post, I introduced programming with contracts. Contracts offer a
principled and ergonomic way of validating data and enforcing invariants with a
good error reporting story. Contracts can express arbitrary properties that are
hard to enforce statically, and they can handle higher-order functions.

Contracts also have a special relationship with static typing. While we compared
them as competitors somehow, contracts and static types are actually
complementary, reunited in the setting of [gradual typing][gradual-typing].
Nickel has gradual types, which will be the subject of a coming post.

The examples here are illustrative, but we'll see more specific and compelling
usages of contracts in yet another coming post about Nickel's meta-values,
which, together with contracts, serve as a unified way to describe and validate
configurations.

[nickel-1]: https://www.tweag.io/blog/2020-10-22-nickel-open-sourcing/
[contract-vente]: https://fr.wikipedia.org/wiki/Contrat_de_vente_en_France#Formation_du_contrat
[racket-contracts]: https://docs.racket-lang.org/reference/contracts.html
[refinement-types]: https://ucsd-progsys.github.io/liquidhaskell-blog/
[laziness]: https://en.wikipedia.org/wiki/Lazy_evaluation
[gradual-typing]: https://wphomes.soic.indiana.edu/jsiek/what-is-gradual-typing/
[nickel-repo]: https://github.com/tweag/nickel/
[nickel-gradual-typing]: https://www.tweag.io/blog/2021-03-18-nickel-gradual-typing/
