---
title: "Union and intersection contracts are hard, actually"
shortTitle: "Union and intersection contracts"
author: Yann Hamdaoui
tags: [nix, nickel]
description: "Why union and intersection contracts are hard in a lazy language, and how Nickel handles them."
---

[Nickel][nickel-repo], a configuration language that we are developing at Tweag,
strives to provide first-class data validation. Nickel does so thanks to its
contract system (although contracts are useful beyond data validation). While
the whole story is more involved, contracts can be loosely seen as validation
functions. They can be nicely combined using built-in constructors: one can form
record contracts, array contracts, function contracts, and more.

A natural and useful addition that we soon considered are the boolean
combinators `or` and `and`: at first sight, implementing them for boolean
predicates looks trivial. But contracts are not exactly predicates, and this
subtle difference makes the implementation of general unions (`or`) and
intersections (`and`) contracts deceptively difficult. The existing literature,
while keenly aware of the fact, doesn't really explain why. This post intends to
explain and illustrate this why. You'll find a thorough exposure in our paper
[_Union and intersection contracts are hard, actually_][paper] presented at
[DLS21][dls21].

## Contracts

### Constructors

The generic way to define a custom contract in Nickel is to write a function
that takes the value to check (and a `label` that you can ignore for now), and
either fails, or returns the value otherwise. It's sufficient to define any
contract we may need in principle, but it's not always very ergonomic. Let's
write such a contract for an array whose elements are records with a field
`foo`. The value of each `foo` must also be a number greater than `10`.

```nix
let Foos = fun label value =>
  if builtin.is_array value then
    value
    |> array.map (fun elem =>
      if builtin.is_record elem && record.fields elem == ["foo"] then
        if builtin.is_num elem.foo && elem.foo >= 10 then
          value
        else
          contract.blame_with
            "a foo field is not a number greater than 10"
            label
      else
        contract.blame_with "an element is not a record with a foo field" label)

  else
    contract.blame_with "not an array" label in

[{foo = 20}, {foo = "30"}] | Foos
```

The pipe operator `|` is used to apply a contract to a value. Now compare the
previous definition with a version using the built-in constructors:

```nix
let GreaterThan = fun bound =>
  contract.from_predicate (fun value => builtin.is_num value && value >= bound) in
let Foos = Array {foo | GreaterThan 10} in

[{foo = 20}, {foo = "30"}] | Foos
```

We've used the `from_predicate`, `Array` and record constructors to assemble the
same contract in a more concise, more modular and clearer way. Because such
constructors have built-in support, the error messages are also better
localized[^1].

Currently, Nickel features contract constructors corresponding to native values:

- primitive contracts (numbers, strings, booleans)
- record contracts
- dictionary contracts
- array contracts
- function contracts

### Unions and intersections

The existing constructors can get us quite far already, but some common
contracts are still out of reach. Sometimes, we wish to apply several conditions
to the same value. For example, ensuring that a field is not only a valid port
number, but also a non-reserved port number (greater than `1023`). There is
currently no combinator to build this contract out of `Port` and `GreaterThan`.
If we had intersections (written `/\` thereafter), we could[^2]:

```nix
let NonReservedPort = Port /\ GreaterThan 1023 in
{
  port | NonReservedPort
  ...
}
```

Unions would be very useful too. An ubiquitous example is nullable contracts,
accepting a value that either satisfies some contract `A`, or can be null. This
would simply be `A \/ Null`.

Beyond nullable values, we may allow a field to accept several alternative
representations. For example, a date contract which accepts both a structured
record or a valid ISO-8601 string. Once again, with unions, this contract is
straightforward to write (assuming prior definitions of `Iso8601Date`, `Day`, and
so on):

```nix
let Data = {day | Day, month | Month, year | Year} \/ Iso8601Date
```

## Unions and intersections are hard

However appealing union and intersection contracts may be, they happen --
perhaps surprisingly -- to break fundamental properties of the core Nickel
language. In the following, I only mention unions for simplicity, but all of the
points made have a counterpart for the dual case of intersections.

### Laziness

The crux of the issue concerns _lazy contracts_. By lazy, I mean that the checks
embodied by such contracts don't, and often can't, fire right when the contract
is first evaluated.

#### Eager contracts

Primitive contracts (`Num`, `Bool` and `Str`), and more generally any contract
defined as a boolean predicate (e.g. using `contract.from_predicate`), are
eager. When evaluating `exp | Bool`, the `Bool` contract checks the nature of
`exp` and either fails immediately, or returns the boolean value of `exp`
unchanged. The contract won't interfere with the evaluation anymore.

Union of predicates can be trivially defined as the pointwise `or` operator `||`:

```
P1 \/ ... \/ PN := fun value => P1 value || ... || PN value
```

#### Data contracts

On the other hand, datatype contracts like arrays and records are lazy. To
understand why, consider, for example, [Nixpkgs][nixpkgs]: it is a dictionary
mapping packages to build recipes. That is, a massive, over-50
000-key-value-pair wide dictionary. It is absolutely out of the question to
evaluate the entirety of this dictionary every time one needs to install 10 new
packages: this would result in a painfully slow experience. Outside of Nix, one
may want to query just a field or a subset of a configuration, without having to
evaluate the whole thing.

To cope with such use-cases, Nickel has been made lazy. Expressions are only
evaluated when needed, including the content of arrays and records. To preserve
this capability, array and record contracts must be lazy as well. For if they
were simple eager predicates, applying a top-level contract like `nixpkgs | Packages` would require the full evaluation of `nixpkgs`, in spite of the
language's lazyness.

Concretely, a record contract `{foo | Str, bar | Num}` will:

- check that the value is a record with fields `foo` and `bar`. This part
  happens immediately.
- lazily maps contracts `Str` and `Num` onto the inner fields. That is:

  ```nix
  {foo = 1 + 1, bar = 2} | {foo | Str, bar | Num}`
  # evaluates to
  {foo = 1 + 1 | Str, bar = 2 | Num}
  ```

Here, the `Str` contract violation for `1 + 1` will only cause an error once
`foo` is used or serialized to a configuration, but not when the contract is
first evaluated. If `foo` is never used, the contract won't fail the execution.
Lazy contracts return the original value, but with delayed checks buried inside.

### Union contracts as a side-effect

The problem with lazy contracts is that **union contracts can't know right away
which branch of the union to take**. This implies that the implementation of
union requires a form of backtracking and exception-like control flow, making
them effectful. Take the following example (assume `Pos` and `Neg` are contracts
for positive and negative numbers):

```nix
let Contract = {foo | Pos, bar | Pos } \/ {foo | Neg, bar | Neg} in

let data | Contract = {
  foo = 0 + 5,
  bar = 0 - 7,
}
```

This contract should fail at some point, because `foo` and `bar` are neither
both positive nor both negatives. However, because of lazyness, the union
contract can't evaluate `foo` or `bar` right away. Hence, it doesn't know which
branch of the union to try yet.

If we use `data.foo` later in the program, the union contract will evaluate the
`Neg` contract, acknowledge its failure, and rule out the second branch `{foo | Neg, bar | Neg}`. We still can't figure if `bar` satisfies `Pos` yet.

Symmetrically, if we rather use `data.bar` alone, we can only rule out the first
branch of the union, because `data.bar` fails `Pos`.

Detecting the violation of the original contract is possible **only once we have
used both `data.bar` and `data.foo` in the same program**. Now, imagine that
`data` is defined in a library `data.ncl`. We import the library in two
different files `foo.ncl` and `bar.ncl`, each using only one of the fields:

```nix
# foo.ncl
let data = import "data.ncl" in data.foo

# bar.ncl
let data = import "data.ncl" in data.bar
```

They run totally fine in isolation. Now, if in a third program we import and use
both `foo.ncl` and `bar.ncl`:

```nix
let foo = import "foo.ncl" in
let bar = import "bar.ncl" in
foo + bar
```

The interpreter now reports a contract violation pointing to one of the imports!
This is a spooky action at a distance, or side-effect.

- For the programmer, side-effects are hard to reason about because they prevent local
  reasoning.
- For the interpreter, side-effects inhibit many optimizations and
  program transformations. Nickel being lazy and pure, a lot of program
  optimizations can be applied unconditionally. Not so much once we add unions.

Unions are also complex to implement efficiently. Here, the union contract needs
to maintain shared mutable state between all the use points of `data`. At each
contract violation on `data.foo` or `data.bar`, we need to update the shared
state, and use it to decide if we should actually raise an error. This also
implies that not all contract failures immediately stop the execution anymore,
which requires to turn the simple bail-out semantics of `contract.blame` into a
recoverable exception-like mechanism.

Lazy data contracts may look quite specific to Nickel. Alas, we can recast the
same arguments and examples for function contracts, even in a strict (non-lazy)
language. This is what we did in [the paper][paper]. As first-class functions are a
founding principle of functional programming, a contract system for functional
languages without function contracts would be seriously impaired. Thus, any
functional language with contracts faces difficulties when adding unions to the
mix.

## A way out

We've seen through an example why adding general union and intersection
contracts to any contract system with lazy data contract or function contracts
incurs a prohibitive cost in complexity.

Those issues arise when one tries to implement unions that must work with
arbitrary contracts. We already observed that a number of contracts are not
lazy, such as predicates. And indeed, the union of predicates is trivial to
implement. If Nickel could distinguish the contracts built from
`contract.from_predicate` and remember their boolean definition, we could form
the union of an arbitrary number of predicates together with one arbitrary
contract. Just apply each predicate in order, and if one succeeds, return the
value. If they all fail, apply the last contract.

Even for lazy contracts like records, a lot of unions are actually workable.
Take for example `{foo | Num} \/ {foo | Num, bar | Str}`. Just looking at the
shape of the records, we see that the right one has an additional field `bar`.
Nickel could systematically generate a discriminating predicate based on the
structure of the operands. Here, that would be `record.has_field "bar"`. We can
then implement the union easily, because we can decide right away which branch
to try: if the predicate returns `true`, apply the right contract, otherwise
apply the left one. Such a restricted union constructor would bail out on harder
cases such as `{foo | A} \/ {foo | B}`, where there is no obvious discriminating
predicate.

A [similar analysis and strategy][racket-union] is implemented for unions of
function contracts in the Racket language.
Having such a theoretically restricted but practically useful union constructor is probably
the road we are going to take for Nickel.

[^1]:
    Contracts constructors are also helpful in the interaction with the static
    side of the gradual type system of Nickel, but this is orthogonal to the
    issues explored in this post.

[^2]:
    One can actually attach several contract to a value, and this particular
    example is already possible to write as `{port | Port | GreaterThan 1023}`.
    However, this is only works for the `and` combinator, and such an `and` isn't
    first class: we can't write a contract for an array of non reserved ports in a
    simple way.

[nickel-repo]: https://github.com/tweag/nickel/
[playground]: https://nickel-lang.org/playground/
[paper]: http://arxiv.org/abs/2106.06278
[dls21]: https://conf.researchr.org/home/dls-2021
[racket-union]: https://docs.racket-lang.org/reference/data-structure-contracts.html#%28def._%28%28lib._racket%2Fcontract%2Fbase..rkt%29._or%2Fc%29%29
[nixpkgs]: https://github.com/NixOS/nixpkgs
