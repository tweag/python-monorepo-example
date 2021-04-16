---
title: "Nickel: better configuration for less"
shortTitle: "Nickel: better configuration for less"
author: Yann Hamdaoui
tags: [nix, nickel]
---

1. Presenting Nickel: better configuration for less
2. [Programming with contracts in Nickel][nickel-contracts]
3. [Types à la carte in Nickel][nickel-gradual-typing]

We are making the [Nickel][nickel] repository public. Nickel is an experimental configuration
language developed at Tweag. While this is not the time for the first
release yet, it is an occasion to talk about this project. The goal of this
post is to give a high-level overview of the project. If your curiosity is tickled
but you are left wanting to learn more, fear not, as we will publish
more blog posts on specific aspects of the language in the future. But for
now, let's have a tour!

\[**Disclaimer**: the actual syntax of Nickel being still worked on, I'm freely
using as-of-yet non-existing syntax for illustrative purposes. The underlying
features are however already supported.\]

## The inception

We, at Tweag, are avid users of the [Nix][nix] package manager. As it
happens, the configuration language for Nix (also called Nix) is
a pretty good configuration language, and would be applicable to many
more things than just package management.

All in all, the Nix language is a lazy JSON with functions. It is
simple yet powerful. It is used to generate Nix's package descriptions
but would be well
suited to write any kind of configuration ([Terraform][terraform],
[Kubernetes][kubernetes], etc…).

The rub is that the interpreter for Nix-the-language is tightly
coupled with Nix-the-package manager. So, as it stands, using the
Nix language for anything else than package management is a rather
painful exercise.

Nickel is our attempt at answering the question: what would
Nix-the-language look like if it was split from the package manager?
While taking the opportunity to improve the language a little,
building on the experience of the Nix community over the years.

## What's Nickel, exactly ?

Nickel is a lightweight generic configuration language. In that it can
replace YAML as your application's configuration language. Unlike
YAML, though, it anticipates large configurations by being
programmable. Another way to use Nickel is to generate static
configuration files -- _e.g._ in JSON, YAML -- that are then fed to another system. Like
Nix, it is designed to have a simple, well-understood core: at its
heart, it is JSON with functions.

But past experience with Nix also brings some insights on which aspects of the
language could be improved. Whatever the initial scope of a language is, it will
almost surely be used in a way that deviates from the original plan: you create
a configuration language to describe software packages, and next thing you know,
somebody needs to implement a [topological sort][toposort].

Nickel strives to retain the simplicity of Nix, while extending it
according to this feedback.
Though, you can do perfectly fine without the new features and just write Nix-like code.

## Yet another configuration language

At this point you're probably wondering if this hasn't already been done elsewhere.
It seems that more and more languages are born every day, and surely there
already exist configuration languages with a similar purpose to Nickel:
[Starlark][starlark], [Jsonnet][jsonnet], [Dhall][dhall] or [CUE][cue], to name
a few. So why Nickel?

## Typing

Perhaps the most important difference with other configuration languages is
Nickel's approach to typing.

Some languages, such as [Jsonnet][jsonnet] or [Starlark][starlark], are not
statically typed. Indeed, static types can be seen as superflous in a configuration
language: if your program is only run once on fixed inputs, any type error will
be reported at run-time anyway. Why bother with a static type system?

On the other hand, more and more systems rely on complex configurations, such as
cloud infrastructure ([Terraform][terraform], [Kubernetes][kubernetes] or
[NixOps][nix-ops]), leading the corresponding programs to become increasingly
complex, to the point where static types are beneficial. For reusable code --
that is, library functions -- static types add structure, serve as
documentation, and eliminate bugs early.

Although less common, some configuration languages are statically typed,
including [Dhall][dhall] and [CUE][cue].

Dhall features a powerful type system that is able to type a wide range of
idioms. But it is complex, requiring some experience to become fluent in.

CUE is closer to what we are striving for. It has an optional and well-behaved
type system with strong guarantees. In exchange for which, one can't write nor
type higher-order functions in general, even if some simple functions are
possible to encode.

### Gradual typing

Nickel, features a [_gradual type system_][gradual-typing].
Gradual types are unobtrusive: they make it possible to statically
type reusable parts of your programs, but you are still free to write
configurations without any types. The
interpreter safely handles the interaction between the typed and untyped worlds.

Concretely, typed library code like this:

```
// file: mylib.ncl
{
  numToStr : Num -> Str = fun n => ...;
  makeURL : Str -> Str -> Num -> Str = fun proto host port =>
    "${proto}://${host}:${numToStr port}/";
}
```

can coexist with untyped configuration code like this:

```
// file: server.ncl
let mylib = import "mylib.ncl" in
let host = "myproject.com" in
{
  host = host;
  port = 1;
  urls = [
    mylib.makeURL "myproto" host port,
    {protocol = "proto2"; server = "sndserver.net"; port = 4242}
  ];
}
```

In the first snippet, the body of `numToStr` and `makeURL` are statically
checked: wrongfully calling `numToStr proto` inside `makeURL` would raise an
error even if `makeURL` is never used. On the other hand, the second snippet is
not annotated, and thus not statically checked. In particular, we mix an URL
represented as a string together with one represented as a record in the same
list. The interpreter rather inserts run-time checks, or _contracts_, such
that if `makeURL` is misused then the program fails with an
appropriate error.

Gradual types also lets us keep the type system simple: even in
statically typed code if you want to write a component that the type
checker doesn't know how to verify, you don't have to type-check that
part.

### Contracts

Complementary to the static type system, Nickel offers _contracts_. Contracts
offer precise and accurate dynamic type error reporting, even in the
presence of function types. Contracts are used internally by
Nickel's interpreter to insert guards at the boundary between typed and untyped
chunks. Contracts are available to the programmer as well, to give them the
ability to enforce type assertions at run-time in a simple way.

One pleasant consequence of this design is that the exposure of the user to the
type system can be progressive:

- Users writing configurations can just write Nix-like code while ignoring
  (almost) everything about typing, since you can seamlessly call a typed
  function from untyped code.
- Users writing consumers or verifiers of these configurations would use
  contracts to model data schemas.
- Users writing libraries would instead use the static type
  system.

An example of contract is given in the next section.

## Schemas

While the basic computational blocks are functions, the basic data blocks in
Nickel are records (or objects in JSON). Nickel supports writing self-documenting
record schemas, such as:

```
{
  host | type: Str
       | description: "The host name of the server."
       | default: "fallback.myserver.net"
  ;

  port | type: Num
       | description: "The port of the connection."
       | default: 4242
  ;

  url | type: Url
      | description: "The host name of the server."
  ;
}
```

Each field can contain metadata, such as a description or default
value. These aim at being displayed in documentation, or queried by
tools.

The schema can then be used as a contract. Imagine that a function has
swapped two values in its output and returns:

```
{
  host = "myproject.com",
  port = "myproto://myproject.com:1/",
  url = 1
}
```

Without types, this is hard to catch. Surely, an error will eventually pop up
downstream in the pipeline, but how and when? Using the schema above
will make sure that, whenever the fields are actually evaluated, the
function will be blamed in the type error.

Schemas are actually part of a bigger story involving merging records
together, which, in particular, lets the schema instantiate missing
fields with their default values. It is very much inspired by the
[NixOs module system][module-system] and the [CUE][cue] language, but
it is a story for another time.

## Conclusion

I hope that I gave you a sense of what Nickel is trying to achieve. I
only presented its most salient aspects: its gradual type system with
contracts, and built-in record schemas. But there is more to explore!
The language is not ready to be used in real world applications yet, but a good
share of the design presented here is implemented. If you are curious about it,
[check it out][nickel]!

[nix]: https://nixos.org/
[module-system]: https://nixos.org/manual/nixos/stable/index.html#sec-configuration-syntax
[nickel]: https://www.github.com/tweag/nickel
[starlark]: https://github.com/bazelbuild/starlark
[jsonnet]: https://jsonnet.org/
[dhall]: https://dhall-lang.org/
[cue]: https://cuelang.org/
[toposort]: https://github.com/NixOS/nixpkgs/pull/11484
[typescript]: https://www.typescriptlang.org/
[terraform]: https://www.terraform.io/
[kubernetes]: https://kubernetes.io/
[nix-ops]: https://github.com/NixOS/nixops
[gradual-typing]: https://en.wikipedia.org/wiki/Gradual_typing
[nickel-contracts]: https://www.tweag.io/blog/2021-01-22-nickel-contracts/
[nickel-gradual-typing]: https://www.tweag.io/blog/2021-03-18-nickel-gradual-typing/
