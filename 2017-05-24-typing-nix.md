---
title: Typing nix
author: Théophane Hufschmitt
---

We're currently working on adding a type-system to the nix language.

Thanks to [the financial help](https://www.gofundme.com/typing-nix) of the nix
community, we welcome Théophane Hufschmitt for a six month internship with us.

Théophane is working on providing the nix language with a static type system,
which is a long running issue and a much awaited feature for the language.

# Nix?

## The purely functional package manager

[Nix](https://nixos.org/nix) is a package manager for Linux (and other Unixes
such as macOS).

Conventional package manager (apt, rpm, pacman, ...) treat the file system as
one giant shared mutable data-structure. Every programmer knows how having a
giant shared mutable data-structure is an awful idea^[There probably are
exceptions, of course, but in the general case, it is]. And in this regard,
sysadmin is exactly like programming. This approach, while simple is the source
of an incredible lot of problems in package management (like the lack of
reproducibility, the difficulty of rolling-back, tho risk of putting the system
in an inconsistent state because of a power failure in the middle of an
upgrade...).

Various "configuration management" tools try to abstract over this structure to
give administrators a higher level view of the system. Yet, they operate on top
of classical systems, and do only partially solve those problems. For example,
applying the same configuration with [salt](https://saltstack.com/) on two
different machines do not offer any guaranty that the result will be the same
on both − which also means that rolling the configuration back after an
unfortunate change won't always restore the system to its exact original state.

That's the kind of problems that nix aims to solve, by applying tools from the
programming language world (in particular functional programming) to the system
administration world.

## And what does it look like for real?

The result has been more than convincing: nix has an ever growing community
(the nix package repository recently popped up as one of most active github
repos^[classed sixth by number of reviewers on
[octoverse](https://octoverse.github.com)]) and is used by a number of
companies, including Mozilla, Lookout, Teamviewer
and many others (and [tweag.io](http://tweag.io) of course).

Nix provides safe package management (in the sense that your system is never in
an inconsistent state like it happens during the operations of other package
managers), free rollbacks, transparent support for multiple versions of the
same package (bye bye dependency hell) and by far more reproducibility
guaranties than conventional package managers^[In a certain sense, it is rather
similar to snaps, but with a totally different background that makes it really
more flexible − and allows for much more sharing, in exchange of a possible
higher entry barrier to understand the way it works].

A Linux distribution ([nixOS](https://nixos.org)) has been realized on top of
it, raising at the whole system level the benefits that nix gave locally, and
an orchestration tool ([nixops](https://nixos.org/nixops), formerly charon) to
raise this at the network level.

# Why would we type this?

## What can we type?

Nix is a package manager, but it is also a programming language that is used to
describe the packages and their interactions (how to build them, their
dependencies, etc..).
Using a full featured programming language to do this (instead of simpler
configuration files like for example the PKGBUILD files used by pacman) allows
to abstract in a really nice way over build systems. Want to package a bunch of
programs, all following the exact same building convention? Just add a function
to describe their build process, and apply it to each one of the sources.

## And why would we type this?

This language is untyped. And given the size of the code base − nixpkgs, the nix
package repository is almost 1 million lines of code big − things sometimes
begin to be quite complicated as soon as you need to do some refactoring.

A recent example was the addition of multiple-outputs derivation into nixpkgs.
Without diving into the details of the change (you may have a look at
[this](http://lipa.ms.mff.cuni.cz/%7Ecunav5am/nix/closure-size-notes.pdf) if
you want some details), this was a modification which needed **a lot** of
refactoring in nixpkgs.

Thanks to a rather intensive testing, most of the problems were fixed before
the change reach the mainstream branch, but still, this required a lot of
efforts, and some bugs still slipped through the tests, which could have been
caught by a type system.

# And in practice?

## Interaction with standard nix

A first and really important remark is that we don't want the typed nix to be
incompatible with the legacy one. Our goal is not to invent a new language that
could one day replace nix. Given the size of nixpkgs and the efforts invested
in it, backward compatibility is primordial.

The problem of course, is that the current code has not been designed with
typing in mind^[Just take a look at the internal of the nixos module system to
understand how much it __really__, __really__ doesn't care about types], and
thus could probably never type check in any reasonable static type system.
But thanks to the wonders of gradual typing^[If you don't know what this is,
[this](http://homes.soic.indiana.edu/jsiek/what-is-gradual-typing/) is a rather
nice introduction, the gist of it being that in addition to static types, you
got a special "gradual" type (often noted "?" or "☆") that means "I don't know
how to type this, let's just assume it is well typed"], this isn't a real
problem: we just have to gradually type the untypeable part. Furthermore, we
have the chance of having Jeremy Siek − the inventor of gradual typing − in
Paris for a month, and he already provided us some help in designing this.

## Some examples

This is really nice, but no really concrete. So here are some examples to show
how this could look like in practice.


### Simple ML-like type inference

Here is a sample nix expression (a simplified version of the
`stdenv.mkDerivation` function which is used in nixpkgs to build packages):

```nix
{ buildInputs, meta ? {}, outputs ? [ "out" ], ... }@attrs:
  derivation (attrs //
    {
      builder = attrs.realBuilder or "/bin/sh";
      args = [ "-e" (atrrs.builder or ./default_builder.sh) ];
    }
  )
```

Let's show how this would be typed^[It is currently unclear what kind of
inference is possible without adding type annotations at function definitions.
For now, let's just assume we don't need any].

- First, this is a function definition, so with a type of the form `τ → σ`.

- By looking at the pattern, we can deduce that `τ` must be a record type,
  which must contain the field `buildInputs`, may contain the fields `meta` and
  `outputs`, and may also contain anything else (because of the "...").

- Furthermore, by looking at the default values for `meta` and `outputs`, we
  can see that the `meta` field can have a record type^[In reality, we can't
  say much about this, because the type system will probably have union types.
  So knowing that `meta` can have a record type doesn't prevent it from having
  any other type such as `int` if the expected type is `{...}|int` (where
  `{...}` is the type of records and `|` denotes the union)] and `outputs` the
  type of a list of strings.

- Knowing that the `derivation` builtins expects its `builder` argument to be a
  string, we can deduce that if the argument of our function contains a field
  `realBuilder`, then it is of type `string`. Likewise, a field `builder`
  should be of type `path`, and the field `buildInputs` of type `[derivation*]`
  (a list of derivations)

- The result of the function is the result of the derivation built-in, which is
  of type derivation^[Which in reality is just a special record type], so the
  return type `σ` will be `derivation`.

All this put together, we got that the expression has type:

```
{ buildInputs = [derivation*];
  meta =? Any;
  outputs =? [string*];
  realBuilder =? string;
  builder =? path; ... }
  → derivation
```

Where `=?` means that the field is optional, and `Any` is the super type of all
types (as we don't know anything about how `meta` is used, we can't say much
about his type.


### Introducing a type error...

Now, assume we got almost exactly the same function, but with another default
value for `outputs`:

```nix
{ buildInputs, meta ? {}, outputs ? "out", ... }@attrs:
  derivation (attrs //
    {
      builder = attrs.realBuilder or "/bin/sh";
      args = [ "-e" (atrrs.builder or ./default_builder.sh) ];
    }
  )
```

Here the type system should make the same deductions, but will notice that
`outputs` can have type `string` while `derivation` expects an `output`
argument of type `[string*]`. So this won't type check.

### ... And fixing it
Now, we may write the (correct) function:

```nix
{ buildInputs, meta ? {}, outputs ? "out", ... }@attrs:
  let realOutputs =
    if isList outputs then
      outputs
    else [ outputs ];
  in
  derivation (attrs //
    {
      builder = attrs.realBuilder or "/bin/sh";
      args = [ "-e" (atrrs.builder or ./default_builder.sh) ];
      outputs = realOutputs;
    }
  )
```

Here, the type system should be able to see that `outputs` can have type
`string` or `[string*]`, and that in both cases, `realOutputs` will have type
`[string*]`, so the call to `derivation` will be well-typed. The type of the
function would then be:

```
{ buildInputs = [derivation*];
  meta =? {...};
  outputs =? [string*]|string;
  realBuilder =? string;
  builder =? path; ... }
  → derivation
```

In reality, this is probably not inferable without annotating the type of the
function − at least not according to the state of the art in type inference.
If we write the function ourselves, no problem, we can just add type
annotations, but if we use a function from nixpkgs that we don't want to mess
with, we can circumvent the problem using gradual types. In this case, the
type-checker would say "OK, I don't understand what's going on here, so I'll
just give up and say that this has the type `? → ?`" − or probably something
slightly more precise because it is should always be possible to deduce for
example that the argument must contain a field `buildInputs` and that the
return type should be a derivation, just not an exact static type.

# Long is the road

Of course, this is still at the very early stage: the theory is still a work in
progress (thanks to the help of [Giuseppe Castagna](https://www.irif.fr/~gc/))
and the implementation has just started.

In the meantime, you can follow the progress on the github pages of the
project, on <https://github.com/regnat/tix-papers> for the theoretical part and
<https://github.com/regnat/tix> for the implementation.
