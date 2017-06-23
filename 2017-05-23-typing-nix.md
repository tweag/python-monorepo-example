---
title: Typing Nix
author: Théophane Hufschmitt
---

*Théophane is a summer intern here at Tweag I/O. Thanks also in part
to [the financial help][gofundme] from the Nix community, he's
spending six months on devising a type system for the Nix language.*

[gofundme]: https://www.gofundme.com/typing-nix

# Nix, the purely functional package manager

[Nix][nix] is a cross platform tool for managing the *configuration*
(set of packages installed, `/etc/` config files etc) of your system.
It currently supports Linux and macOS and support for more platforms
are in the works.

Conventional package managers
([APT][apt], [Yum][yum], [pacman][pacman] and friends) treat the file
system as one giant shared mutable data structure. The problem with
this approach is that mutating global state is *hard*. Ever ended up
with a hosed system because you lost power in the middle of a system
upgrade? This is because package managers never figured out how to
make mutation of the global state *atomic*. Wished you could easily
rollback your system to a previous state and know for sure that this
state is identical to how your system really was at some point in the
past? Traditional package managers can't cope with this because
tracking state changes precisely and exhaustively is again an unsolved
problem when package update scripts are allowed to make arbitrary
changes to arbitrary parts of such a huge piece of mutable state as
your entire root filesystem.

*Configuration management tools*
([Chef][chef], [Puppet][puppet], [Salt][salt] and friends) were
invented to ensure that the global state of your filesystem converges
to some intended state that you can precisely and declaratively
define. But in practice applying e.g. the same Salt configuration on
two different machines with *a priori* divergent starting states
seldom leaves both machines in exactly the same state. This also means
that rolling back the configuration after an unfortunate change won't
always restore the system to its exact original state.

Nix tries to solve this very problem by drastically reducing the
amount of mutable state it has to manage. Less state and fewer ways to
change that state means atomic updates and more reliable rollbacks.
Drastically reducing mutable state... sound familiar? If you're
a functional programmer, it should! In fact, this is just one page
that Nix took from functional programming books. Good abstraction
facilities for writing configurations is another one. Today we'll talk
about borrowing yet another page: using powerful static type checking
to provide guidance when configurations get refactored.

[nix]: https://nixos.org/nix
[apt]: https://wiki.debian.org/Apt
[yum]: http://yum.baseurl.org/
[pacman]: https://wiki.archlinux.org/index.php/pacman
[chef]: https://www.chef.io/chef/
[puppet]: https://puppet.com/solutions/configuration-management
[salt]: https://saltstack.com/

## Nix today

Users typically specify the configuration of their system by reusing
large amounts of configuration modules already written by the
community. The largest collection of such modules, [Nixpkgs][nixpkgs],
is today an incredibly active and diverse project. In fact that
repository is
now [one of most active repositories on all of Github][octoverse]. It
has also fallen victim of its own success. Clocking in at over
1 million lines of code, it's becoming increasingly difficult to
perform global refactorings of all configuration modules at once.
Indeed any such refactoring might silently break these modules. This
is where type checking can help a great deal: the checker can tell you
half way through a refactoring what other code needs to change and
how.

[octoverse]: https://octoverse.github.com

## Nix in the future, with types

Configurations in Nix are written in a full-fledged programming
language, featuring all manner of primitive datatypes (numbers,
strings, file paths, etc), anonymous records and first-class
functions. This is a very big deal for writing configurations in the
large. Functions allow code reuse and abstraction, two crucial
ingredients without which writing out configurations by hand would
become unwieldy.

This language is untyped. Lack of types have created challenges in the
past. A recent example was the integration of multiple-output
derivation into Nixpkgs. Without diving into the details of the change
(have a look
at
[this](http://lipa.ms.mff.cuni.cz/%7Ecunav5am/nix/closure-size-notes.pdf) if
you want some details), this was a modification which needed **a lot**
of refactoring in Nixpkgs.

Thanks to intensive testing, most of the problems were fixed before
the change reach the mainstream branch. This testing required a lot of
effort, and yet despite that some bugs still slipped through the
tests. Some of these bugs could have been caught by a type system.

## Design principles

A first and really important remark is that we don't want the typed
Nix to be incompatible with the legacy one. Our goal is not to invent
a new language that could one day replace Nix. Given the size of
Nixpkgs and the efforts invested in it, backward compatibility is
primordial.

The problem of course, is that the current code has not been designed
with typing in mind. This code as it stands will probably never type
check in any reasonable static type system. But thanks to the wonders
of [gradual typing][gradual-typing], this isn't a real problem: we
just have to gradually type the untypeable part. Furthermore, we had
the chance of having Jeremy Siek − the inventor of gradual typing − in
Paris for a month, and he provided us some substantial help in designing
this.

The gist of gradual types is the that in addition to static types, you
got a special "gradual" type (often noted "?" or "☆") that means "I
don't know how to type this, let's just assume it is well typed".

[gradual-typing]: http://homes.soic.indiana.edu/jsiek/what-is-gradual-typing

## Some examples

This is really nice, but no really concrete. So here are some examples
to show how this could look like in practice.

### Simple ML-like type inference

Here is a sample Nix expression (a simplified version of the
`stdenv.mkDerivation` function which is used in Nixpkgs to build packages):

```nix
{ buildInputs, meta ? {}, outputs ? [ "out" ], ... }@attrs:
  derivation (attrs //
    {
      builder = attrs.realBuilder or "/bin/sh";
      args = [ "-e" (atrrs.builder or ./default_builder.sh) ];
    }
  )
```

Let's show how this would be typed:

- First, this is a function definition, so with a type of the form `τ → σ`.

- By looking at the pattern, we can deduce that `τ` must be a record type,
  which must contain the field `buildInputs`, may contain the fields `meta` and
  `outputs`, and may also contain anything else (because of the "...").

- Furthermore, by looking at the default values for `meta` and `outputs`, we
  can see that the `meta` field can have a record type and `outputs` the
  type of a list of strings.

- Knowing that the `derivation` builtins expects its `builder` argument to be a
  string, we can deduce that if the argument of our function contains a field
  `realBuilder`, then it is of type `string`. Likewise, a field `builder`
  should be of type `path`, and the field `buildInputs` of type `[derivation*]`
  (a list of derivations)

- The result of the function is the result of the derivation built-in, which is
  of type derivation (which in reality is just a special record type), so the
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
about his type).

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
annotations, but if we use a function from Nixpkgs that we don't want to mess
with, we can circumvent the problem using gradual types. In this case, the
type-checker would say "OK, I don't understand what's going on here, so I'll
just give up and say that this has the type `? → ?`" − or probably something
slightly more precise because it is should always be possible to deduce for
example that the argument must contain a field `buildInputs` and that the
return type should be a derivation, just not an exact static type.

# Long is the road

Of course, this is still at a very early stage: the theory is still a work in
progress (thanks to the help of [Giuseppe Castagna](https://www.irif.fr/~gc/))
and the implementation has just started.

In the meantime, you can follow the progress on the github pages of the
project, on <https://github.com/regnat/tix-papers> for the theoretical part and
<https://github.com/regnat/tix> for the implementation.
