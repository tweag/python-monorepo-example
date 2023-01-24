---
title: "Nix with; with Nickel"
author: François Caddet
tags: [nix, nickel, rust]
description: "The Nix `with` keyword is challenging, but we transpiled it to Nickel anyway."
---

Tweag is a big supporter and user of Nix and NixOS. In our experience,
however, we have seen that it is hard to maintain a Nix codebase as it
grows. Indeed, the only way to know if a Nix expression is correct is to
evaluate it, and when an error occurs it can be hard to locate the root
cause. This is more of a problem with bigger codebases, such as the ones we write.

At Tweag, we are working on [Nickel][nickel], a configuration language
featuring a [gradual type system][blog-nickel-gradual-typing]. Defining
Nix derivations, NixOS modules or flakes using Nickel would catch more
mistakes, catch them earlier and identify their precise location.

However, the Nix language is used to define more than 80,000
packages,[^nixpkgs] to which one could add all [flakes][nix-flakes-blog], NixOS modules and
Home Manager configurations. For all this historical effort to not go to
waste, Nickel code needs to be able to leverage Nixpkgs.

We can either make Nix able to call Nickel code, or the other way
around. We chose the latter: make the Nickel interpreter able to
evaluate Nix code. Indeed, making Nix evaluate Nickel code would negate
its benefits, namely, its error reporting, the integrated documentation
and the nice way records are merged with priorities.

This comes with several technical challenges. One being the handling of
the widely used, yet [controversial][with-antipattern], [`with`
construct][nix-with].

In this blog post, we'll explain the challenges of calling Nix from
Nickel, specifically for expressions which use the `with` construct.

## The challenge of calling Nix from Nickel

Evaluating Nix presents two contradictory challenges. On the one hand,
calling Nix from Nickel should be done without breaking the safety
provided by the Nickel type system. Nix is dynamically typed, but one
purpose of the gradual type system of Nickel is precisely to make
statically typed code and dynamically typed code interact gracefully.
On the other hand, we want to update the core of Nickel only when
strictly necessary, because Nickel is not limited to targeting Nix and
NixOS. Fortunately, Nickel is almost a superset of Nix, so a balance
between these objectives can be found.

To evaluate Nix expressions in Nickel, we can use either an
[FFI][wiki-ffi] or code translation (a.k.a. transpilation). We decided
to go for the latter: translating Nix code into Nickel code, which can
then be evaluated. Indeed, since the Nix and Nickel languages are built
on similar foundations (loosely, a lazy JSON with functions),
translating from Nix's AST to Nickel's is almost seamless. Such
translation also allows us to implement special features specific to
this use case. For instance, Nixpkgs annotates types as comments for
most of its functions, so we could infer types from these. Moreover, we
estimated that implementing an FFI system would be too complex for what
we wanted to achieve here. Thus, we made a Nix to Nickel transpiler.

However, some constructions weren't so straightforward to translate:

- the [`inherit`][nix-inherit] keyword (which will probably be one
  of the Nix constructs added to Nickel's core.[^nickel-inherit]), and

- the `with` keyword.

In addition, none of the standard library built-ins can be evaluated at the moment, as most of them don't have an equivalent in Nickel.

## Nix `with` is useful, but confusing

In Nix, the `with` keyword is used to bring all the fields of a record
in scope.

Let's consider a simple Nix expression.

```nix
{ pkgs, ... }:

{
  packages = [
    pkgs.firefox
    pkgs.thunderbird
    pkgs.libreoffice
  ];
}
```

As you can see, all the elements of the list are accessed via `pkgs`.
This looks quite repetitive, and is arguably harder to read. We can use `with`
to make it clearer.

```nix
{ pkgs, ... }:

{
  packages = with pkgs; [
    firefox
    thunderbird
    libreoffice
  ];
}
```

Such Nix expressions can be found anywhere, from Nixpkgs to your own
NixOS configuration. Thanks to the `with` construct, you don't have to
prefix each item with `pkgs.`. That is quite convenient.

So what could be the issue here?

At first, one might think that the `with` construct is syntactic sugar
that statically prepends record field access (here `pkgs.`) to some
identifiers. Unfortunately, the way it actually works is more complex
than that.

To demonstrate some possible issues, let's look at a more complex Nix
expression.

```nix
let
 env = {
    linux = {name = "linux-env";};
    system = {name = "system-env";};
  };
  lib = {
    linux = {name = "linux-lib";};
    systemd = {name = "systemd-lib";};
  };
  linux = "x86_64_linux_gnu";
in

with env; {
  system = system.name;
  deps = with lib; [
    linux
    system
  ];
}
```

What would this evaluate to? Try to think about it, then check the
answer below.

```nix
let
 env = {
    linux = {name = "linux-env";};
    system = {name = "system-env";};
  };
  lib = {
    linux = {name = "linux-lib";};
    systemd = {name = "systemd-lib";};
  };
  linux = "x86_64_linux_gnu";
in

with env; {
  system = system.name; # "system-env"
  deps = with lib; [
    linux        # We get the one defined in `let`, not lib.linux,
                 # which is "x86_64_linux_gnu"
    system       # It's a typo! We wanted lib.systemd, but we get
                 # env.system instead of an error
  ];
}
```

Did you get it right? Probably not!

When `with` blocks are nested, the behaviour can be
confusing.[^nested-with]

Finally, as you may have noticed, we can't know before evaluation which
field contains the record passed to `with`. This is due to Nix's dynamic
typing. Because of this behaviour, we encounter something that looks
like standard (static) variable access, but in reality, is dynamic record
access. In code where one would expect "unbound variable" errors, Nix
will instead throw runtime errors that are generally tricky to
debug.[^with-debugging] Moreover, these errors cannot be caught by an
LSP, so they cannot be shown in a code editor. For the same reason, we
can't provide auto-completion hints within a `with` block, at least not
without hacks that perform evaluations.

## Transpiling `with` to Nickel

Because of the problems explained above, the Nickel team decided to not
implement any `with`-like operator in the interpreter. At least not with
the exact same behaviour. Regardless, for compatibility with Nix, we should
find a way to evaluate it anyway.

What we propose in this article, which will be implemented in the near
future, is a mix of Nickel code generation and compatibility function
calls. To detail this, step-by-step, let's revisit our simple example from
earlier:

```nix
{ pkgs, ... }:

{
  packages = with pkgs; [
    firefox
    thunderbird
    libreoffice
  ];
}
```

This will be translated to something like:

```nickel
fun { pkgs, .. } =>

{
  packages = [
    compat.with [pkgs] "firefox",
    compat.with [pkgs] "thunderbird",
    compat.with [pkgs] "libreoffice",
  ]
}
```

You will notice two things on the `compat.with` call:

1. The array `[pkgs]` is the first parameter. We will detail why later,
   but it has to do with nested `with`.

2. The field being looked for is passed as a string and not a static
   identifier. That's because, at parse time, we can't know which record
   contains which fields. Indeed, in Nickel, every variable access is
   checked statically; the usage of an identifier instead of a string
   here would throw an "unbound variable" error.

An implementation of the `compat.with` function might look like:

```nickel
with
  : Array {_: Dyn} -> Str -> Dyn
  = fun envs field => (
    array.fold (fun current acc =>
      if !acc.found && record.has_field field current
      then { value = current."%{field}", found = true}
      else acc
  ) {found = false, value = null} envs).value
```

The function folds on the first parameter, `envs`, which is an array of
`with` records. This array has to be ordered from the outermost to the
innermost `with` block; the outermost being the head of the array. This
order is justified because the more internal a `with`, the
higher its priority. Please note that `fold` is a right folding. If
multiple records contain `field`, the rightmost one is returned by the
fold, that is, the innermost one. In the case where none of them have
this field, the initial value of fold (`{found = false}`) is returned.
Finally, we try to access the field `value`. This last access operation will
differ in the final implementation: it will probably use a contract
application to assert on `found`.

This operation will fail only if `{}` is returned. In other words, only
if none of the records in `envs` contain the required field.

That's all for the stuff that happens at runtime.

For what is done at parse time, we have:

- `let`-defined variables,
- functions parameters, and
- fields inside a recursive record.

Let's revisit our more complicated example:

```nix
{ env, lib, ... }:

let
linux = "x86_64_linux_gnu";
in

with env; {
  system = system.name;
  compatible = system == linux;
  deps = with lib; [
    linux
    system
  ];
}
```

It will be translated as follows, during the parse
phase:[^transpile-note]

```nickel
let env = {
  linux = {name = "linux-env"},
  system = {name = "system-env"},
}
in

let lib = {
  linux = {name = "linux-lib"},
  systemd = {name = "systemd-lib"},
}
in

let linux = "x86_64_linux_gnu" in

let _with_ = compat.with [env] in
{
  # NOTE Even though records, in Nickel, are recursive by default,
  # the record in this AST is not.

  # No var named "system" found, so substitute by a call to __with__
  system = (_with_ "system").name,

  deps = let _with_ = compat.with [env, lib] in [
    linux,              # Nickel statically found a defined var named "linux" so didn't substitute
    _with_ "system",  # "system" isn't found in lib so it takes the env field
  ]
}
```

To perform this translation in our Rust implementation, we pass around
two extra things: a list of in-scope variables, and a stack of `with`
records. The first is updated by inserting the identifiers when entering
any of:

- a `let` binding body,
- a function definition, or
- a recursive record.

When leaving these blocks, the list has to be reset to the state it was in before
entering. The `with` stack is updated during a `with` block translation: we
push the identifier at the beginning and pop it at the end.

Finally, for each variable translation, we perform these steps:

1. We check the existence of the variable identifier in the list of
   variables. If we find one, or if the `with` stack is empty, the
   variable is translated to a Nickel variable.

2. If the variable has not been defined before, and if the `with` stack
   is not empty, the variable is translated to a `compat.with` call with
   the `with` stack as its first parameter, and the identifier stringified
   as its second.

By checking the `with` stack as described above, we keep the undefined
variable errors at type checking time when we are not in a `with` block.
This is more or less how Nix evaluates `with` inside its interpreter. We
only rewrite the static part into Rust and the runtime one with Nickel.

## The future of `with` in Nickel

As we discussed, Nickel cannot avoid the use of dynamic fields access
to emulate Nix `with`. So then, what could a Nickel-friendly `with` look
like? Moreover, given Nickel's type system and the fact that it already has a way
to [destructure records][nickle-issue81], do we really want a Nickelized
`with`?

Remember the two main issues regarding Nix `with`:

- shadowing or, more accurately, not shadowing of already-defined variables; and
- the fact that the variables made available by `with` are unknown statically.

To handle the first issue we could make `with` overload already-defined variables.
Alternatively, we could throw an error if we call `with` on a record
containing a field that shares a name with a variable in scope. Without knowing the
fields of the record, the first option remains confusing, while the
second is not possible.

In Nickel, the only way to know for sure that a record contains a field
is to rely on the type checker. So a correct `with` can exist in Nickel
only if it requires a statically typed record, where the fields we want
to use must also be typed (e.g.: `myrec: {field: Dyn, otherfield: Num}`).
Obviously, a type like `{_: SomeType}` does not respect this rule
because fields are not statically defined.

That said, the use of a closed record contract is possible. In this
case, we can see the contract as a dynamic cast. The power of this
approach is to permit static typing and auto-completion in the `with`
block. This goes a long way towards fulfilling the necessity of statically
knowing every record's fields, and focuses the possibility of evaluation
errors to the position of the cast. These errors are clear enough (e.g.
missing or extra fields). In this manner, a Nickel `with` would behave like
an `import` or an `open` in languages with statically typed modules
(Rust, OCaml, Haskell, etc.).

## Conclusion

This is only the beginning of the Nix/Nickel story. For now, Nickel is
only able to evaluate Nix expressions which do not use any standard
built-ins or `inherit`, and the error reporting is not always as
detailed as Nix's.

The question of Nickel `with` is far from settled. It is clearly useful,
when we need to bring a lot of fields from a record into scope, but the
requirement for types may only end up moving the extra code to the type
annotation, from the destructuring pattern. Moreover, at least two
proposals for Nix ([RFC 110][nix-rfc110] and [RFC 120][nix-rfc120]) open
the possibility of a future deprecation of `with`. Their proposed syntax
is a better way to write the most common use case of `with`, so that may
also be a direction that we consider for Nickel.

In summary, we are not totally rejecting the idea of a Nickel `with`,
but ultimately, it will be really different from its Nix cousin.

<!-- Footnotes -->

[^nixpkgs]: As of writing (2022-09-02). See https://search.nixos.org/packages
[^nickel-inherit]: See Nickel's [field punning][nickel-issue747] proposal.
[^nested-with]: See Nix [issue #490][nix-issue490] and [#1361][nix-issue1361].
[^with-debugging]:
    For example, see Nixpkgs [PR #101139][nixpkgs-pr101139] and, again,
    the [`with` antipattern][with-antipattern].

[^transpile-note]: Heads up: This is a print-out of the AST, which may differ from the result of parsing.

<!-- Links -->

[blog-nickel-gradual-typing]: https://www.tweag.io/blog/2021-03-18-nickel-gradual-typing/
[nickel]: https://github.com/tweag/nickel
[nix-flakes-blog]: https://www.tweag.io/blog/2020-05-25-flakes/
[nickle-issue81]: https://github.com/tweag/nickel/issues/81
[nickel-issue747]: https://github.com/tweag/nickel/issues/747
[nix-with]: https://nix.dev/tutorials/nix-language#with
[nix-inherit]: https://nix.dev/tutorials/nix-language#inherit
[nix-issue490]: https://github.com/NixOS/nix/issues/490
[nix-issue1361]: https://github.com/NixOS/nix/issues/1361
[nix-rfc110]: https://github.com/r-burns/nixos-rfcs/blob/rfc-inherit-as-list/rfcs/0110-inherit-as-list.md
[nix-rfc120]: https://github.com/zseri/rfcs/blob/no-nested-with/rfcs/0120-no-nested-with.md
[nixpkgs-pr101139]: https://github.com/NixOS/nixpkgs/pull/101139
[wiki-ffi]: https://en.wikipedia.org/wiki/Foreign_function_interface
[with-antipattern]: https://nix.dev/anti-patterns/language#with-attrset-expression
