---
title: "Battle-tested monadic OCaml: Tezos"
shortTitle: "Monadic OCaml: Tezos"
author: Clément Hurlin
tags: [ocaml]
description: "Understanding monads in OCaml: mixing promises and errors"
---

Monads allow to chain effectful operations in a correct and practical way. In
some languages, such as Haskell, they are commonplace, while in OCaml they are
less common. However, they are also a useful and practical tool in OCaml too!

In this post, I will describe what monads are good for, and what they look like
in OCaml, taking as example the promises-and-failures monad used in the
[codebase][tezos-codebase] of [Tezos][tezos]. Hopefully, this will convince you
that they are not only an elegant framework, but also a solution that can be
useful for you right now.

[tezos]: https://tezos.com/
[tezos-codebase]: https://gitlab.com/tezos/tezos

## What are monads good for?

Monads are often described as a method for chaining effectful operations. Two
examples of effectful operations are:

- Operations that can fail. You may either want execution to stop at the first
  failure (critical errors); or you may want to continue through failures, and
  gather as many of them as possible (when writing a parser or when validating
  form entries for example).

- Operations that can be executed asynchronously. It is useful to be able to
  write code in a way where there is no distinction between whether the
  function readily provides a value or returns a promise of a value.

Both of these operations can be difficult to manage correctly because they
affect the entire codebase: errors can happen anywhere and asynchronicity
spreads from the functions producing promises to any functions consuming them.

In this post, we will build an OCaml monad that combines the two use cases
above. That is, we will define a monad that makes easy to:

- declare and handle errors
- write asynchronous code without even knowing it

This monad will allow to manipulate these operations in a way that is both
safe, because it is enforced by static typing, and easy to use, because the
number of operations to understand is small. In order to define it, we will be
using various features of OCaml, most notably [extensible variant][extensible-variant]
types and polymorphic operators.

A last comment is that we will not use the new monadic [syntax][monad-syntax]
that was added to OCaml recently. The main reason for this is to show that
monads are worth using, even without dedicated syntax! The Tezos codebase
exemplifies this, as it uses the same syntax as this post. The absence of
syntax sugar avoids having to introduce and understand an additional layer of
complexity.

[monad-syntax]: https://jobjo.github.io/2019/04/24/ocaml-has-some-new-shiny-syntax.html
[extensible-variant]: https://caml.inria.fr/pub/docs/manual-ocaml/extensiblevariants.html

## The error monad

Let's start by thinking about errors, without the promises framework. Errors
are structured values belonging to the type `error`, which is an [extensible
variant][extensible-variant] type:

<!-- Load required libraries, for executing the OCaml snippets with
     https://github.com/smelc/exdown upon saving
```ocaml
#require "lwt"
#require "lwt.unix"
```
-->

```ocaml
(* Declare an empty (for now) type *)
type error = ..

(* The monad's error type *)
type 'a mresult = ('a, error list) result

(* Add an inhabitant of the error type *)
type error += Div_by_zero of int
```

Using an extensible variant allows errors to be declared locally to the code
that raises them. The main drawback is that you lose the check for
pattern-matching exhaustiveness that is usually associated with algebraic
variants. This is, however, usually harmless for error handling, because a
specific code location is concerned with a specific list of errors and it is
fine treating all unknown errors with a catch-all statement.

Here's how ground monadic values (i.e. values of type `'a mresult`) are built:

```ocaml
(* Wrap the return value of a successful execution *)
let return (x : 'a)  : 'a mresult = Ok x
(* Wrap a single error *)
let fail (e : error) : 'a mresult = Error [ e ]
```

Because the type `mresult` contains a list of `error`, it can accumulate
errors, which is useful, for example, when implementing parsers to report all
errors and not only the first one.

We now need to sequence operations in the error monad. This is done
with the infix operator `>>?`:

```ocaml
let (>>?) v f =
  match v with
    (* Success produced x, continue execution by applying f *)
    | Ok x -> f x
    (* Error, stop execution: do not execute continuation *)
    | Error trace -> Error trace
```

The `>>?` operator has type `'a mresult -> ('a -> 'b mresult) -> 'b mresult`.
The second parameter is the function to be executed when the first parameter is
successfully evaluated.

Here is an example of code in the error monad:

```ocaml
let safe_div x y =
  if y = 0 then return y else fail (Div_by_zero x)
  >>? fun y_neq_0 ->
  return (x / y_neq_0)
```

Using the error monad lets you treat successful cases and error cases in an
uniform manner, because they both belong to the same type: `mresult`. This type
goes hand in hand with the `>>?` operator that reraises errors on its
left and go on with successful cases on its right.

## What is a monad?

Now that we have seen a practical example of a monad, let's give an intuition
of what a monad is, more generally. It consists of:

- A datatype capturing the result of computations. In the error monad of the
  previous section, it is the type `'a mresult`.

- Functions to create ground values of this datatype. In the error monad, the
  functions are `return` and `fail`.

- Functions to sequence computations that return values of the datatype
  described in the first item. These functions are usually infix operators and
  the most important one is usually called `bind`. In the case of the error
  monad, there is a single sequencing operator: `>>?`.

That's it! Therefore, in order to define another monad, we just have to follow
the steps described above. Now we are ready for a more complicated problem, and
tackle asynchronous operations.

## The promises monad: Lwt

[Lwt](https://ocsigen.org/lwt/latest/manual/manual) is a library for dealing with
asynchronous operations. In a nutshell, Lwt ensures that IO operations do not
block an entire program by using cooperative promises. A promise is a value
that will eventually be available or whose computation will fail. A promise for
a value of type `'a` has type `'a Lwt.t`.

A ground value in the `Lwt` monad can be obtained as follows:

- Using `Lwt.return` whose type is `'a -> 'a Lwt.t`. This lifts a readily
  available value into the `Lwt` monad.

- Using one of the functions provided by `Lwt` to perform IO, available in
  [`Lwt_io`](https://ocsigen.org/lwt/5.2.0/api/Lwt_io).

Here's an example of code in the `Lwt` monad:

```ocaml
let get_password (read_before : string option) : string Lwt.t =
  match read_before with
    (* Password obtained before: reuse it *)
    | Some x ->
        Lwt.return x
    (* Ask password *)
    | None ->
        (* Lwt_io.read_line has type input_channel -> string Lwt.t *)
        Lwt_io.(read_line stdin)
```

Sequencing operations in the `Lwt` monad is done using the `>>=` operator,
which is called [bind](http://ocsigen.org/lwt/5.3.0/api/Lwt#VALbind). It has
type `'a Lwt.t -> ('a -> 'b Lwt.t) -> 'b Lwt.t`. Like the `>>?` operator
above, the second parameter is the function that will be executed in the case
where first parameter is a promise that executes successfully.

Here's an example of code in the `Lwt` monad that uses `>>=`:

<!--
```ocaml
open Lwt.Infix

let hash = function s -> Lwt.return s
```
-->

```ocaml
let get_password_double_hash (read_before : string option) : string Lwt.t =
  get_password read_before
  >>= fun (password : string) ->
  hash password (* hash function has type string -> string Lwt.t *)
  >>= fun (h : string) ->
  hash h
```

Using the Lwt monad allows you to abstract over promises easily: call functions
returning values in the monad, i.e. whose type is `'a Lwt.t`. Then, sequence
operations with the `>>=` operator, that takes care of unwrapping these values
by removing the `Lwt.t` head symbol in continuations.

## The complete monad: mixing errors and promises

The main monad used in the Tezos codebase combines the two monads described
above. Its type is `'a mresult Lwt.t`. A value of this type can be understood
as _a promise of a value of type 'a, which can fail_.

A value in this monad can be obtained as follows:

- Using `return`, whose type is `'a -> 'a mresult Lwt.t`. This builds
  a successful promise and is defined as follows:

  ```ocaml
  let return x = Lwt.return (Ok x)
  ```

- Using `fail`, whose type is `error -> 'a mresult Lwt.t`. This builds
  a failed promise and is defined as follows:

  ```ocaml
  let fail err = Lwt.return (Error [ err ])
  ```

While `Lwt` already has a primitive for failure (`Lwt.fail`), the monad does
not use it. `Lwt.fail` is akin to exceptions, whereas the Tezos monad uses
regular values (list of errors) which are handier to manipulate.

The operator `>>=?` sequences operations in this monad. It has type
`'a mresult Lwt.t -> ('a -> 'b mresult Lwt.t) -> 'b mresult Lwt.t`.
Like the sequencing operators I have shown so far, it takes a value in the
monad, applies a function to it; and returns the function's application in the
monad:

```ocaml
let (>>=?) v f =
  v >>= function
    (* Success produced x, continue execution by applying f *)
    | Ok x -> f x
    (* Error, stop execution: do not execute continuation *)
    | Error errs -> Lwt.return (Error errs)
```

Continuing our `get_password` example:

```ocaml
type error += Weak_password of string

let get_strong_password : string mresult Lwt.t =
  get_password None
  >>= fun (s : string) ->
  match s with
    | _ when String.length s <= 8 -> fail (Weak_password s)
    | _                           -> return s
  >>=? fun (s' : string) ->
  print_endline "Password accepted";
  return s'
```

In this code, `>>=` eliminates the `Lwt` monad by making `get_password`'s
return value available as a `string`. The same goes for `>>=?` and `s'`,
because the statement producing `s'` has type `string mresult Lwt.t`.

Variants of usual functions are defined to work conveniently in the monad.
As an example, consider `map_s`, which applies a monadic function to
each element of a list and returns a list in the monad:

```ocaml
let rec map_s (f : 'a -> 'b mresult Lwt.t) (l : 'a list) : 'b list mresult Lwt.t =
  match l with
  | [] ->
      return []
  | h :: t ->
      f h
      >>=? fun rh ->
      map_s f t
      >>=? fun rt ->
      return (rh :: rt)
```

Another example is `filter_s`, which applies a monadic filter to elements of
a list and returns a list in the monad:

```ocaml
let rec filter_s (f : 'a -> bool mresult Lwt.t) (l : 'a list) : ('a list mresult Lwt.t) =
  match l with
  | [] ->
      return []
  | h :: t -> (
      f h
      >>=? function
      | false ->
          filter_s f t
      | true ->
          filter_s f t
          >>=? fun t ->
          return (h :: t) )
```

# Conclusion

I've shown you some examples of monads in OCaml, concluding with a monad that
integrates APIs for errors and asynchronous operations. It provides a very good
reference for many typical tasks in programming, where both of these
characteristics are necessary. It is agnostic about the underlying `error`
type, but specific to the `Lwt` promises library, making it flexible and yet
based on a solid solution. The whole [Tezos codebase](https://gitlab.com/tezos/tezos)
showcases on a large scale that this monad is useful and convenient.

Newcomers to monads are usually initially puzzled by the code they read. But
this initial reaction doesn't last long, because a few operators suffice to
write code in the monad we've presented: `Lwt.return`, `>>=`, and `>>=?`. I
hope this text convinced you to try monads in your next project, and make sure
to check out the [syntax support][monad-syntax] that the language offers for them!
