---
title: Announcing Nickel 0.3.1
author: The Nickel Team
tags: [nickel]
description: "What's new in Nickel 0.3.1"
---

The Nickel team is delighted to announce a new version of Nickel, 0.3.1.
Nickel is a programming language that helps people write configurations
that are modular, correct and boilerplate-free.

If you haven't tried Nickel for yourself, you can learn how to get started on
the [website][getting_started].

## What's in Nickel 0.3

### LSP record & standard library completion

The Nickel language server (nls) now supports autocompletion for the Nickel
standard library, as well as for the fields of user-defined records.

This significantly improves the experience of writing complex configurations
in any editor with LSP support. It also increases discoverability of existing
standard library contracts and functions, as simply typing `record.` or
`array.` will now show a list of all exported fields.

### Priority metadata annotations for record merging

You can now express the priority of record fields precisely in metadata to
get more fine-grained control over the results of record merging.

```nickel
let sleep_config = {
  sleep_timer | default = 5000,
  requires_password_on_sleep = false,
} in
let my_config = {
  sleep_timer = 6000,
  requires_password_on_sleep | force = true,
} in

my_config & sleep_config
# evaluates to: { sleep_timer = 6000, requires_password_on_sleep = true }
```

In addition to `default` (the lowest priority) and `force` (the highest), there
is also a `priority` keyword, which must be followed by a number.

```nickel
{ x | priority 10 = "a" } & { x | priority 20 = "b" }
# evaluates to: { x = "b" }
```

Finally, it is also possible to recursively "push" priorities down into a
record. For example:

```nickel
let default_config = {
  sleep | rec default = {
    timer = 5000,
    requires_password = false
  }
} in

let my_config = { sleep.timer = 6000 } in

default_config & my_config
# evaluates to: { sleep = { timer = 6000, requires_password = false } }
```

You can read more about priority metadata annotations in [the relevant
documentation][merging_documentation]. Note that the exact behaviour of this
functionality is not yet stable, so be aware that the above examples may change
in future versions of Nickel.

### `optional` metadata

A new `optional` metadata keyword has been added, which makes it possible to
express, for example, that certain fields may not be present in a record
contract.

```nickel
let ConfigLang = { name | Str, evaluation | optional | [| `Lazy, `Strict |] } in

let langs | Array ConfigLang = [
  { name = "Nickel", evaluation = `Lazy },
  { name = "Starlark", evaluation = `Strict },
  { name = "YAML" },
] in

langs
```

### `switch` is now `match`

Nickel's `switch` statement has been renamed to `match`.

This has also come with a subtle change in behaviour: a `match` is now a
function with an anonymous argument. This is perhaps best explained with an
example:

```nickel
let state_to_str : [| `Open, `Closed |] -> Str =
  match {
    `Open => "open",
    `Closed => "closed",
  }
in

state_to_str `Open
# evaluates to: "open"
```

Combined with the `|>` operator for reversing the order of function application,
this change also makes it possible to move the value that's being matched on
to the front of the expression. This can make it more obvious what's being
matched on:

```nickel
let Door = { house_number | num.Nat, door_state | [| `Open, `Closed |] } in

let is_open | Door -> Bool =
  fun door =>
    door.state |> match {
      `Open => true,
      _ => false,
    }
in

is_open { house_number = 4, state = `Closed }
# evaluates to: false
```

### Other changes

There are other changes in Nickel 0.3.1, including:

- Enum case names can now overlap with language keywords. Previously cases
  like `` `if`` would fail with a parsing error.
- Enum case names can no longer contain whitespace between the backtick and the
  identifier.
- It is no longer possible to use multiline strings in enum identifiers.
- Various bug fixes & performance improvements.

[getting_started]: https://nickel-lang.org/getting-started
[merging_documentation]: https://nickel-lang.org/user-manual/merging/#merge-priorities
