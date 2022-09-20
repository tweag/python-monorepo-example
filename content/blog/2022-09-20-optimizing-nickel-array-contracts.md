---
title: "Optimizing Nickel's Array Contracts"
author: Mahmoud Mazouz
tags: [nickel, internship]
description: "A short dive into Nickel's reference interpreter"
---

Nickel is a gradually-typed, purely functional configuration language with contracts and lazy evaluation.

My internship at Tweag consisted of (attempts at) performance improvements in the Nickel reference interpreter.
In this article, I will describe what I think is the most interesting problem I had on my hands: array operations
like `head` were _sometimes_ running in linear time instead of constant time.

What do I mean by "sometimes"? Well, to answer that, we need some familiarity with **contract application** and **lazy evaluation**.

# Contract application

Contracts are a key feature of Nickel that empowers you to write _correct_ configurations in a flexible way
by _extending_ the type system with your custom dynamically-checked properties.
Thus you can write types for TCP ports, email addresses, or even the Cargo [Manifest](https://doc.rust-lang.org/cargo/reference/manifest.html) format,
using runtime checks on your data.

Prime numbers are cool. Let's write a contract for them[^1]:

```sml
let Prime = fun label value =>
  if is_prime value then value
  else contract.blame_with label "not a prime number"
```

An expression such as `42 | Prime` will expectedly fail, i.e. the interpreter will run the "function" `Prime` on the _blame label_, a special value containing error-reporting information, and the value `42`.
This is syntactic sugar — before evaluation, this expression will be transformed into `contract.apply Prime label 42`.

For arrays, we can use the constructor `Array` and write `[3, 19] | Array Prime` to enforce the prime number contract on all elements of the array. In this case, the expression will be roughly transformed into:

```sml
array.map (contract.apply Prime label) [3, 19]
```

This is not completely accurate: the interpreter uses unique labels for each element to avoid blaming the entire array for a contract violation, as that would result in poor error messages.

At this point, you can already see where the linear behavior might come from. Since evaluating:

```sml
array.head (array.map (contract.apply Prime label) [3, 19])
```

will involve two contract applications, instead of one.
The argument of `array.head` will first be evaluated to `[3 | Prime, 19 | Prime]`, meaning that `Prime` will be applied on both `3` and `19`. _Only then_ can we retrieve `3 | Prime`.

This effect gets amplified for recursive functions guarded by an array contract, such as:

```sml
let rec product
  | Array Prime -> Num
  = fun ps =>
    if array.length ps == 0 then 1
    else array.head ps * product (array.tail ps)
```

Here, every recursive call will apply the `Array Prime` contract to the tail of the input array.
Consequently, every call to `product` will be done in linear time, making the full operation quadratic.

Unfortunately, this reasoning doesn't completely explain the observed behavior, because Nickel is _lazy_.

# Lazy evaluation

During evaluation, Nickel expressions are put behind **thunks**.
Thunks are mutable memory locations that initially hold unevaluated expressions, and are only updated with evaluation results if some part of the program _forces_ them.

This means that evaluating:

```sml
let ps = [3, 9 + 10] | Array Prime in array.head ps
```

will mean mapping `Prime` over `[3, 9 + 10]` to get something that resembles `[3 | Prime, (9 + 10) | Prime]`. Next, `3 | Prime` would be extracted and evaluated.

Hence the evaluation result will be `3` and `(9 + 10) | Prime` won't be computed.
In fact, one could replace `9 + 10` with a computation which never succeeds, such as `0 / 0`,
and the above snippet will _still_ evaluate to `3`.

Thanks to lazy evaluation, array contracts are mapped in linear time _but_ the resulting contract applications are not evaluated unless they're needed.
If you wish to force the entire array to be _fully_ evaluated, you can use `builtin.deep_seq`.
This special function will make sure that the thunk behind each of the array's elements is updated.

# A solution

Instead of mapping contract application on arrays, the interpreter should make the array hold on to its contract, and only apply it when data leaves the array. This came in the form of a new internal operation which I will refer to as `contract.lazy_apply`. This means that our `[3, 19] | Array Prime` example will be equivalent to:

```sml
contract.lazy_apply Prime label [3, 19]
```

This time, `array.head ([3, 19] | Prime)` will return `contract.apply Prime 3` in constant time, _even if_ the thunk corresponding to the array had never been updated before.

After refactoring the interpreter to use the new machinery, I realized that `builtin.deep_seq` was subtly broken.
Given arbitrary terms `x` and `y`, `builtin.deep_seq x y` recursively traverses all the thunks of `x` so that nested records and arrays will be fully evaluated. Once that's done, `builtin.deep_seq` makes the interpreter resume normal evaluation of `y`.

To preserve the semantics of array contracts, any _pending_ contract should be applied during the `builtin.deep_seq` operation. At first, I made it so that when `builtin.deep_seq` is called on an array, the interpreter will evaluate each element with the array's contract applied to it. This worked in the majority of cases, except for record contracts.

The problem with record contracts was that, under certain conditions[^2], they have to clone their thunks. Which led to cases where calling `builtin.deep_seq` on a record with a contract applied, such as `{ x = y + 1, y = 1 } | { x | Num, y | Num }`, updated the newly cloned thunks and not the original thunks.

Therefore, in some cases `builtin.deep_seq x y` left `x` with unevaluated thunks: `x` doesn't “see” the cloned thunks. This isn't the desired behaviour.
Furthermore, serialization functions to JSON and friends, as well as the pretty-printer in the REPL assume that they are given values without thunks, which I couldn't guarantee anymore.

To address this, I introduced a new internal operator.

# Yet another operator

Because of how record contracts work, applying a contract to an arbitrary term may yield a version of said term where some thunks have been cloned.
And so given a term `t`, when `builtin.deep_seq t t` recursively evaluates arrays _with their contracts mapped_,
some contract application results will not be reflected in `t` as they live in cloned thunks.

This is where a new internal unary operation comes into the picture: `builtin.force`.

How it works, is that `builtin.force t` not only evaluates `t`, but also returns _a new copy of `t`_ where everything is guaranteed to be evaluated. In particular, when `t` is an array or records, `builtin.force t` will return a new array filled with the records' (evaluated) cloned thunks, rather than the original, unevaluated thunks.

# Future opportunities

Because array contracts are now saved in a _pending_ state for later application, it's possible to eliminate duplicates using a limited notion of contract equality. I say _limited_ because the terms inside a contract can be arbitrarily complex. Still, it might be worth the effort to optimize for the common cases such as name aliases, and the Nickel Team recently made some headway towards deciding [contract equality](https://github.com/tweag/nickel/issues/724) in many of those cases.

# Benchmarks

<!-- NOTE: Is there a policy w.r.t linking articles like this? -->

It turns out that this is one of [those](https://nee.lv/2021/02/28/How-I-cut-GTA-Online-loading-times-by-70/) cases where the obtained performance improvement is significant.
Let's write a function for computing array slices by generating a range of indices from a `from` index and `to` index.
For simplicity's sake, an empty array is returned if the provided range is invalid.

```sml
let slice
  | Num -> Num -> Array Num -> Array Num
  = fun from to xs =>
    if to < array.length xs && from >= 0 && from <= to then
      let range = array.generate ((+) from) (to - from)
      in array.map (fun idx => array.elem_at idx xs) range
    else []
```

I ran `slice 200 800` on an array of `1000` elements and obtained the following results, courtesy of the excellent command-line tool [hyperfine](https://github.com/sharkdp/hyperfine):

| Version                    |    Mean [ms] | Min [ms] | Max [ms] |        Relative |
| :------------------------- | -----------: | -------: | -------: | --------------: |
| Without `array.lazy_apply` | 618.9 ± 21.2 |    601.3 |    670.1 | **7.59 ± 0.36** |
| With `array.lazy_apply`    |   81.6 ± 2.6 |     78.8 |     94.5 |            1.00 |

[^1]: A more idiomatic way of writing this contract is: `contract.from_predicate is_prime`. Of course, the two styles are equivalent.
[^2]: This is due to the interaction between Nickel's merging primitive with recursive records. As documented in [this Pull Request](https://github.com/tweag/nickel/pull/472)
