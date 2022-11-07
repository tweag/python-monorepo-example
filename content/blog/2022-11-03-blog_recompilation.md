---
title: Recompilation avoidance in rules_haskell
author: Guillaume Genestier
tags: [haskell, bazel]
description: "Bazel avoids superfluous recompilation for free, but we can make it even better."
---

When one is programming, they may compile the same project dozens of times per day,
often after making only small modifications.
Recompiling all the modules each time would be extremely time-consuming and disrupt the programmer's workflow.
Therefore, build tools are careful to avoid recompiling files if they
know the result would be identical to what is already built. This is
sometimes called _recompilation avoidance_.

[Bazel][bazel] is an open-source tool to build and test projects.
It is particularly well-suited for multilingual monorepos. As such,
Bazel has a strategy for recompilation avoidance.
Bazel tracks the inputs and outputs of build actions precisely in
order to decide what files need to be recompiled after a change.
The exact mechanism is out of scope for this article, but well described in [this blog series][bazel cache blogs].
The simplified version is: Bazel will only rerun an action if it hasn't been run with the same inputs before.

One could think that, since Bazel provides such a mechanism to prevent superfluous recompilation,
then developers of Bazel rules would
not have to worry about recompilation avoidance.

Well, the story in this blog post would be quite uninteresting if things were so simple.

In this blog post I explain how I improved recompilation avoidance in
[`rules_haskell`][rules_haskell], a Bazel rule set to build Haskell code.

When GHC compiles a Haskell module,
it needs the source code of the module and the interface files of all its dependencies[^simple-case].
Hence, those files are given as inputs to the Bazel actions compiling a Haskell module.
Since the interface file of a module contains all its dependencies hashes,
any modification would trigger the recompilation of all the modules transitively depending on it.
That was the case in version 0.15 of `rules_haskell`, before the improvements discussed here.

[^simple-case]:
    If the module uses Template Haskell or a plugin,
    this is not sufficient, as explained in next footnote.

## How GHC deals with recompilation avoidance

When compiling a module (`A.hs`), in addition to the object file (`A.o`),
GHC generates an [interface file][ghc wiki interface] (`A.hi`).
This file is used for sharing inter-module information that would otherwise be difficult to extract from a compiled object file.

These files contain various information, useful in different contexts, including:

- The list of symbols it exports, including the type of all symbols and the hash of their implementation,
- Implementations of inlinable functions,
- The list of modules and external packages it depends on,
- The list of orphan instances.

This file is used to determine if a module which depends on `A` should be recompiled after `A.hs` has been modified.
However, to determine if recompilation is required,
not all the information mentioned is useful.
For instance, the list of packages a module depends on
is not relevant information when determining if its dependency should be recompiled.
Similarly, the precise implementation of functions only matters if the module is compiled with inlining turned on.

The relevant bits for the recompilation avoidance mechanism are summarised in an ABI (Application Binary Interface) hash, as explained in the [GHC Wiki][ghc wiki recompilation]:

> When considering whether or not a module's dependent modules need to be recompiled due to changes in the current module,
> a changed ABI hash is a necessary but not sufficient condition for recompilation[^and-object].

[^and-object]:
    This does not apply if the importing modules use Template Haskell or a plugin,
    since in this case the result of compiling a module can depend on the implementation of imported modules,
    not just their interfaces.
    In both cases, to decide if a recompilation should be performed,
    GHC simply relies on the hash of the generated object file rather than just the interface file.

## Example

To illustrate the mechanism, let us consider 3 simple files:

<style>
/*
Formatting to place Haskell files next to each other on large screens
*/
#file-container {
  display: flex;
  flex-wrap: wrap;
}

#file-container pre {
  height: 90%;
}

#file-container code {
  height: 100%;
}
</style>

<section id="file-container">

```haskell
module A (const, change_T0, T, not_imported) where

data T = T0 | T1

not_imported :: T
not_imported = T1

const :: a -> b -> a
const x y = x

change_T0 :: T -> T
change_T0 T0 = not_imported
change_T0 T1 = T1
```

```haskell
module B where

import A (const, change_T0, T)

aux :: A.T -> A.T
aux x = A.const (A.change_T0 x) x

const3 :: a -> a -> a -> a
const3 x y = A.const (A.const x) (A.const y)
```

```haskell
module C where

import qualified B

data N = Z | S N

const_bis :: a -> a -> a
const_bis x = B.const3 x x
```

</section>

There are many changes one can make to `A.hs` which would change the interface file `B.hi`,
but do not affect the ABI hash of `B`, thus avoiding triggering the recompilation of `C.hs`.

Changing the export list of `A`:

```diff
- module A (const, change_T0, T, not_imported) where
+ module A (const, change_T0, T) where
```

Mark a function as always being inlined:

```diff
+ {-# INLINE const #-}
const :: a -> b -> a
```

These 2 changes affect the exports of module `A`,
hence the ABI hash of `A` is affected.
However, these changes only impact the imports of `B`,
so the section on dependencies of `B.hi` is modified,
but not the ABI hash, which only hashes the exports and declarations of `B`.

Modify the import list of `B`:

```diff
- import A (const, change_T0, T)
+ import A (const, change_T0, T, not_imported)
```

Modifying the import list of `B` simply adds one symbol to the section regarding its imports,
but does not affect the part on exports, hence does not modify the ABI hash.

If `B.hs` is compiled without exposing the unfoldings of the symbols it declares[^no-unfoldings],
then one could make more modifications to the code, without modifying the ABI hash of `B`.

One can not only change the definiton of a function of `A`, but also change its type:

```diff
- const :: a -> b -> a
- const x y = x
+ const :: a -> b -> b
+ const x y = y
```

Modifying the type of `A.const` changes the ABI of `A`, no matter if `A.hs` was compiled with unfoldings exposed or not.
Then the modifications to `B` are limited to its import and the implementation of functions,
but since no types in `B` are affected, if the unfoldings of `B` are not exposed,
then the ABI hash of `B` remains the same.

One can also modify the definition of a symbol in `B` (but not its type):

```diff
- const3 x y = A.const (A.const x) (A.const y)
+ const3 x y = id
```

[^no-unfoldings]:
    Unfoldings of functions defined in a module `X` are exposed to the other modules
    if optimisations are turned on (using `ghc -O`) and the compiler decides that they should be included in the interface file
    (either because the definition is quite short and might be useful to enable further optimisations,
    or because the programmer added a pragma regarding inlining).

All those modifications affect the part of the interface file of `B` regarding imports, hence it changes `B.hi`.
So, with version 0.15 of `rules_haskell`, `C`, which depends on `B`, would have been recompiled.
However, those changes do not impact the ABI stored in `B.hi`,
Hence `C` is not recompiled, when using `ghc --make` or the most recent version of `rules_haskell`,
since it is not impacted by those changes.

## Mimicking this behaviour

Now that we have understood the mechanism used by GHC to decide if recompilation is required,
we want to teach Bazel to use it.

### ABI files

Since the relevant information to know if recompilation is required is the ABI hash nested inside the interface file,
and files are the unit considered by Bazel to detect modifications,
one has to first extract this hash and put it in its own file.

The strategy chosen for this is to first generate the human-readable version of the interface file
(using `ghc --show-iface A.hi`) and then store only the line containing the ABI hash into a file `A.abi`.

### Tweak the caching mechanism with `unused_inputs_list`

This new file `A.abi` is then added to the list of inputs required to compile the modules importing `A`.
However, it cannot completely replace the `A.hi` file,
since whenever the modification of `A.hs` is important enough to affect the ABI hash,
the whole interface file is required by GHC to compile the other modules.

As Bazel's caching mechanism inspects all the inputs to know if the recompilation rule should be executed,
adding a new file to the list of inputs can only cause recompilation to occur more often than in the previous state.

This is exactly the opposite of our goal,
hence we have to somehow teach Bazel to not inspect all the inputs when deciding if a "target" should be regenerated.
Fortunately, there is a mechanism in Bazel which has exactly this effect:
declaring some inputs as "unused".

When an input occurs in the `unused_inputs_list`,
it is not considered in the set of inputs used to decide if regeneration of a target is required.
Hence declaring all the interface files as "unused inputs" allows us to instruct the Bazel caching mechanism not to inspect the interface files,
but only the associated ABI files[^safety], when deciding which targets to regenerate.
Furthermore, since the interface files are still in the input list,
when recompilation is needed Bazel will use them,
despite us tagging them as "unused".

It must be noted that Bazel documentation on `unused_inputs_list` is [pretty light][unused_inputs_list doc],
but mentions that "Any change in those files must not affect in any way the outputs of the action".
Hence, the non-consideration of the inputs listed in this field when computing the hash for caching is quite expected.
However, it is not clear from the documentation that Bazel can use those inputs when recompiling.

[^safety]:
    Even if the target module uses Template Haskell or a plugin,
    it is safe to hide the interface files from Bazel's caching mechanism.
    As mentioned in previous notes, in those cases, it could happen that recompilation is required whereas no ABI hash changed.
    But the object files of all the modules it depends on is given as input to the Bazel rule compiling this kind of module.
    Hence any modification affecting an object file will trigger recompilation,
    no matter its impact on the ABI hash.

## Benchmark

I tested this feature on Symbiont's code base, where I modified a file which was a leaf in the dependency graph,
where I added a dummy field to a record.

This change seemed fitting to me, since modifying a record type very
deep in the dependency tree affected a lot of modules transitively,
but not all actually need recompiling. This makes the benchmark long
enough to observe an improvement.

### Results

- When running `bazel build` with the version 0.15 of `rules_haskell` (using the `haskell_module`, but not the ABI files): \
  153 targets built in 7min 38

- When running `bazel build` with the new version of `rules_haskell`:\
  131 targets built in 6min 41

This looks like a non-negligible enhancement.
However, it is hard to say more without a reference.
Hence I also built the project with `stack`,
a widely used build system in the Haskell ecosystem:

- When running `stack build --no-run-tests`:\
  125 modules compiled in 6min 07

We can see that our enhancement correctly avoided 22 of the 26 avoidable recompilation targets;
an accuracy of 80%.

It is not on par with tools using the native recompilation avoidance mechanism of GHC (like `stack`)
because there are more criteria used by GHC than just a change of the ABI hash, which is only a necessary condition.
More on the precise condition used by GHC can be found in the [GHC Wiki][ghc wiki recompilation] and
an example of unnecessary recompilation despite a change in the ABI hash of a dependency was sent to the [Haskell mailing list][mailing list example]

## Closing Remarks

This project was possible thanks to the generous funding from [Symbiont][symbiont].
Currently, the optimisation presented in this post is only applied to modules built using [`haskell_modules`][haskell_modules].

In this post, I presented a technique to declare some inputs as "irrelevant" when Bazel decides if recompilation is required,
applied to the specific case of the GHC compiler.
Since this problem seems quite common (recompilation avoidance is a problem that every language has),
I expect it to find other applications soon.
Especially, I hope this post to raise awareness in the Bazel community about how useful "irrelevant for caching" inputs are,
and would lead to a clarification of the purpose of `unused_inputs_list`.

<!-- Links -->

[bazel]: https://bazel.build/
[rules_haskell]: https://haskell.build/
[ghc wiki interface]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/iface-files
[ghc wiki recompilation]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/recompilation-avoidance
[symbiont]: https://symbiont.io/
[unused_inputs_list doc]: https://bazel.build/rules/lib/actions#run
[haskell_modules]: https://62f52b001e02e10edef0675d--tweag-www.netlify.app/blog/2022-06-23-haskell-module/
[bazel cache blogs]: https://sluongng.hashnode.dev/series/bazel-caching-explained
[mailing list example]: https://mail.haskell.org/pipermail/ghc-devs/2022-August/020896.html
