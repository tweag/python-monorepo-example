---
title: "Haskell dark arts, part I: importing hidden values"
shortTitle: "Haskell dark arts, part I: importing hidden values"
author: Cheng Shao, Richard Eisenberg
tags: [haskell]
description: "How to break encapsulation and import a hidden value in Haskell."
---

You are a Haskeller debugging a large codebase. After hours of hopping around
the source code of different modules, you notice some dirty and interesting code
in one of your dependency's `Util` or `Internal` module. You want to try calling
a function there in your code, but hold on -- the module (or the function) is
hidden! Now you need to make your own fork, change the project build plan and do
a lot of rebuilding. Some extra coffee break time is not bad, but what if we
tell you this encapsulation can be broken, and you can import hidden functions
with ease? Of course, this comes with some caveats, but no spoilers -- read the
rest of the post to find out how (and when).

## Importing a hidden value with Template Haskell

Suppose we'd like to use the `func` top-level value defined in the `Hidden`
module of the `pkg` package. We can't simply `import Hidden` and use it if
`func` is not exported or `Hidden` is not exposed. But don't worry, with a
single line of code in our own codebase, we can jailbreak the encapsulation:

```haskell
myFunc = $(importHidden "pkg" "Hidden" "func")
```

`myFunc` can now be used just like the original `func` value. It doesn't need to
be defined as a top-level value; one can drop an `importHidden` splice anywhere.
We only need to ensure the `pkg` package is a transitive dependency of the
current package, enable the `TemplateHaskell` extension and import the module
which implements `importHidden`.

The curious reader may check the Template Haskell API documentation and try to
come up with their own `importHidden` implementation. It is well known that with
Template Haskell, one can reify the information of datatypes and summon its
hidden constructors, but summoning arbitrary hidden values is not directly
supported. The next section reveals the secret.

## Implementing the importHidden splice

### Finding a package's unit id

Let's forget about `importHidden` for a minute and consider how to handwrite
Haskell code to bring a hidden value into scope. Since we already know the
package/module/value name, we can construct a Template Haskell `Name` that
refers to the value, then use it to create the `Exp` that brings the value back.
Time to give it a try in ghci:

```
Prelude> :set -XTemplateHaskell
Prelude> import Language.Haskell.TH.Syntax
Prelude Language.Haskell.TH.Syntax> myFunc = $(pure $ VarE $ Name (OccName "func") (NameG VarName (PkgName "pkg") (ModName "Hidden")))

<interactive>:3:12: error:
    • Failed to load interface for ‘Hidden’
      no unit id matching ‘pkg’ was found
    • In the expression: (pkg:Hidden.func)
      In an equation for ‘myFunc’: myFunc = (pkg:Hidden.func)
```

Oops, GHC complains that the `pkg` package can't be found. The `PkgName` type in
Template Haskell is a bit misleading here; GHC expects it to be the full unit ID
of a package instead of the package name. What do unit IDs look like?

For packages shipped with GHC, they're either the package name (e.g. `base`), or
the package name followed by the version number (e.g. `Cabal-3.0.1.0`). However,
unit IDs of third-party packages have a unique ABI hash suffix (e.g.
`aeson-1.4.7.1-BBxO5joHKZ5L11K8E1qG5k`), and the hash suffix differs if a
package is built with different build plans. Thanks to this mechanism, most
packages can be rebuilt multiple times and coexist in the same package database,
a `cabal build` run will never fail due to version conflict with existing
packages, and the so-called "cabal hell" becomes an ancient memory.

For `importHidden` to be useful, it needs to support third-party packages,
therefore we need to find a way to query the exact unit ID given a package name
via Template Haskell. Among the existing Template Haskell APIs, the closest
thing to achieve this goal is `reifyModule`, which given a module name, returns
its import list. So if `Hidden` appears in the current module's import list, we
can use `reifyModule` to get `Hidden` metadata which includes `pkg`'s unit ID.
However, this approach has a significant restriction: it doesn't work for hidden
modules.

### Abusing GHC API in Template Haskell

Recall that Template Haskell is usually run by a GHC process, so it's possible
to jailbreak the usual Template Haskell API and access the full GHC state when
running a Template Haskell splice. The `Q` monad is defined as:

```haskell
newtype Q a = Q { unQ :: forall m. Quasi m => m a }
```

This encodes a program that uses the `Quasi` class as its "instruction set". In
GHC, the typechecker monad `TcM` implements its `Quasi` instance which drives
the actual Template Haskell logic. When running a splice, the type variable `m`
is instantiated to `TcM`. If we can disguise a `TcM a` value as a `Q a` value,
then we can access the full GHC session state inside `TcM`, which grants us
access to the complete GHC API:

```haskell
import DynFlags
import FastString
import Language.Haskell.TH.Syntax
import Module
import Packages
import TcRnMonad
import Unsafe.Coerce

unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = unsafeCoerce (\_ -> m)
```

The implementation of `unsafeRunTcM` requires a bit of understanding about the
dictionary-passing mechanism of type classes in GHC. The definition of `Q` can
be interpreted as:

```haskell
data QuasiDict m = QuasiDict {
  qNewName :: String -> m Name,
  ..
}

newtype Q a = Q { unQ :: forall m . QuasiDict m -> m a }
```

A `QuasiDict m` value is a dictionary which carries the implementation of
`Quasi` methods in the `m` monad. A `Q a` value is a function which takes a
`QuasiDict m` dictionary and calls the methods in it to construct a computation
of type `m a`. When we instantiate `m` to a specific type constructor like
`TcM`, GHC picks the corresponding dictionary and passes it to the function.

In our case, we know in advance that the `Q a` type is just a `newtype` of the
`Quasi m => m a` computation which will be coerced to run in the `TcM` monad,
therefore we can wrap a `TcM a` value in a lambda which discards its argument
(which will be the `Quasi` instance dictionary for `TcM`) and coerce it to `Q a`. Another way to implement the coercion is:

```haskell
unsafeRunTcM :: TcM a -> Q a
unsafeRunTcM m = Q (unsafeCoerce m)
```

The `unsafeCoerce` application must return a polymorphic value with the `Quasi`
class constraint, and if we simply do `unsafeRunTcM = unsafeCoerce`, the
resulting `Q a` value has the wrong function arity which leads to a segmentation
fault at runtime.

Now that we can hook into GHC internal workings by running `TcM a` computations,
it's trivial to query the package state and find a package's unit ID given its
name. The rest of `importHidden` implementation follows:

```haskell
qGetDynFlags :: Q DynFlags
qGetDynFlags = unsafeRunTcM getDynFlags

qLookupUnitId :: String -> Q UnitId
qLookupUnitId pkg_name = do
  dflags <- qGetDynFlags
  comp_id <- case lookupPackageName dflags $ PackageName $ fsLit pkg_name of
    Just comp_id -> pure comp_id
    _ -> fail $ "Package not found: " ++ pkg_name
  pure $ DefiniteUnitId $ DefUnitId $ componentIdToInstalledUnitId comp_id

qLookupPkgName :: String -> Q PkgName
qLookupPkgName pkg_name = do
  unit_id <- qLookupUnitId pkg_name
  pure $ PkgName $ unitIdString unit_id

importHidden :: String -> String -> String -> Q Exp
importHidden pkg_name mod_name val_name = do
  pkg_name' <- qLookupPkgName pkg_name
  pure $
    VarE $
      Name
        (OccName val_name)
        (NameG VarName pkg_name' (ModName mod_name))
```

Summarizing, our summoning ritual consists of:

- Use `unsafeCoerce` to enable running a typechecker action in the Template
  Haskell `Q` monad.
- Obtain the `DynFlags` of the current GHC session and query the package state
  to find a package's full unit ID.
- Construct a `Name` that refers to the hidden value and create the
  corresponding `Exp`.

With these hacks combined, now you can transcend the barriers of modules and
packages!

## Conclusion

Through a bit of knowledge about GHC internal workings, we practiced some
Haskell dark arts and were able to summon hidden values. Before plugging this
hack into a real-world codebase, let's discuss the drawbacks of this approach.

If a top-level value isn't exported, then the GHC inliner may choose to inline
it at its call sites, therefore the interface file won't contain its entry, and
the summoning will fail at compile-time.

Given that we expect the splices to be run in the GHC process, it surely won't work
with an [external interpreter or cross GHCs][th-cross-post].
On the other hand, for the particular use case of `importHidden`, we just need
to query a package's unit ID, so it should be fairly easy to patch GHC to
support it when cross compiling: just add a method in the `Quasi` class, and
support one more message variant in the external interpreter.

Running `TcM` actions in the `Q` monad is an interesting hack that doesn't seem
to have been used in the wild, and Richard Eisenberg has a
nice [video][rae-video] that introduces it. However, there's a more principled
way: GHC plugins, since they have full access to the GHC session state and can
call arbitrary GHC API anyway.

Should you use `importHidden`? Most likely not, since patching the desired
dependencies is always simpler and more robust. Nevertheless, it's a fun
exercise, and we hope this post serves as a peek into how GHC works under the
hood :)

[rae-video]: https://www.youtube.com/watch?v=Z6z3Bnnh_iY
[th-cross-post]: https://www.tweag.io/blog/2020-11-25-asterius-th
