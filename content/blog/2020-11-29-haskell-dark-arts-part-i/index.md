---
title: "Haskell dark arts, part I: importing hidden values"
shortTitle: "Haskell dark arts, part I: importing hidden values"
author: Cheng Shao
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
with ease? Of course, this comes with lots of caveats, but no spoilers -- read
the rest of the post to find out how (and when).

## Importing a hidden value with Template Haskell

Suppose we'd like to use the `func` top-level value defined in the `Foo` module
of the `foo` package. We can't simply `import Foo` and use it if `foo` is not
exported or `Foo` is not exposed. But don't worry, with a single line of code in
our own codebase, we can jailbreak the encapsulation:

```haskell
myFunc :: Int -> Int -- must exactly match the type of `func`
myFunc = $(importHidden "foo" "Foo" "func")
```

`myFunc` can now be used just like the original `func` value. And `myFunc`
itself doesn't need to be defined as a top-level value; one can drop an
`importHidden` splice anywhere, provided the value's type is explicitly
annotated and matches the original type. We only need to ensure the `foo`
package is a transitive dependency of the current package, enable the
`TemplateHaskell` extension and import the module which implements
`importHidden`.

The curious reader may check the Template Haskell API documentation and try to
come up with their own `importHidden` implementation. It is well known that with
Template Haskell, one can summon the hidden constructors of datatypes, but
summoning arbitrary hidden values is not directly supported. The next section
reveals the secret.

## Implementing the importHidden splice

### Abusing foreign imports

Let's forget about `importHidden` for a minute and think: how to handwrite
Haskell code to bring `foo` into scope without actually importing `Foo`? The
answer may seem surprising at first glance: use a foreign import!

GHC compiles Haskell definitions into machine code in static/dynamic libraries.
The Haskell module encapsulation doesn't affect machine code; regardless of
whether a Haskell value is hidden or not, its symbols will be visible to the
linker. This opens up the possibility of jailbreaking: if we can calculate a
value's symbol name, we can bring its address into scope using a foreign import,
and reconstruct the high-level Haskell value using some kind of unsafe coercion.

So what do GHC-generated symbols look like? We can use `nm` to inspect the
symbol table of an installed Haskell package, say, `base`:

```sh
$ cd $(ghc --print-libdir)
$ cd base-4.14.1.0
$ nm libHSbase-4.14.1.0.a
...
0000000000000000 D base_ForeignziForeignPtrziImp_withForeignPtr_closure
0000000000000018 T base_ForeignziForeignPtrziImp_withForeignPtr_info
                 U base_GHCziForeignPtr_ForeignPtr_con_info
...
```

The symbol names may look weird in the beginning, but they all follow a similar
pattern:

- The symbols are Z-encoded. Think of Z-encoding as a way to escape certain
  characters to make the C toolchain happy. For instance, the `zi` substring
  decodes to `.` in the example above.
- There are 4 underscore-delimited components. The package's unit ID, the module
  name, the value name, and the type of the thing indexed by that symbol. In our
  case, we're interested in `closure` symbols which represent the static heap
  object address of a Haskell value.

For whatever hidden value we'd like to use, we already know the
package/module/value name, so do we have all the ingredients to cook the correct
closure symbol name? Not yet. The unit ID is not trivial to obtain.

What does unit IDs look like? For packages shipped with GHC, they're either the
package name (e.g. `base`), or the package name followed by the version number
(e.g. `Cabal-3.0.1.0`). However, unit IDs of third-party packages have a unique
hash suffix (e.g. `aeson-1.4.7.1-BBxO5joHKZ5L11K8E1qG5k`). If a package is built
with different build plans, the hash suffix will differ. Thanks to this
mechanism, packages can be rebuilt multiple times and coexist in the same
package database, a `cabal build` run will never fail due to version conflict
with existing packages, and the so-called "cabal hell" becomes an ancient
memory.

For `importHidden` to be useful, it needs to support third-party packages,
therefore we need to find a way to query the exact unit ID given a package name
via Template Haskell. Among the existing Template Haskell APIs, the closest
thing to achieve this goal is `reifyModule`, which given a module name, returns
its import list. So if `Foo` appears in the current module's import list, we can
use `reifyModule` to get `Foo` metadata which includes `foo`'s unit ID. However,
this approach has a significant restriction: it doesn't work for hidden modules.

### Abusing GHC API

Recall that Template Haskell is usually run by a GHC process, to which the high-level
build tool like `cabal`/`stack` will pass a bunch of command-line arguments.
These will include something like `-package-id foo-xxx` if `foo` is a transitive
dependency! We may call `getArgs` in Template Haskell to get these arguments
then look for package flags, but a better solution would be using GHC API itself
to handle the parsing logic:

```haskell
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

import qualified CLabel as GHC
import qualified DynFlags as GHC
import qualified FastString as GHC
import Foreign.Ptr
import qualified GHC
import GHC.Exts
import qualified IdInfo as GHC
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import qualified Module as GHC
import qualified Name as GHC
import qualified Outputable as GHC
import qualified Packages as GHC
import System.Environment.Blank
import qualified Unique as GHC

findUnitId :: String -> Q GHC.UnitId
findUnitId pkg_name = runIO $ do
  args <- getArgs
  (dflags0, _, _) <-
    GHC.parseDynamicFlags GHC.unsafeGlobalDynFlags $
      map GHC.noLoc args
  (dflags1, _) <- GHC.initPackages dflags0
  let Just comp_id =
        GHC.lookupPackageName dflags1 $ GHC.PackageName $ GHC.fsLit pkg_name
  pure $
    GHC.DefiniteUnitId $
      GHC.DefUnitId $
        GHC.componentIdToInstalledUnitId
          comp_id
```

Here, inside a Template Haskell function, we use the GHC API to parse the
command-line arguments, handle the package-related arguments, then use the given
package name to look up the corresponding unit ID.

After the unit ID is obtained, calculating the value's closure symbol is pretty
simple:

```haskell
findClosureSymbol :: String -> String -> String -> Q String
findClosureSymbol pkg_name mod_name val_name = do
  unit_id <- findUnitId pkg_name
  pure $
    GHC.showSDoc GHC.unsafeGlobalDynFlags $
      GHC.pprCode GHC.AsmStyle $
        GHC.ppr $
          GHC.mkClosureLabel
            ( GHC.mkExternalName
                (GHC.mkUniqueGrimily 0)
                (GHC.mkModule unit_id (GHC.mkModuleName mod_name))
                (GHC.mkVarOcc val_name)
                GHC.noSrcSpan
            )
            GHC.MayHaveCafRefs
```

Now that we can calculate a value's closure symbol, we need to generate a
top-level foreign import declaration to convert it to a pointer, and return an
expression splice which coerces the pointer to a Haskell value of any
user-annotated type:

```haskell
closureFromPtr :: Ptr () -> a
closureFromPtr (Ptr addr) = case addrToAny# addr of
  (# a #) -> a

importHidden :: String -> String -> String -> Q Exp
importHidden pkg_name mod_name val_name = do
  closure_name <- findClosureSymbol pkg_name mod_name val_name
  import_name <- newName "__hidden"
  import_dec <-
    forImpD
      CCall
      Unsafe
      ("&" <> closure_name)
      import_name
      [t|Ptr ()|]
  addTopDecls [import_dec]
  [|closureFromPtr $(varE import_name)|]
```

Summarizing, our summoning ritual consisted of:

- Using GHC API and user-specified info to calculate a hidden value's closure
  symbol.
- Using a foreign import to obtain the closure address as a `Ptr` value.
- Using `addrToAny#` to cast the closure address back to a Haskell value.

With these hacks combined, now you can transcend the barriers of modules and
packages!

## Conclusion

Through a bit of knowledge about compiled Haskell code and abusing GHC API in
Template Haskell, we practiced some Haskell dark arts and were able to summon
hidden values. Before plugging this hack into a real-world codebase, let's
discuss the problems of this approach.

First, `importHidden` is dynamically typed. Since all Haskell type information
is lost at the lowest level, we can't do meaningful type checking in the
returned expression. All of its use sites must be explicitly annotated with the
original types, otherwise, segmentation faults await.

If a top-level value isn't exported, then the GHC inliner may choose to inline
it at its call sites, therefore the closure symbol we're seeking may be
non-existent. In that case, the error message won't be good, since it'll only be
an "undefined symbol" error at link-time.

The types of hidden values are also restricted. It's fine if it doesn't carry
constraints, but it won't work if constraints are present, since our
implementation doesn't take the dictionary passing mechanism of type classes
into account.

And finally, given we're trying to query GHC arguments via `getArgs`, it surely
won't work with an external interpreter or cross GHCs. And even if we only
intend to support the non cross-compilation scenario, calling the GHC API in
Template Haskell code is highly unsafe, given that the GHC API has process-global state
and doesn't have any reentrancy guarantee. It just happens to work in our case since
we only use a small subset of it, and don't create a proper GHC session.

Should you use `importHidden`? Most likely no, since patching the desired
dependencies is always simpler and more robust. Nevertheless, it's a fun
exercise, and we hope this post serves as a peek into how Haskell code works
under the hood :)
