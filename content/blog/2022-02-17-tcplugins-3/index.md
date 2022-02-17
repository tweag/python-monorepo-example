---
title: "Type-checking plugins, Part III: writing a type-checking plugin"
shortTitle: "Writing a type-checking plugin"
author: Sam Derbyshire
tags: [haskell, ghc]
description: "A tutorial on how to write a type-checking plugin for GHC"
---

In this final post of the series, we finally get down to business: writing a type-checking plugin.

We concentrate on giving a high-level overview of the process.
For worked examples and the nitty-gritty details, please refer to my new
[`ghc-tcplugin-api`](https://hackage.haskell.org/package/ghc-tcplugin-api)
library, which aims to provide a carefully curated and documented interface.

- [I: Why write a type-checking plugin?](https://www.tweag.io/blog/2021-10-21-tcplugins-1/)
- [II: GHC's constraint solver](https://www.tweag.io/blog/2021-12-09-tcplugins-2/)
- **III: Writing a type-checking plugin**

### Table of contents

- [Writing a type-checking plugin](#writing-a-type-checking-plugin)

  - [Defining and using type-checking plugins](#defining-and-using-type-checking-plugins)
  - [The TcPlugin datatype](#the-tcplugin-datatype)
  - [Initialisation and name resolution](#initialisation-and-name-resolution)
  - [Solving constraints](#solving-constraints)

    - [Inspecting constraints](#inspecting-constraints)
    - [Providing evidence](#providing-evidence)

      - [Creating dictionary evidence](#creating-dictionary-evidence)
      - [Creating equality evidence](#creating-equality-evidence)

  - [Type family rewriting](#type-family-rewriting)

- [Debugging a type-checking plugin](#debugging-a-type-checking-plugin)

- [Further resources](#further-resources)

## Writing a type-checking plugin

Within a type-checking plugin, we manipulate types and constraints, as they are represented
internally in GHC. That is, for a type-checking plugin, a type is a value of type `Type`,
and a constraint is a value of type `Ct`. These two types are defined by GHC, and should
not be confused with the Haskell kinds `Type` and `Constraint` exported by `Data.Kind`.

### Defining and using type-checking plugins

Defining a plugin is similar to defining an ordinary Haskell module.

Unlike an executable which must export `main :: IO ()`,
a GHC plugin instead must declare and export `plugin :: Plugin`,
where `Plugin` is the [GHC plugin data-type](https://hackage.haskell.org/package/ghc-9.2.1/docs/GHC-Driver-Plugins.html#t:Plugin),
as exported from the [`ghc` package](https://hackage.haskell.org/package/ghc).

To use a plugin, we import it using a special `OPTIONS_GHC` pragma:

```haskell
{-# OPTIONS_GHC -fplugin PluginModuleName #-}
```

One can pass additional arguments to the plugin using the syntax

```haskell
{-# OPTIONS_GHC
  -fplugin PluginModuleName
  -fplugin-opt PluginModuleName:arg_1
  -fplugin-opt PluginModuleName:arg_2
  -fplugin-opt PluginModuleName:arg_3
  #-}
```

We are only concerned here with type-checking plugins, so we only define the `tcPlugin` field of `Plugin`,
which should be a function accepting a list of such arguments and returning `Just (tcPlugin :: TcPlugin)`:

```haskell
plugin :: Plugin
plugin = defaultPlugin { tcPlugin = \ args -> Just $ myTcPlugin args }
```

If our type-checking plugin is pure (that is, always yields the same result for the same input),
one can inform GHC of this fact by further specifying the field `pluginRecompile = purePlugin`.
This avoids GHC pessimistically recompiling modules using the plugin, which it would need to do
in general if the outcome depends on the time of day, the presence of certain files on disk, etc.

### The TcPlugin datatype

To define a type-checking plugin, we must provide a record of type `TcPlugin`:

```haskell
myTcPlugin :: [String] -> TcPlugin
myTcPlugin args =
  TcPlugin
    { tcPluginInit    = myTcPluginInit
    , tcPluginSolve   = myTcPluginSolve
    , tcPluginRewrite = myTcPluginRewrite
    , tcPluginStop    = myTcPluginStop
    }
```

where the `TcPlugin` datatype is defined as follows:

```haskell
data TcPlugin = forall s. TcPlugin
  { tcPluginInit    :: TcPluginM s
  , tcPluginSolve   :: s -> TcPluginSolver
  , tcPluginRewrite :: s -> UniqFM TyCon TcPluginRewriter
  , tcPluginStop    :: s -> TcPluginM ()
  }
```

Note that `TcPlugin` abstracts over a type `s` which the plugin author is free to choose;
it corresponds to the plugin environment/state that will be initialised.

When type-checking a module which uses our type-checking plugin, GHC will thus:

- initialise the plugin when beginning type-checking,
- query the plugin solver and rewriter repeatedly over the course of type-checking the module,
- shutdown the plugin using the user-provided `stop` function once done type-checking the module.

<p align="center">
<img src=GHC_Tc_state_machine.svg alt="GHC's typechecker interacting with type-checking plugins" />
</p>

Note that all four stages run in the `TcPluginM` monad, which gives access to GHC's type-checker
environment, as well as the ability to perform `IO` using `tcPluginIO :: IO a -> TcPluginM a`.

### Initialisation and name resolution

The plugin initialisation stage is where one usually performs the following operations:

- Initialisation of any external tools we will be using (such as an SMT solver),
  using `tcPluginIO` to perform the necessary IO actions.
- Name resolution: plugins don't refer to Haskell types simply by importing them; instead,
  they must look them up by name. This is how the plugin will get a hold of the
  typeclasses, type families, etc., which it intends to manipulate.

Focusing on the task of name resolution, let's see how to look up
the type family `MyFam` in module `MyModule` in package `my-pkg`.

```haskell
lookupMyModule :: TcPluginM Module
lookupMyModule = do
   findResult <- findImportedModule ( mkModuleName "MyModule" ) ( Just $ fsLit "my-pkg" )
   case findResult of
     Found _ myModule -> pure myModule
     _ -> error "MyPlugin couldn't find MyModule in my-pkg"

lookupMyFam :: Module -> TcPluginM TyCon
lookupMyFam myModule = do
  let
    myTyFam_OccName :: OccName
    myTyFam_OccName = mkTcOcc "MyFam"
  myTyFam_Name <- lookupOrig myModule myTyFam_OccName
  tcLookupTyCon myTyFam_Name
```

This returns the `MyFam` type family's type constructor.
One then typically proceeds by defining a record of the objects the type-checking plugin will manipulate:

```haskell
data MyPluginState =
  MkMyPluginState
    { myTyFam   :: TyCon
    , myClass   :: Class
    , myDataCon :: DataCon
    -- etc
    }
```

In the `tcPluginInit` stage, we look up all these objects, returning a `MyPluginState` record:

```haskell
myTcPluginInit :: TcPluginM MyPluginState
myTcPluginInit = do
  myModule  <- lookupMyModule
  myTyFam   <- lookupMyFam     myModule
  myClass   <- lookupMyClass   myModule
  myDataCon <- lookupMyDataCon myModule
  pure $ MkMyPluginState {..}
```

This record is then passed on to the other stages.

See the documentation of [`ghc-tcplugin-api` on Hackage](https://hackage.haskell.org/package/ghc-tcplugin-api)
for additional details on this name resolution process as well as the various functions such as `findImportedModule`, `mkTcOcc`, `lookupOrig`, etc.

### Solving constraints

A type-checking plugin's solver can be invoked in two different ways:

- to simplify Given constraints,
- to solve Wanted constraints.

Plugin authors can distinguish these two invocations by inspecting the Wanted constraints passed to the plugin:
in the first case (and the first case only), the plugin is only passed Given constraints, and no Wanted constraints.
The plugin should then respond with a `TcPluginSolveResult`:

```haskell
data TcPluginSolveResult
  = TcPluginSolveResult
  { tcPluginContras   :: [Ct]
  , tcPluginSolvedCts :: [(EvTerm, Ct)]
  , tcPluginNewCts    :: [Ct]
  }
```

That is, the plugin can specify:

- Contradictory/insoluble constraints, to be reported as errors to the user.
- Solved constraints, with associated evidence. GHC will record the evidence and discard the constraint.
- New constraints, which will be processed by GHC.

Note that the plugin must respond with constraints of the flavour that is appropriate for its invocation.
That is, when simplifying Givens it should only return Given constraints, and when solving Wanteds it should
only return Wanted constraints; all other constraints will be ignored by GHC.

Even though a solver is invoked in only two different ways, it can be invoked in many different circumstances.
It's usually best not to dwell too much on this aspect: the solver should simply do its best
with whatever constraints it is passed, not paying too much thought about why GHC is interested
in this particular constraint-solving problem.

#### Inspecting constraints

As we saw in [Part II: § Constraint canonicalisation](https://www.tweag.io/posts/2021-12-09-tcplugins-2.html#constraint-canonicalisation),
constraints fall into the following categories:

| Predicate   | Examples                              | Evidence      |
| ----------- | ------------------------------------- | ------------- |
| Typeclass   | `Ord a`, `Num a`, `(c1, c2)`, `a ~ b` | Dictionary    |
| Equality    | `a ~# b`, `a ~R# b`                   | Coercion      |
| Quantified  | `forall a. Eq a => Eq (f a)`          | Function      |
| Irreducible | `c a`, `F x y`                        | Not yet known |

We can use [`ctPred :: Ct -> PredType`](https://hackage.haskell.org/package/ghc-tcplugin-api/docs/GHC-TcPlugin-API.html#v:ctPred)
and [`classifyPredType :: PredType -> Pred`](https://hackage.haskell.org/package/ghc-tcplugin-api/docs/GHC-TcPlugin-API.html#v:classifyPredType)
to sort constraints into one of the above categories. This is preferable to manually inspecting GHC's internal representation of the constraint, which might undergo changes across GHC versions.

#### Providing evidence

We saw in [Part II: § Solving constraints](https://www.tweag.io/posts/2021-12-09-tcplugins-2.html#solving-constraints) that when solving constraints,
a type-checking plugin's solver must provide evidence. For a typeclass constraint,
this is a dictionary of its methods; for an equality constraint, a coercion.

##### Creating dictionary evidence

For class evidence, we need to build up a dictionary of the right type, much like in the original example
from [Part II: § Dictionary constraints](https://www.tweag.io/posts/2021-12-09-tcplugins-2.html#dictionary-constraints).

We start by using `classDataCon :: Class -> DataCon` to obtain the data constructor associated
with the class dictionary. To create evidence for the typeclass constraint, we then apply the obtained `DataCon` to Core expressions of the methods using

```haskell
mkCoreConApps :: DataCon -> [CoreExpr] -> CoreExpr
```

This will result in a `CoreExpr` that can be turned into an evidence term using `EvExpr :: CoreExpr -> EvTerm` (recall that solver plugins must return values of type `EvTerm` when solving constraints).

##### Creating equality evidence

For coercions, recall the syntax of coercions from [Part II: § Coercions, a reading guide](https://www.tweag.io/posts/2021-12-09-tcplugins-2.html#coercions-a-reading-guide).
Note however that, most of the time, a plugin can get away creating unsafe coercions using `mkUnivCo`:

```haskell
mkUnivCo :: UnivCoProvenance
         -- ^ What type of unsafe coercion is this?
         -- Plugins usually pass @PluginProv (str :: String)@ here.
         -> Role     -- ^ The desired 'Role' of the result coercion @co@
         -> Type     -- ^ t1 :: k1
         -> Type     -- ^ t2 :: k2
         -> Coercion -- ^ co :: t1 ~r t2
```

A `Coercion` can be turned into an `EvTerm` using `evCoercion :: Coercion -> EvTerm`,
to be used when returning solved equality constraints with `TcPluginOk`.

### Type family rewriting

To rewrite type families, a type-checking plugin must provide a value

```haskell
tcPluginRewrite :: s -> UniqFM TyCon TcPluginRewriter
```

That is, we must provide a map from type family `TyCon` to rewriting functions.[^1]

`TcPluginRewriter` is defined as follows:

```haskell
type TcPluginRewriter
  =  [Ct]   -- ^ Givens
  -> [Type] -- ^ Type family arguments
  -> TcPluginM TcPluginRewriteResult

data TcPluginRewriteResult
  = TcPluginNoRewrite
  | TcPluginRewriteTo
    { tcPluginReduction    :: Reduction
    , tcRewriterNewWanteds :: [Ct]
    }
```

That is, for each type family `TyCon`, the plugin is supplied with the type family arguments
(which are guaranteed to exactly saturate the type family), as well as the Given constraints.
The plugin can then specify how to rewrite this type family application;
as well as emit extra Wanted constraints when rewriting. This can be used, for instance,
to emit custom type errors when rewriting. Note that this behaviour is rather different
from how GHC usually goes about rewriting type family applications, as GHC otherwise never
emits new constraints as a byproduct of reducing a type family application.

It is important to remember that, as per [Part II: § A primer on type family reduction](https://www.tweag.io/posts/2021-12-09-tcplugins-2.html#a-primer-on-type-family-reduction),
the type family arguments also include all the invisible arguments. For example:

```haskell
type Id :: forall k. k -> k
type family Id a where
  Id a = a
```

`Id` takes two arguments; one invisible and one visible.
For instance: `Id @Type Int`, `Id @Nat 17`.  
When dealing with type family arguments, one must not forget to account
for these invisible arguments; they are treated the same as the visible arguments
within a type-checking plugin.

With this in mind, how do we rewrite a type family application?
The evidence required to reduce a type family application is a `Reduction`.
This is defined as follows:

```haskell
data Reduction = Reduction Coercion !Type
```

That is, to rewrite the type family application `F a_1 ... a_n` to `xi`,
a rewriter plugin should answer with a `Reduction co xi`, where `co :: F a_1 ... a_n ~# xi`.  
Note in particular that the coercion is oriented left-to-right: the original type family application
is on the left, and the rewritten type is on the right.

As in [§ Creating equality evidence](#creating-equality-evidence), it is usually sufficient
to simply pass an unsafe universal coercion, using `mkUnivCo`.

## Debugging a type-checking plugin

### tcTrace

To start off, it can be useful to see precisely what constraints the plugin is seeing.
To immediately print out all the constraints that the plugin is provided with, use  
[`tcPluginTrace :: String -> SDoc -> TcPluginM ()`](https://hackage.haskell.org/package/ghc-tcplugin-api/docs/GHC-TcPlugin-API.html#v:tcPluginTrace)

```haskell
myTcPluginSolve _s givens wanteds = do
  tcPluginTrace "---Plugin start---" (ppr givens $$ ppr wanteds)
  pure $ TcPluginOk [] []
```

Then, compiling with `-ddump-tc-trace -ddump-to-file`
(see [the GHC manual](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html#type-checking-and-renaming))
will allow you to take a look at the constraints that the plugin is being given.

Here `ppr :: Outputable a => a -> SDoc` turns any `Outputable` thing into a pretty-printable
`SDoc` document. Many combinators are available in the `GHC.Utils.Outputable` module, such as:

- `(<>), (<+>) :: SDoc -> SDoc -> SDoc` for concatenating documents horizontally (with or without an extra space in between, respectively),
- `text :: String -> SDoc` to turn a textual literal into a pretty-printable document,
- `vcat :: [SDoc] -> SDoc` to concatenate documents vertically.

It's often useful to intersperse the logic of your type-checking plugin with trace statements,
to ensure that it's behaviour is as intended.

### Core Lint

Recall from [§ Writing a type-checking plugin](#writing-a-type-checking-plugin) that
the types manipulated by a type-checking plugin are values of type `Type`, where `Type`
is GHC's internal datatype used to represent types. This system doesn't keep track of kinds:
in a type-checking plugin, one will not get a type error if one passes a `Symbol`
to a type family expecting an argument of kind `Nat`: for GHC, internally,
these are both represented as values of type `Type`. So it's quite easy to introduce
kind errors in a plugin. And this is just one of the many sorts of elementary mistakes
one can make when writing a type-checking plugin:

- forgetting to include invisible arguments to a type family,
- constructing evidence of the wrong type,
- creating coercions with the wrong orientation,
- ...

The main safety net available to type-checking plugin authors is **Core Lint**,
which functions as a type-checker for Core.
Core Lint provides the [following strong guarantee](https://gitlab.haskell.org/ghc/ghc/-/blob/6566ad3e27444311f069c2365efbdb819bb18c23/compiler/GHC/Core/Lint.hs#L99): if

- the Core program passes Core Lint,
- there are no erroneous unsafe coercions, and
- all case matches are complete,

then the resulting compiled program will not seg-fault
(modulo bugs downstream such as in the code generator).

It is thus **critically important** to enable Core Lint during development of a type-checking plugin.
To do so, pass the `-dcore-lint` flag when comping with GHC; equivalently, include

```haskell
{-# OPTIONS_GHC -dcore-lint #-}
```

in the preamble of the modules making use of your type-checking plugin.

It can also be useful to include debugging info in the coercions that the plugin creates.
Recall that the plugin can specify coercions using `mkUnivCo`, e.g.

```
mkUnivCo (PluginProv myDebugString) role lhs rhs
```

If a Core Lint error occurs, the debug strings included in the coercions can help keep track
of the logic of the plugin.

## Further resources

To help everyone get started writing a type-checking plugin, I've written the [`ghc-tcplugin-api`](https://hackage.haskell.org/package/ghc-tcplugin-api) library. Its Hackage documentation is designed
to provide guidance with the necessary minutiæ of writing a type-checking plugin and working
with the GHC API.

---

[^1]:
  `UniqFM` stands for "unique finite map"; it's an `IntMap` in which we use
  `Unique` as a key. (`Unique` is the type of unique identifiers used in GHC;
  every `Name` has an associated `Unique`.)
