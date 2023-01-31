---
title: "A Semester of HLS: An Internship Report"
author: Berk Özkütük
tags: [internship, haskell]
description: "An experience report on improving HLS during my internship at Tweag."
---

Historically, tooling hasn't been the strongest aspect of the Haskell ecosystem. However, it has come a long way. A major contribution from the last few years is the [Haskell Language Server][hls-repo] (HLS). I wanted to learn more about GHC internals, and the general compilation pipeline. What better way to learn about these than to work on HLS, helping further improve Haskell tooling in the meanwhile?

In this post I will discuss the experience working on HLS during my internship at Tweag.

## What is Haskell Language Server?

Before we move on to digging into the inner workings of HLS, let's first take a look into what HLS is. Quoting the HLS documentation:

> The `haskell-language-server` (HLS) project is an implementation of a server (a “language server”) for the [Language Server Protocol][lsp] (LSP).

LSP is a protocol that enables programming languages to provide IDE capabilities in an editor-agnostic way. These capabilities can range from basic functionality that we all expect from any IDE (like code completion and semantic code navigation) to more exotic functionalities specific to each language. HLS is an implementation of the Language Server Protocol for Haskell that provide such capabilities.

HLS has a modular design. Every piece of functionality is implemented as a separate plugin, which can be enabled or disabled independently. This plugin architecture makes it very easy to experiment with adding new capabilities to the server, as you make your changes as a separate package, without having to touch the rest of the HLS code[^1].

[^1]: Well, almost. There is the occasional need to modify the compatibility modules so that the changes introduced can work on a wide range on GHC versions.

## Hello, "Explicit Record Fields" plugin!

One of the planned goals of my internship was to create a new "code action" in HLS to desugar [`RecordWildCards`][record-wildcards] into [`NamedFieldPuns`][named-field-puns].

Records are ubiquitous in Haskell. Yet, dealing with plain [Haskell98][haskell-98-report] records gets tedious pretty quickly. `RecordWildCards` and `NamedFieldPuns` are two language extensions that emerged over the years to alleviate the syntactic burden of Haskell98 records.

First a quick recap. Haskell allows to create datatypes with field labels:

```haskell
data C = F { f1 :: Int, f2 :: Bool }
```

In plain Haskell98, pattern matching on `C` requires the following syntax:

```haskell
g :: C -> Int
g C { f1 = i, f2 = b } = if b then i else 0
```

The [`NamedFieldPuns`][named-field-puns] extension allows us to "pun"[^2] the field selectors `f1` and `f2` with the value of their relative field. That is, we can instead use the following lighter syntax:

```haskell
g :: C -> Int
g C { f1, f2 } = if f2 then f1 else 0
```

[^2]: _pun_: A play on words, sometimes on different senses of the same word and sometimes on the similar sense or sound of different words.

The [`RecordWildCards`][record-wildcards] extension goes one step further and allows us to use the wildcard `{..}` to bring all the fields into the current scope.

```haskell
g :: C -> Int
g C { .. } = if f2 then f1 else 0
```

Some Haskell users think this is a step too far, since it is no longer obvious at first sight where a name comes from.

The idea behind [`hls-explicit-record-fields-plugin`][explicit-fields-plugin] is to provide a code action which converts record wildcards into their respective expanded forms, explicitly listing all the used fields as field puns.

## Anatomy of an HLS plugin

At this point I knew _what_ I wanted to do, and the next step was to figure out _how_. It seemed sensible to start out by writing a small plugin which does essentially nothing, just to figure out the general plugin architecture and have the required plugin boilerplate by the end.

HLS provides the `PluginDescriptor` type which, as expected, describes an HLS plugin. The `PluginDescriptor` type contains many fields to accommodate the needs of many different kinds of plugins. However, for our purposes, leaving most fields with their default values is sufficient. In fact, `hls-explicit-records-fields-plugin` currently has the following descriptor:

```haskell
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
  , pluginRules = collectRecordsRule recorder *> collectNamesRule
  }
```

- `pluginHandlers` specifies which requests from the client (i.e. the editor) this plugin handles. In our case, we want this plugin to provide a code action, so we create a plugin handler with our `codeActionProvider`.

- `pluginRules` allows plugins to provide custom rules for the HLS build graph. This way, we can cache the results of the plugin invocation and only refresh them if required. The build graph handles dependency-checking and caching automatically, we just need to create our rules and hand them to HLS.

We handle the "infrastructure work" in `codeActionProvider`, which is the function invoked when the user triggers a code action. This function obtains the list of wildcard records in the current editor buffer using the `collectRecordsRule` rule we define, filters the result by the range currently selected in the editor and finally textually replaces the result in the buffer.

Note that we could opt for doing all work within `codeActionProvider`, rather than defining a custom rule. However, doing so would not benefit from the caching performed by the build graph and the plugin would recompute everything each time the code action is triggered. Having a separate rule also provides a clear separation of concerns: the rule takes care of the GHC side of the work, whereas the rest of the `codeActionProvider` function handles the LSP side of things.

## Using GHC programmatically

As we briefly touched upon in the previous section, our custom defined rule is doing the actual work, but what is that work exactly? It boils down to two simple steps:

1. Extracting the records with wildcards from the current Haskell source.
2. Transforming those records such that wildcards are replaced with field puns.

Let's focus on the two steps separately.

### Scrap Your Wildcards

We want to extract the records from the Haskell source, but textual extraction is no good. What we want is to extract the fragments corresponding to records from the abstract syntax tree (AST) that results from GHC's parsing stage. Luckily, we don't even need to speak to GHC ourselves. There is a rule that ships with HLS that does just that: `GetParsedModule`. As the name might imply, given a source file, it yields a `ParsedModule` (i.e. the AST) of the file.

However, for the purposes of this specific plugin, we can do even better. Let's think about what we want to achieve in the bigger picture: we want to expand the record wildcards, and for that, we need to figure out the fields that correspond to each record. However, in order to be able to compile down the code, GHC already does all the name resolution work in its [renamer phase][renamer]. Therefore, we can simply grab the result of that phase. There isn't a rule in HLS specific to the renamer phase; so we will use the `TypeCheck` rule instead, which contains the output of the renamer phase, alongside the output of the typechecker.

We have our AST, now we need to extract the records from it. The Haskell ecosystem has different approaches for this kind of work, but I have opted for using the ["Scrap Your Boilerplate"][syb-paper] approach, with its canonical library [`syb`][syb]. But before we get down to writing down the code, we need to figure out what we want to extract from the AST. That is, we still don't know which part of the AST corresponds to the records.

There are bunch of flags that make GHC dump the results of its intermediate compilation phases, and I have found them very helpful in figuring out what I need from the AST. To that end, I prepared a small Haskell file that contains record construction and pattern matching on records, then used the following to obtain the AST:

```sh
$ ghc Test.hs -ddump-rn-ast -ddump-to-file -fforce-recomp
```

This dumps the output of the renamer phase to a file[^3]. Looking at the dumped AST, we notice two constructs of interest:

[^3]: `-fforce-recomp` isn't strictly necessary; it forces GHC to recompile the file even if the source hasn't changed, which is useful if you want to compile the same source with just some flags changed.

1. [`RecordCon`][recordcon] constructor of `HsExpr`, for the record construction expressions.
2. [`ConPat`][conpat] constructor of `Pat`, for the record patterns[^4].

[^4]: Records can be pattern-matched in prefix or infix form as well. We are only interested in the pattern matches with the record syntax. The actual code only collects `ConPat`s of this form.

Finally we can put `syb` to use. We need a data type to capture the record-related information, so let's create one:

```haskell
data RecordInfo
  = RecordInfoPat RealSrcSpan (Pat (GhcPass 'Renamed))
  | RecordInfoCon RealSrcSpan (HsExpr (GhcPass 'Renamed))
```

Now we just write a SYB traversal, and everything almost happens auto-magically by just providing the types:

```haskell
collectRecords :: GenericQ [RecordInfo]
collectRecords = everything (<>) (maybeToList . (Nothing `mkQ` getRecPatterns `extQ` getRecCons))

getRecCons :: LHsExpr (GhcPass 'Renamed) -> Maybe RecordInfo
getRecCons e@(unLoc -> RecordCon _ _ flds)
  | isJust (rec_dotdot flds) = mkRecInfo e
getRecCons _ = Nothing

getRecPatterns :: LPat (GhcPass 'Renamed) -> Maybe RecordInfo
getRecPatterns conPat@(conPatDetails . unLoc -> Just (RecCon flds))
  | isJust (rec_dotdot flds) = mkRecInfo conPat
getRecPatterns _ = Nothing
```

The exact details of `mkRecInfo`, `getRecCons` and `getRecPatterns` aren't too important. They do a few extra things like only collecting the wildcard records, and massaging the extracted AST fragment so that it fits into the `RecordInfo` type we created.

### From Source to AST and Back Again

Now we need to render these AST fragments back to Haskell source such that they are in the punned form. In GHC, types that are constituents of an AST bear an instance of the `Outputable` class, which is a pretty-printing abstraction that is prevalent within GHC. However, when I used this instance to render the records back to Haskell source the records had not changed at all! They still were in the wildcard form. So, what happened?

Reading through the `Outputable` instances of these AST constructs, it occurred to me that GHC is actually remembering, in its AST, some details of the concrete syntax. Therefore, even if GHC has resolved the fields of a wildcard record during its renaming phase, GHC keeps formatting the record it in its original wildcard form. Where does this leave us? Well, we can trick GHC to thinking the record was _not_ in the wildcard form by manipulating its `rec_dotdot` field, so that it will print the record as we want it to:

```haskell
preprocessRecord getName names flds = flds { rec_dotdot = Nothing, rec_flds = rec_flds' }
  where
    rec_flds' = ...
```

We preprocess the record before rendering it, fiddling with the necessary fields to make it render just as we want. In actuality, this function does quite a bit more: leaving the explicitly provided fields as is, removing unused fields, etc. However, the idea is the same, we tweak the AST fragment until we are satisfied with the pretty-printed result.

## Conclusion

We have covered all the main parts of the plugin, but this doesn't mean our work is done. Getting a plugin merged into HLS involves a bit more work:

- Writing compatibility layers to make sure the plugin works across multiple GHC versions.
- Adding tests to ensure the correctness of the plugin under different conditions.
- Writing lots of documentation.

However, this shouldn't discourage anyone from writing a plugin. The HLS team is very friendly and there are always people hanging out in the HLS chatroom helping out with questions. The code in general is pleasant to work with (at least until writing CPP code) and the plugin architecture makes it very easy to experiment with new ideas, without the fear of breaking other parts of the server.

Whether you have the next big idea for IDEs or just want to fix a minor quirk of HLS that's been bugging you, I invite you to contribute to HLS! It has been both an educational and a rewarding experience for me, and I believe it will be the same for you too.

As final words, I want to thank Tweag for this internship opportunity. Apart from the work I have done, I have also met great people over coffee breaks. I find the conversations I had with each of them an invaluable part of my internship. Finally, I want to thank my supervisor Andrea Bedini for his guidance and encouraging words throughout my internship, without which I wouldn't be able to achieve what I have.

[haskell-98-report]: https://www.haskell.org/onlinereport/
[record-wildcards]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_wildcards.html
[named-field-puns]: https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/record_puns.html
[hls-repo]: https://github.com/haskell/haskell-language-server
[lsp]: https://microsoft.github.io/language-server-protocol/
[explicit-fields-plugin]: https://haskell-language-server.readthedocs.io/en/latest/features.html#expand-record-wildcard
[syb-paper]: https://www.microsoft.com/en-us/research/wp-content/uploads/2003/01/hmap.pdf
[syb]: https://hackage.haskell.org/package/syb
[renamer]: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/renamer
[recordcon]: https://hackage.haskell.org/package/ghc-9.4.4/docs/Language-Haskell-Syntax-Expr.html#v:RecordCon
[conpat]: https://hackage.haskell.org/package/ghc-9.4.4/docs/GHC-Hs-Pat.html#v:ConPat
