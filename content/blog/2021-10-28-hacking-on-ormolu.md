---
title: "Hacking on Ormolu: An internship report"
author: Alexander Esgen
description: An experience report on polishing Ormolu, the Haskell code formatter, during my internship at Tweag.
tags: [internship, haskell]
---

After being convinced of the usefulness of code formatters by the excellent [scalafmt][scalafmt], which is very widely used in the Scala ecosystem, I was on the lookout for a similar tool for Haskell. In 2018, most options did not completely cut it for me, due to e.g. only formatting certain parts of the source file or having many unfortunate bugs. Upon discovering Ormolu in 2019, I was first appalled by its style, but this reaction was only short-lived, as I found these stylistic preferences to be easily malleable via familiarisation.

I started using Ormolu for all personal projects, and submitted bug reports and minor pull requests. Therefore, an internship to work on Ormolu full-time came like a call!

As described in the [announcement post][ormolu-internship-ann], I worked on support for recent GHC versions, improved the CI setup, fixed various bugs and could also bring in my own suggestions. Let's get into it!

### Upgrading `ghc-lib-parser`

Like most modern tools operating on Haskell source files, Ormolu leverages the parser in GHC via [`ghc-lib-parser`](ghc-lib-parser). As major GHC upgrades often result in significant changes in the exposed compiler API, upgrading Ormolu to a new version of `ghc-lib-parser` often involves a non-trivial amount of work. At the start of my internship, the upgrade to `ghc-lib-parser-9.0` was long overdue. After playing type tetris for a while to get everything to compile, I had to dive into the details in order to debug subtle failures in the test suite.

One cool change in the GHC API which allowed me to simplify the code at several places works like this: In GHC 8.8, the pattern match coverage checker got smarter in detecting that constructors containing a `Void`-like type can not occur.

```haskell
-- A type with no inhabitants
data Void

-- Ex falso quodlibet
absurd :: Void -> a
absurd = \case {}

data Music a
  = AutoDetect !a
  | Opus
  | Flac
```

Now values of type `Music FilePath` could either be `AutoDetect filePath`, `Opus` or `Flac`, but values of type `Music Void` will always be either `Opus` or `Flac`. Types like this are ubiquitous in the GHC API due to a technique called [Trees that grow][ttg], and can be thought of as a way to emulate anonymous sum types.

Note that the strictness annotation of `a` is crucial here to ensure that it is impossible to plug in something like `undefined` to create a value of type `Music Void` other than `Opus` and `Flac`. In GHC 9.0, these strictness annotations were added in the appropriate places, which allowed me to rewrite code like

```haskell
isLossy :: Music Void -> Bool
isLossy = \case
  AutoDetect x -> absurd x
  Opus -> True
  Flac -> False
```

to

```haskell
isLossy :: Music Void -> Bool
isLossy = \case
  Opus -> True
  Flac -> False
```

which is a nice reduction in cognitive load.

### Getting to appreciate Nix

Ormolu provides binary releases, as compiling Ormolu from scratch takes a long time, especially due to the dependency on `ghc-lib-parser`. I had previously contributed a simple GitHub Actions workflow to do this, but it did not use the existing Nix setup, and in particular possibly not the exact same set of dependencies that are used on Ormolu's CI.

This sparked the idea of using [`haskell.nix`][haskell.nix] in Ormolu's Nix setup. In our case, the following features were particularly nice:

- haskell.nix uses the build plan of cabal, instead of using a fixed package set. This is very convenient, as one does not have to manually override the version of ones dependencies if the default ones are insuitable.
- It is trivial to create a fully static (musl-based) Linux binary, and, amazingly, even to cross-compile to Windows! The following is the entire Nix code which is responsible for creating Ormolu's standalone binaries:

```nix
binaries = {
  Linux = hsPkgs.projectCross.musl64.hsPkgs.ormolu.components.exes.ormolu;
  macOS = pkgs.runCommand "ormolu-macOS" {
    buildInputs = [ pkgs.macdylibbundler ];
  } ''
    mkdir -p $out/bin
    cp ${ormoluExe}/bin/ormolu $out/bin/ormolu
    chmod 755 $out/bin/ormolu
    dylibbundler -b -x $out/bin/ormolu -d $out/bin -p '@executable_path'
  '';
  Windows = hsPkgs.projectCross.mingwW64.hsPkgs.ormolu.components.exes.ormolu;
};
```

In addition, `haskell.nix` was for a long time the only way to reliably use recent GHCJS versions, which will be relevant as seen in the next section.

### Reviving Ormolu Live

Earlier, I enjoyed using [Ormolu Live][ormolu-live-old], which allowed one to play around with Ormolu in the browser without installation. I suggested to revive this project as part of my internship, which was met with encouragement.

The original incarnation of Ormolu Live relied on `reflex-platform`, which does not yet support GHC 8.10, yet `ghc-lib-parser-9.2` requires at least version 8.10. Therefore, I rewrote Ormolu Live using [`miso`][miso], a small Elm-like framework, and added some configurability and the option to view the GHC AST in the process.

The new Ormolu Live now lives in the Ormolu repo and is updated automatically on every commit to `master`. Feel free to play around with it [here][ormolu-live]!

### New features in Ormolu

Of course, I did not only work on peripheral tasks, but also on Ormolu itself directly. Two highlights:

#### Respecting `.cabal` files

In many projects, certain GHC language extensions are enabled for all modules in the project's `.cabal` file:

```cabal
library
  default-extensions: BangPatterns LambdaCase PatternSynonyms
  default-language: Haskell2010
```

Since 0.2.0.0, specifying the `--cabal-default-extensions` flag will make Ormolu automatically take these into consideration when parsing your Haskell source files.

As I am guilty of always pasting a huge set of extensions in my `.cabal` file for personal projects and found it very annoying having to manually add these to Ormolu as CLI arguments, I am happy to having got this implemented!

This feature is also enabled by default in [ormolu-action][ormolu-action], the official way to run Ormolu via GitHub Actions.

#### Robust support for `CPP` and disabling formatting locally

Unfortunately, some Haskell code is impossible to be correctly formatted automatically, like complex usage of the `CPP` language extension, or preserving a very specific code layout of a single function. This necessarily requires one to make tradeoffs, which was an interesting process with rewarding discussions. I want to thank @kukimik on GitHub for suggesting the basic idea we ended up incorporating.

We decided to replace the previous mechanism to handle these cases with a more principled approach, so in particular, you can now be confident that text between Ormolu's magic comments won't be touched at all:

```haskell
{- ORMOLU_DISABLE -}
U can't touch this!
{- ORMOLU_ENABLE -}
```

We follow a simple but effective strategy:
At first, all lines between these magic comments, but also lines between `#if` and `#endif` and similar constructs of `CPP`, are marked. Then all contiguous regions of unmarked lines are formatted individually, with the raw marked lines being interspersed at the end.

There are files using `CPP` that cannot be formatted correctly with this strategy, but with a basic mental model of how Ormolu works, as well as appropriately inserted magic comments, even more complex cases should not be hard to adapt.

### Bugs, bugs, bugs

Even though Ormolu is continuously tested on thousands of lines of Haskell code, various special cases of less used language features were still lurking around, waiting to disrupt someone's workflow. All such known incidents are now resolved. In particular, support of the `Arrows` extension is now significantly more robust, and a long standing bug involving misplaced Haddock comments has been fixed.

As one of its goals, Ormolu strives to be _idempotent_, meaning that formatting twice will always yield the same result as only formatting once. It does a pretty good job at this, but as soon as comments are added to the mix, there are still many cases where one has to format twice (or sometimes even more often) to get to an idempotent snippet. This is not a perfect state of affairs, but fixing these kinds of issues is often very brittle and intricate with little real benefit, so we decided that these bugs should not be the primary focus of my internship. Possibly, an entirely new approach to printing comments might be necessary to get to the root of this problem.

### Conclusion

In summary, the internship was an excellent experience. I learned many new things about the GHC API and finally got my hands dirty with Nix. I really enjoyed talking to many awesome people as part of numerous coffee breaks, and had a lot of fun with my mentor Mark!

[scalafmt]: https://scalameta.org/scalafmt
[ormolu-internship-ann]: https://www.tweag.io/blog/2021-04-23-ormolu-intership/
[ghc-lib-parser]: https://github.com/digital-asset/ghc-lib
[ttg]: https://www.microsoft.com/en-us/research/uploads/prod/2016/11/trees-that-grow.pdf
[haskell.nix]: https://input-output-hk.github.io/haskell.nix/motivation/
[ormolu-live-old]: https://github.com/monadfix/ormolu-live
[miso]: https://github.com/dmjio/miso
[ormolu-live]: https://ormolu-live.tweag.io
[ormolu-action]: https://github.com/mrkkrp/ormolu-action
