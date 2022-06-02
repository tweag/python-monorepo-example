---
title: "Smooth, non-invasive Haskell Stack and Nix shell integration"
shortTitle: "Stack and Nix integration"
author: "Julien Debon"
tags: [haskell, nix]
description: "Integrating Haskell Stack and Nix in a smooth yet non-invasive way, with an explanation, example and template."
---

This blog post is for developers who want to build their Haskell project with [Stack](https://docs.haskellstack.org/en/stable/README/) and provide system dependencies and tools with [Nix](https://nixos.org/).

**TL;DR: create your project with `stack new myproject github:tweag/nix-integration`.**

### Stack

Stack is one of the two popular build tools for [Haskell](https://www.haskell.org/) projects.
It takes care of setting up a build environment by providing the [GHC](https://www.haskell.org/ghc/) compiler and works hand-in-hand with [Stackage snapshots](https://www.stackage.org/#about): a specific version of GHC paired with a subset of packages from [Hackage](https://hackage.haskell.org/), where each package's version was chosen to make the set self-consistent, relieving developers of the need of finding compatible Haskell package versions.

This post is about Stack as this is our main Haskell build tool at Tweag. Without entering into an emacs-vs-vi kind of debate, a similar post could be written about [Cabal](https://www.haskell.org/cabal/), whose interaction with Nix is a bit different.

### Nix

Nix is a package manager that provides — among other benefits — the [nix-shell](https://nixos.wiki/wiki/Development_environment_with_nix-shell), a sort of virtual environment for everything, including:

- system dependencies, e.g. `zlib` or your favorite database driver
- compilers, e.g. `ghc` or `javac`

Nix is popular among Haskell developers, and [at Tweag](https://www.tweag.io/blog/tags/nix).
The `nix-shell`, in particular, is a solution to the dreaded "it works on my machine" class of problems often encountered by teams working on a project.
However, there are many reasons why your coworkers or other contributors would not use Nix:

- they are on Windows and don't want to or can't use [Windows Subsystem for Linux](https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux)
- they can't install Nix on their machine (e.g. if they don't have root access)
- they don't know how

So while Nix might be great for you, it's helpful if it's optional for others.
Therefore, any solution combining Nix and Stack should be non-invasive and also work for those who don't use Nix.

### Everything will just work, right?

This is where things get awkward: Stack (via snapshots) and Nix (via `nix-shell`) partially overlap!
Remember, they can both provide GHC.
This is unfortunately not a simple "pick one" situation, as we will see below.

We want a solution where:

- Both Nix users and non-Nix users can work on the project with Stack (non-invasive)
- Nix users get all the system packages and tools they need from `nix-shell` (Nix integration)

In this blog post we will:

- look at common solutions and their pitfalls
- build a solution that fits all our goals
- provide templates so that you can easily reuse this solution in your projects

## The optimistic solution: Add Stack to Nix shell

This is the very first solution I tried back when I originally had this problem.

The idea is:

1. `nix-shell` provides Stack for Nix users
2. Stack will take care of providing GHC
3. Profit

```nix
# shell.nix
let
  pkgs = ...;
in
pkgs.mkShell {
  buildInputs = [ pkgs.stack ];
}
```

Let's see how this fares on a small Haskell project.
Say, a project using the excellent [Servant](https://docs.servant.dev/en/stable/) library:

```yaml
# package.yaml
[...]
dependencies:
  - base
  - servant-server
```

Let's compile this before we can start hacking on our project:

```
$ nix-shell
[nix-shell]$ stack build
[...]
zlib > configure
zlib > Configuring zlib-0.6.2.3...
zlib > Cabal-simple_mPHDZzAJ_3.2.1.0_ghc-8.10.7: Missing dependency on a foreign
zlib > library:
zlib > * Missing (or bad) header file: zlib.h
zlib > * Missing (or bad) C library: z
zlib > This problem can usually be solved by installing the system package that
zlib > provides this library (you may need the "-dev" version). [...]
```

Oh, right, Servant depends on the `zlib` system package. It's ok, we add it to our `shell.nix`:

```nix
# shell.nix
let
  pkgs = ...;
in
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.zlib
  ];
}
```

And then we recompile:

```
$ nix-shell
[nix-shell]$ stack build
[...]
zlib > configure
zlib > Configuring zlib-0.6.2.3...
zlib > Cabal-simple_mPHDZzAJ_3.2.1.0_ghc-8.10.7: Missing dependency on a foreign
zlib > library:
zlib > * Missing (or bad) header file: zlib.h
zlib > * Missing (or bad) C library: z
zlib > This problem can usually be solved by installing the system package that
zlib > provides this library (you may need the "-dev" version). [...]
```

Huh. What is happening here? Is it time to blame a cache? 🤔

The problem is that the `zlib` package is not visible to the GHC provided by Stack!

Another problem is that if other GHC-backed tools like [Haskell Language Server](https://haskell-language-server.readthedocs.io/en/latest/) or [Hoogle](https://hackage.haskell.org/package/hoogle) are provided by Nix, then they will use a different GHC from the one used to build the project, leading to all kinds of weird errors.

You can test for yourself that it does not work with the [full code of this section](https://github.com/tweag/haskell-stack-nix-example/tree/optimistic-stack-in-nix-shell).

**Key takeaway**: For Nix users, GHC should be provided by Nix, and Stack should use that GHC.

## The invasive solution: Use Stack-Nix integration

Stack conveniently provides [Nix integration](https://docs.haskellstack.org/en/stable/nix_integration/).
This is exactly what we were looking for!
In particular, Stack will use the Nix-provided GHC, and we can specify extra Nix packages like `zlib` that Stack will use during compilation.

This integration can take two forms:

- [Lightweight](https://docs.haskellstack.org/en/stable/nix_integration/#additions-to-your-stackyaml), by passing configuration flags in `stack.yaml` or in the command-line
- [Full](https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file), by passing a configuration flag pointing at a Nix file

For the remainder of this post, we will only use the "full" form, but every solution described below also works in the "lightweight" form.

In essence:

```nix
# shell.nix
let
  pkgs = ...;
in
pkgs.mkShell {
  buildInputs = [ pkgs.stack ];
  # Note that Stack relies on the `NIX_PATH` environment variable to discover
  # the `nixpkgs` and obtain the right `ghc`.
  NIX_PATH = "nixpkgs=" + pkgs.path;
}
```

```yaml
# stack.yaml
resolver: lts-18.28
packages:
  - .
nix:
  enable: true
  pure: false
  # See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
  shell-file: nix/stack-integration.nix
```

Lo and behold, our project now compiles! 🎉

But in the process, Nix concerns have now leaked into the non-Nix file `stack.yaml`! It means that non-Nix users are no longer able to build our project and contribute 😞

A less invasive alternative is to pass all those Nix flags in the command line: non-Nix users would not be impacted. But then Nix users would need to pass `--nix --no-nix-pure --nix-shell-file=nix/stack-integration.nix` options **on every `stack` command**! Not only is this inconvenient, but it is also easy to forget, and in that case, you would have weird or confusing errors.

You can test for yourself with the [full code of this section](https://github.com/tweag/haskell-stack-nix-example/tree/invasive-stack-nix-integration).

**Key takeaways**:

- We need a way to pass Stack-Nix integration flags only when Stack is used by Nix-users
- We must not modify non-Nix files (e.g. we must not modify `stack.yaml` or `package.yaml`)

## The solution

We want to:

- change some behavior for Nix users only, so the solution must happen in the Nix files
- change the behavior of Stack in particular, and it is provided by `shell.nix`, so the solution must happen in `shell.nix`
- provide some Nix flags to the `stack` command-line interface by default, so we need a way to "override", "alias" or "wrap" the `stack` executable.

A great candidate for this job is Nix's [wrapProgram](https://nixos.org/manual/nixpkgs/stable/#fun-makeWrapper) shell function from the `makeWrapper` Nix package!

We can wrap `stack` into a new executable (conveniently named `stack`) where the Nix flags are set, and provide this wrapped, enhanced `stack` rather than the default one in the shell:

```nix
let
  pkgs = ...;

  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --no-nix-pure \
          --nix-shell-file=nix/stack-integration.nix \
        "
    '';
  };

in
pkgs.mkShell {
  # Do NOT use `stack`, otherwise system dependencies like `zlib` are missing at compilation
  buildInputs = [ stack-wrapped ];
  NIX_PATH = "nixpkgs=" + pkgs.path;
}
```

Here I chose to use the full Stack-Nix integration by pointing Stack at a `nix/stack-integration.nix` file, but this would work similarly with the lightweight integration, by passing all Stack-Nix flags to `wrapProgram`, e.g., `--nix-packages zlib`.

In both cases, the `NIX_PATH` environment variable customization is also required by Stack-Nix.

Here is how the implementation works: `symlinkJoin` creates a copy (with symbolic links) of the `stack` output in the Nix store; after this is done, `wrapProgram` renames `stack` to `.stack-wrapped` and names the wrapper `stack`. This way, if you need the vanilla stack, without the extra arguments (e.g. for debugging purposes), it is available in the `nix-shell` as `.stack-wrapped`.

You can see the [full code](https://github.com/tweag/haskell-stack-nix-example/tree/main) for yourself.

### Stack template

Besides the full code linked above, I also prepared a Stack template, so all you need to do is:

```shell
stack new myproject github:tweag/nix-integration
```

and you will have a Haskell project with full Stack-Nix integration!

## Conclusion

This blog post was as much about giving a decent solution to Stack + Nix integration as it was about explaining how one can solve this kind of problem, taking into account both human constraints (e.g. not everyone uses Nix) and technical constraints (e.g. it must compile!).

Don't hesitate to open an issue to either repository (code examples and the template) if you have questions, remarks, suggestions!
