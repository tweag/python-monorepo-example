---
title: "Worry-free NixOS refactors"
author: Taeer Bar-Yam
description: "Comparing Nix input hashes can guarantee that some refactors are bug-free."
tags: [devops, nix]
---

As a NixOS configuration grows in complexity, it may eventually become clear that it's not optimally organized.
Maybe the entire configuration is in a single unwieldy file and needs to be broken up.
Maybe there are bits repeated for every machine which should really be placed in a common module.
This can become a problem in any codebase, but there is a trick for NixOS configurations that makes the process a whole lot less stressful.

## Input Hashes

A NixOS system is defined by a configuration written in the Nix language.
The configuration sets various options and is evaluated into a build task for the system, which incorporates instructions for how to build, along with all the inputs they depend on.
Building the system involves fetching the inputs and executing the instructions, which generates the system configuration files that are usually distributed throughout a Linux system.
These are all are written into one result directory.^[For this post, we will ignore the question of how this directory is used to set up a running system.]
Everything that goes into building that directory, the build instructions and the inputs they depends on, get hashed.
This hash appears in the path where the output is stored, the Nix store path:

```
/nix/store/<hash>-<output name>
```

I'll call a refactor _pure_ if it consists entirely of refactoring — only the internal structure changes, but it evaluates to the same output (the derivation, or build task). If we do a pure refactor of the Nix language code then the hash and the store path will also stay the same.
And any time the store paths are the same, you can be sure the configurations will behave the same!
In fact, it's not even necessary to redeploy.
In _fact_, Nix won't even rebuild the system. If it sees that the store path already exists, it will use that cached value.

You can find out the current system's store path with

```
readlink /run/current-system
```

and when building a new system, you can see the store path with

```
nixos-rebuild build && readlink result
```

If those two are the same, your refactor is pure, and you don't have to worry about introducing new bugs (or fixing old ones).
You can apply that refactor worry-free.

## Why does it work?

What makes this possible in NixOS?
One factor is easy to take for granted: there is such thing as a system definition in the first place!
For non-NixOS systems, "refactoring" your system may not even be a meaningful concept, since the system is defined by a bunch of state spread across the machine.
With NixOS, there is static, well-defined configuration written in the Nix language.

For systems with some kind of system definition which can be refactored, (e.g. through a Dockerfile), the way in which the configuration gets realized is usually not sufficiently isolated from its environment for this kind of comparison to be useful.
The setup script might access the internet, or inputs might be defined externally.
If the system is likely to change from one invocation to the next, it doesn't help much to know that it's inconsistent in the same way as before.^[It's worth noting that this can technically happen in Nix too. Build scripts are not prevented from generating randomness or checking the time. But in practice, this happens far less in NixOS systems, and almost always in ways that are inconsequential to the output.]

Another critical piece that makes this useful is that the input hash is calculated only after the configuration is turned into a _derivation_, the system's build task.
This means that only the options that actually end up determining the system are taken into account.
If you don't enable a service, then any other setting for that service is irrelevant.
If you introduce a new option that is used to set others, that won't affect the hash either.

For example, this diff is a pure refactor, and will not change the hash:[^documentation_impure]

[^documentation_impure]: unless you turn on [`documentation.nixos.includeAllModules`](https://search.nixos.org/options?channel=22.05&show=documentation.nixos.includeAllModules&type=packages&query=documentation.nixos.includeAllModules) which builds documentation for user-defined options

```diff
 { config, lib, ... }:
 {
+  options.foo = lib.mkOption {
+    type = lib.types.str;
+    default = "hello";
+  };
   config = {
+    environment.etc."foo".text = config.foo;
-    environment.etc."foo".text = "hello";
   };
 }
```

## When does it not work?

One important thing to note is that there are _no_ false negatives.
If a refactor has the same hash as before, they are necessarily the same system.^[As discussed above, there are pathological cases where the same hash can produce different systems when randomness is involved or something depends on the time. But that behaviour will also be unchanged by the refactor.]

There are, however, limitations to comparing input hashes — false positives, where systems will seem to change, but where no _meaningful_ change has really occurred.

The input hash cannot distinguish relevant from irrelevant changes; it can only determine that no _semantic_ change was introduced.
If you have a network of computers connected via WireGuard, and you update a cryptographic key pair everywhere in the system, that will not change the system in any way that you _care_ about.
But it will still change each system, and so the hashes will change.
This isn't really a pure refactor though; you can observe this change from outside the system if, for example, a key is compromised and you spoof one of the machines.
When trying to detect changes in a system, there is no way to tell what changes matter to you personally.

If you add a comment to a bash script only used in building the system (it is not a runtime dependency), this has _no_ effect on the resulting system.
However, one of the inputs to the system has still changed, so the input hash will still change.
This is not a fundamental limitation though, and would in fact be solved by the [content-addressed store](https://github.com/NixOS/rfcs/blob/master/rfcs/0062-content-addressed-paths.md).

In practice I have found that a surprising number of changes leave the hash unchanged.
I have extensively refactored a codebase, and not had to worry that I accidentally altered something critical, because in a sense I didn't alter anything at all!

## Applications

You can use this on your personal systems to make sure your refactors are pure, but where it really shines is in the CI for critical systems.
This strategy was developed in collaboration with [Bevuta](https://www.bevuta.com/), a company working on emergency services.

The way we've set it up, on every merge request, the new hashes are checked against all the deployed machines.
If there are no changes, we can confidently push to production.
If there are, we do a sanity check.
Maybe we thought we had only changed testing machines, but implemented that check wrong, and find out that this would also change production machines.
Better to find out before production machines start going down.

One can also compare input hashes for any Nix derivations, not just NixOS systems.
This is exactly what makes the Nix store a build cache.
It is also used to test that changes to the Nix language are backward compatible.
If all the packages in Nixpkgs evaluate to the same derivations, that's a good indication that the changes at most introduce new features, and don't break existing ones.

## Next steps

How can we push this even further?
As I mentioned before, the content-addressed store is already in progress, and will add a few more cases where we could be sure the refactor hasn't changed anything.

While assurance that a refactor didn't change anything is useful on its own, information about how a system _did_ change between two versions would be even better — some kind of diff between the deployed configuration and the one you just built.
There are projects that have attempted this, with some degree of success.
See [nix-diff](https://github.com/Gabriella439/nix-diff) and [nvd](https://gitlab.com/khumba/nvd).
What would really be useful in this case is a diff of the _configuration_, not of the output.
Unfortunately, some option values are not comparable (functions), and others will produce an error when you try to evaluate them (ones that haven't been set).
Some refactoring of the NixOS module system might be necessary for this to reach its full potential.
Check out [this issue](https://github.com/NixOS/nixpkgs/issues/190033) to get involved in that conversation.

Lastly, there are things that make NixOS configurations uniquely suited to this kind of refactor analysis, but I wonder also about other applications.
For instance, the way that [Unison](https://www.unison-lang.org/) defines functions by a hash on their implementation suggests promising possibilities.
