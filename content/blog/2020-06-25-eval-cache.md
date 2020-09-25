---
title: "Nix Flakes, Part 2: Evaluation caching"
shortTitle: "Nix Flakes (2): Evaluation caching"
author: Eelco Dolstra
tags: [nix]
description: "How Nix flakes enable caching of evaluation results of Nix expressions."
---

Nix evaluation is often quite slow. In this blog post, we'll have a
look at a nice advantage of the hermetic evaluation model enforced by
flakes: the ability to cache evaluation results reliably. For a short
introduction to flakes, see our [previous blog post](https://www.tweag.io/blog/2020-05-25-flakes/).

## Why Nix evaluation is slow

Nix uses a simple, interpreted, purely functional language to describe
package dependency graphs and NixOS system configurations. So to get
any information about those things, Nix first needs to _evaluate_ a
substantial Nix program. This involves parsing potentially thousands
of .nix files and running a Turing-complete language.

For example, the command `nix-env -qa` shows you which packages are
available in Nixpkgs. But this is quite slow and takes a lot of memory:

```shell-session
$ command time nix-env -qa | wc -l
5.09user 0.49system 0:05.59elapsed 99%CPU (0avgtext+0avgdata 1522792maxresident)k
28012
```

Evaluating individual packages or configurations can also be slow. For
example, using `nix-shell` to enter a development environment for
[Hydra](https://github.com/NixOS/hydra), we have to wait a bit, even
if all dependencies are present in the Nix store:

```shell-session
$ command time nix-shell --command 'exit 0'
1.34user 0.18system 0:01.69elapsed 89%CPU (0avgtext+0avgdata 434808maxresident)k
```

That might be okay for occasional use but a wait of one or more
seconds may well be unacceptably slow in scripts.

Note that the evaluation overhead is completely independent from the
time it takes to actually build or download a package or
configuration. If something is already present in the Nix store, Nix
won't build or download it again. But it still needs to re-evaluate
the Nix files to determine _which_ Nix store paths are needed.

## Caching evaluation results

So can't we speed things up by _caching_ evaluation results? After
all, the Nix language is purely functional, so it seems that
re-evaluation should produce the same result, every time. Naively,
maybe we can keep a cache that records that attribute A of file X
evaluates to derivation D (or whatever metadata we want to cache).
Unfortunately, it's not that simple; cache invalidation is, after all,
one of the [only two hard problems in computer
science](https://martinfowler.com/bliki/TwoHardThings.html).

The reason this didn't work is that in the past Nix evaluation was not
_hermetic_. For example, a `.nix` file can import other Nix files
through relative or absolute paths (such as
`~/.config/nixpkgs/config.nix` for Nixpkgs) or by looking them up in
the Nix search path (`$NIX_PATH`). So unless we perfectly keep track
of _all_ the files used during evaluation, a cached result might be
inconsistent with the current input.

(As an aside: for a while, Nix has had an experimental replacement for
`nix-env -qa` called `nix search`, which used an ad hoc cache for
package metadata. It had exactly this cache invalidation problem: it
wasn't smart enough to figure out whether its cache was up to date
with whatever revision of Nixpkgs you were using. So it had a manual
flag `--update-cache` to allow the user to force cache invalidation.)

## Flakes to the rescue

Flakes solve this problem by ensuring fully hermetic evaluation. When
you evaluate an output attribute of a particular flake (e.g. the
attribute `defaultPackage.x86_64-linux` of the `dwarffs` flake), Nix
disallows access to any files outside that flake or its
dependencies. It also disallows impure or platform-dependent features
such as access to environment variables or the current system type.

This allows the `nix` command to aggressively cache evaluation results
without fear of cache invalidation problems. Let's see this in action
by running Firefox from the `nixpkgs` flake. If we do this with an
empty evaluation cache, Nix needs to evaluate the entire dependency
graph of Firefox, which takes a quarter of a second:

```
$ command time nix shell nixpkgs#firefox -c firefox --version
Mozilla Firefox 75.0
0.26user 0.05system 0:00.39elapsed 82%CPU (0avgtext+0avgdata 115224maxresident)k
```

But if we do it again, it's almost instantaneous (and takes less
memory):

```
$ command time nix shell nixpkgs#firefox -c firefox --version
Mozilla Firefox 75.0
0.01user 0.01system 0:00.03elapsed 93%CPU (0avgtext+0avgdata 25840maxresident)k
```

The cache is implemented using a simple SQLite database that stores the
values of flake output attributes. After the first command above, the
cache looks like this:

```shell-session
$ sqlite3 ~/.cache/nix/eval-cache-v1/302043eedfbce13ecd8169612849f6ce789c26365c9aa0e6cfd3a772d746e3ba.sqlite .dump
PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;
CREATE TABLE Attributes (
    parent      integer not null,
    name        text,
    type        integer not null,
    value       text,
    primary key (parent, name)
);
INSERT INTO Attributes VALUES(0,'',0,NULL);
INSERT INTO Attributes VALUES(1,'packages',3,NULL);
INSERT INTO Attributes VALUES(1,'legacyPackages',0,NULL);
INSERT INTO Attributes VALUES(3,'x86_64-linux',0,NULL);
INSERT INTO Attributes VALUES(4,'firefox',0,NULL);
INSERT INTO Attributes VALUES(5,'type',2,'derivation');
INSERT INTO Attributes VALUES(5,'drvPath',2,'/nix/store/7mz8pkgpl24wyab8nny0zclvca7ki2m8-firefox-75.0.drv');
INSERT INTO Attributes VALUES(5,'outPath',2,'/nix/store/5x1i2gp8k95f2mihd6aj61b5lydpz5dy-firefox-75.0');
INSERT INTO Attributes VALUES(5,'outputName',2,'out');
COMMIT;
```

In other words, the cache stores all the attributes that `nix shell`
had to evaluate, in particular
`legacyPackages.x86_64-linux.firefox.{type,drvPath,outPath,outputName}`. It
also stores negative lookups, that is, attributes that don't exist
(such as `packages`).

The name of the SQLite database, `302043eedf….sqlite` in this example,
is derived from the contents of the top-level flake. Since the flake's
lock file contains content hashes of all dependencies, this is enough
to efficiently and completely capture all files that might influence
the evaluation result. (In the future, we'll optimise this a bit more:
for example, if the flake is a Git repository, we can simply use the
Git revision as the cache name.)

The `nix search` command has been updated to use the new evaluation
cache instead of its previous ad hoc cache. For example, searching for
Blender is slow the first time:

```shell-session
$ command time nix search nixpkgs blender
* legacyPackages.x86_64-linux.blender (2.82a)
  3D Creation/Animation/Publishing System
5.55user 0.63system 0:06.17elapsed 100%CPU (0avgtext+0avgdata 1491912maxresident)k
```

but the second time it is pretty fast and uses much less memory:

```shell-session
$ command time nix search nixpkgs blender
* legacyPackages.x86_64-linux.blender (2.82a)
  3D Creation/Animation/Publishing System
0.41user 0.00system 0:00.42elapsed 99%CPU (0avgtext+0avgdata 21100maxresident)k
```

The evaluation cache at this point is about 10.9 MiB in size. The
overhead for creating the cache is fairly modest: with the flag
`--no-eval-cache`, `nix search nixpkgs blender` takes 4.9 seconds.

## Caching and store derivations

There is only one way in which cached results can become "stale", in a
way. Nix evaluation produces store derivations such as
`/nix/store/7mz8pkgpl24wyab8nny0zclvca7ki2m8-firefox-75.0.drv` as a
side effect. (`.drv` files are essentially a serialization of the
dependency graph of a package.) These store derivations may be
garbage-collected. In that case, the evaluation cache points to a path
that no longer exists. Thus, Nix checks whether the `.drv` file still
exist, and if not, falls back to evaluating normally.

## Future improvements

Currently, the evaluation cache is only created and used
locally. However, Nix could automatically _download_ precomputed
caches, similar to how it has a binary cache for the contents of store
paths. That is, if we need a cache like `302043eedf….sqlite`, we could
first check if it's available on `cache.nixos.org` and if so fetch it
from there. In this way, when we run a command such as `nix shell nixpkgs#firefox`, we could even avoid the need to fetch the actual
source of the flake!

Another future improvement is to populate and use the cache in the
evaluator itself. Currently the cache is populated and cached in the
user interface (that is, the `nix` command). The command `nix shell nixpkgs#firefox` will create a cache entry for `firefox`, but not for
the dependencies of `firefox`; thus a subsequent `nix shell nixpkgs#thunderbird` won't see a speed improvement even though it
shares most of its dependencies. So it would be nice if the evaluator
had knowledge of the evaluation cache. For example, the evaluation of
thunks that represent attributes like
`nixpkgs.legacyPackages.x86_64-linux.<package name>` could check and
update the cache.

## Nix Flakes Series

- [Part 1: An introduction and tutorial](../2020-05-25-flakes)
- Part 2: Evaluation caching
- [Part 3: Managing NixOS systems](../2020-07-31-nixos-flakes)
