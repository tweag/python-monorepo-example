---
title: "Long-term reproducibility with Nix and Software Heritage"
shortTitle: "Nix and Software Heritage"
author: Antoine Eiche and Matthias Meschede
description: "How Nix is collaborating with Software Heritage for long-term software reproducibility."
tags: [nix]
---

Reproducible builds and deployments — this is the ambition that Nix proclaims for itself.
This ambition comes, however, with a fine print: builds are reproducible _only if the original source code still exists_.
Otherwise, Nix can't download, build and deploy it.
The community maintains binary caches like `cache.nixos.org`, but these don't preserve anything — caches are ephemeral.
After all, preserving source code is not Nix's primary mission!

[Software Heritage](https://www.softwareheritage.org/), on the other hand, aspires to preserve software forever.
Software Heritage is an independent foundation, grounded in academia, supported by public money, and backed by many of the world's largest software companies.
It can thus reliably pursue the tedious task of collecting and archiving public code and making it available through its website.

Quite naturally, this situation suggested an opportunity for collaboration:
can we combine Nix's reproducible build definitions with Software Heritage's long-term archive?
Will this collaboration bring us closer to the dream of forever replicable and successful builds?
We thought that this was an effort worth pursuing, so a [partnership started](https://www.softwareheritage.org/2020/06/18/welcome-nixpgks).

This is a story about the challenges that we encountered when trying to make this happen, what we already achieved, and the lessons that we have learned.

## Archiving all sources in Nixpkgs

Whenever a Nix user installs a package that isn't in the binary cache, Nix tries to download the original source and build it from scratch.
When these sources don't exist anymore, or don't correspond anymore to what Nix expects them to be, the experience can be frustrating —
instead of a reproducible build, the user ends up with a “file not found” HTTP error message.

With a long-term source code archive at hand, we could simply redirect the download request to it and move on with the build process.
In other words, we would like to make Nix fall back on the Software Heritage archive to download the missing source from there.

For this to work, we must ensure that the source code has been fed to the Software Heritage archive previously. The best way to do it is
simply to tell Software Heritage which source code we would like to have archived. Indeed, Software Heritage’s long term goal is to archive
all source code produced in the world, and is quite eager to get pointed to locations where to get more!

The first step of this joint effort was to compile a list of all source code URLs required by Nixpkgs, and make them available to Software Heritage.
A [Nix community project](https://github.com/nix-community/nixpkgs-swh) is in charge of generating the list of source code URLs required by a Nixpkgs build, and it is available [here](https://nix-community.github.io/nixpkgs-swh/sources-unstable.json).
This list indexes every tarball used by the Nixpkgs `master` branch, with other sources such as patches, JAR's, or git clones being currently excluded, and hence not archived.

A source list is a simple JSON file that looks like this:

```
{
  "sources": [
    {
      "type": "url",
      "urls": [
        "https://ftpmirror.gnu.org/hello/hello-2.10.tar.gz",
      ],
      "integrity": "sha256-MeBmE3qWJnbon2nRtlOC3pWn732RS4y5VvQepy4PUWs=",
    },
    ...
  ],
  "version": 1,
  "revision": "cc4e04c26672dd74e5fd0fecb78b435fb55368f7"
}

```

Here,

- `version` is the version of this file's format,
- `revision` is a Nixpkgs commit ID,
- `type` is the type of the source. Only the `url` type is currently supported,
- `integrity` is the hash of the Nix fixed output derivation.

We then implemented a Software Heritage [loader](https://docs.softwareheritage.org/devel/glossary.html#term-loader) which fetches this JSON file once per day and archives all listed tarballs in a snapshot.

You can see an example snapshot [here](https://archive.softwareheritage.org/browse/origin/directory/?origin_url=https://nix-community.github.io/nixpkgs-swh/sources-unstable.json&timestamp=2020-06-03T11:25:05Z).
This snapshot was created the 03 June 2020, points to the Nixpkgs commit [`46fcaf3c8a13f32e2c147fd88f97c4ad2d3b0f27`](https://github.com/NixOS/nixpkgs/tree/46fcaf3c8a13f32e2c147fd88f97c4ad2d3b0f27) and contains 21,100 branches, each one corresponding to the tarballs used by this Nixpkgs revision.

Every day, Software Heritage is now archiving more than 21,000 tarballs used by Nixpkgs.
But how we can plug them back into Nix?

## Falling back on Software Heritage

Two issues need to be addressed to make this happen:

First, we need to find the correct source, the one that Nix tried to download unsuccessfully, in the Software Heritage archive.
We cannot simply use the original source URL to query the Software Heritage archive because the URL alone doesn't identify uniquely the content that is behind it.
What if the code that a URL points to has changed over time?
To check this, Nix associates a hash with each downloaded artifact.
Such a build step is called a [fixed output derivation](https://nixos.org/nix/manual/#fixed-output-drvs) because Nix verifies that the output hash remains immitigably unchanged.
It thus ensures that the content that it downloads is always the same, and emits an error otherwise.
What we therefore really want is querying Software Heritage with the _contents_, that is, the hash of the source code artifact itself.

Let's do it then!
Does this mean that the problem is solved?
Unfortunately not, and the reason for this are the design decisions behind Nix, Software Heritage and other package repositories, that are not fully compatible.
And tarballs play a central role in this.

# Content Hash vs Tarball Hash

When Software Heritage archives a tarball, it first unpacks it and then stores all files and directories.
This makes it possible to _deduplicate_ files: when two tarballs contain the same file it is only stored once.
This is a nice solution, but unfortunately, this new tarball may differ from the original one — for example, if file permissions or timestamps were not preserved in this reassembly procedure.
Since Nix computes the checksum of the tarball, it will detect that it differs from the hash of the original, and has no means to ensure that the tarball downloaded from the Software Heritage archive corresponds to the one it expects.

Nix, being really stubborn with respect to reproducibility, computes the checksum of the tarball, and detects that it differs from the hash of the original.
Nix simply cannot ensure that the tarball downloaded from the Software Heritage archive corresponds to the one it expects, and rightly so because this could easily lead to a different build.

Nix already deals with some situations where the hash of a source tarball can change without affecting build reproducibility.
For example, `fetchFromGitHub` unpacks the tarball _before_ computing the hash of the unpacked contents.
This is because GitHub produces release tarballs on the fly, and a change in the compression algorithm can invalidate the hash expected by Nix.
It happens that when Nix uses the `fetchFromGithub` strategy, it manages to download tarballs from Software Heritage.
Currently, this is the case for about 6,000 sources in Nixpkgs.

For all other sources, we would have to modify the Nixpkgs fetchers to compute checksums on the unpacked tarball contents.
This seems reasonable, since we want to make sure that the source code is the same, and we don't care much about the format in it is transferred (which can also be important for security reasons).
However, using the hash of the tarball directly is often more convenient, since it is exposed by many repositories such as [Pypi](https://pypi.org) and [Hackage](https://hackage.haskell.org).
When updating a package in Nixpkgs, the maintainer can just pick the checksum provided by the package repository, instead of recalculating it locally.

Ideally, Nix fetchers and these package repositories would provide checksums of the unpacked _contents_ of their packages as much as possible.
This would give us the freedom to identify content independently of the way it is packed, transferred and stored.
We haven't yet decided how to move on, but rest assured that we will continue to work on it.

# Wrap up

Thanks to Software Heritage, a significant part of the sources used by Nixpkgs are now archived forever.
Moreover, about 6,000 out of the 21,000 tarballs used by Nixpkgs can already be used by the future Nix fallback mechanism.
We now want to increase the number of archived source code tarballs, add support for Git sources and start to implement the fallback mechanism in Nix.
At the same time, we aim to increase the number of fixed output derivations whose hash is computed on the unpacked tarball.

We want build reproducibility to become the standard in the software world.
For this reason, the Software Heritage loader archiving Nixpkgs source code tarballs has been designed to be easily reused by other actors.
For example, the [Guix](https://guix.gnu.org/) project already started publishing and archiving their sources using the same component!
Finally, we want to thank [NLnet](https://nlnet.nl/) for the funding that makes this work possible.
