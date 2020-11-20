---
title: "Self-references in a content-addressed Nix"
author: Théophane Hufschmitt
description: "Why wasn't Nix built with a content-addressed store?"
tags: [nix]
---

In a [previous post][first-nix-post] I explained why we were eagerly trying to change the Nix store model to allow for content-addressed derivations.
I also handwaved that this was a real challenge, but without giving any hint at **why** this could be tricky.
So let's dive a bit into the gory details and understand some of the conceptual pain points with content-addressability in Nix, which forced us to some trade-offs in how we handle content-addressed paths.

[first-nix-post]: /blog/2020-09-10-nix-cas/

# What are self-references?

<a name="self"></a>

> This is a self-reference
>
> -- <cite>Théophane Hufschmitt, [This very article](#self)</cite>

A very trivial Nix derivation might look like this:

```nix
with import <nixpkgs> {};
writeScript "hello" ''
#!${bash}/bin/bash

${hello}/bin/hello
''
```

The result of this derivation will be an executable file containing a script that will run the `hello` program.
It will depend on the `bash` and `hello` derivations as we refer to them in the file.

We can build this derivation and execute it:

```console
$ nix-build hello.nix
$ ./result
Hello, world!
```

So far, so good.
Let's now change our derivation to change the prompt of `hello` to something more personalized:

```nix
with import <nixpkgs> {};
writeScript "hello-its-me" ''
#!${bash}/bin/bash

echo "Hello, world! This is ${placeholder "out"}"
''
```

where `${placeholder "out"}` is a magic value that will be replaced by the output path of the derivation during the build.

We can build this and run the result just fine

```console
$ nix-build hello-its-me.nix
$ ./result
Hello, world! This is /nix/store/c0qw0gbp7rfyzm7x7ih279pmnzazg86p-hello-its-me
```

And we can check that the file is indeed who it claims to be:

```console
$ /nix/store/c0qw0gbp7rfyzm7x7ih279pmnzazg86p-hello-its-me
Hello, world! This is /nix/store/c0qw0gbp7rfyzm7x7ih279pmnzazg86p-hello-its-me
```

While the `hello` derivation depends on `bash` and `hello`, `hello-its-me` depends on `bash` and… itself.
This is something rather common in Nix.
For example, it's rather natural for a C program to have `/nix/store/xxx-foo/bin/foo` depend of `/nix/store/xxx-foo/lib/libfoo.so`.

# Self references and content-addressed paths

How do we build a content-addressed derivation `foo` in Nix? The recipe is rather simple:

1. Build the derivation in a temporary directory `/some/where/`
2. Compute the hash `xxx` of that `/some/where/` directory
3. Move the directory under `/nix/store/xxx-foo/`

You might see where things will go wrong with self-references: the reference will point to `/some/where` rather than `/nix/store/xxx-foo`, and so will be wrong (in addition to leak a path to what should just be a temporary directory).

To work around that, we would need to compute this `xxx` hash before the build, but that's quite impossible as the hash depends on the content of the directory, including the value of the self-references.

However, we can hack our way around it in most cases by allowing ourselves a bit of heuristic.
The only assumption that we need to make is that all the
self-references will appear textually (_i.e._ running `strings` on a
file that contains self-references will print all the self-references out).

Under that assumption, we can:

1. Build the derivation in our `/some/where` directory
2. Replace all the occurrences of a self-reference by a magic value
3. Compute the hash of the resulting path to determine the final path
4. Replace all the occurrences of the magic value by the final path
5. Move the resulting store path to its final path

Now you might think that this is a crazy hack − there's so many ways it could break.
And in theory you'll be right.
But, surprisingly, this works remarkably well in practice.
You might also notice that _pedantically speaking_ this scheme isn't exactly content-addressing because of the “modulo the final hash” part.
But this is close-enough to keep all the desirable properties of proper content addressing, while also enabling self-references, which wouldn't be possible otherwise.
For example, the Fugue cloud deployment system used [a generalisation
of this technique][fugue-circular-hash] which not only deals with
self-references, but with reference cycles of arbitrary length.

[fugue-circular-hash]: https://www.fugue.co/blog/2016-05-18-cryptographic-hashes-and-dependency-cycles.html

However, there's a key thing that's required for this to work: patching strings in binaries is generally benign, but the final string must have the same length as the original one.
But we can do that: we don't know what the final `xxx` hash will be, but we know its length (because it's a fixed-length hash), so we can just choose a temporary directory that has the right length (like a temporary store path with the same name), and we're all set!

The annoying thing is that there's no guarantee that there are no self-references hidden in such a way that a textual replacement won't catch it (for example inside a compressed zip file).
This is the main reason why content-addressability will not be the default in Nix, at first at least.

# Non-deterministic builds − the diamond problem strikes back

No matter how hard Nix tries to isolate the build environment, some actions will remain inherently non-deterministic − anything that can yield a different output depending on the order in which concurrent tasks will be executed for example.
This is an annoyance as it might prevent _early cutoff_ (see [our previous article on the subject][first-nix-post] in case you missed it).

But more than limiting the efficiency of the cache, this could also
hurt the correctness of Nix if we're not careful enough.

For example, consider the following dependency graph:

![Dependency graph for foo](./foo-dependency-graph.svg)

Alice wants to get `foo` installed.
She already built `lib0` and `lib1` locally.
Let's call them `lib0_a` and `lib1_a`.
The binary cache contains builds of `lib0` and `lib2`.
Let's call them `lib0_b` and `lib2_b`.
Because the build of `lib0` is not deterministic, `lib0_a` and `lib0_b` are different -- and so have a different hash.
In a content-addressed word, that means they will be stored in different paths.

A simple cache implementation would want to fetch `lib2_b` from the cache and use it to build `foo`.
This would also pull `lib0_b`, because it's a dependency of `lib2_b`.
But that would mean that `foo` would depend on both `lib0_a` and `lib0_b`.

![Buggy runtime dependency graph for foo](./foo-runtime-dependency-graph.svg)

In the happy case this would just be a waste of space − the dependency is duplicated, so we use twice as much memory to store it.
But in many cases this would simply blow-up at some point -- for example if `lib0` is a shared library, the C linker will fail because of the duplicated symbols.
Besides that, this breaks down the purity of the build as we get a different behavior depending on what's already in the store at the start of the build.

## Getting out of this

[Nix's foundational paper][eelco-phd] shows a way out of this by rewriting hashes in substituted paths.
This is however quite complex to implement for a first version, so the current implementation settles down on a simpler (though not optimal) behavior where we only allow one build for each derivation.
In the example above, `lib0` has already been instantiated (as `lib0_a`), so we don't allow pulling in `lib0_b` (nor `lib1_b`) and we rebuild both `lib1` and `foo`.

While not optimal − we'll end-up rebuilding `foo` even if it's already in the binary cache − this solution has the advantage of preserving correctness while staying conceptually and technically simple.

[eelco-phd]: https://edolstra.github.io/pubs/phd-thesis.pdf

# What now?

Part of this [has already been implemented](https://github.com/NixOS/nix/pulls?q=is%3Apr+label%3Aca-derivations+is%3Aclosed) but there's still [quite a long way forward](https://github.com/NixOS/nix/issues?q=is%3Aopen+is%3Aissue+label%3Aca-derivations).

I hope for it to be usable (though maybe still experimental) for Nix 3.0.

And in the meantime stay tuned with [our regular updates on discourse][discourse-dev-update].
Or wait for the next blog post that will explain another change that will be necessary -- one that is less fundamental, but more user-facing.

[discourse-dev-update]: https://discourse.nixos.org/t/tweag-nix-dev-update-4/9862
