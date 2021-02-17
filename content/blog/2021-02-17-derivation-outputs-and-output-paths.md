---
title: Derivation outputs in a content-addressed world
author: Théophane Hufschmitt
tags: [nix]
---

This is another blog post on the upcoming content-addressed derivations for Nix.
We've already [explained the feature and some of its advantages][cas-post-1], as well as [the reasons why it isn't easy to implement][cas-post-2].
Now we're going to talk about about a concrete user-facing change that this feature will entail: the distinction between “derivation outputs” and “output paths”.

Note that the changes presented here might not yet be implemented or merged upstream.

# Store paths

Store paths are pervasive in Nix.
Run `nix build`? This will return a store path.
Want to move derivation outputs around? Just `nix copy` their store path.
Even if you can run `nix copy nixpkgs#hello`, this is strictly equivalent to `nix build nixpkgs#hello --out-link hello && nix copy $(realpath ./hello)`.
Need to know whether a derivation has been built locally or in a binary cache? Just check whether its output path exists.

This is really nice in a world where the output of the derivations are input-addressed, because there's a direct mapping between a derivation and its output paths − the `.drv` file actually explicitly contains them − which means that given a derivation Nix can directly know what its output paths are.

However this falls short with the addition of content-addressed derivations: if `hello` is content-addressed then I can't introspect the derivation to know its output path anymore (see [the previous post on that topic][cas-post-1]).
Locally, Nix has a database that stores (amongst other things) which derivation produced which outputs, meaning that it knows that `hello` has already been built and that its output path is `/nix/store/1234-hello`.
But if I just copy this output path to another machine, and try to rebuild `hello` there, Nix won't be able to know that its output path is already there (because it doesn't have that mapping), so it will have rebuild the derivation, only to realise that it yields the output path `/nix/store/1234-hello` that's already there and discard the result.

This is very frustrating, as it means that the following won't work:

```console
$ nix copy --to ssh://somewhereelse nixpkgs.hello
# Try to build with `--max-jobs 0` to make it fail if it needs to rebuild anything
$ ssh somewhereelse nix build nixpkgs.hello --max-jobs 0
error: --- Error ----- nix
252 derivations need to be built, but neither local builds ('--max-jobs') nor remote builds ('--builders') are enabled
```

We could ask for Nix to copy the mapping between the `hello` derivation and its output paths as well, but many derivations might have produced this path, so if we just say `nix copy --to ssh://somewhereelse /nix/store/1234-hello`, does that mean that we want to copy the `1234-hello` store path? Or `1234-hello` the output of the `hello` derivation? Or `1234-hello` the output of the `hello2` derivation? Nix has no way to know that.

This means that we need another way to identify these outputs other than just their store paths.

# Introducing derivation outputs

The one thing that we know though, and that uniquely identifies the derivation, is the hash of the derivation itself.
The derivation for `hello` will be stored as `/nix/store/xxx-hello.drv` (where `xxx` is a hash), and that `xxx` definitely identifies the derivation (and is known before the build).
As this derivation [might have several outputs][multiple-outputs], we need to append the name of the considered output to get a truly unique identifier, giving us `/nix/store/xxx-hello.drv!out`.

So given this “derivation output id”, Nix will be able to both retrieve the corresponding output path (if it has been built), and know the mapping `(derivation, outputName) -> outputPath`.

With this in hand, we now can run `nix copy --to ssh://somewhereelse /nix/store/xxx-hello.drv!out`, which will both copy the output path `/nix/store/1234-hello` and register on the remote machine that this path is the output `out` of the derivation `/nix/store/xxx-hello.drv`.
Likewise, `nix copy nixpkgs.hello` will be a shortcut for `nix copy /nix/store/xxx-hello.drv`.
And now we can do

```console
$ nix copy --to ssh://somewhereelse nixpkgs.hello
# Try to build with `--max-jobs 0` to make it fail if it needs to rebuild anything
$ ssh somewhereelse nix build nixpkgs.hello --max-jobs 0
$ ./result/bin/hello
Hello, world!
```

# In practice

What will this mean in practice?

This means that the Nix cli will now return or accept either store paths or derivation output ids depending on the context.
For example `nix build` will still create symlinks to the output paths and `nix shell` will add them to the `PATH` because that's what makes sense in the context.
But as we've seen above, `nix copy` will accept both store paths and derivation output ids, and these will have different semantics.
Copying store paths will just copy the store paths as it used to do (in the case you don't care about rebuilding them on the other side) while copying derivation outputs will also register these outputs on the remote side.

Once more, right now, this feature is still under development, so the changes presented here might not yet be implemented or merged upstream.
So don't be surprised when the feature lands in the near future!

[multiple-outputs]: https://nixos.org/manual/nix/unstable/expressions/derivations.html?highlight=outputs
[cas-post-1]: https://www.tweag.io/blog/2020-09-10-nix-cas/
[cas-post-2]: https://www.tweag.io/blog/2020-11-18-nix-cas-self-references/
