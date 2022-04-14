---
redirect_from: [/posts/2019-11-21-untrusted-ci.html]
title: "Untrusted CI:  Using Nix to get automatic trusted caching of untrusted builds"
shortTitle: "Untrusted CI using Nix"
author: "Florian Klink"
description: "Learn how Nix's new post-build hooks feature can automatically sign and upload artifacts to a binary cache in a trusted way."
tags: [nix, devops]
---

[rules_haskell]: https://github.com/tweag/rules_haskell/#readme
[rules_nixpkgs]: https://github.com/tweag/rules_nixpkgs/#readme
[post-build-hooks]: https://nixos.org/nix/manual/#chap-post-build-hook
[multi-user-nix]: https://nixos.org/nix/manual/#ssec-multi-user

[^1]: Please refer to the "Implementation Caveats" section in the Nix manual-depending on the circumstances, it might be desirable to handle uploads in an asynchronous fashion without blocking the build loop.
[^2]: [https://github.com/NixOS/nix/issues/2636](https://github.com/NixOS/nix/issues/2636)

Nix's infrastructure offers a cache for the build products of
derivations contained in Nixpkgs, which greatly accelerates the CI
process. Ideally, the CI cache should benefit developers too: no
need to build packages yourself if CI already did. However, sharing
the CI cache with developers raises security issues.

In this post, we show an approach based on _multi-user Nix_ and
_post-build hooks_ that solves these security issues and more.

## The standard cache setup

Let's walk through the standard procedure for setting up such a Nix cache.
First, create a signing key pair:

```bash
nix-store --generate-binary-cache-key example-nix-cache-1 ./nix-cache-key.sec ./nix-cache-key.pub
```

The substituter url and the key generated in `nix-cache-key.pub` should be
added to `/etc/nix/nix.conf` on developer machines and CI nodes, which should
look like this:

```
substituters = https://cache.nixos.org/ https://s3.amazonaws.com/example-nix-cache/
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= example-nix-cache-1:1/cKDz3QCCOmwcztD2eV6Coggp6rqc9DGjWv7C0G+rM=
```

On NixOS, this can be accomplished by setting

```nix
nix.binaryCachePublicKeys = [ "example-nix-cache-1:1/cKDz3QCCOmwcztD2eV6Coggp6rqc9DGjWv7C0G+rM=" ];
nix.binaryCaches = [ "https://s3.amazonaws.com/example-nix-cache/" ];
```

We used Amazon S3 in our example above but Nix can also use other S3-based backends or simple
HTTP/WebDAV-like (supporting GET and PUT) endpoints—there's even a
[PR](https://github.com/NixOS/nix/pull/3021) for [Google Cloud Storage (GCS)](https://cloud.google.com/storage/) support. You can
also use [Cachix](https://cachix.org/).

The following assumes a public S3 Bucket called `example-nix-cache`.
For other binary cache backends, instructions will slightly differ.

## Building and uploading

Now Nix is configured to substitute from our new cache too, but how do we
upload to that cache? Assuming your project contains a `default.nix` with an
attribute `deps`, a naive solution to upload build artifacts would be something
like the following:

```bash
for o in $(nix-build --no-out-link -A deps); do
  nix copy --to s3://example-nix-cache?secret-key=/run/keys/nix-signing-key $o
done
```

The above `nix-build` command will build all dependencies and
execute `nix copy` for each of them, which will take care of signing and
uploading the whole closure. If these paths already exist on the binary cache,
uploading will be skipped.

But there is a problem. You can't allow untrusted contributors to upload
packages to the cache: they could easily replace the uploaded build
artifact by another, possibly with a backdoor. This artifact would
masquerade as the build product of a legitimate Nix expression, it would
be signed, and used by all the developers' machines, or even
production systems. This would be a serious security vulnerability.

On the other hand, preventing untrusted contributors to upload—which creates a build artifact—means that many caching opportunities will be lost. That
leads to increased load on CI that can seriously impact the
cache hit ratio and roundtrip time when iterating over a work in
progress.

This vulnerability can happen in one of two ways:

- The untrusted contributor can populate the Nix store with the wrong build
  artifact, after which, `nix copy` will happily sign the faulty
  artifacts and upload them.
- The untrusted contributor can sign an unrelated build artifact themselves,
  and upload the faulty artifact regardless of what is actually in the
  Nix store.

We need to harden both of these vulnerabilities so we can restore the ability
to cache build artifacts from untrusted contributors. Let's begin.

## Multi-user Nix: trusted building

[Multi-User Nix][multi-user-nix] decouples the steps of assembling the build
recipe (`nix-instantiate`), doing the actual build and writing to the store,
and executes these as different users.

This means that untrusted contributors can upload a "build recipe" to a privileged Nix
daemon which takes care of running the build as an unprivileged user in a
sandboxed context, and of persisting the build output to the local Nix store
afterward.

This "build recipe" effectively prevents untrusted contributors from manipulating the local Nix store—assuming there are no user privilege escalation exploits or hash collisions. They
simply can't manipulate it directly, and the only way to get something in the
store is by uploading a build recipe, with the output hash being done by the
daemon.

## Post-build hooks: trusted uploading

Multi-user Nix prevents the manipulation of the local Nix store, and isolates
different parts of a `nix-build` between different users. However, we still
need to sign and upload the build artifacts to the binary cache so other
machines can make use of it.

It is clear that we should not let an untrusted contributor sign packages, because
this would provide a way to sign and copy bad outputs into the remote Nix
store. Luckily, since Nix 2.3, there's a way around this issue. With
[post-build hooks][post-build-hooks], the Nix daemon can be configured to
execute a script after building a derivation.

In order to do so, add to your `/etc/nix/nix.conf`:

```
post-build-hook = /etc/nix/upload-to-cache.sh
```

where a trivial implementation of `upload-to-cache.sh` could be[^1]:

```bash
set -eu
set -f # disable globbing
export IFS=' '

echo "Signing and uploading paths" $OUT_PATHS
exec nix copy --to 's3://example-nix-cache?secret-key=/run/keys/nix-signing-key' $OUT_PATHS
```

In a multi-user Nix setting, this runs in the context of the daemon
(that is, as a
root user), so we can properly shield the signing keys from the untrusted contributor,
by making sure that the key file if only readable by the root user. In a cloud
environment, special care needs to be taken in how the instance retrieves
secrets on startup—a regular user shouldn't be able to extract the secret by
querying the metadata server for its startup script.

## More benefits

We can now safely upload build artifacts from untrusted contributors to the
cache, greatly improving caching. But we gain more along the way.

With the standard setup, the uploading process is a step in your CI
pipeline with an explicit list of all the build artifacts you want
to upload. This approach has three unfortunate consequences:

- If one of the packages in the `deps` list fails to build,
  intermediate build artifacts won't be uploaded: `nix copy` is never
  issued on the store path introducing that dependency, as it was
  never built.
- Even if all the `deps` succeed, how do you know you have cached
  everything? Build dependencies might not all be readily available at
  a central location that can be called via `nix-build`. Often,
  multiple scripts are invoking Nix by themselves, via `nix-shell`,
  via Bazel rules ([rules_haskell][rules_haskell],
  [rules_nixpkgs][rules_nixpkgs], or calling `nix-build` themselves
  during build) to provide dependencies. Manually tracing these
  dependencies in a `.nix` file (and keeping them in sync) is an arduous task,
  it gets even harder if some of your packages rely on
  [IFD](https://nixos.wiki/wiki/Import_From_Derivation).
- The logic for uploads has to be copied to each of your pipeline
  definition.

Nix's post-build hook moves the caching logic out of the
pipeline definition and into the builder environment—that means it's
shareable across all of your pipelines. And, we upload _every single build
artifact_: it doesn't matter if later builds fail, it doesn't matter
if we don't know what these artifacts are.

## Conclusion

We learned how Nix's new post-build hooks feature can automatically sign and upload artifacts to a binary cache in a trusted way.

How? This post-build-hook setup lets untrusted contributors' artifacts be
uploaded on CI: contributors don't have direct access to the local Nix
store anymore and don't have access to the signing key. So there's no
way to produce a modified signed artifact under the original store
path or to upload it to the cache. In some cloud environments, it might
still be possible to alter files in the cache. But, since the signatures
will be broken, substitutions on other machines using that cache will
fail, and they will fall back to building locally.

The post-build-hook setup has other benefits valuable
enough that you should consider using it even if you don't have any untrusted
contributors. For example, CI pipeline setup is much easier since your developers won't need
to remember the invocations required to upload cache, or figure out
what list of artifacts they want to cache: it's all configured in the
builder already and done automatically. The caching is even improved slightly with successful partial builds since intermediate builds are
cached.

Currently, this setup is most suitable when you provide self-hosted
builders because:

- Multi-User Nix requires multiple users, ruling out some of the hosted CI
  solutions which don't allow to provide self-hosted runners. However, by just using
  post-build hooks in a single-user Nix setting, you still have
  simpler and better caching.

- Running Nix inside Docker currently requires a privileged container (because
  we want sandboxing to be enabled to ensure some of the isolation properties)
  [^2], making it unsuitable for some container platforms. On top of that,
  the "official" `nixos/nix` docker image doesn't provide a multi-user Nix
  installation, it's a single-user installation based on Alpine (there's
  `lnl7/nix` though).

For more on this subject, you can also watch [my NixCon 2019 talk](https://www.youtube.com/watch?v=NB8YHWc7dLk).
