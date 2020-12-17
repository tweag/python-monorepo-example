---
title: "Trustix: Distributed trust and reproducibility tracking for binary caches"
shortTitle: Announcing Trustix
author: Adam Hoese
tags: [nix]
description: Trustix aims at enhancing trust in software binaries and improve reproducibility tracking using distributed binary caches.
---

Downloading binaries from well-known providers is the easiest way to install new software.
After all, building software from source is a chore -- it requires both time and technical expertise.
But how do we know that we aren't installing something malicious from these providers?

Typically, we trust these binaries because we trust the provider.
We believe that they were built from trusted sources, in a trusted computational environment, and with trusted build instructions.
But even if the provider does everything transparently and in good faith, the binaries could still be _anything_ if the provider's system is compromised.
In other words, the build process requires _trust_ even if all build inputs (sources, dependencies, build scripts, etc...) are known.

Overcoming this problem is hard -- after all, how can we verify the output of arbitrary build inputs?
Excitingly, the last years have brought about ecosystems such as Nix, where all build inputs are known and where _significant amounts of builds are reproducible_.
This means that the correspondence between inputs and outputs can be verified by building the same binary multiple times!
The [r13y](https://r13y.com/) project, for example, tracks non-reproducible builds by building them twice on the same machine, showing that this is indeed practical.

But we can go further, and that's the subject of this blog post, which introduces [Trustix][trustix], a new tool we are working on.
Trustix compares build outputs for given build inputs across _independent_ providers and machines, effectively decentralizing trust.
This establishes what I like to call [build transparency][build-transparency] because it verifies what black box build machines are doing.
Behind the scenes Trustix builds a [Merkle tree][merkle-tree]-based [append-only][append-only] log that maps build inputs to build outputs, which I'll come back to in a later post.
This log can be used to establish [consensus][consensus] whether certain build inputs always produce the same output -- and can therefore be trusted.
Conversely, it can also be used to uncover non-reproducible builds, corrupted or not, on a large scale.

The initial implementation of Trustix, and its description in this post are based on the Nix package manager.
Nix focuses on isolated builds, provides access to the hashes of all build inputs as well as a high quantity of bit-reproducible packages, making it the ideal initial testing ecosystem.
However, Trustix was designed to be system-independent, and is not strongly tied to Nix.

The developmentent of [Trustix][trustix] is funded by [NLnet foundation][nlnet] and the European Commission's [Next Generation Internet][ngi] programme through the [NGI Zero PET][pet] (privacy and trust enhancing technologies) fund.
The tool is still in development, but I'm very excited to announce it already!

## How Nix verifies binary cache results

Most Linux package managers use a very simple signature scheme to secure binary distribution to users.
Some use GPG keys, some use OpenSSL certificates, and others use some other kind of key, but the idea is essentially the same for all of them.
The general approach is that binaries are signed with a private key, and clients can use an associated public key to check that a binary was really signed by the trusted entity.

Nix for example uses an ed25519-based key signature scheme and comes with a default hard-coded public key that corresponds to the default cache.
This key can be overridden or complemented by others, allowing the use of additional caches.
The list of signing keys can be found in `/etc/nix/nix.conf`.
The default base64-encoded ed25519 public key with a name as additional metadata looks like this:

```
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=
```

Now, in Nix, software is addressed by the hash of all of its build inputs (sources, dependencies and build instructions).
This hash, or the output path is used to query a cache (like <https://cache.nixos.org>) for a binary.

Here is an example:
The hash of the `hello` derivation can be obtained from a shell with `nix-instantiate`:

```
$ nix-instantiate '<nixpkgs>' --eval -A hello.outPath
"/nix/store/w9yy7v61ipb5rx6i35zq1mvc2iqfmps1-hello-2.10"
```

Here, behind the scenes, we have evaluated and hashed all build inputs that the `hello` derivation needs (`.outPath` is just a helper).
This hash can then be used to query the default Nix binary cache:

```
$ curl https://cache.nixos.org/w9yy7v61ipb5rx6i35zq1mvc2iqfmps1.narinfo
StorePath: /nix/store/w9yy7v61ipb5rx6i35zq1mvc2iqfmps1-hello-2.10
URL: nar/15zk4zszw9lgkdkkwy7w11m5vag11n5dhv2i6hj308qpxczvdddx.nar.xz
Compression: xz
FileHash: sha256:15zk4zszw9lgkdkkwy7w11m5vag11n5dhv2i6hj308qpxczvdddx
FileSize: 41232
NarHash: sha256:1mi14cqk363wv368ffiiy01knardmnlyphi6h9xv6dkjz44hk30i
NarSize: 205968
References: 9df65igwjmf2wbw0gbrrgair6piqjgmi-glibc-2.31 w9yy7v61ipb5rx6i35zq1mvc2iqfmps1-hello-2.10
Sig: cache.nixos.org-1:uP5KU8MCmyRnKGlN5oEv6xWJBI5EO/Pf5aFztZuLSz8BpCcZ1fdBnJkVXhBAlxkdm/CemsgQskhwvyd2yghTAg==
```

Besides links to the archive that contains the compressed binaries, this response includes two relevant pieces of information which are used to verify binaries from the binary cache(s):

- The `NarHash` is a hash over all Nix store directory contents
- The `Sig` is a cryptographic signature over the `NarHash`

With this information, the client can check that this binary really comes from the provider's Nix store.

## What are the limitations of this model?

While this model has served Nix and others well for many years it suffers from a few problems.
All of these problems can be traced back to a [single point of failure][single-point-of-failure] in the [chain of trust][chain-of-trust]:

- First, if the key used by cache.nixos.org is ever compromised, all builds that were ever added to the cache can be considered tainted.
- Second, one needs to put either full trust or no trust at all in the build machines of a binary cache -- there is no middle ground.
- Finally, there is no inherent guarantee that the build inputs described in the Nix expressions were actually used to build what's in the cache.

## Trustix

Trustix aims to solve these problems by assembling a mapping from build inputs to (hashes of) build outputs provided by many build machines.

Instead of relying on verifying packages signatures, like the traditional Nix model does, Trustix only exposes packages that it considers trustworthy.
Concretely, Trustix is configured as a proxy for a binary cache, and hides the packages which are not trustworthy.
As far as Nix is concerned, the package not being trustworth is exactly as if the package wasn't stored in the binary cache to begin with.
If such a package is required, Nix will therefore build it from source.

Trustix doesn't define what a trustworthy package is.
What your Trustix considers trustworthy is up to you.
The rules for accepting packages are entirely configurable.
In fact, in the current prototype, there isn't a default rule for packages to count as trustworthy: you need to configure trustworthiness yourself.

With this in mind, let's revisit the above issues

- In Trustix, if an entity is compromised, you can rely on all
  other entities in the network to establish that a binary artefact is
  trustworthy. Maybe a few hashes are wrong in the Trustix mapping,
  but if an overwhelming majority of the outputs are the same, you can
  trust that the corresponding artefact is indeed what you would have
  built yourself.

  Therefore you never need to invalidate an entire binary cache: you
  can still verify the trustworthiness of old packages, even if newer
  packages are built by a malicious actor.

- In Trustix, you never typically consider any build machine to be
  fully trusted. You always check their results against the other
  build machines. You can further configure this by considering some
  machines as more trusted (maybe because it is a community-operated
  machine, and you trust said community) or less trusted (for instance,
  because it has been compromised in the past, and you fear it may be
  compromised again).

  Moreover, in the spirit of having no single point of failure,
  Trustix's mapping is not kept in a central database. Instead every
  builder keeps a log of its builds; these logs are aggregated on your
  machine by your instance of the Trustix daemon. Therefore even the
  mapping itself doesn't have to be fully trusted.

- In Trustix, package validity is not ensured by a signature scheme.
  Instead Trustix relies on the consistency of the input to output
  mapping. As a consequence, the validity criterion, contrary to a
  signature scheme, links the output to the input. It makes it
  infeasible to pass the build result of input `I` as a build result for
  input `J`: it would require corrupting the entire network.

## Limitations: reproducibility tracking and non-reproducible builds

A system like Trustix will not work well with builds that are non-reproducible, which is a limitation of this model.
After all, you cannot reach consensus if everyone's opinions differ.

However, Trustix can still be useful, even for non-reproducible builds!
By accumulating all the data in the various logs and aggregating them, we can track which derivations are non-reproducible over all of Nixpkgs, in a way that is easier than previously possible.
Whereas the [r13y project](https://r13y.com/) builds a single closure
on a single machine, Trustix will index _everything ever built_ on every
architecture.

## Conclusion

I am very excited to be working on the next generation of tooling for trust and reproducibility, and for the purely functional software packaging model pioneered by Nix to keep enabling new use cases.
I hope that this work can be a foundation for many other applications other than improving trust -- for example, by enabling the Nix community to support new CPU architectures with community binary caches.

Please check out the code at the [repo][trustix] or join us for a chat over in `#untrustix` on [Freenode][freenode].
And stay tuned -- in the next blog post, we will talk more about Merkle trees and how they are used in Trustix.

<a href="https://nlnet.nl/" style="width=40%;margin=2%;">![NLNet](./nlnet-banner.png)</a>

<a href="https://nlnet.nl/NGI0" style="width=40%;margin=2%;">![NGI0](./NGI0_tag.png)</a>

[trustix]: https://github.com/tweag/trustix
[build-transparency]: https://build-transparency.org
[merkle-tree]: https://en.wikipedia.org/wiki/Merkle_tree
[append-only]: https://en.wikipedia.org/wiki/Append-only
[consensus]: https://en.wikipedia.org/wiki/Consensus_(computer_science)
[nlnet]: https://nlnet.nl/project/Trustix
[pet]: https://nlnet.nl/PET
[ngi]: https://ngi.eu
[single-point-of-failure]: https://en.wikipedia.org/wiki/Single_point_of_failure
[chain-of-trust]: https://en.wikipedia.org/wiki/Chain_of_trust
[freenode]: https://webchat.freenode.net/
[untrusted-ci]: https://www.tweag.io/blog/2019-11-21-untrusted-ci/
