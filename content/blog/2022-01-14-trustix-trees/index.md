---
title: "Trustix - Good things come in trees"
shortTitle: Trustix trees
author: Adam Hoese, Taeer Bar-Yam
tags: [nix, trustix]
description: In this post we introduce Merkle trees, the data structures needed to make the Trustix log append-only and searchable.
---

In the [previous Trustix post](https://www.tweag.io/blog/2020-12-16-trustix-announcement/) Adam introduced Trustix and the general ideas behind it.
In this post we will talk you through the data structures behind Trustix, in particular _Merkle trees_.

At its core, Trustix verifies computations by comparing pairs of hashable inputs and outputs between multiple builders by using _build logs_.
A simple set of input-output pairs would be one way to implement this build log -- the log would be queried with an input hash, and would then yield the corresponding output hash.
Users could then compare it to the output hashes from other logs or binary caches, to check whether they have all obtained the same answer to a computation.

For security, these input-output pairs could be signed by the builder, which ensures their integrity as long as the signature key is not compromised.

Trustix goes further than this, using a "verifiable log" to which data can be _appended_ but that cannot be modified in any other way.
This log is structured as a tree where sets of entries have an identifying hash called the log head, which clients can use to verify that the past data has been preserved unmodified.
This ensures that the log isn't tampered with retroactively and if, for instance, a signature key is compromised, we only have to invalidate entries from that point forward.

## Merkle trees

The way the append-only log works is somewhat similar to Git commits.
In a Git repository, each commit is given a hash, which depends on its content and on the previous commits.
If you modify an earlier commit X (and `push --force`) then all the commits after X see their hash change, and anybody who has a reference to one of theses commits will immediately notice that the branch has been modified.
Therefore, a Git branch acts as a verifiably append only log of commits.

Trustix logs are similar, except that they use a tree structure, called a [Merkle tree](https://en.wikipedia.org/wiki/Merkle_tree), instead of a chain.
Merkle trees are ubiquitous in computer science and can be found in Blockchains, databases, file systems, peer-to-peer software and many other use cases.

Why a tree?
While a simple chain of hashes, like Git's, is sufficient to ensure that the log is append-only, looking up a particular input-output pair would require a linear scan of the entire log.
With a Merkle tree, a lookup only requires walking and downloading one (logarithmically sized) branch, as we describe below.

### How Merkle trees work

A Merkle tree has data in its leaf nodes, with immediate parents of those nodes being the _hashes_ of each datum.
In our case the data are the input-output hashes of the builds, and the first-level parent nodes thus contain hashes of these pairs of hashes.
At the second level, we have hashes once more -- but now we have the hashes for sets of nodes at the first level.
That is, we have hashes of hashes of the input-output hashes.
This goes on and on.

What is important to retain here is that gradually hashes get aggregated into fewer and fewer nodes, until the root node is reached.
This root node (the log head) transitively depends on all leaf nodes and thus all data that is stored in the log.

Consider, for example, the following tree:

            root0
           /    \
          /      \
         /        \
        m          n
       / \        / \
      a   b      c   d
      |   |      |   |
      d0  d1     d2  d3

The `d0, d1, d2, d3` nodes are data nodes and contain the input-output hashes in the log. `a=h(d0), b=h(d1), c=h(d2), d=h(d3)` are their hashes computed with the hash function `h`.
The aggregated hashes `m=h(a,b)=h(h(d0),h(d1))` and `n=h(c,d)=h(h(d2),h(d3))` coalesce in the root hash `root0 = h(m, n)`, which depends on all leaf nodes.

Let's append two more nodes, maintaining the binary tree structure, and see what happens:

                   root1
                  /    \
                 /      \
                /        \
               /          \
            root0          \
           /    \           \
          /      \           \
         /        \           \
        m          n          o
       / \        / \        / \
      a   b      c   d      e   f
      |   |      |   |      |   |
      d0  d1     d2  d3    d4  d5

Since we already have `root0` from the previous state of the tree, the only thing we need to calculate `root1` is to hash `d4` and `d5`, let that propagate up the tree to `o` and hash `root0` combined with `o`.
Appending new branches thus doesn't require us to traverse the whole tree again.

We can verify that nothing from the previous state was modified by seeing that `root0` is still in the tree; we have only appended to it.[^append_only]

It is also possible to easily verify that a branch or a leaf node of the tree belongs there.
For instance, if we want to check that `d2` is in the tree, the builder sends us the hashs `d`, `m`, and `o`: the hashes of the _siblings_ of the nodes on the path from `root1` to `d2`.
With `d2`, we compute `c`, with `c` and `d`, we compute `n`, with `n` and `m` we compute `root0`, and `root0` and `o` we compute `root1`: if `root1` coincides with the root of the builder's log, then indeed, we have verified that `d2` belongs to the log.

Let's explore this in more detail in the context of Trustix.

## Solving trust with Merkle trees

Let's say we want to verify that an output of the builder is indeed what that builder built for that input derivation.
We get pointed to an entry in the build log (a leaf node in a Merkle tree) that has the derivation's input hash, and the build's output hash as its values.

We need to verify that this is not a faked entry of the tree, so we take the path from the leaf node up to the root tree.
We hash each node with its sibling to calculate their parent's hash, and eventually we reach the root hash.
If the result we get is the same as the root hash in the tree, then this entry is indeed a part of the log.

In addition, the root hash is signed with the log's [private key](https://en.wikipedia.org/wiki/Public-key_cryptography), which allows us to verify that the tree as a whole is correct according to its owner.
We now have a single signed entity from which we can verify everything, as opposed to the current Nix binary cache which signs each entry individually.
One advantage of this new approach is a defense against targeted attacks.
Everyone can publish the log head they receive from a builder, so others can check they are receiving the same one.

The downside of a Merkle tree is that while it's fast to verify an entry, it's slow to _find_ that entry -- in general, we have to search the whole tree.
In other words, the questions "is this (input hash, output hash) in the tree" and "what is the output hash value of this input hash?" are inefficient to answer.

### Sparse Merkle trees to the rescue!

The [sparse Merkle tree][sparse-merkle] is a very clever _indexed_ variation on the standard Merkle tree.
Suppose that our hashes are 256 bit long, we make a Merkle tree with $2^{256}$ leaves. Each leave is initially empty.
To add an input-output pair, we compute $I$, the hash of the input, and we change the $I$-th leaf to contain the (hash of) the output.
Effectively the leaves form an array of length $2^{256}$.

Now we can easily find entries (or show they're not present in the tree), by hashing the input and looking up the corresponding leaf.
We can still verify that the entry belongs in the tree once we find it, by hashing our way up as we did before.

There are two problems. First, sparse Merkle trees are huge and time-consuming to generate. This can be solved by (ab)using the fact that most nodes in the tree are empty, to cache large sections of the tree that all look the same.
Second, as you may have noticed our tree is no longer append-only. We're _not_ appending entries anymore, we're modifying them from empty → something.

### Combining trees for fun and profit

By combining both types of trees in a single log we can get the best of both worlds!

We want the log itself to be an append-only standard Merkle tree.
So we use an input-hash indexed sparse Merkle tree for lookups, as earlier; but, instead of storing outputs directly, we store a reference to the input-output pair in the standard Merkle tree.

The new submission process is:

1. Append (input hash, output hash) to the standard Merkle tree
2. Get the new root hash of the tree
3. Sign the new root hash
4. Write a reference to that entry into the sparse Merkle tree, indexed by the input hash
5. Get the new root hash of the sparse tree
6. Sign the new sparse root hash
7. Publish the signed roots at a well-known location

The lookup process is:

1. Find the input hash of the derivation you want
2. Look up that input hash in the sparse Merkle tree
3. Verify that entry belongs in the tree, and the tree is correctly signed
4. Follow the reference to the standard Merkle tree
5. Verify that entry belongs in the tree, and the tree is correctly signed

Success! We can look up log entries, and prove that the log itself is append-only.

## Readily available implementations

### Why blockchains are not fit for purpose

Some astute readers may have noticed already that what we have described above is awfully close to a blockchain, and you may think that intuitively a blockchain would make sense as a foundation of trust. After all isn't this what blockchains are all about?
The problem comes down to the consensus models. Blockchains are all about distributed consensus, but what Trustix aims to solve require a far more local idea of what consensus means, which makes all current blockchains unsuitable as a foundation.

Consensus, and therefore blockchains, comes with required financial models such as Proof-of-Work and Proof-of-Stake. Our feeling is that neither of these models are applicable to something like Trustix. They might be great for financial transactions, but carry too much inherent cost for a system like Trustix where we want log operation to come at essentially zero extra costs (hosting aside).

### Trillian

Trillian is a mature implementation of Merkle trees that's already widely used at internet scale, mainly for [Certificate Transparency](https://tools.ietf.org/html/rfc6962), something that has greatly inspired Trustix.

The performance of Trillian is excellent and it runs on top of many popular storage layers like [MariaDB/MySQL](https://mariadb.org/), [Google Cloud Spanner](https://cloud.google.com/spanner) and [PostgreSQL](https://www.postgresql.org/).
Thanks to certificate transparency already being deployed at large scale there are many caching patterns to adopt from this ecosystem that apply directly to Trillian.

Trillian has support for smartcard based signing using the [PKCS #11 standard](https://en.wikipedia.org/wiki/PKCS_11), something you don't necessarily get that easily with the other approaches.

This makes Trillian a very solid foundation to build on. It does require a more complex setup than the other solutions considered.
Trillian also ties you to an RDBMS like MySQL or PostgreSQL, making it a very heavy weight solution.

### Git

The one major thing Git has going for it is that it's a simple, well understood format that could be stored easily, potentially even for free at providers like GitHub or GitLab.
Git is also based on a structure of Merkle trees, however these are not exposed or designed in a way that makes them suitable for Trustix.

The performance numbers we saw out of Trillian over using Git were also far better at around 3300 submissions per second vs the around 200 we achieved with the Git-based approach.
This shows that other solutions can be much more optimized and that Git is too much of a bottleneck.

### Rolling your own (or using lower level libraries)

Rolling our own implementations from scratch has some major advantages in terms of allowing us to control the implementation and optimize for problems specific to the package management verification space, the requirements that the NixOS foundation has to deploy this at scale.
This makes it much easier to optimize the structures.

Another advantage of this approach is that we entirely control the storage. A valuable property we get from this is that Trustix can run entirely self-contained with its own embedded database.

This turned out to be the best solution for Trustix as we highly benefit from the level of customization we can do.

## Conclusion

By combining the strengths of two data structures -- traditional and sparse Merkle trees -- we can get the best of both worlds and prove the following about a log efficiently:

- Non-inclusion of a derivation (i.e. this log never built a package)
- Inclusion of a derivation (this entry is indeed a part of the log)
- Prove correct operation (the append-only property of the log)

It requires a full log audit to prove that the sparse Merkle tree is append-only. This is needed less often, though, and can be offloaded onto semi-trusted verifiers. This will be explained in depth in a future blog post. When we look up builds, we verify them in the standard Merkle tree, which is easily verified to be append-only.

In the next post in the series we will elaborate on how Trustix compares logs between builders and makes decisions about which build outputs to use.

[^append_only]: In general it may take slightly more work to verify that a Merkle tree is append-only. Imagine if we add more nodes at this point: `o` will get hashed with something new, and _that_ will get hashed with `root0`, replacing `root1`. However, we can still find `root0` and `o` in the tree, and reconstruct `root1`, showing it is contained unmodified in the new tree. Importantly, this is still a fast operation.

---

The development of [Trustix](https://github.com/tweag/gomod2nix) is funded by [NLnet](https://nlnet.nl/) through the [PET](https://nlnet.nl/PET/)(privacy and trust enhancing technologies) fund.

<a href="https://nlnet.nl/" style="width=40%;margin=2%;">![NLNet](./nlnet-banner.png)</a>

<a href="https://nlnet.nl/NGI0" style="width=40%;margin=2%;">![NGI0](./NGI0_tag.png)</a>

[sparse-merkle]: https://medium.com/@kelvinfichter/whats-a-sparse-merkle-tree-acda70aeb837
