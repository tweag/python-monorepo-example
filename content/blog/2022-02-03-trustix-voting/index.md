---
title: "Trustix - Consensus and voting"
author: Adam Hoese, Taeer Bar-Yam
tags: [nix, trustix]
description: "We discuss existing consensus models and why no global consensus
  mode is well suited to Trustix, opting for a user-defined algorithm instead."
---

In our last posts, we [introduced the general idea of Trustix](https://www.tweag.io/blog/2020-12-16-trustix-announcement/) and its [log-based append-only data structure](https://www.tweag.io/blog/2022-01-14-trustix-trees/).
In brief, Trustix is a verification system for binary cache providers, and a tool that allows you to compare those software binaries across independent providers to ensure you're getting the correct software, but without trusting a single binary cache.

In this post we look at the situation where providers disagree on the correct binaries, and how to aggregate those opinions and decide which output to trust.
This problem comes up in many kinds of decentralized systems, and different solutions have arisen for each use-cases.
We'll discuss some of these solutions and see that, unfortunately, none of them would work for Trustix.
The good news is, the solution Trustix does employ is far simpler and more robust.

## Trusted consensus

Sometimes a service is decentralized on multiple servers for robustness, but all the servers are still owned and operated by the same group.
One example of this is [Signal's "secure value recovery"](https://signal.org/blog/secure-value-recovery/).
In order to agree upon state across the machines, they employ the consensus algorithm [raft](https://raft.github.io/).

However, notice that in this situation all the machines are pre-defined and trusted, and the problem is only to deal with is synchronizing their state.
A malicious attacker that controlled one of the machines could corrupt the consensus process.
The whole point of Trustix is to be robust against a single build server being malicious.

## Federated chat

[Matrix](https://matrix.org/) is a chat system where there are many servers that communicate with each other and must synchronize state.
It is unlike the previous example in that new, potentially untrusted servers can join.
They have designed a [system for state resolution](https://github.com/matrix-org/matrix-doc/blob/old_master/proposals/1442-state-resolution.md).

The problem they are looking to solve is different, though.
They are not concerned with accepting certain states and rejecting others, but with merging states.
Their core method of trust is explicitly extending or rejecting permissions to different servers and users.
This is like a blacklists of spam email servers.
It works fine for messages, where if a few spam messages get through, we can simply ignore them.
For software binaries though, a single malicious binary can run arbitrary code on your system.
It can also be subtle enough that it is not immediately detected and added to the blacklist.

In other words, the Matrix system also fundamentally assumes that everyone is trustworthy, and simply adds a mechanism for distrusting servers.
This won't do for our purposes.

## Blockchains

Blockchains are closer to our case: they are open to anyone to join, they don't trust any individual actor, and the costs of a single corrupted block are high.

At its core, the solution is to have all participants in the block-chain vote, and take whatever the majority says.
Now in order to hijack the system a bad actor needs to control a majority of the votes, known as a [51% attack](https://en.wikipedia.org/wiki/Double-spending#51%_attack).

There is one additional challenge, which is dealing with "voter fraud".
Voting in a simple and anonymous digital system comes at essentially zero cost; a single bad actor can simply vote a million times to gain a majority of votes.
There are two methods employed to deal with this, [proof of work](https://en.wikipedia.org/wiki/Proof_of_work) and [proof of stake](https://en.wikipedia.org/wiki/Proof_of_stake).

In a proof of work system, popularized by [bitcoin](https://bitcoin.org/bitcoin.pdf) and adopted by most cryptocurrencies since, a vote must be accompanied by a proof of an expensive computation.
This makes the "zero cost vote" now cost CPU power, and therefore money.
It means that a 51% attack requires 51% of the compute power of all participants.
This isn't impossible, but is much harder and more costly.

One disadvantage of proof of work systems is their high energy cost and environmental impact.
This has lead some blockchains such as [Cardano](https://cardano.org/) and [Tezos](https://tezos.com/) to use a proof-of-stake system instead, which distributes votes according to investment in the cryptocurrency.
This means that a 51% attack requires a large up-front investment, and devalues the coin, which they have invested heavily in.

Notice that both proof-of-X solutions require big investment of capital. In the context of a cryptocurrency, where there is significant money to be made, those investments make sense.
On the other hand, it's not nearly as appealing for a company or an individual to operate a build log that comes with a similar cost every time a package build is submitted.

## Local voting

So none of the solutions we've looked at will work for Trustix.
Fortunately, they don't need to.

Trustix, in contrast with all of these use-cases, does not actually require a globally agreed upon truth.
If two of Signal's servers have different records, that could lead to problems if a user switches from one server to another.
If two matrix servers disagree about the messages sent in a room, this can manifest as two groups of users unable to talk to each other.
If everyone does not agree on the series of blocks in a blockchain, you have two blockchains, not one.

If two users disagree about which package to trust they… download different packages.
This does not pose the same kind of existential threat or bad user experience as it does for the other systems.

So we lean into this, and allow each user to define what consensus means to them, fully scriptable in Lua[^lua].
This is especially well suited to Trustix for two reasons.
First, unlike other systems we've discussed, it is not essential to reach a consensus in Trustix.
If every builder reports a different output hash, the user can simply build that package from source.

[^lua]: Lua is a popular scripting language with [good Go interoperability](https://github.com/Shopify/go-lua)

Second, different users will have different threat models, different trusted servers, and different resources available.
An everyday user may be more inclined to trust a reasonably robust server, and save themselves some compile-time.
A high-profile security-conscious organization may care more about not having a single point of failure, and have access to the CPU power to recompile many packages if there's any ambiguity among different build servers' output.

## Example Trustix voting methods

With that in mind, Trustix does not come with ready-built solutions for every need.
A user can program their own decision procedure in Lua.
Two common cases are included in Trustix already:

### Minimum percentage voting

This is the idea that "X% of build servers must agree on the output", which can be anywhere from "I trust all build servers implicitly, trust any of them" to "majority rules" to "it must be unanimous, or I'm building it myself."

A simple extension of this could be weighting different servers based on degree of trust.

### Name match

This allows you to specify a trusted build server, and returns whatever that server says the output is.
This is how systems without Trustix work, simply trusting a given build server.

This method is primarily meant as a fallback method to express trust scenarios like "match only if at least two thirds of all logs agree on the output, but if no decision can be made return the value of log with name cache.nixos.org", which would be a combination of the two provided methods.

## A 51% attack in Trustix

In some sense, a 51% attack in Trustix isn't possible or meaningful, since there is no distributed consensus taking place.
In practice, for NixOS, there will be some default voting mechanism specified in the Trustix module, and most users will not change it.
So there are a few attack vectors:

1. If the default voting mechanism is majority-rules, then an attacker would need to gain control of 51% of all configured nix Trustix logs, including access to the log's private key, an attack which is extremely unlikely.
   Keep in mind that even if this should happen, high-profile targets would likely have configured their own voting procedure, so they would be unaffected.

1. With any kind of digital voting there is concern over someone making many accounts to amplify their vote.
   In this case "accounts" would each be a Trustix log. In order to register those logs as valid votes, they would either have to be added in an individual user's configured list of logs, or added to the default list in the nixos module.
   If someone has compromised the nixos module though, they can run arbitrary code anyways.

1. A targeted attack would have to first find out the voting procedure being used by the target, which provides a small amount of protection.
   Then they would have to compromise enough of the right logs to sway that voting procedure. How hard that is depends on the specific voting procedure, but can be easily made extremely difficult.

# Conclusion

In Trustix many global consensus models can't be made functional for various reasons, but also not everyone has the same level of trust in different build servers, or the same security requirements. By defining consensus and having the vote client-side, we end up with a model that is much more flexible and can be tuned to your use cases and threat model.

---

The development of [Trustix](https://github.com/tweag/gomod2nix) is funded by [NLnet](https://nlnet.nl/) through the [PET](https://nlnet.nl/PET/)(privacy and trust enhancing technologies) fund.

<a href="https://nlnet.nl/" style="width=40%;margin=2%;">![NLNet](./nlnet-banner.png)</a>

<a href="https://nlnet.nl/NGI0" style="width=40%;margin=2%;">![NGI0](./NGI0_tag.png)</a>
