---
title: Simulating Tenderbake
description: "Announcing a simulator for Tenderbake"
author: Mark Karpov, Divesh Otwani
tags: [ocaml, blockchain, distributed-algorithms]
---

_This is a repost of [this blog post][nl-post] originally published on the Nomadic Labs blog._

The consensus algorithm is a crucial part of any blockchain project. Because
of the distributed nature of blockchains, different nodes can have different
ideas of what the current state of the blockchain is supposed to be. The
role of the consensus algorithm is to decide which of these possible states,
called _forks_ or _branches_, will be selected globally. In the world of
distributed systems, there are two distinctive families of consensus
algorithms: _Nakamoto-style_ and _BFT-style_. Most blockchain solutions use
**Nakamoto-style** algorithms that allow the existence of any number of
forks of any length, but make longer forks increasingly unstable, so that
they eventually collapse to a single branch. We say that these algorithms
have _probabilistic finality_. [**Byzantine fault tolerance
(BFT)**][bft-wiki] algorithms have _deterministic finality_. They stipulate
definite conditions that must be fulfilled for a block to become final.

[Nomadic Labs][nomadic-labs] is intending to propose
[Tenderbake][tenderbake-blog-post] -- a BFT-style algorithm -- as the next
consensus algorithm of Tezos. Its deterministic finality allows us to make
solid claims about the period of time that should pass for a transaction to
become final. In Tenderbake, a block becomes final when there are two blocks
on top of it -- this is the only condition. So, if the system produces one
block per 15 seconds, a transaction will become final in about 30 seconds.
This is the kind of performance that users expect from a successful
blockchain solution.

Historically, Tenderbake started as a variant of
[Tendermint][tendermint-paper] and was subsequently adapted to fit into the
existing Tezos system. However, as time passed, the description from [the
paper][tenderbake-paper] and its nascent implementation in the Tezos
codebase started to differ. Thus, for a person starting working on
Tenderbake, reading the paper is not enough. It is also hard to study the
algorithm by reading the Tezos code, because the consensus algorithm is not
implemented in isolation from other components of the system. This is where
the Tenderbake simulator project comes into play.

## The simulation framework

Nomadic Labs asked Tweag to develop a framework that would be general enough
to model any consensus algorithm (be it BTF-style, Nakamoto-style or in
styles yet to be invented) in a clear way to facilitate onboarding of
newcomers and for exploration of consensus algorithms in the future. The
results of our work can be found [in this repository][simulator-repo]. The
language of choice at Nomadic Labs is OCaml and we saw no reason not to use
it for this project. Similar to the Tezos code itself, the simulator is
distributed under the MIT license.

### Principles of operation

We provide here a simplified description that nevertheless should give the
reader an idea of what the framework can do. The project comes with a
[guide][guide] that explains in detail how the simulation framework works
and how to implement a consensus algorithm in it.

The simulator allows us to observe the evolution of a system that comprises
a collection of nodes -- independent processes that do not share memory but
can exchange messages by means of asynchronous broadcast that can be
fine-tuned by the user if desired.

Consensus algorithms in the framework are implemented as event handlers --
functions that are called when an event occurs at a particular node. A call
of an event handler is called an _iteration_. Event handlers have the
following type signature:

```ocaml
type event_handler =
  Algorithm.params ->
  Time.t ->
  Event.t ->
  Signature.private_key ->
  Algorithm.node_state ->
  Effect.t list * Algorithm.node_state
```

Let's go over the arguments of the function:

- `Algorithm.params` is the parameters of the algorithm such as e.g. round
  duration in seconds.
- `Time.t` is the current time.
- `Event.t` is the event the node needs to react to. Currently there are two
  kinds of events: reception of a message and a “wake up” call that the node
  can schedule for itself. Message types are defined per consensus
  algorithm.
- `Signature.private_key` is the private key that every node magically
  knows. It is used for signing of messages. This is important because the
  framework allows us to program and use Byzantine versions of nodes, too.
- `Algorithm.node_state` is the node state. The type is defined per
  consensus algorithm.

The return type of an `event_handler` is an effect list and an updated node
state. An effect in the list can be one of the following:

- Broadcast a message to all nodes.
- Schedule a wake up call.
- Shut down.

### Testing

We have written two kinds of tests: _stress tests_ and _scenario tests_.

**Stress tests** are about letting an algorithm run for a number of
iterations with a large enough network and realistic propagation of
messages, including messages getting lost and messages arriving out of
order. The framework allows us to specify a set of predicates that must hold
at each iteration in such a test. This way we can determine if an algorithm
satisfies [liveness][liveness-wiki] and [safety][safety-wiki] properties.
According to the tests, all models that we have written satisfy both
properties.

**Scenario tests** are about adjusting propagation of messages and/or
lifetime of nodes in order to model a situation of interest. We can then
inspect execution logs and check whether the nodes behaved in the expected
way. It is easy to do because simulations return typed logs that we can
pattern match on.

### Implemented algorithms

We have implemented four consensus algorithms (listed below roughly in order
of increasing complexity), and applied stress tests and scenario tests as
discussed above:

- Leader election, see
  [`src/leader_election`](https://gitlab.com/nomadic-labs/tenderbake-simulator/-/tree/master/src/leader_election).
- Ouroboros (the simple BFT version), see
  [`src/ouroboros`](https://gitlab.com/nomadic-labs/tenderbake-simulator/-/tree/master/src/ouroboros).
- Emmy<sup>+</sup>, see
  [`src/emmy_plus`](https://gitlab.com/nomadic-labs/tenderbake-simulator/-/tree/master/src/emmy_plus);
  this is the current consensus algorithm used by Tezos.
- Tenderbake, see
  [`src/tenderbake`](https://gitlab.com/nomadic-labs/tenderbake-simulator/-/tree/master/src/tenderbake);
  this is the algorithm Nomadic Labs is planning to propose as a future
  amendment to the Tezos blockchain.

Every algorithm is explained in its `README.md`. Our focus was not only
explaining how a particular algorithm works in principle, but also how it
translates to code that the simulator framework can run.

## Conclusion

The simulator has already proven useful. People who have tried it out report
that it has helped them understand the algorithm of Tenderbake and
experiment with it. In the future, we can expect new consensus algorithms to
be implemented and explored using this framework. Please, feel free to [give
it a try][simulator-repo] and contribute!

[nl-post]: https://blog.nomadic-labs.com/simulating-tenderbake.html
[nomadic-labs]: https://nomadic-labs.com/
[bft-wiki]: https://en.wikipedia.org/wiki/Byzantine_fault
[tenderbake-blog-post]: https://blog.nomadic-labs.com/a-look-ahead-to-tenderbake.html
[tendermint-paper]: https://arxiv.org/pdf/1807.04938.pdf
[tenderbake-paper]: https://arxiv.org/pdf/2001.11965.pdf
[simulator-repo]: https://gitlab.com/nomadic-labs/tenderbake-simulator
[guide]: https://gitlab.com/nomadic-labs/tenderbake-simulator/-/blob/master/GUIDE.md
[liveness-wiki]: https://en.wikipedia.org/wiki/Liveness
[safety-wiki]: https://en.wikipedia.org/wiki/Safety_property
