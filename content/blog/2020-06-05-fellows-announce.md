---
title: "Tweag Open Source Fellows"
author: Richard Eisenberg
tags: [jobs, fellowship]
description: "Tweag introduces our first cohort of Open Source Fellows."
---

Tweag is delighted to introduce our first cohort of Tweag Open Source Fellows.

The [Tweag Open Source Fellowship](https://www.tweag.io/blog/2020-02-14-os-fellowship/) is an opportunity for open-source contributors
of all stripes and backgrounds to be paid for their hard work. Fellows are selected
twice a year (applications due March 30 and September 30), and then they work with
a mentor for 12 weeks of the Fellowship. Mentors are full-time Tweag employees
with an interest in the open-source project being proposed.

We received 19 applications by our application due date of March 30.
These applications went through a three-stage reviewing process. First,
each application was reviewed by at least two members of the Fellowships
team, to check that the project was feasible to fund. At this stage, we
looked for projects with a reasonable chance for success in 12 weeks and
that were well-defined and impactful enough to consider funding. Twelve
applications went to the next round.

These remaining applications were each assigned to at least two full-time
Tweag employees _not_ on the Fellowships team, for in-depth reviews. Using
these reviews, the Fellowships team selected seven applications for our
shortlist. Key factors at this stage were our ability to identify knowledgeable
Tweag-internal mentors and how well projects aligned with broader Tweag
interests. The shortlist was reviewed with Tweag's admin team, resulting
in four offers, sent out on 5 May, 2020. All four offers were accepted, and we're delighted to welcome
four Fellows to work with us:

## Avi Dessauer

For his Fellowship, Avi will be implementing Sundial GC, his design for a
pauseless, concurrent, copying garbage collector. Sundial is designed around a novel
approach that exploits monomorphism and immutability in order to offload GC
work onto dedicated threads, while minimizing inter-thread synchronization.
Sundial thereby eliminates the bane of low latency systems: GC pauses.
Sundial's immediate goal is to enable functional programming in safe Rust,
with the eventual aim of being embedded in future low latency functional
language runtimes.

Avi is a self-taught polyglot systems developer from upstate NY. He contributes to
numerous open source projects, primarily in Haskell and Rust. He fell in love
with FP, and Haskell specifically, a few years ago. Unfortunately, Avi's other
passion, distributed database design, is latency sensitive. Therefore, he set
out to solve GC latency in copying collectors. In addition to CS, his interests
include political science and cooking.

Avi will be mentored by Tweager Eelco Dolstra.

## Zubin Duggal

The goal for this Fellowship is to build a responsive and featureful
Haskell editor experience by contributing to successful efforts
on the Haskell Language Server. Zubin plans to do this by taking advantage of
the indexing and caching capabilities offered by HIE files and hiedb.
These will support fast, project-wide queries, such as reference lookup,
jump-to-definition for dependencies, and best-effort IDE support even in the face
of compile failures, so that your IDE can aid you when it is most
needed, such as when you are in the middle of a big refactor. Zubin also
hopes to make ghcide much more scalable and responsive with these
changes.

A part of this work is to help in the transition from haskell-ide-engine to
haskell-language-server, so that the Haskell community can build on ghcide to offer a
featureful IDE, as well as a platform for tooling developers to easily
integrate their tools into and get them in front of users with minimal
effort.

You can follow progress on this project with weekly updates at
https://mpickering.github.io/ide/

Zubin is currently a Master's student in Computer Science. He has been
involved with Haskell IDEs since 2017 when he completed a Haskell
Summer of Code project which led to the first release of
haskell-ide-engine in its LSP incarnation. Since then, Zubin has completed
two more GSoC projects, allowing GHC to emit .hie files and working on
hiedb, haskell-ide-engine, hie-bios, cabal-helper and other tools in
the ecosystem.

Zubin will be mentored by Tweager Cheng Shao.

## Guillem Marpons

This project is aimed at enhancing Pandoc with reading support for the markup
language Asciidoc, and also improving Pandoc's Asciidoc generation. The parser
Guillem proposes to develop is not a regular Pandoc Reader, but something more
similar to commonmark-hs: a Pandoc front-end whose syntax can be easily
extended, tracks accurate information about source positions and can
potentially support all Asciidoc features.

Guillem is a software developer with more than 20 years of experience and also some
background in research. He has always been a FLOSS advocate and has devoted
the last few years to providing guidance and training on FLOSS adoption for
public institutions and projects. He has also implemented documentation
strategies based on lightweight markup languages and docs-as-code principles
in those projects.

Guillem will be mentored by Tweager Juan Raphael Diaz Simões.

## Georg Rudoy

Dependent types are a powerful tool allowing expressing almost arbitrary
specifications about a program's behavior and proving that the program, in
fact, follows its specification. Unfortunately, using dependent types also requires the
programmer to explicitly write down the proofs involved, which is certainly
hindering their wider adoption. On the other hand, refinement types are a
subset of dependent types enabling decidable and efficient proof automation,
and, although their expressive power is nowhere near that of full dependent
types, they still are of tremendous help for day-to-day programming. The
natural question then is whether it is possible to seamlessly unify both in a
single language, compiling refinement types to the base dependently typed
language. The aim of this project is to investigate whether this idea is
viable by means of a proof-of-concept toy language implementing a flavor of
refinement types, with a translator to Idris. This language and the translator
will then serve as a basis for a later development introducing refinement
types into an existing dependently typed language.

Georg is a software engineer whose projects over the last decade ranged from
machine learning to compilers for domain-specific languages. Lately, his
interests shifted towards provably correct programming and branches of
mathematics like type theory. His production development experience together
with his eagerness to do more research-oriented work means he is excited
to begin this project.

Georg will be mentored by yours truly, Tweager Richard Eisenberg.

---

Interested in applying for a Tweag Fellowship? Our next deadline will be
on September 30, but the [application](https://boards.greenhouse.io/tweag)
is open at all times.
