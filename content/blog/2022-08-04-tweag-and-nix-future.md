---
title: "Our roadmap for Nix"
author: "Théophane Hufschmitt"
tags: [nix]
description: "Tweag's Nix team plans for the coming years"
---

I lead Tweag's Nix team.

The Nix ecosystem is at a turning point in its evolution. After more than a decade of slow but steady growth, Nix gained a lot of traction in the past few years, leading the community to expand at an astonishing rate.
But this growth hasn't been without issue.

As consultants, we see that nearly all of our clients that are using Nix are, at best, annoyed by the accidental complexity that comes with it.
Some of them even decide to back off because of that, left with a nondescript feeling that Nix is not ready for them.

We care about Nix, and we want it to thrive.

The way to get there is to strengthen Nix's core as much as possible.
Instead of getting lost in many exciting things that Nix _could_ do, we must take a pragmatic approach in ensuring that the core functionality of Nix works well, and is available to everyone.

In particular, we'd like to focus on making Nix a great developer tool. We think that requires Nix to:

- solve people's problems,
- work reliably everywhere, and
- be ubiquitous.

## We want Nix to solve people's problems

I love the Nix model.
Coming from a theoretical CS background, the principles behind Nix are those the whole computing industry should have adopted, and I long for the day when they become the standard.
But real-life™ is also a thing, and despite my partiality towards Nix, I occasionally have to admit that, well, it's just not good-enough in practice for what I want to do.
And if I have to admit that, it means that most people who don't care about the theoretical side of things will just give up on it, and rightfully so. Why should anyone use a tool that doesn't solve their problem?

So we want Nix to provide a clear path towards solving clear problems.
Identifying these problems is already a non-trivial part of the work, but from our past experience we can sketch out some broad axes along which to improve.

### Development environments

Veterans Nix users generally consider `nix-shell` or its successor `nix develop` to be a nice byproduct of the broader Nix world: _If_ you package your program to be built with Nix, then you also incidentally get this nifty environment that you can use to develop it.
And that's actually how `nix-shell` started -- a quick hack for debugging packaging issues.

But the truth is that these developments environments are incredibly valuable by themselves.
Even if you don't have a proper clean Nix packaging, you can just drop in a `shell.nix`/`flake.nix` somewhere and get all of your dependencies magically pulled-in for you.

We want to rethink this functionality to directly address the needs of people, rather than having its usefulness be a mere incidental nicety.
This means:

- Actually trying to understand why and how people are using it, and how they could benefit more from it – be it through principled user-studies or by listening to the feedback of the community and our clients.
- From this high-level vision, rethink `nix develop` to better answer these needs, using a documentation-driven approach to keep the end-user in the center of the loop, maybe building on or drawing inspiration from other projects like Numtide's [devshell] or [nix-output-monitor].

[devshell]: https://numtide.github.io/devshell/
[nix-output-monitor]: https://github.com/maralorn/nix-output-monitor

### Better distributed pipelines

Be it as a backend for their development environments or because they are more hooked into Nix, a number of Nix users need more than just being able to run a Nix command on their own machine.
Even a reasonably simple Nix setup might quickly require running Nix on the CI, with a binary cache, maybe even distributed builds, etc.
And even simple instances of this are generally a huge pain to do properly.

Some projects like the awesome [cachix] and the related [github actions][cachix_actions] or [nixbuild.net] provide some of this infrastructure as a service, but they aren't always an acceptable option, nor are they an excuse for a bad out-of-the-box experience.
Besides, there's only so much that they can do, and some problems (performance in particular) can only be solved at the root – the Nix implementation itself.

[cachix_actions]: https://github.com/search?q=action+user%3Acachix&type=repositories
[cachix]: https://www.cachix.org/
[nixbuild.net]: https://nixbuild.net/

Solving this issue in its full generality is a hard problem, however we can ensure that the simplest and most common use cases are properly covered – as simply as possible and thoroughly documented.

## We want Nix to work reliably everywhere

In the simplest scenarios, Nix works great. Nix on NixOS is a breeze, Nix on Ubuntu works well 99% of the time, and Debian even makes Nix available in its own repositories.
The other scenarios range from OK to awful (any Mac users around? 👋).

This is a sad state, but not a fatality.
We (the community) have the resources to make Nix a success story everywhere it can be, and we (Tweagers) hope to be a driving force for that effort.

This means in particular:

- Improving the Nix installer. It is a tiny but crucial piece of the ecosystem – being generally the very first thing that people run before using Nix on a system – yet it is one of the least loved parts of the Nix codebase.
- Fostering more efforts to keep nixpkgs healthy on MacOS. The [Nix
  community survey][2022_survey_report] reports that there's more than
  three time more Linux users than MacOS users in the
  Nix community. Compare with developers in general: there are only [slightly less Mac users that Linux users](https://survey.stackoverflow.co/2022/#section-most-popular-technologies-operating-system) according to the Stack Overflow survey.
- Allowing Nix to run in restricted corporate environments, for example with no or restricted root access, or using access-control systems like SELinux.

[2022_survey_report]: https://discourse.nixos.org/t/2022-nix-survey-results/18983

## We want Nix to be ubiquitous

Being simple and reliable is great. But know what's better? Being everywhere.

Take the good old shell script.
It's not simple for anything beyond just sequencing a few commands.
It's arguably not reliable either in that it has so many traps that most shell scripts in the wild are bound to blow up given anything as malicious as a filename with spaces or an empty parameter.
Yet everyone -- myself included -- uses shell scripts for so many things, because it's just the de facto standard, so we know that it will be available and that people will understand what's going on.
Even when people try to replace it, they keep it as a point of reference or even try to stay compatible with it.

Likewise, we want Nix to be everywhere.
We want a world where finding a `flake.nix` at the root of a software project is not a pleasant surprise but something expected and common.
And we want the people who design tomorrow's software distribution mechanism to at least know and recognize the Nix model, and take that into account.

To get there, we plan to:

- keep the marketing team efforts going, to make more people aware of Nix and give the community tool to talk about it,
- keep writing and talking about Nix in public,
- participate or help organize conferences and workshops.

---

This is obviously a broad roadmap, and bound to evolve.
Some of that, like the [marketing team efforts](https://discourse.nixos.org/t/marketing-team-can-we-present-nix-nixos-better/6249) or the work on the Nix package manager has already been going on for some time.
Some like the [coordinated documentation effort](https://discourse.nixos.org/t/documentation-team-flattening-the-learning-curve/20003) or the [nixpkgs-architecture team](https://github.com/nixpkgs-architecture/) is just getting started.
Some have yet to begin.
However, we hope that it will help shed light on the work that Tweag does and intends to do for the Nix community, and maybe also serve as an inspiration both for a potential community-wide roadmap and for individual contributions.

We also intend to regularly publish a shorter-term roadmap on [discourse](https://discourse.nixos.org) alongside our usual dev updates. So stay tuned!
