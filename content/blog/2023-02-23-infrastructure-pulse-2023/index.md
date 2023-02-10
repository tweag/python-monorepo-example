---
title: "Taking the pulse of infrastructure management in 2023"
author: Yann Hamdaoui
description: "We went to FOSDEM23 and CfgMgmtCamp23. Here are my impressions of the trends and evolutions in infrastructure and configuration management"
tags: [nix, nickel, devops]
---

February started with a busy week for several of us Tweagers. We went as a group
-- or, dare I say, a delegation -- to [FOSDEM23][fosdem-23] and
[CfgMgmtCamp23][cfg-mgmt-camp-23] (Config Management Camp if, like me, you can't
read through so many consonants). Tweagers [Bryan Honof][bryan-honof] and
[Théophane Hufschmitt][theophane-hufschmitt], together with [Ryan
Lahfa][ryan-lahfa], [Julien Malka][julien-malka] and [Matthew
Croughan][matthew-croughan] from the Nix community, got us the first Nix DevRoom
at FOSDEM. And not the last, as you can see from the picture below; the room
just couldn't handle the Nix fame!

![FOSDEM Nix Dev Room #1](./fosdem-nixdevroom-1.jpg)
![FOSDEM Nix Dev Room #2](./fosdem-nixdevroom-2.jpg)

Our involvement didn't end there. At FOSDEM:

- Théophane [presented the new Nix (core) team][fosdem-nix-team].
- [Guillaume Desforges][guillaume-desforges] lightened the mood with his
  enthusiastic talk on [making anyone use Nix][fosdem-make-anyone-use-nix].
- I went on to [introduce Nixel][fosdem-nixel], a framework to write Nix
  expressions in Nickel.

Followed right away by CfgMgmtCamp:

- Bryan first did a lightning talk to tease Nix, then took curious people on a
  deeper dive during a 50-minute gentle introduction to Nix.
- [Viktor Kleen][viktor-kleen] presented his work on [Terraform-Nickel][tf-ncl],
  a library to use Nickel in place of HCL for Terraform.
- I talked about how to write modular and customizable configuration using Nickel's
  merging system.

What follows are my takeaways on the trends and the trajectory of infrastructure
and configuration management. Please don't take my word as a confident
prediction, as predictions don't age well. This post is merely a patchwork of my
biased, fresh impressions -- as a programming language nerd
rabbit-holed into devops and infrastructure by Nix -- on my way back home.

## Self-service infrastructure

Several key speakers at CfgMgmtCamp emphasized how much infrastructure is
critical for many business services and how, more often than not, it is a
bottleneck in practice.

Developers must be able to spin up new services rapidly, not in hours or days.
They can't wait one week for someone from the infra team to finally find time,
between daily maintenance tasks, to hit the green button (or do something more
complex), potentially followed by a long painful back-and-forth. Non-technical
roles, such as marketing and sales, might also need to get a small website or a
service up, but are even less empowered.

This doesn't mean one should give everyone full access to the infrastructure.
I've rather heard a call for simpler, safer, and better automated processes. For
example, having a simple web interface for spinning up instances, with a clear
policy on cleaning inactive resources. The interface shouldn't require much
infrastructure knowledge nor expose low-level options: this isn't simply about
granting people access to an AWS console. If users are developers, this can be
achieved using infrastructure as code as well, with adapted restrictions.

Continuous integration, continuous delivery, one-off experiments -- all those
components should be automated, fully integrated in the software development
workflow, and just a few clicks (or keystrokes, for you Vimmers) away.

## YAML is dead

I'm sorry, that was just clickbait. YAML has been pronounced dead for the last 10
years or so, and unfortunately, here it stands.

But something is in the air. There is an undeniable movement toward a unified
and structured approach to writing and managing configuration.

Scattering configuration data, schemas and knowledge across many different tools,
written in many different languages (HCL, YAML, JSON, TOML, Puppet, Ansible,
Helm, etc.) isn't sustainable. And I feel like we can state a variant of
[Greenspuns' tenth rule of programming][greenspuns-tenth-rule] for
configuration:

> Every sufficiently complicated infrastructure tool's native configuration
> language is an ad-hoc, informally-specified, bug-ridden, arbitrarily-limited
> reinvention of half of CommonLisp.

This is exactly why we created [Nickel][nickel]. Projects that were the new kids
on the block a few years ago -- I'm thinking about [Pulumi][pulumi] or [CUE][cue],
for example -- are gaining strength and run today in production. Those are not
curiosities any more. The actors behind those tools seem to share the same
dissatisfaction with the state of configuration management and infrastructure
engineering. While our experience and vision led us to come up with our own
answer (Nickel), we are all trying to unify configuration management by using a
common language or platform.

## Declarative, but what about interactive?

[Adam Jacob's][twitter-adam-jacob] opening talk on Breaking The Rules, as well
as [Ringo De Smet's][twitter-ringo-de-smet] talk on Pulumi's _imperative on top of
declarative_ approach made me think about the user experience aspect of
declarative configuration. I'm still convinced that the declarative approach is
ultimately the right one, or the only sustainable one to this day.

However, there is something to be said about offering different interfaces
to this declarative layer: Pulumi is one example, providing an imperative API to
manipulate it. `npm add` is another simple example of an imperative interface to
a declarative configuration (`packages.json`).

Adam introduced the notion of "infrastructure as a model", instead of code, to
which code would only be one possible interface. Which reminded me of similar
internal discussion at Tweag about
[projectional editors](https://martinfowler.com/bliki/ProjectionalEditing.html),
which is in fact the very same idea but applied to programming in general.
Textual code is just one view, but other views -- such as a graphical interface --
of the abstract representation of a program are also possible, and more
desirable for some tasks (refactoring, renaming, diffing, visualizing, ...).
Typically, a diagrammatic view is often better than text at clarifying
the relations and dependencies between the entities being managed (e.g.
Terraform resources). To each task, its projection?

## Nix is getting stronger every year

[Ron Efroni][ron-efroni], from Flox, often makes the half-joke that the number of
GitHub stars on the Nix repos are following Moore's law: they double every two
years. So far, he's mostly right.

I was impressed by the number of people flooding the Nix DevRoom at FOSDEM (the
pictures at the beginning of this post), queuing in the corridor to wait for a
free spot.

At CfgMgmtCamp, Bryan's lightning talk on Nix took place on the main track
during the very first morning. The (huge) auditorium was full, fitting several
hundred people. When he asked who knew about Nix, I expected a solid 50 people
to raise their hand. More than half, probably around two third of the audience,
raised their hand!

## Nix is vibrant

A lot of cool things are happening in the Nix world. The ecosystem is evolving
at a fast pace. The NixOS Foundation, itself rather new, is structuring to
handle the change of scale, for example with a dedicated
[Nix maintenance team][nix-team].
[Flox][flox] just released the first version of their product.
[Domen Kozar][domen-kozar] introduced [devenv.sh][devenv-sh], which has gained
traction rapidly. We presented [Nixel][nixel], the starting point of a whole new
user experience for writing Nix.

## Nickel

We got a lot of encouraging words after our various talks on [Nickel][nickel].
The project has been making steady progress in the long run, but the hard and
technical work on the core language, the type system and the semantics of
merging is naturally less palpable.

However, we have reached the point from where we can start (and have started, in
fact) to harvest impactful outcomes, which is exciting. A handful of months' work
on the LSP by our own Gaga Ebresafe and we are already able to provide
completions based on definitions, types and contracts, in-code documentation,
code navigation and typechecking live in the editor, which is quite ahead of the
current Nix experience. Although it's only the beginning, we can already write
basic derivations and modular and overridable development shells using
[Nixel][nixel]. Daniele Palombi is doing an internship to bring incremental
evaluation and self-adjusting computations to the interpreter, for fast
re-evaluation of a configuration upon a small change.

## Concluding words

It's hard to say where the current momentum will bring us. Getting people
excited and aware of a cool technology is not the same as making it established
and used in production. There are many non-technical determining factors, even
irrational ones. Nonetheless, it's clear that infrastructure engineering is
distinguished by the ebullience of its youth. It's an exciting time to be around!

[fosdem-23]: https://fosdem.org/2023/
[cfg-mgmt-camp-23]: https://cfgmgmtcamp.eu/ghent2023/
[fosdem-nix-team]: https://youtu.be/_uPnVNoghzo
[tf-ncl]: https://github.com/tweag/tf-ncl
[fosdem-make-anyone-use-nix]: https://youtu.be/FdxvWFDwgZw
[fosdem-nixel]: https://youtu.be/E4VtKSrSSu8
[nix-team]: https://discourse.nixos.org/t/nix-team-creation/22228
[nickel]: https://github.com/tweag/nickel
[flox]: https://floxdev.com/
[devenv-sh]: https://devenv.sh/
[ron-efroni]: https://mobile.twitter.com/ronefroni
[greenspuns-tenth-rule]: https://wiki.c2.com/?GreenspunsTenthRuleOfProgramming
[nixel]: https://github.com/nickel-lang/nickel-nix
[twitter-adam-jacob]: https://twitter.com/adamhjk?s=20
[twitter-ringo-de-smet]: https://twitter.com/ringods
[domen-kozar]: https://twitter.com/domenkozar
[guillaume-desforges]: https://twitter.com/gdforj
[viktor-kleen]: https://github.com/vkleen
[theophane-hufschmitt]: https://github.com/thufschmitt
[bryan-honof]: https://github.com/bryanhonof
[ryan-lahfa]: https://github.com/RaitoBezarius
[julien-malka]: https://github.com/JulienMalka
[matthew-croughan]: https://twitter.com/matthewcroughan
[pulumi]: https://www.pulumi.com/
[cue]: https://cuelang.org/
