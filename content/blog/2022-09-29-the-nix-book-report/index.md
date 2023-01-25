---
title: Four months into The Nix Book
author: Valentin Gagarin
tags: [nix]
description: "A comprehensive report on what happened in four months of dedicated work on Nix documentation, and what I learned from it."
---

At Tweag, any employee can pitch proposals for internal projects.
This is how we got [content-addressed derivations] in Nix, the formal verification tool [Pirouette][pirouette], or a yearly book budget to support continuous learning.

[pirouette]: https://www.tweag.io/blog/2022-07-01-pirouette-2/
[content-addressed derivations]: https://www.tweag.io/blog/2021-12-02-nix-cas-4/

In May 2022 our Chief Architect Arnaud Spiwack ([@aspiwack](https://github.com/aspiwack)) accepted my pitch for "The Nix Book", agreeing to fund work on improving Nix documentation and onboarding experience for three full months.
This is a comprehensive report on what happened since and what I learned from it.

## tl;dr

The goal of the project was to improve Nix onboarding and documentation experience to increase community growth by writing "The Nix Book".

![Task failed successfully](task-failed-successfully.jpg)

In short:

- Failing fast failed. The project took a different course than intended, and this is probably good.
- We are years away from "The Nix Book".
- Writing is hard – many significant improvements are underway, but it needs time.
- Science: it works. Usability studies, surveys, and expert insights are leading the way. Also: Cognitive biases are lurking everywhere.
- The challenge is social, not technical. A documentation team was formed to tackle this.
- We should focus on enabling occasional contributors and help them grow into maintainers.

Since it is a very long text, each section is designed to be read on its own:

- [Backstory](#backstory): how this project came into being
- [Key results](#key-results): summary of notable contributions
- [Additional results](#additional-results): complete list of concrete changes and ongoing discussions
- [Measuring success](#measuring-success): elaboration on what went well and how to do better in the future
- [Next steps](#next-steps): my personal outlook on the future of Nix

The report in full detail will mostly be interesting for active contributors or those inclined to contributing to the Nix ecosystem.
It may also be interesting for people who are working or want to work on a software project that is in a similar situation as Nix.[^2]

[^2]: Haskell may be a good example. Nix and Haskell, both as software projects and communities, share many features (reliability, expressive power, highly motivated contributors, pluralistic governance) and problems (learning curve, documentation, diverging feature sets). Maybe not coincidentally, there is also a significant overlap of users and contributors between the two.

## Backstory

My work on Nix documentation started in March 2022 by participating in the regular Nix UX meeting. There I encountered Nix contributor and developer at Obsidian Systems John Ericson ([@Ericson2314](https://github.com/Ericson2314)), who set out to document Nix's architecture.
While reviewing his [pull request](https://github.com/NixOS/nix/pull/6420/files) together, we quickly agreed that I should take the editorial lead.
I learned a lot about Nix internals during our review sessions we had over multiple weeks, trying to sort out the facts and to present them in a consistent, readable manner.

The insights we found during these discussions helped refine the idea that got me excited about Nix in the first place:
making software build and run is no different from writing the software to begin with – it’s all just programming.
We can do it effectively or clumsily, depending on the (mental) tools we employ.

I was eager to write down what apparently was there all along, but somewhat hidden between the lines in [Eelco Dolstra’s PhD thesis](https://edolstra.github.io/pubs/phd-thesis.pdf) and [Build Systems à la Carte](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf).
This led to my blog post [Taming Unix with functional programming](https://www.tweag.io/blog/2022-07-14-taming-unix-with-nix/), which illustrates how we can think about building and deploying software in terms of programming language theory.

Most importantly, the principles underlying Nix and many of its mechanisms are amazingly _simple_ – it’s just that most often they are not explained well.
This discrepancy between contents and presentation in the Nix ecosystem always struck me as painful… and unnecessary.

During this time the desire to extend the scope of improving Nix documentation and learning material culminated in an internal pitch to compile and write what I would boldly but tentatively call _The Nix Book_.

The pitch had a fairly broad mission statement[^1]:

[^1]: Most of the original pitch is reproduced in the [Summer of Nix 2022 project proposal][son-project]. The quote is slightly reworded from the [tl;dr] to make it consistent with the proposal's contents.

[son-project]: https://gist.github.com/fricklerhandwerk/78f8f77ff73aef848649f58a34575d0e
[tl;dr]: https://gist.github.com/fricklerhandwerk/78f8f77ff73aef848649f58a34575d0e#tldr

> **Goals**
>
> - Improve the autodidactic Nix onboarding experience to increase community growth
>
> **Tasks**
>
> - Write a book _actually explaining_ Nix and its surrounding ecosystem at a high level of abstraction
> - Overhaul the Nix manual to make it a focused technical reference
> - Improve discoverability of existing learning material
> - Lead a Summer of Nix 2022 project to help achieve this

Success of the project hinged on the extent to which existing material already served its purpose, and whether attempts to improve it would be fruitful.
This report shows what became of "The Nix Book".

## Key results

- [Taming Unix with functional programming](https://www.tweag.io/blog/2022-07-14-taming-unix-with-nix/)

  The article took a while to fully develop, but the result turned out to be highly successful with over 13,000 readers (making it the most viewed Tweag blog post so far) and [a day on the Hacker News front page](https://news.ycombinator.com/front?day=2022-08-05#32355327).

- [Detailed 2022 Nix community survey results](https://discourse.nixos.org/t/2022-nix-survey-results/18983)

  I needed more information than available in the original report, so I reached out to the Nix marketing team to get my hands on the raw data.
  From there, I compiled graphs for all quantitative questions.
  While this only represents a portion of what's going on in the Nix community, it is _evidence_. This is a significant improvement over anecdata and intuition and can be used to more confidently reason through strategic decisions, such as [prioritizing Nix over NixOS for onboarding](https://discourse.nixos.org/t/should-we-give-a-name-to-nix-on-non-nixos/21020/3).

- [Nix documentation team](https://discourse.nixos.org/t/documentation-team-flattening-the-learning-curve/20003)

  Multiple discussions led to a conclusion that a team should be formed to serve as a sounding board for the effort.

  - I had encounters with Nix author Eelco Dolstra ([@edolstra](https://github.com/edolstra)), NixOS contributor and NixOS Wiki maintainer Jörg Thalheim ([@Mic92](https://github.com/Mic92)), Cachix author Domen Kožar ([@domenkozar](https://github.com/domenkozar)), and my colleagues, Matthias Meschede ([@MMesch](https://github.com/MMesch)) and Rok Garbas ([@garbas](https://github.com/garbas)).
  - Encouraged by the simultaneous developments around [reforming the NixOS Foundation](https://discourse.nixos.org/t/expanding-the-nixos-foundation/19929), I had additional exchanges with my colleagues Théophane Hufschmitt ([@thufschmitt](https://github.com/thufschmitt)), Silvan Mosberger ([@infinisil](https://github.com/infinisil)), Tweag's Founder Mathieu Boespflug ([@mboes](https://github.com/mboes)), Flox' CEO Ron Efroni ([@ron](https://discourse.nixos.org/u/ron)), and TVL developer Vincent Ambo ([@tazjin](https://discourse.nixos.org/u/tazjin)).

  Most importantly, the team provides a widely visible point of contact for potential contributors.

- [2022 Summer of Nix documentation stream](https://discourse.nixos.org/t/summer-of-nix-documentation-stream/20351/)

  Led by Matthias ([@MMesch](https://github.com/MMesch)), we organized a part of the Summer of Nix program dedicated to improving documentation. In addition, we drafted a [line-up of presentations], one of which was also on [Nix documentation](https://www.youtube.com/watch?v=WFRQvkfPoDI).

- [How to contribute to documentation](https://discourse.nixos.org/t/how-to-contribute-to-documentation/21028)

  Based on discussions, feedback, the practice of helping contributors, and the need to accommodate Summer of Nix participants’ work, I drafted a contribution guide to documentation in the Nix ecosystem to have as a reference.

- [Usability study](https://discourse.nixos.org/t/usability-studies/21404)

  10 sessions with (absolute or relative) Nix beginners of different software development backgrounds quickly produced some observations:

  - People love control and reproducibility.
  - Developers just want to get things done and do not care how it works.
  - Engineers usually care most about one specific programming language or framework.
  - People do not read, only skim.
  - [nixos.org](http://nixos.org) navigation often does not meet user needs.
  - Information about the Nix ecosystem is perceived as being highly dispersed and disorganized.
    Confusion and disorientation quickly kicks in and often results in “tab explosion”.
  - The learning curve is perceived as extremely steep.
  - The Nix language is easy for Haskell users, and obscure to almost everyone else without appropriate instructions.

- [Nix language tutorial](https://github.com/NixOS/nix.dev/pull/267/)

  Identified as the highest-value objective by Rok ([@garbas](https://github.com/garbas)), based on a [comprehensive comparison of existing Nix language tutorials](https://github.com/NixOS/nix.dev/issues/288#issuecomment-1210004159), and already partially validated by testing it with beginners, it should hopefully become the centerpiece of future efforts to learn and teach Nix.

- No Nix Book this year

  As originally envisioned and [agreed upon by the Nix documentation team](https://github.com/NixOS/nix.dev/issues/285#issuecomment-1198131047), work should continue based on [nix.dev](http://nix.dev) and [nixos.wiki](http://nixos.wiki).
  Contrary to what was originally envisioned it should happen in a strictly incremental fashion, slowly migrating material towards more curated resources and as close to the source code as possible.

[line-up of presentations]: https://youtu.be/h8hWX_aGGDc?list=PLt4-_lkyRrOMWyp5G-m_d1wtTcbBaOxZk

### Additional results

<p>
<details><summary>Contributions to merged pull requests</summary>

- [NixOS/nix#6862](https://github.com/NixOS/nix/pull/6862) docs/flake-update: fix example
- [NixOS/nix#6863](https://github.com/NixOS/nix/pull/6863) manual: remove "Writing Nix Expressions" chapter
- [NixOS/nix#6907](https://github.com/NixOS/nix/pull/6907) reword description of language properties
- [NixOS/nix#6968](https://github.com/NixOS/nix/pull/6968) do not render relative links in help pages
- [NixOS/nix-pills#200](https://github.com/NixOS/nix-pills/pull/200) Add EPUB link
- [NixOS/nix.dev#294](https://github.com/NixOS/nix.dev/pull/294) update sponsor sidebar
- [NixOS/nix.dev#301](https://github.com/NixOS/nix.dev/pull/301) update list of recommended Nix language tutorials
- [NixOS/nix.dev#302](https://github.com/NixOS/nix.dev/pull/302) update site description
- [NixOS/nix.dev#306](https://github.com/NixOS/nix.dev/pull/306) Update [glossary.md](https://nix.dev/glossary)
- [NixOS/nix.dev#308](https://github.com/NixOS/nix.dev/pull/308) fix rendering issue with nested markdown
- [NixOS/nix.dev#313](https://github.com/NixOS/nix.dev/pull/313) fix typo
- [NixOS/nix.dev#315](https://github.com/NixOS/nix.dev/pull/315) update installation instructions
- [NixOS/nix.dev#316](https://github.com/NixOS/nix.dev/pull/316) fix git version in example
- [NixOS/nixos-homepage#881](https://github.com/NixOS/nixos-homepage/pull/881) add redirects to accommodate changes in Nix manual
- [NixOS/nixos-homepage#885](https://github.com/NixOS/nixos-homepage/pull/885) update documentation team info
- [NixOS/nixos-homepage#889](https://github.com/NixOS/nixos-homepage/pull/889) Nix language manual redirect for all versions
- [NixOS/nixos-homepage#891](https://github.com/NixOS/nixos-homepage/pull/891) NixOS -> Nix in page titles
- [NixOS/nixos-summer#35](https://github.com/NixOS/nixos-summer/pull/35) fix formatting and typos
- [NixOS/nixos-summer#36](https://github.com/NixOS/nixos-summer/pull/36) Add deployment story for simple Jitsi Meet server
- [NixOS/nixos-summer#37](https://github.com/NixOS/nixos-summer/pull/37) Fix jibri option names
- [NixOS/nixos-summer#39](https://github.com/NixOS/nixos-summer/pull/39) Improve design and accessibility
- [NixOS/nixos-summer#40](https://github.com/NixOS/nixos-summer/pull/40) videos: Replace single videos with playlist
- [NixOS/nixos-summer#42](https://github.com/NixOS/nixos-summer/pull/42) Highlight console output differently
- [NixOS/nixpkgs#184848](https://github.com/NixOS/nixpkgs/pull/184848) doc: Add anchor to Recursive attributes in `mkDerivation`
- [NixOS/nixpkgs#185069](https://github.com/NixOS/nixpkgs/pull/185069) Correct instructions to obtain a hash for git repos
- [NixOS/nixpkgs#185150](https://github.com/NixOS/nixpkgs/pull/185150) fetchgit: allow disabling cone mode for sparse checkouts
- [NixOS/nixpkgs#188805](https://github.com/NixOS/nixpkgs/pull/188805) doc: specify that `longDescription` should be Markdown
- [NixOS/nixpkgs#189566](https://github.com/NixOS/nixpkgs/pull/189566) doc: Clarify default value of sourceRoot
- [NixOS/nixpkgs#189649](https://github.com/NixOS/nixpkgs/pull/189649) doc/languages-frameworks/rust: fix typo

</details>
<details><summary>Contributions to unmerged pull requests</summary>

- [NixOS/nix#7006](https://github.com/NixOS/nix/pull/7006) manual: generalize anchor redirects
- [NixOS/nix#6870](https://github.com/NixOS/nix/pull/6870) doc/manual: define {local,remote} store, binary cache, substituter
- [NixOS/nix#6877](https://github.com/NixOS/nix/pull/6877) Greatly expand architecture section, including splitting into abstract vs concrete model
- [NixOS/nix#6906](https://github.com/NixOS/nix/pull/6906) add syntax overview from NixOS manual
- [NixOS/nix#6934](https://github.com/NixOS/nix/pull/6934) Revert #6420 "Document what Nix _is_"
- [NixOS/nix#6953](https://github.com/NixOS/nix/pull/6953) Update install-systemd-multi-user.sh
- [NixOS/nix#6969](https://github.com/NixOS/nix/pull/6969) refactor rendering command documentation to markdown
- [NixOS/nix.dev#307](https://github.com/NixOS/nix.dev/pull/307) Bring anti-patterns more inline with nixpkgs contributing guide
- [NixOS/nix.dev#311](https://github.com/NixOS/nix.dev/pull/311) add target audience and chapter structure to contribution guide
- [NixOS/nixos-common-styles#17](https://github.com/NixOS/nixos-common-styles/pull/17) use Nix instead of NixOS in navigation bar
- [NixOS/nixos-homepage#882](https://github.com/NixOS/nixos-homepage/pull/882) make nix.dev official
- [NixOS/nixos-homepage#883](https://github.com/NixOS/nixos-homepage/pull/883) make NixOS Wiki official
- [NixOS/nixos-summer#38](https://github.com/NixOS/nixos-summer/pull/38) add deployment story for lecture series infra
- [NixOS/nixos-summer#41](https://github.com/NixOS/nixos-summer/pull/41) Create blogpost 'callPackage, a tool for the lazy'
- [NixOS/nixos-summer#43](https://github.com/NixOS/nixos-summer/pull/43) Allow multiple authors for one post
- [NixOS/nixpkgs#186764](https://github.com/NixOS/nixpkgs/pull/186764) remove Nix language syntax summary
- [NixOS/nixpkgs#187498](https://github.com/NixOS/nixpkgs/pull/187498) submitting-changes.chapter.md: Suggest adding cosmetic changes to .git-blame-ignore-revs
- [NixOS/nixpkgs#188478](https://github.com/NixOS/nixpkgs/pull/188478) partialzip-rs, fetchPartial: init
- [NixOS/nixpkgs#189241](https://github.com/NixOS/nixpkgs/pull/189241) doc/contributing: enforce full commit hashes on github
- [NixOS/nixpkgs#191378](https://github.com/NixOS/nixpkgs/pull/191378) coding-conventions.chapter.md: update to account for #89885

</details>
<details><summary>Ongoing discussions</summary>

- [NixOS/nix-book#25](https://github.com/NixOS/nix-book/issues/25) migrate to nix.dev
- [NixOS/nix.dev#275](https://github.com/NixOS/nix.dev/issues/275) establish nomenclature
- [NixOS/nix.dev#277](https://github.com/NixOS/nix.dev/issues/277) decide what is a project in the Nix ecosystem
- [NixOS/nix.dev#279](https://github.com/NixOS/nix.dev/issues/279) clarify NixOS Wiki administration
- [NixOS/nix.dev#280](https://github.com/NixOS/nix.dev/issues/280) establish writing standards
- [NixOS/nix.dev#281](https://github.com/NixOS/nix.dev/issues/281) teaching Nix 3 CLI and Flakes
- [NixOS/nix.dev#284](https://github.com/NixOS/nix.dev/issues/284) strategy to bring together existing material
- [NixOS/nix.dev#285](https://github.com/NixOS/nix.dev/issues/285) name and place for central learning resource
- [NixOS/nix.dev#287](https://github.com/NixOS/nix.dev/issues/287) project domain name
- [NixOS/nix.dev#288](https://github.com/NixOS/nix.dev/issues/288) Nix language tutorial
- [NixOS/nix.dev#289](https://github.com/NixOS/nix.dev/issues/289) versioned guides
- [NixOS/nix.dev#290](https://github.com/NixOS/nix.dev/issues/290) unified product and story
- [NixOS/nix.dev#296](https://github.com/NixOS/nix.dev/issues/296) scope of contents and team efforts
- [NixOS/nix.dev#310](https://github.com/NixOS/nix.dev/issues/310) Accommodate divnix/nix-book
- [NixOS/nixos-homepage#858](https://github.com/NixOS/nixos-homepage/issues/858) Commercial support: `nix develop` output rendered on web site
- [NixOS/nixos-homepage#880](https://github.com/NixOS/nixos-homepage/issues/880) get usage statistics
- [NixOS/nixos-homepage#892](https://github.com/NixOS/nixos-homepage/issues/892) Merge `nixos-common-styles` repository
- [NixOS/nixos-search#524](https://github.com/NixOS/nixos-search/issues/524) Show `nix-shell` installation by default
- [NixOS/nixos-search#525](https://github.com/NixOS/nixos-search/issues/525) Render `meta.longDescription` from markdown
- [NixOS/nixos-search#527](https://github.com/NixOS/nixos-search/issues/527) Show link to package details as static text
- [NixOS/nixpkgs#180492](https://github.com/NixOS/nixpkgs/issues/180492) Define platform support tiers (RFC 36)
- [NixOS/nixpkgs#184971](https://github.com/NixOS/nixpkgs/issues/184971) CONTRIBUTING.md and `nixpkgs` manual contributing section are redundant
- [NixOS/nixpkgs#184982](https://github.com/NixOS/nixpkgs/issues/184982) links from manuals to source code
- [NixOS/nixpkgs#188392](https://github.com/NixOS/nixpkgs/issues/188392) Document how to package `.deb` or `.rpm`
- [kristapsdz/lowdown#105](https://github.com/kristapsdz/lowdown/issues/105) Feature request: add option to ignore relative links
- [kristapsdz/lowdown#106](https://github.com/kristapsdz/lowdown/issues/106) Render nroff block quotes without empty lines
- [nix-community/awesome-nix#123](https://github.com/nix-community/awesome-nix/issues/123) alphabetic ordering does not tell support status
- [nix-community/awesome-nix#124](https://github.com/nix-community/awesome-nix/issues/124) centralize collection of resources and recommendations
- [nix-community/awesome-nix#125](https://github.com/nix-community/awesome-nix/issues/125) summaries should should be factual

</details>
</p>

## Measuring success

Some of the following insights appear almost trivial in retrospect.
Yet, contributing to a major open source project is a set of skills on its own, one most people don't learn at university or work.

As a high-level summary, one could say:

> 1\. Do your homework first.

This presents a dilemma:

- Becoming competent at making improvements requires time which will not be available to actually making those improvements.
- Trying to make improvements without the necessary competence requires maintainers' time, which was highly limited to begin with, or may backfire by making matters worse.

Therefore, what follows is an attempt to share experience, and, based on that experience, proposals to deal with the above dilemma.

Again, as a high-level summary:

> 2\. Make it easier for others to do _their_ homework.

### Failing fast

Following best practices, the pitch contained abort criteria to avoid the [sunk-cost fallacy](https://en.m.wikipedia.org/wiki/Sunk_cost#Fallacy_effect):

> 1. until 2022-05-31: Summer of Nix 2022 project proposal rejected by organization team (excluding Tweag staff)
> 2. until 2022-05-31: not enough participants to cover planned tasks
> 3. until 2022-07-15: preliminary questionnaires demonstrate satisfactory effectiveness of existing material for defined learning goals
>    - may leave room for conducting targeted incremental improvements
> 4. until 2022-07-15: setting up surveys and collecting results shows that timeline is not realistic
> 5. until: 2022-07-31: elaborating outline and surveying existing material shows that timeline is not realistic
>    - may leave room for cutting scope

While the Summer of Nix proposal was accepted (1) and user testing showed desperate need for improvements (3), it was not clear how many Summer of Nix participants would actually want to focus on documentation (2) until the end of July when the program had started.
We estimated that of 20 participants, multiple would contribute to documentation in one way or another.
It later turned out that only one would work on documentation specifically, and a few others would decide to write a [blog post about their ongoing work].

At this point it was already quite evident that a more incremental approach would be inevitable.
Both the evidence (4) and envisioned scope for the book (5) were unambiguous in that there was an order-of-magnitude divide between the possible and the desired.

By the beginning of July, the newly founded Nix documentation team decided to focus on more immediate problems and only [briefly discussed "The Nix Book" as a long-term vision](https://github.com/NixOS/nix.dev/issues/284#issuecomment-1198130213).
Cutting scope occurred naturally: from now on, the focus would be on just the part up to teaching the Nix language.

[blog post about their ongoing work]: https://summer.nixos.org/blog/

Around that time I updated Mathieu ([@mboes](https://github.com/mboes)) on the current state and changed strategy, where we re-iterated on the cost-benefit estimate of spending internal budget and project goals – improving Nix onboarding and increasing Tweag’s visibility.

Note that I originally estimated the project to take six months, not three or four.
While the changed schedule partially invalidated the relation between estimate and objectives, the time constraints forced me to keep even stricter focus on priorities.
At the same time, clearly not being able to deliver on the vision due to the problem size removed most pressure with regard to producing specific artifacts and, thus, any temptation to cut corners.

Instead of spending three months, I spent four months.
High time to evaluate.

### What went well

#### Community building

Taking the time to listen and talk to people helped a lot with understanding the problem space and honoring [Chesterton’s fence](https://en.m.wikipedia.org/wiki/G._K._Chesterton#Chesterton's_fence).
Getting key people on board helped to build commitment and momentum as well as weed out bad ideas through critical discussion.
It also meant that actual changes have to go through at least partial consensus, which requires each of those changes to be fairly small.

In the original pitch, I assumed that I would have to rely on [Aaron Swartz’ Wikipedia authorship principle](http://www.aaronsw.com/weblog/whowriteswikipedia) (which suggests that most open source contributors engage only occasionally and typically work on cosmetics, while the substance is provided by regulars). The assumption turned out to be true and Swartz' findings were confirmed again.

Providing a central point of contact, naming directly responsible individuals, contacting potential contributors immediately, and actively setting examples appears to have resulted in a modest but noticeable increase of attention towards documentation issues, as well as many small and multiple significant pull requests.

#### User studies

Immediately starting with [user studies](https://discourse.nixos.org/t/usability-studies/21404) quickly helped pinpoint concrete user needs and some obvious issues, and either validated or debunked some preconceptions that (at least as far as I perceived) had been discussed mostly based on intuition and oral history.
It will still take more time to sort through them again and match the notes to GitHub issues and pull requests. This is to help (1) maintainers to keep track of what can be done and (2) whoever consults the session notes to keep up with what has already been resolved.
These studies should continue if possible: at the least, to validate new material (as for example with the new [Nix language tutorial](https://github.com/NixOS/nix.dev/pull/267/)) and measure the reduction in onboarding time after improvements have been implemented.

#### Increasing visibility

Publishing regular updates such as meeting notes, participating in ongoing discussions, and linking to relevant posts, issues, and pull requests seem to have increased awareness of the trajectory of the Nix ecosystem and of what Tweag is doing.

_Getting involved consistently and staying active in a constructive manner helped a lot._

All feedback from within the community so far has been positive.
Beginners and regular users found the changes in organization and the specific documentation work we got done helpful.
Expert users and contributors are vocally happy about the efforts.
(At least those who I did not annoy by nagging too much about phrasing and terminology.)

Note: there are also outside voices on the internet who doubt that this (or any) effort will lead to a significant improvement in terms of user experience.

#### Overview and visualization

Presenting high-level summaries and diagrams at the very beginning of introducing people to various topics was perceived as very helpful, both in the usability tests as well as in multiple informal interactions.
It increases the readers' confidence, and allows them to set realistic expectations before going into details.
This is supported by [scientific evidence](https://www.lesswrong.com/posts/mAdMkFqWzbJRB544m/book-review-how-learning-works#Strategies_).

_I think there should be many more such overviews at the top of learning resources and reference materials._

Examples:

- [Nix store architecture](https://github.com/NixOS/nix/pull/6420/files#diff-9acdd64993877588f8c878b2c94134956204266532d2c9685abb589fd042efa4R13-R29)
- [Nix concept map](https://github.com/NixOS/nix/pull/6420/files#diff-485e9f03f514869c78ebfa54354bd03b1fd0ba9ceb817578f31b573be303b5f4R10-R25)
- [Modeling files and processes in a purely functional fashion with Nix](https://github.com/NixOS/nix/pull/6420/files#diff-485e9f03f514869c78ebfa54354bd03b1fd0ba9ceb817578f31b573be303b5f4R111-R141)
- [Overview of using the Nix language in practice](https://github.com/NixOS/nix.dev/pull/267/files#diff-c08ceee24137f72990af94b2e6a5eb487ec1ec767437a6082ba06d9d4244b835R29-R39)
- [NixOS Wiki: Data flow of overlays](https://nixos.wiki/wiki/Overlays#Data_flow_of_overlays)

A particularly inspiring example of making complex problems accessible through visuals is [Life cycle of a Poetry project](https://toraritte.github.io/poetry-intro/#life-cycle-of-a-a-hrefhttpspython-poetryorg-titlethe-poetry-websitepoetrya-project) by Attila Gulyas ([@toraritte](https://discourse.nixos.org/u/toraritte)).

#### Brute-force analysis

Nix, Nixpkgs, and NixOS have a multitude of features and obscure corner cases which are barely, badly, or not at all documented.
There are many resources of varying quality which have overlapping contents.
The only way to get on top of things, apart from experimentation or diving into source code, often turns out to be research and an exhaustive analysis of prior art:
to avoid the [Dunning-Kruger effect](https://en.m.wikipedia.org/wiki/Dunning%E2%80%93Kruger_effect) (”I can easily do better.”), to account for [Chesterton’s fence](https://en.m.wikipedia.org/wiki/G._K._Chesterton#Chesterton's_fence) (”This is not good and can be removed/must be changed.”), and finally _to simply get things right_.

> Incorrect documentation is often worse than no documentation.
>
> — attributed to Bertrand Meyer

Improving over the current state is only reliably possible if the current state, and how it came about, is known.

Despite thorough [initial overview](https://gist.github.com/fricklerhandwerk/78f8f77ff73aef848649f58a34575d0e#tutorials) it took me a while to even stumble upon relevant materials after sitting down again and again to dig through countless Discourse threads and NixOS Wiki pages:

- [Nix documentation task force](https://discourse.nixos.org/t/nixcon-2019-documentation-task-force-meeting-outcome/4551) (NixCon 2019)
- [Make Nix friendlier to beginners](https://media.ccc.de/v/nixcon2015-3-MakeNixfriendlierforBeginners#t=202) (NixCon 2015)
- [Reading the Nix language](https://youtu.be/hbJkMl631FE?t=1572) (NixCon 2019)

Oftentimes, such research reveals underlying problems (as opposed to mere symptoms) or what caused those problems in the first place.

This kind of due diligence takes a lot of time and concentration, and can be very challenging work.
There is also an enormous overhead of preserving insights for the future.
However, I am convinced that if no one else has to repeat the effort, the results are worth it in long run.
Each time I tried the brute-force approach, the quality of work turned out to be convincing as opposed to my other, less well-prepared proposals, which (rightfully) received substantial headwind.

#### Scientific method

Leveraging insights from scientific evidence (unsurprisingly) proved to be highly effective, and as a side effect removed most uncertainty about procedure.

The most important resources which shape my day-to-day documentation work:

- [How Learning Works](https://www.lesswrong.com/posts/mAdMkFqWzbJRB544m/book-review-how-learning-works)

  Practical advice on effective teaching and learning, backed by broad and deep evidence.

  The best-written and probably most important book I have ever read.

- [Diátaxis](https://diataxis.fr/)

  A framework for structuring software documentation around user needs.

- [Plain language guidelines](https://www.plainlanguage.gov/guidelines/)

  A set of guidelines to write clearly in English.

This is meant quite literally – I refer to each of them every day, one way or another.

Many thanks to my colleague Andrea Bedini ([@andreabedini](https://github.com/andreabedini)) for recommending _How Learning Works_ by Ambrose, et al. – it keeps changing my life to the better.
I have heard multiple times that [Visible Learning](https://visible-learning.org/) is the state of the art in learning science.
For teaching in the context of software development, I also recommend the ideas behind [Software Carpentry](https://www.youtube.com/watch?v=1e26rp6qPbA).

### Lessons learned

#### Gather more context in the beginning

Looking beyond one's own backyard by collecting testimonials from other software projects would have been helpful to more quickly see the big picture and, as a result, identify the most pressing, underlying issues.
While I talked to many Nix experts and did much research on internal proceedings, I spent very little time on how other projects approached similar problems and which strategies were successful.

Recommendation:

> Talk to people who solved similar problems in different contexts.

Example:

Ron ([@ron](https://discourse.nixos.org/u/ron)) interviewed several leaders of open source foundations when preparing the [NixOS Foundation reform](https://discourse.nixos.org/t/expanding-the-nixos-foundation/19929), which surfaced very helpful, non-obvious insights – and also tales of caution.

#### Avoid planning fallacy

The idea one person could get even close to a complete book in less than half a year was, while not fully serious, quite presumptuous, and a pathological case of [planning fallacy](https://en.m.wikipedia.org/wiki/Planning_fallacy).
It was not evident to me in the beginning, but the ecosystem is simply too large, the problems too numerous, and the high-level tasks too big to tackle at once.

Things are moving, but they are moving very, very slowly.
Writing the architecture documentation chapter, which covers at most 60% of the topics that it would need to be considered comprehensive, took 8 weeks of wall-clock time.
Writing a Nix language tutorial took 4 weeks of wall-clock time.
This is extremely frustrating, but unavoidable due to lack of prior experience, some _essential_ complexity, much _accidental_ complexity, external factors considered in the other sections, and – of course – planning fallacy.

Recommendation:

> Find out how much time it took to produce comparable results, and take it seriously.

Example:

The [nix.dev](http://nix.dev) version history goes back to [2016](https://github.com/NixOS/nix.dev/commit/7c4daba3ad01f2646f02f60a652fed9ca4508573), and has been actively developed on the side since May 2020.
In the time-span of about 2 years, 12 original articles were produced, i.e., one article per two months.
After the fact, this matches my own experience very closely.

#### Identifying low-effort high-impact tasks

The Nix ecosystem is large and fragmented.
There are many people involved, each with different – and sometimes diverging – interests.
It is not enough to ask users what they _need_, because they will usually instead answer with what they _want_.

I spent some time dabbling at working on assorted issues before converging on a more systematic approach.

Recommendation:

> Take enough time to identify issues (user studies) and sort them by effort-impact ratio (brute-force analysis) before delving into work.

Examples:

- The “Writing Nix Expressions” chapter in the Nix manual was a pretty bad introduction to the Nix language, throwing many people off early on (including myself, back in the day).
  Due to the sheer amount and length of other Nix language tutorials, it was not clear before actually working through all of them that this specific one really did not contain anything uniquely valuable.
  [Removing](https://github.com/NixOS/nix/pull/6863) the section was quick and painless.
- The [Nix Pills](https://nixos.org/guides/nix-pills/) cover advanced topics and have been reported to be confusing to beginners (including myself, back in the day) many times.
  The problem was that they were touted as beginner material in many places.
  [Reordering recommendations and rewording the description](https://github.com/NixOS/nixos-homepage/pull/855) appears to have helped substantially.
  (Although better guidance is still needed, see below.)

#### Make incremental changes

My colleague Clément Hurlin ([@smelc](https://github.com/smelc)) already wrote about this in [Getting Things Merged](https://www.tweag.io/blog/2022-04-14-getting-things-merged/#the-technical-aspect-of-getting-things-merged). In an open source project's community, where essentially everyone can be considered a volunteer, reviewers’ time is even more limited.
There is no chance of getting a large pull request merged without having close allies among maintainers – people naturally won’t do more than take a glance, it’s too much work.

This imposes a significant additional cost on authoring pull requests, which has to be taken into account.
One has to keep the big picture in mind while only presenting the next obvious step towards a vision.
So far even merging [a rendering of the vision](https://github.com/NixOS/nix.dev/pull/265/files?short_path=eca12c0#diff-eca12c0a30e25b4b46522ebf89465a03ba72a03f540796c979137931d8f92055) appears to be too large a task.

On the other hand, small changes keep cognitive load manageable and allow for easier switching between tasks: simply because small tasks get finished quickly.

Recommendation:

> Never stop asking the question, "What is the smallest possible change required for a tangible improvement?"

Examples:

- Unfortunately the pull request documenting Nix architecture still [has not been merged properly](https://github.com/NixOS/nix/pull/6934).
  Therefore, it is not yet visible in the Nix manual.
  Although it is limited to the parts I felt confident publishing, it is a large addition.
  It will have to be split up into multiple parts to ease review.
- The [Nix language tutorial](https://github.com/NixOS/nix.dev/pull/267) is not finished yet.
  It takes 1-2 hours just to _work_ through it – not to mention the time needed to _make a review_.
  Good progress so far was only possible due to Silvan Mosberger's ([@infinisil](https://github.com/infinisil)) persistent involvement and patient reviews.

#### Focus on the basics first

The usability study was particularly helpful in demonstrating the gap between what we may wish to have and what people actually need to succeed.

The problems people got stuck on were often trivial, such as not understanding a term or not finding a crucial bit of information to continue.
This could be addressed with much less effort than required for creating full-blown tutorials or meticulously working out precise reference material.

Recommendations:

- Reorganize the [nixos.org](http://nixos.org) web site.
- Establish materials to help guide beginners across different problem domains in the ecosystem.
- Make improving documentation more appealing for contributors.
- Provide guidance to navigate each source repository.

#### Enable contributors

I was expecting to adjust the original goals and targets during the process, since observing actual users dealing with the material would unfold further requirements to guide my work.

However, the usability study results, as well as my own experience, showed that it is much more difficult to improve upon the overall situation than it originally appeared.

I believe coordinated incremental improvements will be more effective than having a few people attack large problems at full-steam.
It's not just that the sheer number of entangled issues is overwhelming, but also that Nix experts are subject to the [curse of knowledge].
Nix beginners, seeing our work with fresh eyes, have time and again proven invaluable allies by pointing out and often themselves addressing problems that tend to become invisible after getting used to them.

[curse of knowledge]: https://en.m.wikipedia.org/wiki/Curse_of_knowledge

Systematically building momentum, setting examples, creating a culture, and enabling volunteers to contribute appears much more promising and is already bearing fruit.

Recommendation:

> Focus future efforts on enabling contributors, by providing comprehensive guidance into the process of developing the Nix ecosystem.

This is not to say to stop improving the onboarding process for beginners.
Becoming a contributor should instead be considered part of that process.

Examples:

<p>
<details>
<summary>More blog posts, tutorials, guides, and reference documentation are in the making.</summary>

- [NixOS/nixos-summer#36](https://github.com/NixOS/nixos-summer/pull/36)
- [NixOS/nixos-summer#38](https://github.com/NixOS/nixos-summer/pull/38)
- [NixOS/nixos-summer#41](https://github.com/NixOS/nixos-summer/pull/41)
- [NixOS/nix.dev#274](https://github.com/NixOS/nix.dev/pull/274/)
- [NixOS/nix.dev#317](https://github.com/NixOS/nix.dev/pull/317)

</details>
<details>
<summary>The source code of beginner-oriented materials is opaque to outsiders.</summary>

- [nixos.org is labyrinthine](https://github.com/NixOS/nixos-homepage/issues/833) even after hours of working with it.
- [nix.dev](http://nix.dev) performs a markdown to ReST conversion, but provides [no hints](https://github.com/NixOS/nix.dev/tree/d32652b94fcc1ed4336f3a8ebf7fc5678b48c1eb) about this or how to work with it.
- The [Nix manual](https://nixos.org/manual/nix/stable/) is compiled through an involved process, which is in itself undocumented and sometimes [obscure on its own](https://github.com/NixOS/nix/pull/6969).

</details>
</p>

## Next steps

I agree with Tweag's VP of Engineering Steve Purcell's ([@purcell](https://github.com/purcell)) and Flox' CEO Ron Efroni's ([@ron](https://discourse.nixos.org/u/ron)) assessment that Nix is at the inflection point towards a trajectory to mass adoption.
At the same time I fear that we as a community are not ready for the corresponding influx of users and potential contributors – both in terms of documentation and organization.

Teaching in person is, of course, the most effective way of getting people into Nix, and Tweagers enjoy this rare privilege by default.
But it does not scale.
To handle more than 0.5%[^3] of the world’s [24 million software developers](https://www.developernation.net/developer-reports/de20), we have to leverage that most of them are self-taught.

[^3]: This number is based on informal estimates that at most 100,000 people have heard of or are using Nix.

My current estimate to get Nix documentation and learning material into a shape that allows for growing the community to scale is on the order of multiple person-years.
For comparison, this is how long it took to get flakes and the new CLI “almost ready”.
Or [how long it took to create “The Rust Book”](https://github.com/rust-lang/book/graphs/contributors), which was [started end of 2015](https://github.com/rust-lang/book/commit/63492d42444be84982f656c8b1430f99f8f26f98) and saw intense development activity for over two years.

Taming and helping to navigate the (mostly accidental) complexities of the Nix ecosystem continues to be a huge undertaking.
It requires dedicated work and coordination, which is mostly about learning and teaching, communication, and social problems.
Having been in both roles, I am more than ever convinced that volunteers are not sufficient to handle this on their own, and that we need more paid regular contributors.

With the new NixOS Foundation board and its corporate backing, in principle we have all the means to systematically grow the Nix pie for everyone.
I am looking forward to the Foundation's board delivering on their [promise to develop a roadmap] and to enable teams by providing organizational structure, the necessary permissions, and leadership's attention.
In my opinion, part of this endeavour should be a long-term funding scheme for ongoing development and maintenance targeting key objectives, including improving onboarding and documentation.

[promise to develop a roadmap]: https://discourse.nixos.org/t/nix-board-meeting-minutes-september-9-2022/21814

I would love to help with setting this up.
And, while I don’t care who does the job as long as it’s being done, I thoroughly enjoyed doing what I did in the past four months, and would just as passionately continue if there was a possibility.
