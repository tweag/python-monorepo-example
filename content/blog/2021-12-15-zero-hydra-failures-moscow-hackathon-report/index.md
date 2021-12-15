---
title: "21.11 Zero Hydra Failures Moscow Hackathon: Report and Suggestions"
shortTitle: "21.11 Zero Hydra Failure Moscow report"
author: Alexander Bantyev
description: "Details about the 21.11 Nix Moscow Hackaton, and some suggestions for organizers of similar events."
tags: [nix]
---

Twice a year -- in May and November, nixpkgs maintainers and
contributors get together to fix as many build failures as possible
for the new release. The event is, as per tradition, called Zero Hydra
Failures, or ZHF for short.

This year, me and fellow hacker [cab404](https://github.com/cab404)
had organized a hackathon to help the cause and spread the Nix love
around. The basic premise was, quoting cab404:

> Go—go—go!

We wanted to fix as many builds as possible, right before the
branchoff. Fixing the broken builds would allow NixOS 21.11 to have a more complete package
set.

The main point of this post is to share the experience with people
looking to organize similar events.

## Preparation

Due to the current lockdown in Moscow, we
weren't able to decide whether the hackathon would happen at all until
about a week before the last possible date (the branchoff). This
limited our ability to advertise the event in time for all potentially
interested people to be able to join. Despite this, we tried our best to
advertise the event using the following channels:

- We created a website, both in
  [English](https://nixhax.github.io/zhf-21.11/index.en.html) and
  [Russian](https://nixhax.github.io/zhf-21.11);
- We sent announcements to the [Russian NixOS community telegram
  group](https://t.me/ru_nixos),
  [Discourse](https://discourse.nixos.org/t/zhf-hackathon-celebration-event/16079)
  and Matrix;
- cab404 announced the event in his hackathon group chat.

### Improvements

Obviously, we should have planned the event earlier. A week's notice
is way too little for many people, especially on a Friday. In hindsight,
we could have anticipated that the branch-off was going to be late and ran
the event on Saturday.

## Setup

The event took place in [undefspace](https://undef.club), both
physically and virtually (via
[workadventu.re](https://workadventu.re)).

We had provided lots of tea and snacks to physical participants, and a
build machine to speed up builds.

### Improvements

First of all, the hackerspace was quite small. All of us managed to
fit, just. If the event attracted any more people, it could become
problematic. Plan your capacity!

Another problem was with the build server setup: while it was running,
we didn't have time to provide people with actual instructions on
using it, so a lot of time was spent building packages on slow laptop
CPUs instead of the significantly powerful build machine. The theme of
lack of instructions limiting the impact of the event deserves a
separate section.

## Hacking

As happens, most participants came in late. This meant that the spoken
instructions I gave at the beginning weren't heard by everyone, and it
resulted in a slowdown in the hacking while people were trying to
understand the process.

Another issue was that the written instructions on the website were
aimed at mostly at experienced contributors, but most of participants
didn't have much nixpkgs experience -- in fact, two of them made
their first open-source contributions during the hackathon!

### Improvements

The takeaway here is that a lot of attention should be given to making
instructions for the hacking you're going to do -- make sure to have
something for newcomers as well as experienced hackers.

## Results

### Friends were made

![Picture from the hackathon](./picture.jpg)

### Nix{,OS,pkgs, Flakes} knowledge was shared

One of the best things about in-person hackathons is the ability to
share knowledge and ideas -- and plenty were shared!

- I have learned some neat git-over-ssh tricks from Cab (in
  particular, I was pleasantly surprised that one doesn't need any
  special git server for an ssh remote!). Also, thanks to Alexander I
  know about `genericBuild` which runs all phases as is done inside
  the nix sandbox during a regular nix build. And finally, I have
  confirmed I can install NixOS on a headless box with only the keyboard
  connected!
- Nommy and Alexander explored the wonders of Nix Flakes: learned the
  usage basics (`nix build`, `nix shell`, `nix develop`), wrote their
  first `flake.nix`-es, and used their new knowledge to simplify the
  process of build debugging;
- Anton has refreshed his Nix skills and explored the nixpkgs Haskell
  infrastructure in the process of fixing the mime-strings package.

### And, most importantly, builds were fixed!

In total, 10 PRs were submitted during the hackathon:
https://github.com/NixOS/nixpkgs/pulls?q=is%3Apr+zhfmsk

We fixed too many packages to count manually, and it's not an easy
thing to count programmatically. However, the `openjfx11` fix on
`x86_64-linux` has fixed a lot of Java packages, and other pull
requests typically fixed one or two packages.

## How are we going to improve the next one?

Pick the right time, in advance: we will try our best to arrange the hackathon on a weekend, with at
least two weeks' notice.

Inform people about the build server: a fast build server speeds up the debugging process
significantly. Telling people about it, together with instructions
on setting up a build server, is important.

Provide better instructions for all skill levels: prominently displayed instructions on what exactly people need to
do, together with links to learning materials for novices, should
reduce the need for repeated explanations tête-à-tête, and speed the
hacking significantly.
