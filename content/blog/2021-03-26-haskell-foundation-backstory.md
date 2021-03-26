---
title: "Incubating the Haskell Foundation"
author: "Richard Eisenberg, Tim Sears, and Mathieu Boespflug"
tags: [haskell]
description: "Incubating the Haskell Foundation during 2020."
---

[haskell foundation]: https://haskell.foundation/
[hf-announce]: https://www.youtube.com/watch?v=MEmRarBL9kw

For the three of us, the launch of the Haskell Foundation was one of
2020's few bright spots.

In November 2020, Simon Peyton Jones [announced][hf-announce] the
formation of the [Haskell Foundation]. Conceived during the first
lockdowns of early 2020, the Haskell Foundation is a non-profit
organization aimed at promoting adoption of the Haskell programming
language and bolstering its core infrastructure. The Foundation has
now raised approximately \$500K in cash and financial commitments from
sponsors, recruited a volunteer board of 14 directors and a full-time
staff of two: Andrew Boardman, Executive Director (ED); and Emily
Pillmore, Chief Technology Officer (CTO). Today, the Foundation has
its own point of view, plans, sponsors, and community of volunteers.
It has reached escape velocity. Our personal opinions now or back then
don't matter much, but we thought it might be nice to share the story
of what sparked its inception.

In the end, the Haskell Foundation was launched with input from dozens
of friends of Haskell. What we thought might be like rolling a boulder
uphill turned out to be more like kicking a stone and starting
a rockslide. It seems the Haskell Foundation is an idea whose time had come.

## Inception

[rust]: https://www.rust-lang.org/
[swift]: https://swift.org/
[skills matter]: https://skillsmatter.com/

In early 2020, Simon Peyton Jones got a call from Frank Rodorigo,
a US-based entrepreneur, who was in the process of reviving [Skills
Matter], a community of tech creators, users and adopters. Skills
Matter had run into financial difficulties in 2019, and Frank,
together with his CTO Scott Conley, wanted to make sure Haskell was at
the center of their reboot plans. He hoped to explore some ideas with
Simon about the best ways to help Haskell. One thing led to the next:
under the impetus of the rebooted Skills Matter, we brainstormed about
what extra glue the community might need to bolster the lofty goals
that so many seemed to have.

Over the years, we have been encouraged by the inspiration Haskell has
given to other languages like [Rust], Apple's [Swift] and others. We
even saw Java programmers singing the praises of lazy streams and
anonymous functions. It had always seemed like just a matter of time
before more programmers start wanting to use "The Real Thing". While
we saw some progress in the Haskell ecosystem, with the release of
GHC 8.0, and efforts to eliminate "dependency hell" on the part of the
Stack and Cabal projects, it didn't feel like enough.

Enough for what? If you wanted to use Haskell in production at
a company, you still had to be brave and determined. The evidence was
anecdotal but hard to ignore. It ranged from conference attendees
talking about how complicated Haskell has become, to concerns such
as getting Haddock working with GHC's new type system features, to
the trouble enabling Stack to keep working with Hackage. Pull
requests to some of the core libraries were languishing and many felt
there were problems in those libraries simply going unaddressed.

Worst of all, some of us were aware of companies that had adopted and
then later abandoned Haskell. Those would-be Haskellers faced
a confusing collection of projects and committees, none of whom
themselves felt they had a broad mandate to advance Haskell.

To be sure, there were some people thinking about remedies with
proposals like [Simple Haskell](https://www.simplehaskell.org) and
[Boring
Haskell](https://www.snoyman.com/blog/2019/11/boring-haskell-manifesto).
But we thought more was needed. We started with some goals:

- **An easier on-ramp.** Starting out with Haskell is harder than it should be, with
  a wealth of ways of setting up a Haskell dev machine (some of them
  out-of-date!). Even after this first step, newcomers often land on
  unmaintained wiki pages or other seldom helpful destinations.

- **More inclusivity.** To help the community grow, we wanted the
  occasional alienating post or dismissive comment to be reliably
  addressed.

- **More progress, faster.** We wanted to encourage more innovation
  across the many different projects that comprise Haskell. We also
  wanted to explore ways to help the committees and volunteers that
  make up the Haskell community to channel resources and volunteers to
  where they are needed most, and to ensure that each tool works well
  with others, forming a cohesive whole.

- **Funding.** We knew first-hand that many companies were already
  investing in Haskell to ease their own pain points, but their
  efforts weren't very connected. We wanted to make all of the above
  more feasible with the help of sponsors.

## Gaining momentum

[haskell.org committee]: https://www.haskell.org/haskell-org-committee/
[hf-snoyman-post]: https://www.snoyman.com/blog/2020/12/haskell-foundation/

Having envisioned the outlines of the Haskell Foundation, what next? We wrote those ideas
down in a live shared document we called "the whitepaper" and started
gathering feedback from an ever widening group. We really wanted to
iron out the wrinkles to avoid announcing something that could fall
flat on its face.

As the central architect and developer behind the Haskell ecosystem,
the rest of us thought that the visible involvement and leadership of
Simon Peyton Jones would be essential. Happily, Simon agreed to stay
involved and eventually spent way more time than he had planned.

We still had some worries. Would others consider affiliating?
Were we stepping on anyone's toes? We decided to take a slow and
deliberate concentric rings approach to next reach out to core
committees, then companies and large projects, then influential
community members, to socialize the idea further.

We started with the [Haskell.Org Committee]. Expecting pushback, we
were blown away by how eager the chairperson (Jasper van der Jeugt)
was, and then in turn the rest of the haskell.org members: Ryan
Trinkle, Emily Pillmore, Tikhon Jelvis, Rebecca Skinner, and Alex
Garcia de Oliveira. They all became early and essential participants
in the launch of the Foundation. It seems that the idea resonated
strongly with some of their own ongoing discussions.

We were still a small team at this point -- fewer than ten people,
with Tim Sears as our chief day-to-day organizer. We kept adding
anyone who wanted to help to a bi-weekly video call and kept iterating
the whitepaper. After a short time it looked nothing like the first
draft.

Feeling a bit braver, we then decided to also reach out to companies
and key stakeholders in the community. We met skeptics along the way,
like [Michael Snoyman][hf-snoyman-post] (now on the Foundation Board),
both on the vision and the feasibility. These skeptics' input turned
out to be extremely helpful. Among other things, we used their
feedback to sharpen the Foundation's commitment to transparency. Ryan Trinkle
(Obsidian Systems) soon started playing the role of shadow CFO. Duncan
Coutts and Ben Gamari (Well-Typed) provided valuable input, as did
Neil Mitchell, John Wiegley, Ed Kmett, Simon Marlow and others too
numerous to mention.

It was important to connect with the Core Libraries Committee, the
Hackage Trustees and the GHC Steering Committee. Emissaries were
dispatched. Those groups ranged from amenable to enthusiastic, but
they also asked some thorny questions. Did the community really need
another committee? How would the Foundation differentiate itself?
Could we actually raise money? In private, Simon Peyton Jones asked Tim why he wrote
down a 7-figure sponsorship goal in an early draft of the whitepaper.
Was it realistic? Tim had to admit he wasn't sure, but without funding
the Foundation would never have big impact. Only one way to find
out...

At some point we started calling our informal cabal the HF Working
Group. Eventually the invite list numbered in the dozens, with about
12 or so turning up regularly to our video chats. Scheduling was
rarely a problem, since nobody was traveling - the silver lining of
a terrible pandemic.

## The Foundation escapes quarantine

In August, the Working Group asked itself: "Haven't we socialized this
idea enough yet? Can't we freeze and ratify the whitepaper? Why can't
we just launch?". Just like that we turned a corner. We started
a semi-public outreach effort on the following basis:

- The Foundation would be _non-profit_. Our goal would never be to create
  a consultancy or training company. Our goal would be to promote
  Haskell and related technologies. We would _not_ be selling any
  services.
- The Foundation would be _inclusive_. It would seek input from a variety of
  sources and be community-driven. A goal of the Foundation would be to look
  like the community we want to become.
- The Foundation would be _funded_. We would start with ambitious fund-raising
  goals. Specifically, we wanted to aim for a yearly budget of over
  \$1,000,000. Donations would come from industry and from the general
  Haskelling public.
- The Foundation would have an _executive director_. Acknowledging that we are all
  busy people, well-meaning volunteers simply do not have the
  bandwidth to offer sustained attention to where it is needed.
  Instead, the Foundation would have at least one full-time employee, whose
  day job it is to manage the Haskell community and promote its
  interests.

To our eyes, now informed by the community, this seemed like a winning
formula -- a design that would be able to fix Haskell's problems and
promote the language, while strengthening our community.

The Foundation quickly became the world's worst-kept secret as the Working Group
set a public launch date for November.

## Announcement and bootstrapping

[hf-interim-board]: https://haskell.foundation/en/who-we-are/

Right away there was a new chicken-and-egg problem: we wanted the
Board of Foundation to be drawn from the wider community, and yet we
needed to have _someone_ in charge so we could launch quickly. The HF
Working Group landed on a two-step process. The Foundation would start
with an interim Board and launch. The Board would then both replace
itself and hire an ED to run the organization. The Working Group
identified eight prominent Haskellers pre-launch and invited them to
serve as an [interim board][hf-interim-board]. Thanks to Simon Peyton Jones's
persuasive powers, and a promise that their main duty would be
limited to the above, they were quickly recruited.

Tim Sears, Emily Pillmore, Ryan Trinkle, and Alex Garcia de Oliveira
led the fundraising efforts, starting seriously in August with
a launch date slated for November. Even before launch, the Foundation quickly
landed over \$200,000 in commitments. Soon we knew that we would have
enough to fund an ED who could spend enough time to
foster the sponsorship efforts. Ryan started the paperwork to
incorporate the Haskell Foundation as a non-profit, while Emily took over the de facto
running of the Working Group. Jasper led an Affiliation Track to
coordinate transparency policies for the community.

Richard Eisenberg worked with other volunteers (Ben Gamari of
Well-Typed, Davean Scies of xkcd, Emily, Moritz Angermann of IOHK,
Tikhon Jelvis from the Haskell.Org committee, and Tim Sears) to
develop an initial technical agenda, a list of the kinds of projects
the Foundation would seek to accelerate. This was to be used in our
fundraising pitches and in forming a starting place for the real work
to come: we knew that, once the Foundation was made public and we had
selected a board, the agenda could be revised in the light of the
freedom of being able to consult the public.

Rebecca Skinner from the Haskell.Org committee (helped by Tim and
Davean) volunteered to lead up the effort to create a website, in
advance of the upcoming launch, which we decided to incorporate into
the Haskell eXchange conference, hosted by Tweag partner Skills
Matter. Cardano/IOHK provided much of the on-the-ground labor in
putting the initial website draft together, handing it over to Rebecca
to push it over the line for the launch.

Finally, on 4 November, 2020 Simon Peyton Jones publicly announced the
Haskell Foundation. You can watch [the video of his
announcement][hf-announce]. Despite the feverish pace of work leading
up to that announcement, everyone knew that the real work was only
beginning, but we had a launch!

The story we wanted to share now comes to an end.

## Epilogue

In December and January the interim Board recruited an outstanding
[slate of 14
members](https://discourse.haskell.org/t/announcing-the-haskell-foundation-board/1811)
from a large group of applicants. As a special bonus they decided that
the initial funding was sufficient to hire not only an ED, but also
a CTO! Andrew Boardman joined as ED and Emily Pillmore stayed on in
a permanent role as CTO. In its first meeting, the Board elected
Richard Eisenberg as its chair.

The work of the Haskell Foundation proper is finally underway. This includes developing
a process for community input, identifying projects to fund and
otherwise support, and continuing outreach efforts. We expect that the
creation of the Foundation will mark an inflection point in the history of
Haskell. It is extremely gratifying to have played a role in helping
a community that has given so much to us personally. We're eager to see
where it will go from here.

Finally, the Haskell Foundation needs volunteers, people just like
you. The best place to reach the Foundation is via the [Haskell
Foundation category at the Haskell Discourse
instance](https://discourse.haskell.org/c/haskell-foundation/11),
though you can reach out directly to the
[Board](mailto:board@haskell.foundation) or its
[chair](mailto:chair@haskell.foundation), [Andrew the
ED](mailto:andrew@haskell.foundation), or [Emily the
CTO](mailto:emily@haskell.foundation).
