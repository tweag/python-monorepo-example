---
title: Announcing the GHC DevOps Group
author: Manuel M T Chakravarty
featured: yes
---
As Haskell is increasingly used in production environments, the Haskell toolchain is of critical importance to a growing number of people and organisations. At the heart of this toolchain is [GHC (The Glasgow Haskell Compiler)](https://www.haskell.org/ghc/). Conceived nearly
thirty years ago as a research project by [Simon Peyton Jones](https://www.microsoft.com/en-us/research/people/simonpj/), GHC has long existed at the intersection of research and commercial use, due to the increasing popularity of Haskell. To provide stability and a basic level of support, the project was generously supported by Microsoft Research for many years. However, as more and more companies and others are using GHC in mission-critical ways, the bar is going up. We need a solid, reliable, well-engineered, predictable GHC toolchain; and we need to achieve that without compromising GHC innovation and vitality.

## Shared responsibility
In response to this situation, GHC HQ and several stakeholder organisations decided to work together to improve the situation and to provide shared leadership and a broader pool of resources for the DevOps aspects of GHC development. As a concrete first step in this new partnership, we announced the formation of the *GHC DevOps Group* at the 2017 Haskell Implementors’ Workshop in Oxford.

## Our mission
As set out in the group’s [charter](https://ghc.haskell.org/trac/ghc/wiki/DevOpsGroupCharter), the mission of the GHC DevOps Group is threefold:

* to take leadership of the DevOps aspects of GHC,
* to resource it better, and
* to broaden the sense of community ownership and control of GHC.

As important as the goals are the group’s non-goals. The GHC DevOps group is exclusively concerned with the processes and tools for code development, community contributions, and regular, reliable & well-tested releases. It is about **how** and **when** features get into GHC and are shipped to users. In contrast, it is *not* concerned with the choice of **which** features go into GHC, let alone the content of the libraries it ships with. This is the responsibility of the [GHC Steering Committee and proposals process](https://github.com/ghc-proposals/ghc-proposals/#ghc-proposals) and the [Core Libraries Committee](https://wiki.haskell.org/Core_Libraries_Committee), respectively.

## Our current goals
During its formation, the group identified two initial goals:

1. moving to two calendar-based GHC releases per year, and
2. lowering the barrier to contributing to GHC.

### Quality calendar-based releases every six months
As Ben Gamari documented in detail in his [Reflections on GHC’s release schedule](https://ghc.haskell.org/trac/ghc/blog/2017-release-schedule), actual GHC release dates are hard to predict and initial release quality is often low due to critical bugs. These are the typical symptoms that modern DevOps processes, such as continuous and automated integration, testing, and delivery, are designed to eliminate. Hence, we are working towards reliable continuous building and testing of GHC as well as pre-merge commit testing. Automating everything leads to the entire set of release artefacts being built and provided during that same process.

The main choice that we are currently facing is whether to build our own solution and maintain our own infrastructure based on [Jenkins](https://jenkins.io) or whether to use 3rd party services, such as [CircleCI](https://circleci.com) and [AppVeyor](https://www.appveyor.com), instead. Jenkins provides more flexibility (especially with respect to the supported architectures and operating systems), but at the expense of greater development and maintenance costs — that is, developer time that could otherwise be invested in improving GHC itself. In principle, the use of CircleCI and AppVeyor minimise the work needed to set up and maintain the infrastructure, but at the cost of being dependent on those 3rd parties, including their choice of directly supported architectures and operating systems.

Other key requirements involve security and the ability for anyone that forks GHC to also trivially fork and then modify the build infrastructure at will. For a summary of the requirements as well as the pros and cons of the two alternative approaches, have a look at the [CI Trac Wiki page](https://ghc.haskell.org/trac/ghc/wiki/ContinuousIntegration).

### Lowering the barrier to entry
Although only a limited number of developers will feel confident in, say, extending GHC’s type checker, there are many parts of the compiler and associated tools and libraries that are well within reach of any competent Haskell developer. Hence, we feel that —just like other prominent open-source projects— we should make it as easy as possible to build, modify, and contribute to GHC. For better or worse, today, the gold standard for source control and processing code contributions is GitHub — if only, because virtually every professional developer, especially if engaged in open source work, is already familiar with it.

In contrast, GHC currently requires contributors to use the [Phabricator](https://ghc.haskell.org/trac/ghc/wiki/Phabricator) tool. This is not only unfamiliar to most, but also requires the local installation of the command line tool Arcanist. In addition, Phabricator requires maintaining our own Phabricator installation and is known to be cumbersome to integrate with CI tool chains (tying this in with the first goal).

Lowering the barrier to entry involves other changes too of course (e.g., documentation structure). But to start with, what we are discussing is an attempt at encouraging more contributions while simultaneously lowering our set up and maintenance costs. This may well involve moving from Phabricator to GitHub.

## Transparency and contributions
If you are interested in the details of the discussion, please have a look at the [`ghc-dev-ops@haskell.org` archives](https://mail.haskell.org/pipermail/ghc-devops-group/). To provide transparency to the wider GHC user and developer community, all discussions of the GHC DevOps Group will be recorded on that mailing list.

Please let us know what you think. Feel free to approach any [member](https://ghc.haskell.org/trac/ghc/wiki/DevOpsGroupCharter#Membership) of the GHC DevOps Group with feedback or suggestions. For a broader discussion, you may want to follow up on the general [`ghc-devs`](https://mail.haskell.org/cgi-bin/mailman/listinfo/ghc-devs) mailing list.

All this requires resources: time and money. To make this work, we will need significant contributions from companies that get value from GHC (which is itself free). But in exchange we get something valuable: a GHC ecosystem that we can rely on. If you think your company could help, in cash or kind, [please get in touch](mailto:manuel.chakravarty@tweag.io).
