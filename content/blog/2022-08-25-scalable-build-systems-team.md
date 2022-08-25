---
title: "Introducing the Scalable Build Systems Team"
author: Andreas Herrmann
tags: [build-systems, bazel, nix]
description: "Introducing the Scalable Build Systems Team"
---

<!--
- What is it about?
  - Introduce the Scalable Build Systems team
  - What we presented at Bazel eXchange
  - That we provide professional services in the space
  - That we are committed to Bazel, Nix+Bazel, and similar
  - That we are now publishing a newsletter
- Who is it for?
  - Peers, potential hires, and potential clients
-->

In this post I will introduce the _Scalable Build Systems_ team at Tweag and
describe our goals and our work.

We believe that correct, efficient, and reliable builds are critical for
developers to work and collaborate effectively. And that the size and
complexity of a project should not be bounded by its build
system, but by what is best to achieve the goal of the project.

Whether you have a large codebase or a small one, whether your project is
polyglot or monolingual, and whether you work in an enterprise organization or
on an open source project - the build system you have available should provide
correct, efficient, reliable builds.

# Open Source

We believe that the open source build system [Bazel][bazel] takes a leap in the
right direction and establishes important values like correctness,
reproducibility, and scalability in the industry. Therefore, we invested into
the Bazel ecosystem, by contributing to Bazel extensions and to Bazel itself,
as well as into our own Bazel extensions [`rules_haskell`][rules-haskell], to
build Haskell code with Bazel, and [`rules_sh`][rules-sh], to import standard
shell tools into Bazel.

To achieve reproducibility and correctness we must not stop at a project's
boundaries, but must manage system dependencies and the developer
environment as well. Therefore, we developed [`rules_nixpkgs`][rules-nixpkgs] to [make
implicit dependencies explicit][implicit-dependencies] by providing them through
[Nix][nix].

Developer experience matters and the build system
and tooling around it should help developers work more productively.
Therefore, we work on developer tooling, such as automation to achieve
[fine-grained incremental builds with Haskell][blog-haskell-module].

Our goal is to help the open source ecosystem lead the state of the art of
build systems and make the necessary tools and infrastructure readily available
and accessible for anyone from open source to enterprise projects.

[bazel]: https://bazel.build/
[nix]: https://nixos.org/
[rules-haskell]: https://github.com/tweag/rules_haskell
[rules-sh]: https://github.com/tweag/rules_sh
[rules-nixpkgs]: https://github.com/tweag/rules_nixpkgs
[implicit-dependencies]: https://www.tweag.io/blog/2020-09-16-implicit-build-dependencies/
[blog-haskell-module]: https://www.tweag.io/blog/2022-06-23-haskell-module/

# Community

Sharing knowledge is necessary for the industry to build software effectively.
You can read about our work on our [blog][blog-bazel], you can meet
us in the [Bazel community channels][bazel-community], for example in the
mailing list or on Slack, and you can meet us at conferences related to Bazel.

We co-organized [Bazel eXchange 2022][bazel-exchange] as a
partner of [SkillsMatter][skillsmatter]. And we contributed to the program
with talks by Alexei Drake about [Building Rust with
Nix+Bazel][rust-nix-bazel-talk] and Guillaume Maudoux about [Remote execution
with `rules_nixpkgs`][remote-execution-nix], and the end of day panels. Don't miss the other talks either, since the conference had an
excellent program!

We are happy to say that this first instance of Bazel eXchange was a success!
We had 115 participants with a fantastic rate of 80% of sign-ups attending! The event
was held online, but nonetheless, participants were very engaged, contributed
great questions, and got in touch on the chat and in the networking session. We
hope that the next instance of the conference can be held in person.

Continuing on our commitment to the community we are thrilled to announce that
we are starting a build systems newsletter. [Sign up][build-newsletter] and we will
keep you up-to-date on developments in and around the Bazel
ecosystem, as well as upcoming events.

[blog-bazel]: https://www.tweag.io/blog/tags/bazel
[bazel-community]: https://bazel.build/help.md
[build-newsletter]: https://build.news
[bazel-exchange]: https://skillsmatter.com/conferences/13682-bazel-exchange#skillscasts
[skillsmatter]: https://skillsmatter.com/go/partners
[rust-nix-bazel-talk]: https://skillsmatter.com/skillscasts/17631-building-rust-projects-with-nix-and-bazel
[remote-execution-nix]: https://skillsmatter.com/skillscasts/17673-remote-execution-with-rules-nixpkgs
[panel-1]: https://skillsmatter.com/skillscasts/17669-day-1-panel
[panel-2]: https://skillsmatter.com/skillscasts/17670-day-2-panl

# Services

Do you share our vision on build systems and developer productivity? Our team
has experienced, versatile engineers with a broad range of
backgrounds and we are Google’s first Bazel Community Expert. We'd love to help you build your software
correctly, efficiently, and reliably, and make the required tools available to everyone.

[Get in touch][contact], we can help you with the following and more:

- **Assessment**
  We assess your project, if and how it can benefit from a scalable build
  system, what improvements you can expect, what resources may be required, and
  what challenges may lie on the way.
- **Migration**
  We can lead your build system migration or help you along the way. We'll
  embed our engineers in your team, collaborating with them directly on your code and sharing all relevant knowledge.
- **Tuning**
  We can tune your build system to improve its performance and boost your team's
  productivity.
- **Upstream**
  We can improve the open source tools you use and contribute the changes upstream,
  to make them available for the industry – your potential customers and employees.
- **Training**
  We can show your team how to work productively with your new build system,
  and how to get the most out of these tools.

[contact]: https://www.tweag.io/contact
