---
title: "How Nix grew a marketing team"
author: Rok Garbas
tags: [nix]
description: "What do we need to focus on to increase adoption of Nix?"
---

Recently I witnessed the moment when a potential Nix user reached eureka.
The moment where everything regarding Nix made sense.
My friend, now a Nix user, screamed from joy: **“We need to Nix–ify everything!”**

Moments like these reinforce my belief that Nix is a solution from -- and for -- the future.
A solution that could reach many more people, only if learning about Nix didn't demand investing as much time and effort as it does now.

I think that Nix has the perfect foundation for becoming a success but that it still needs better _marketing_.
Many others agree with me, and that's why we formed the [Nix marketing team][marketing-team].

I would like to convince you that indeed, marketing is the way to go and that it is worth it.
Therefore, in this post I will share my thoughts on what kind of success we aim for, and which marketing efforts we are currently pursuing.
The marketing team is already giving its first results, and with your input, we can go further.

## What does success look like?

At the time of writing this post, I have been using Nix for 10 years.
I organized one and attended most of the Nix conferences since then, and talked to many people in the community.
All of this does not give me the authority to say what success for Nix looks like, but it does give me a great insight into what we -- the Nix community -- can agree on.

Success for Nix would be the next time you encounter a project on GitHub, it would already contain a `default.nix` for you to start developing.
Success for Nix would be the next time you try to run a server on the cloud, NixOS would be offered to you.
Or even more ambitious, would be other communities recognising Nix as a _de facto_ standard that improves the industry as a whole.

To some, this success statement may seem very obvious.
However, it is important to say it out loud and often, so we can keep focus, and keep working on the parts of Nix that will contribute the most to this success.

## The importance of marketing

Before we delve into what Nix still lacks, I would like to say that we -- engineers and developers -- should be aware of our bias against marketing.
This bias becomes clear when we think about what we think are the defining aspects for a project's success.
We tend to believe that code is everything, and that good code leads to good results.
But what if I tell you that good marketing constitutes more than 50% of the success of a project?
Would you be upset?
We have to overcome this bias, since it prevents us from seeing the big picture.

Putting aside those Sunday afternoons when I code for the pure joy of stretching my mind, most of the time I simply want to solve a problem.
The joy when seeing others realizing that their problem is not a problem anymore, is one of the best feelings I experienced as a developer.
This is what drives me.
Not the act of coding itself, but the act of solving the problem.
Coding is then only part of the solution.
Others need to know about the existence of your code, understand how it can solve their problem and furthermore they need to know how to use it.

That is why marketing, and, more generally, non-technical work, is at least as important as technical work.
Documentation, writing blog posts, creating content for the website, release announcements, conference talks, conference booths, forums, chat channels, email lists, demo videos, use cases, swag, search engine optimisation, social media presence, engaging with the community...
These are all crucial parts of any successful project.

Nix needs better marketing, from a better website to better documentation, along with all the ingredients mentioned above.
If we want Nix to grow as a project we need to improve our marketing game, since this is the area of work that is historically receiving the least amount of attention.
And we are starting to work on it.
In the middle of March 2020, a bunch of us got together and [announced][marketing-team-announced] the creation of [the Nix marketing team][marketing-team].
Since then we meet roughly every two weeks to discuss and work on non-technical challenges that the Nix project is facing.

But before the Nix marketing team could start doing any actual work we had to answer an important question:

## What is Nix?

I want to argue that the Nix community is still missing an answer to an apparently very simple question: _What is Nix?_.

The reason why _what is Nix?_ is a harder question than it may appear at first, is that any complete answer has to tell us _what_ and _who_ Nix is for.
Knowing the audience and primary use cases is a precondition to improving the website, documentation, or even Nix itself.

This is what the Nix marketing team [discussed first][marketing-meetings]. We identified the following audiences and primary use cases:

1. Development environments (audience: developers)
2. Deploying to the cloud (audience: system administrators)

It doesn’t mean other use cases are not important -- they are.
We are just using the primary use cases as a gateway drug into the rest of the Nix’s ecosystem.
In this way, new users will not be overwhelmed with all the existing options and will have a clear idea where to start.

Some reasons for selecting the two use cases are:

- Both use cases are relatively polished solutions.
  Clearly, there is still much to be improved, but currently these are the two use cases with the best user experience in the Nix ecosystem.

- One use case is a natural continuation of another.
  First, you develop and then you can use the same tools to package and deploy.

- Market size for both use cases is huge, which means there is a big potential.

A differentiating factor -- why somebody would choose Nix over others -- is Nix's ability to provide reproducible results.
The promise of reproducibility is the aspect that already attracts the majority of Nix's user base.
From this, we came up with a slogan for Nix:

> **Reproducible builds and deploys**

With the basic question answered we started working.

## What has been done so far? How can I help?

So far, the Marketing team focused on improving the website:

1. **Moved the website to Netlify.**
   The important part is not switching to Netlify, but separating the website from the Nix infrastructure.
   This removes the fear of a website update bringing down parts of Nix infrastructure.

2. **Simplified navigation.**
   If you remember, the navigation was different for each project that was listed on the website.
   We removed the project differentiation and unified navigation.
   This will show Nix ecosystem as a unified story and not a collection of projects.
   One story is easier to follow than five.

3. **Created a new learn page.**
   Discoverability of documentation was a huge problem.
   Links to popular topics in manuals are now more visible.
   Some work on entry level tutorials has also started.
   Good and beginner friendly learning resources are what is going to create the next generation of Nix users.

4. **Created new team pages.**
   We collected information about different official and less official teams working on Nix.
   The work here is not done, but it shows that many teams don’t have clear responsibilities.
   It shows how decisions are made and invites new Nix users to become more involved with the project.

5. **Improved landing page.**
   Instead of telling the user what Nix is, they will experience it from the start.
   The landing page is filled with examples that will convince visitors to give Nix a try.

The work of the marketing team has just started, and there is still a lot to be done.
We are working hard on [redesigning the website][redesign] and [improving the messaging][message].
The [roadmap][roadmap] will tell you more about what to expect next.

> **If you wish to help** come and say hi to [#nixos-marketing on irc.freenode.org][irc].

## Conclusion

Marketing, and non-technical work, is all too often an afterthought for developers. I really wish it weren't the case.
Having clearly defined problems, audience and strategy should be as important to us as having clean and tested code.
This is important for Nix. This is important for any project that aims to succeed.

[marketing-team-announced]: https://discourse.nixos.org/t/marketing-team-can-we-present-nix-nixos-better/6249
[marketing-team]: https://nixos.org/teams/marketing.html
[redesign]: https://github.com/NixOS/nixos-homepage/issues/449
[message]: https://github.com/NixOS/nixos-homepage/issues/444
[ux-talk]: https://www.youtube.com/watch?v=PjAmr22FZts
[roadmap]: https://github.com/NixOS/nixos-homepage/milestone/1
[irc]: irc://irc.freenode.net/#nixos-marketing
[marketing-meetings]: https://www.youtube.com/watch?list=PLt4-_lkyRrOOO8CYo5XPx6UI05q8kwhj0
