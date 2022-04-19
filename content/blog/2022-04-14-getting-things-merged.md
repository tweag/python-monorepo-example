---
title: "Getting Things Merged"
author: Clément Hurlin
tags: [processes]
description: "How to get things merged in large collaborative projects"
---

At Tweag, we work for clients as consultants. Clients expect Tweag
consultants to deliver by shipping code, and in this context shipping means
having the code land in the main repository to get deployed in production.
Sounds simple, right? Write code, merge it; repeat. Well, it gets more
complicated than that.

<!-- Using imgur as a temporary host. I'm not sure the memes will
     make it past the editors. I think it should, it gives this post
     a different flavor than our usual ones; but I'm fine either way. -->

![png](https://imgur.com/bwBXVLg.png)

In this post, I describe why it can get difficult to merge contributions
and ways to circumvent these difficulties. My analysis applies
both to software consultants and also to open source contributors,
in a context where pull requests must be made and approved by a reviewer before merging.
I will go through a sequence of tips and tricks to have code land faster.
There is no silver bullet in this domain but rather a list of recommended
practices.

# What can go wrong?

Code can be difficult to merge for mainly two reasons:

- Pull requests do not meet agreed-upon quality standards. This is a purely technical problem, because
  pull request authors can work towards solving it on their own.
- The organization managing the repository has a dysfunctional review and
  merge process. This is a social problem, because pull request authors
  need the help of others to get their work accepted.

The second case is the more difficult of the two, because it involves more people and may require changing
processes or the development culture. If these objectives are out
of your reach, you can at least highlight the problems.

I will now detail how to tackle the two kinds of issues.

# The technical aspect of getting things merged

There are many reasons for which code can be difficult to merge.
However, the prominent reasons revolve around the code requiring too much
time and attention to be reviewed. When asking for review, you ask
for someone else's time. To have the best chance of obtaining this time,
you should make the review as easy as possible. For that, I recommend the following:

- Write small pull requests. A small pull request has a smaller impact,
  as a consequence, it is easier to trust that it doesn't break anything.
- Write focused pull requests. This is related to writing small pull requests,
  with the twist that you should refrain from changing things unrelated
  to the core topic of your pull request.

  Valuable unrelated changes can always be done in follow-up pull requests.
  If you are asked for such changes, try not doing it; instead propose
  to create a follow-up issue assigned to you. By creating the issue,
  you show you value the reviewer's opinion, but you don't delay your most
  important work.

- Have continuous integration (CI) pass on as many updates of your pull request as possible.
  CI not passing gives an opportunity to reviewers
  to postpone looking at your code, because it is obviously not mergeable
  in its current state. The minimum I recommend is to have CI pass
  when you ask for the initial review request. First impression matters!
- Have tests covering your changes. Having tests makes your code
  easier to trust, because it is at least partially machine-checked. Reviewers
  are more inclined to click that _approve_ button when automation
  has their backs.
- Write detailed rationale, even for small changes! The description should state:

  - Why is this change needed, why is it important?
  - What are the alternative designs and why was this one picked?
  - How can the contribution be tested? If the pull request introduces new tests,
    give the command line to execute the tests, so that reviewers can reproduce
    them.
  - Is there any follow-up planned? If yes provide a link to it. If there
    is a big picture description of the stream of work to which the pull
    request belongs, link to it.

  Ideally your pull request description and your list of commits should
  read like a book. Each commit makes sense on its own, like chapters in a book.
  And like a book, the entire pull request should tell a compelling story.

  The larger the change, the more difficult your pull request looks to a reviewer.
  That is why putting in some extra work - to make your pull more amenable to review -
  is a good investment of your time.

## Dealing with medium-size pull requests

If you suspect your pull request becomes significant in size, my recommendation is
to put self-contained commits first in the history. A commit
that makes sense on its own is an atomic piece of work that strictly improves the
state of the codebase. Such commits for example include refactorings and renamings
that are valuable independently of the introduction of a new feature.
In case your pull request is being delayed, you can create less controversial spin-off
pull requests with the commits that make sense on their own. Such spin-offs
will likely land before the whole thing, hereby reducing your list of things to keep track of
in the meantime.

This may entail additional work for you, such as dealing with subsequent
merge conflicts or rewriting your unmerged history over and over.
Do not despair! Remember that you can only deliver value by getting your
code into production. Anything you get merged at all contributes
to your track record of productive improvements – stalled pull requests do not.
As time passes, your track record will make you more influential, meaning
your work will be easier to merge in the long run.

## What if my pull request is unavoidably huge?

Indeed, some pull requests are large, because the feature
affects an entire codebase. If you are already a central developer
in the corresponding repository, you may able to merge large pieces of work.
If you are a new developer, however, I recommend the following
for merging large features:

- Try to split your work into multiple pull requests, even if it means introducing some dead code
  or yet-unused generalizations. To convince reviewers about introducing dead code,
  I recommend linking to a design document showing how the dead code will
  ultimately be used. The design document can either be a draft pull request/branch
  or a simple text file (in some projects, design documents are versioned,
  to serve as later reference). In both cases, it should be easily
  accessible from the work you want to merge. Mention it in the description
  of all the pull requests that make your stream of work.
- Introduce a feature flag in the codebase, that controls whether the new
  feature is enabled. It can be as simple as a global Boolean value. While
  the entire feature is not merged, the feature is turned off; preventing the new code
  from executing. The final pull request will enable the feature
  or remove the configuration option altogether.

# The social aspect of getting things merged

The organization managing the repository you contribute to can make assumptions that
you are unaware of. Examples include:

- CI not passing implies that reviewers are encouraged not to look at a
  pull request. Such a policy may exist to force authors to polish
  their pull requests before asking for reviews. Authors _must_
  know about this assumption. Otherwise they will wait an unnecessarily long
  time for their pull requests to be looked at. In this case, and if
  CI can't be fixed before requesting review, you should highlight it.
- Nitpicks block reviewing. It may very well be that not adhering to code
  formatting or leaving trailing whitespaces blocks reviewing. Again, it
  can be a valid policy, but it needs to be known by authors.

These two items are related to discoverability: external contributors should be
aware of the possible existence of such policies, so that they can even think
about discovering them. This is especially important when joining a project, when you need to obtain
the trust of core developers and stakeholders. A common place for finding
such information can be a `CONTRIBUTING` file at the repository root.

## Finding good reviewers

To make your code land, you need to find good reviewers. Here's what makes a good reviewer:

1. Reviews fast when asked.
1. Says no when they cannot review fast (for me fast means within one or two working days).

The second point is important: good reviewers are usually very busy (because
they get asked a lot) and so they happen not to always be able to review in a short
time. By saying no, they allow you to look for another reviewer and hence avoid
you getting blocked. Quickly receiving no as an answer to a review request is therefore good, up to some point.

To find good reviewers, you can:

- Look at pull requests that have been merged recently. Who reviewed what?
- Is there a [CODEOWNERS][codeowners] file? If yes, is it automatically picked
  up by your collaboration platform (e.g. GitHub, GitLab); or is it purely for information?
- Select your reviewers. This is crucial: when you create a pull request, you
  can usually specify your reviewers. If you can do it (honoring
  code ownership of course), it can make a huge difference in landing time.

  In my projects, I will usually have a small list of top-notch reviewers;
  the people I will ask when I want something to land as fast as possible.
  Obviously I can't always ask the same people, so I also have second-line
  reviewers as backups to avoid overloading my favourites.

  It's also good to know the people who never review anything. Such people
  can include previous developers, who authored a lot code and moved on
  to management positions, or are simply not working on the project anymore.
  It can also be people that turn notifications off or do not engage
  in distributed collaboration.

  This can happen with companies that feature
  both on-site developers and remote developers, if the on-site culture
  is the most influential one: developers that are always on-site may not engage with
  remote ones. In this case, you would only be slowing your own work
  by asking assistance from on-site developers.

- Ask on the project's instant messaging tool. As a remote
  consultant or as a new contributor to an organization, you can be slightly isolated
  when joining a project and unable to know who to approach directly.
  Doing extra communication helps overcoming this issue.

[codeowners]: https://docs.github.com/en/repositories/managing-your-repositorys-settings-and-features/customizing-your-repository/about-code-owners

## Building relationships, being a role model

To gain trust of the core developers — the ones that will allow you to
merge your code fast — you should engage with them. Here are my recommendations:

- Do reviews without being asked. It's easy! Crawl open pull requests
  and pick the ones where you will be able to provide some value. You don't need
  to be an expert in the field to provide valuable feedback. Even without
  domain knowledge, a seasoned developer can provide useful insights or at least ask informed questions.

  In this scenario, aim for pull requests authored by developers from whom
  you will need review soon. We all have a cognitive bias of feeling indebted
  to people who make unsolicited gifts. You can use it to your advantage:
  nudge people to reciprocate your support by helping them with their tasks. You will reap
  the benefits later, by having your own code promptly reviewed.

  This is something I do when I feel I'm not in adequate mental shape for
  writing code effectively. It can be less intense to review code than to write code.
  That is why I would sometimes do such free reviews at the end of my working day.

- Engage in technical discussions, bring your expertise to the debate.
  This both makes you discover the team (who decides what, who has influence, etc.)
  and provides immediate value.
- When you are asked for review, be a role model: review fast. Obviously
  you cannot ask of others what you don't do yourself!
- Take responsibility: is there a shepherd role that triages
  incoming pull requests for a limited amount a time? If yes, apply to
  that role. This is the kind of role that developers don't fancy very much,
  so applicants for these roles are always very welcome! Similar duties may also
  include pinging authors of stalled pull requests, triaging issues,
  upgrading dependencies regularly, or maintaining documentation.
  (Don't underestimate the power of a few fixed typos!)
- [Research shows that][cs-111] "women's pull requests tend to be accepted
  more often than men's, yet women's acceptance rates are higher only when they
  are not identifiable as women." Be aware of the systemic unconscious biases that
  are at play in your process. If you notice that a certain person's PRs are often
  blocked, or debated more than others, you might want to consider stepping back
  and involving others in the organisation dealing with making the workplace more
  inclusive.

[cs-111]: https://peerj.com/articles/cs-111/

## Submitting pull requests

To maximize the chance of your pull requests moving fast, I recommend two things
before submitting them:

- If the work is significant or its design can be controversial, have your design validated beforehand.
  For example describe the design you envision during a meeting (is there
  a regular meeting for your project?) and have a document ready for passing on.
  If there is no regular meeting to present your work, post your design
  document on the project's instant messaging tool. If people engage
  with your design, you are building connections. If people don't say anything,
  that's not so bad either: you can always point back to your post if objections
  are raised later on and have a bit more leverage.

  As I said earlier, there
  is no silver bullet solution here; but just practices to give you a slight
  advantage.

- Try to book review time beforehand. If there is a weekly meeting for your project,
  ask people if they are willing to review your upcoming work. Try getting a clear
  commitment that you can refer to when needed.

## Dealing with reviewers

Your pull request has been picked up by reviewers? Great! Let's keep the
grasp on the reviewers while they are at it. For this I recommend replying
quickly to comments, to increase the chances of starting a discussion. If you
answer to comments while the reviewer is still reviewing, there is a high
chance they will reply right away to your answers, creating a virtuous conversation.

![png](https://i.imgur.com/nSz7PU8.png)

When replying to comments, stay constructive and flexible. Your goal
is not to have the last word on everything, but to merge your code.
Beautiful code that doesn't get merged is useless: better
accommodate a picky reviewer and merge your code, reducing your to-do list,
than postponing delivering. Also recall that influence grows with time.
Shipping your code fast and delivering will give you leverage in the long term.

The worst kind of reviewer is the _never happy_ reviewer that engulfs you in
an endless stream of nitpicks and oppositions. This is the worst that can happen.
In this case I recommend two options:

1. Try to obtain a third opinion, maybe finding
   another reviewer that will overrule the never happy reviewer. Remember
   that, in the end, you don't have to convince everyone to merge your code;
   you only need to find the minimum number of reviewers that are required by the
   project's merge policy.
1. Create spin-off pull requests to merge uncontroversial changes first. This
   both reduces your amount of work in progress and the likelihood of merge
   conflicts against the main development branch.
   It also gives you more leverage for merging the rest in the end, because
   part of your design is already in production.

## What if nothing happens?

If your pull requests are stalled and you want to continue working
on the concerned repository, I recommend two things:

1. Stack your pull requests. Both GitHub and GitLab support
   [branch retargeting][branch-retargeting]:
   your first pull request targets the `main` branch, then the second pull
   request targets the branch of the first pull request, and so on. Once the first
   pull request lands in `main`, GitHub and GitLab
   will automatically change the target of the second pull request,
   so that it targets branch `main`.

   This saves you some manual work. My personal limit for stacking pull
   requests is three. More than that creates so much hindrance to keep track
   of everything that it becomes a waste of human time.

1. Work on another task, that affects another part of the repository.
   In other words, parallelize your work. Again, set yourself a limit, because
   parallelizing work involves a lot of cognitive and technical overhead; because you have to deal
   with lots of rebase conflicts as the `main` branch evolves. My personal limit is to have at most five
   pull requests opened at all times on a given repository.

Once you have reached your limits, if you are an external consultant like
us at Tweag… well, then you have obtained data that you cannot deliver as fast as possible.
Then it is time to engage with your client by showing your current
state of work. In this scenario, be constructive: propose improvements
to the merge and review process, to enhance overall productivity.

If you are working on an open source repository, it may be time to
reach out to the stakeholders of the repository; highlighting to them
the contributions that are being delayed. In this case, offer your help
to enhance the process, for example request merge rights. Open source
projects are often willing to give power to more contributors, for example
to increase the [bus factor][bus-factor].

[branch-retargeting]: https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-branches#working-with-branches
[bus-factor]: https://en.wikipedia.org/wiki/Bus_factor

# Conclusion

Getting things merged has two aspects: a technical aspect and a social aspect.
The technical aspect can be summarized by: _make reviewing your work a low-hanging fruit_.
For this, write small pull requests, that are tested and documented, and
have an helpful description of the changes. The social aspect has many subtleties, three of them are central:

1. Identify technical leaders, engage with them, build trust with them.
1. Identify good reviewers: reviewers that review fast and say no quickly if
   they can't. Make them owe you by helping them. Engage in a mutually
   beneficial relationship where you review each others work.
1. Take on responsibility: it makes you both provide more value to the organization
   you are working for and it makes your life easier by gaining influence to
   deliver your contributions.
