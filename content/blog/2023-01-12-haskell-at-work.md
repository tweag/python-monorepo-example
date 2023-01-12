---
title: "Haskell at Work"
author: Marco Perone
tags: [haskell, servant, training]
description: "Learning and adopting Haskell is not hard with the right resources at hand."
---

A common (mis)conception about Haskell is that there is not enough entry-level educational material and therefore it is hard to learn.

As the [2022 State of Haskell Survey](https://taylor.fausak.me/2022/11/18/haskell-survey-results/) highlighted, one of the [most desired improvements](https://twitter.com/marcoshuttle/status/1602344790963765249) to the whole ecosystem is better documentation and learning material.

That issue can only be addressed with great effort. For this reason, many contributions are necessary to tackle the problem from several angles.

In this post I'd like to summarise what Tweag is doing in this space. In particular, there are three projects we are working on to help make Haskell adoption and usage a little bit simpler and more straightforward:

- the [Haskell at Work](https://skillsmatter.com/workshops/883-haskell-at-work) training course, offered through [SkillsMatter](https://skillsmatter.com/);

- [`servant-template`](https://github.com/tweag/servant-template), a well-documented project to showcase [Servant named routes](/blog/2022-02-24-named-routes/) and, in general, a modern web application setup;

- and a collection of learning resources: [`awesome-learning-haskell`](https://github.com/tweag/awesome-learning-haskell).

Let's take a more in depth look into them.

## Haskell at Work training course

It is hard to deny that the Haskell learning curve can be steep, since the language is fairly different from many other mainstream ones based on different paradigms. That's why learning it by yourself can be a slow and time-consuming task.

This initial approach can be smoother if the friction from new concepts and ideas is mitigated by the presence of someone who can explain them and provide practical examples and use cases. The presence of a teacher and/or a mentor can really transform the learning experience.

That is why we decided to create a brand new training course, to help people and companies adopt Haskell more easily.

The course adopts a practical approach to learning Haskell and is developed around the creation and evolution of a concrete project, highlighting where Haskell can help us to create a safe and maintainable application. It focuses on the day-to-day aspects of the language, which allows a programmer to become productive sooner.

### Teaching vs programming

As software developers we are fairly accustomed to constantly learning new things. We might be less proficient at explaining those things, since teaching requires a specific set of skills. Quite often explaining a concept, its trade-offs, and why it is preferable to other alternatives is very different from just implementing it. Teaching has different trade-offs, and we need to be careful on the learning aspect of it.

To help us and guide us in the creation of the course, we stated some guiding principles from the outset, focused on maximising understanding:

- Start from concrete practical examples. People relate more to applications which resemble what they may be doing in their daily work.

- Introduce a new concept only when it [solves an issue](https://mkremins.github.io/blog/doors-headaches-intellectual-need/). Solutions without a problem might be elegant, but they are not likely to be remembered.

- Introduce concepts one by one. People need to focus on a single concept every time, to avoid unnecessary confusion.

- Do not over-abstract before it is needed. An example of this is excessive polymorphism, which in the beginning makes things harder to understand.

### A little gift

If you are interested in participating in the first edition of such a training course, starting on 30th January, 2023, you can get a 10% discount using the promotional code:

> HASKELL_WITH_TWEAG

when registering at [skillsmatter.com/workshops/883-haskell-at-work](https://skillsmatter.com/workshops/883-haskell-at-work).

## `servant-template`

After we developed [`NamedRoutes`](/blog/2022-02-24-named-routes/) for [Servant](https://www.servant.dev/), we wanted to produce a concrete application to showcase its usage.

We ended up creating a [complete application](https://github.com/tweag/servant-template) using not only `NamedRoutes`, but many other interesting libraries in the Haskell ecosystem:

- Interaction with the database is implemented using [`rel8`](https://hackage.haskell.org/package/rel8), which provides a very nice abstraction layer over [PostgreSQL](https://www.postgresql.org/).

- [JWT](https://jwt.io/) authentication is implemented using the combinators from [`servant-auth`](https://hackage.haskell.org/package/servant-auth).

- We use the pretty cool [`co-log-core`](https://hackage.haskell.org/package/co-log-core) library to make use of compositional contravariant logging.

- Configuration is implemented using TOML bidirectional serialisation offered by [`tomland`](https://hackage.haskell.org/package/tomland).

We also documented the whole [architecture](https://github.com/tweag/servant-template/blob/main/ARCHITECTURE.md) of the application, to make the project structure quick and easy to see.

The repository also contains an Elm application which consumes the API exposed by Servant.

The project is still under development, and we'd love to have your feedback. If you are interested in contributing to one of its [open issues](https://github.com/tweag/servant-template/issues), we'd love a helping hand!

## `awesome-learning-haskell`

Inside Tweag we have people who are quite expert with Haskell, but also people with a different background, such as data engineers and infrastructure engineers. Several times, this led to questions about where to find good resources to learn different Haskell topics and concepts. These discussions often repeated and referred to one another, so we decided to distil and share the highlights. This is how [`awesome-learning-haskell`](https://github.com/tweag/awesome-learning-haskell) was born, a collection of Haskell-related resources recommended by Tweagers for Tweagers.

We soon recognised that such a collection would be useful to the broader community, and made it publicly available.

## Conclusion

Adopting Haskell should not be hard with the right resources and mentors at hand.

Not everyone learns in the same way, so it's important to have a range of content to suit differing learning styles.

During 2022, at Tweag, we worked on creating resources that make Haskell a bit more accessible, and we will continue to do so in 2023.

If you are interested in making Haskell easier to learn and adopt, and have ideas on how to make that happen, we would be interested in hearing your thoughts!
