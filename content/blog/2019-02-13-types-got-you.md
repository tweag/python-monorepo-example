---
title: The types got you
author: Mark Karpov
tags: [haskell]
---

Haskell is in a way a unique language. It is, to be clear, a language people
and businesses use for building “[serious][facebook-sigma] [stuff][dons-scb]”,
yet it is also a language which remains a platform for experimentation with
functional programming. This means that the teams which develop in Haskell
have access to expressivity and flexibility few other languages allow.

As the [GHC][ghc] compiler evolves and grows its feature set, one couldn't
help but wonder about the connection between power provided by a programming
language and the process of making design decisions when developing software.

We could start by stating that power and freedom are dangerous. Indeed, in
human societies these have been continuously restricted or contained on
different levels with tools like law, tradition, culture and religion. Which
is not at all bad, because for most people it's hard to work, live, and
coexist without preset constraints. Freedom is generally confusing. The
statement holds in software systems as well: a task is always more feasible if
some questions have been answered definitively before you start working. With
each question answered for you, a possibility to screw up is removed.

Haskellers know and seem to actively acknowledge that limiting expressivity
and freedom is good for them. They like DSLs and can argue endlessly how to
model monadic effects in a way that a piece of code can only do exactly what
is necessary and not more. So why not take a step back and look at the bigger
picture? And by “bigger picture” I mean the language as a whole, as
implemented by GHC.

No, I'm not suggesting removing features from the compiler. But with great
power comes great responsibility. Or in our case, rather a need for great
caution. Using advanced features of the type system takes a fair bit of
judgment to get right. Judgement comes from experience. But who really has
this experience? Who are we but a relatively small group of engineers trying
to build production software using tools that relatively
few people have used for this
before, and even fewer people have used successfully?

Some features may be so new that no one yet truly knows what to do with
them. And by “what to do with them” I don't mean that people don't know how
to make fancy stuff compile. I assure you, this is accessible to many. The
problem is in predicting what use of the features will give you in long
term, is it worth it? Will it actually slow down the development? Will it
make it harder for new people on the team to start working on your code? Are
you actually catching more bugs at the compile time, or do you just think you
do? Are you thinking about the practical results or about intricate niceties
of your code?

Developing in Haskell is hard because it's easy to take a wrong turn that may
be fatal. I'm writing this on the basis of my experience as a consultant. The
anecdata of a single person might not be convincing to you, but if there were
to be just one common theme across many of our projects at Tweag in the past
year, it would be this: we have witnessed the high cost of entirely incidental
complexity many times over. Our data suggests this issue is real and very
common.

Unfortunately, the problem is not limited to code in the language itself.
There seems to be a trend for disregarding common engineering practices,
reinventing solutions, preferring solutions written in Haskell, or
solutions which just seem to be “nicer” without doing unbiased comparison
and analysis.

In the essay [_The bipolar Lisp programmer_][bipolar] Mark Tarver made the
point that the Lisp programming language attracts a certain kind of
personality. I think it is generally true that there is a connection between
personalities and the tools that people choose. Perhaps this is most
prominently revealed in niche communities that are made up from enthusiastic
people, not just people who have to pay the bill.

Haskell does attract a certain kind of personality. Who in their sane mind
would be entirely oblivious to the many barbaric and funny words part of our
lingua franca in documentation, tutorials and books today? Probably someone
who is either very convinced in the benefits of typed FP or just someone who
has, let's admit it, a bit adventurous and curious mind. It is easy to forget
the timeless principles of robust engineering in this intellectual pursuit,
perhaps even without noticing it.

Good music producers _listen_. They judge everything by how it sounds, because
when the result is rendered and saved, people won't know how many effects,
plugins, advanced techniques from some magazine were used. Nope, they really
could not care less about that. They only care about the sound. For a beginner
though, it is tempting to throw in a lot of processing and clever tricks, and
in doing so they often overdo and produce a track that sucks.

The amateur producer from our example tried to make a record better, but he
has not yet understood that his focus is misplaced. I think the situation is
often the same with development in Haskell.

At the most basic level, development is about efficiently turning money into
software that does something while remaining modifiable and maintainable.
Haskell98 with some “benign” extensions is often the optimal solution in the
trade-off between safety and simplicity. With conventional Haskell,
developers effortlessly can stay in the [pit of success][pit].

Even though Haskell is a wonderful language by default, there are ways to
get out of the pit. Ironically, these ways are exceptionally attractive to
Haskellers for exactly the same reason Haskell itself is attractive to them:
exploration and the endless pursuit of correctness. There are probably far
more people who will go with anything that promises an improvement with
respect to type-safety than those who are capable of sober estimation and
balancing.

In the end, the main thought here is the following: build using simple
proven techniques even if you're using a technology like Haskell. For
anything extra (like dependent types, formal verification, etc.) you might
want to think twice and thrice. And still remain uncertain.

[ghc]: https://www.haskell.org/ghc/
[tweag]: https://tweag.io
[bipolar]: http://www.marktarver.com/bipolar.html
[pit]: https://www.youtube.com/watch?v=US8QG9I1XW0
[facebook-sigma]: https://code.fb.com/security/fighting-spam-with-haskell/
[dons-scb]: https://skillsmatter.com/skillscasts/9098-haskell-in-the-large-the-day-to-day-practice-of-using-haskell-to-write-large-systems
