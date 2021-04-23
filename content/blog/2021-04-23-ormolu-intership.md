---
title: Ormolu internship
author: Mark Karpov
tags: [haskell]
description: Tweag is looking for interns who want to contribute to Ormolu.
---

Two years ago I started working on a new formatter for Haskell source code.
My idea was to take advantage of the parser of GHC itself, which finally
became available as a library around that time. Soon Tweag supported the
initiative and the project became Ormolu. It was
[announced][ormolu-first-post] in May, right before ZuriHac 2019. Many
people kindly helped me with it during the Hackathon. We went on to [release
the first version][ormolu-first-release] in October 2019.

The use of the GHC parser and a solid approach to testing won Ormolu the
reputation of a dependable tool. More and more industrial users choose it as
their Haskell formatter. In summer 2020 a rather big company had decided
that they wanted to format their Haskell code with Ormolu, which resulted in
a three-month full-time contract. A new level of quality was reached.

I think it is cool to be paid to work full-time on a project like this. If
you concur, we have good news for you! Tweag currently has an opening for an
intern who would like to work on the formatter. This is a project for
someone who would enjoy iteratively improving a popular Haskell tool and
learn about GHC AST, its parser, and perhaps a bit of Nix.

There are various issues affecting Ormolu, and fixing these would have a
positive impact on the user experience. The internship would address them in
severity order:

1. Upgrading the GHC parser. Presently, Ormolu uses
   [`ghc-lib-parser`][ghc-lib-parser]. GHC 9.0 fixes some [long-standing
   issues][long-standing-bugs], and we can take advantage of that by
   switching to `ghc-lib-parser-9.0.1.xxx`.
2. [Some more bugs][the-bugs] of varying difficulty.
3. [Stylistic changes][stylistic-changes].

Please [let us know][apply] if you are interested! Include a cover letter
describing your Haskell experience; familiarity with GHC AST is a plus. We
will collect applications till Tuesday, June 1, 2021. The internship can
start any time after the offer is made, subject to mutual availability.
Internships typically last 12 weeks, although the duration may be adjusted
if necessary. If you have any questions, feel free to [email me][mkarpov].

[ormolu-first-release]: https://www.tweag.io/blog/2019-10-11-ormolu-first-release/
[ormolu-first-post]: https://www.tweag.io/blog/2019-05-27-ormolu/
[ghc-lib-parser]: https://hackage.haskell.org/package/ghc-lib-parser
[long-standing-bugs]: https://github.com/tweag/ormolu/issues?q=is%3Aissue+is%3Aopen+label%3Abug-upstream
[the-bugs]: https://github.com/tweag/ormolu/issues?q=is%3Aissue+is%3Aopen+label%3Abug
[stylistic-changes]: https://github.com/tweag/ormolu/labels/style
[apply]: https://boards.greenhouse.io/tweag/jobs/5221870002
[mkarpov]: mailto:mark.karpov@tweag.io
