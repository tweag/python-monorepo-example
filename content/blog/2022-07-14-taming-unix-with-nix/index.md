---
title: Nix – taming Unix with functional programming
author: Valentin Gagarin
tags: [nix]
description: "Nix is all about applying functional programming to files and processes."
---

You may be aware of [Nix or NixOS][nixos].
Users love it for being a superior tool for building, deploying, and managing software.
Yet, it is generally perceived as notoriously hard to learn.

The core Nix ecosytem consists of several distinct components:

- The _Nix package manager_ comes with a configuration language to declare software components, the _Nix language_.
- Software made available through the package manager is centrally maintained in the _Nix package collection_, also known as `nixpkgs`.
- There also exists a Linux distribution called _NixOS_, which is based on the package manager and the package collection.

In an attempt to provide an alternative learning approach, this article discusses the Nix package manager (hereafter simply referred to as Nix) and its underlying principles in the context of the history of computing.
The condensed findings presented here reflect only some of our ongoing community effort[^1], started this year to improve documentation and make the benefits of Nix more accessible to software developers, and eventually computer users in general.

## Everything is a… what?

Nix is not new.
It has been under active development [since 2003][nix-initial-commit].

While touted as the purely functional package manager, one cannot say that Nix introduces a fundamentally new paradigm.
[Functional programming][functional-programming] goes back to John McCarthy's [Lisp][lisp] (1962), rooted in Alonzo Church's [lambda calculus][lambda-calculus] (1936), where everything is a function.

It is not even a new idea for Nix to propose parting ways with one of the most pervasive [skeuomorphisms][skeuomorph] in computing, the file system, which [naturally followed][file-system-origin] from an era where everything was a piece of paper.

Ken Thompson and Dennis Ritchie inherited the novelty of a hierarchical file system from one of its predecessors [Multics][multics] and firmly assumed it as a given by 1974:

> The most important job of UNIX is to provide a file system.
>
> — [The UNIX Time-Sharing System][unix] (1974)

Although arguably it is a severely limiting abstraction[^2], it remains largely unquestioned as a cornerstone of software development practice[^3].
The rise of object oriented-programming brought about a number of experimental systems[^4] where everything is an object — an idea attributed to Alan Kay's [Smalltalk][smalltalk] (1972) — but none of them saw mass adoption.

Part of the [Unix philosophy][unix-philosophy] later even turned into the malapropism [everything is a file][everything-is-a-file].
Linus Torvalds clarified in various public emails that it was really about small, composable tools operating on uniform interfaces, not the specific mapping of names to contents:

> The whole point with "everything is a file" is not that you have some random filename (indeed, sockets and pipes show that "file" and "filename" have nothing to do with each other), but the fact that you can use common tools to operate on different things.
>
> — [Linus Torvalds][unix-filenames] (2002)

> The UNIX philosophy is often quoted as "everything is a file", but that really means "everything is a stream of bytes".
>
> — [Linus Torvalds][unix-bytestream] (2007)

Rob Pike and Ken Thompson have further pursued the design of an entire system around a hierarchy of named files with [Plan 9][plan9] since the 1980s, culminating in what Torvalds phrased as "everything is a namespace":

> It may be unnatural to the Plan-9 way of "everything is a namespace", but that was never the UNIX way.
> The UNIX way is "everything is a file descriptor or a process", but that was never about namespaces.[^5]
>
> — [Linus Torvalds][plan9-namespace] (2002)

Today's [most widely used operating systems](https://en.wikipedia.org/wiki/Usage_share_of_operating_systems) (Linux, XNU, and Windows NT) all have file systems at their core.
However, Nix is special not so much because it radically puts that into question with the purely functional approach, but for rather pragmatically offering an intriguing shift in perspective:

What if we could continue developing and using all our software (mostly) as it is, and (mostly) stop bothering with file names, paths, and directories when building and deploying it?

[nixos]: https://nixos.org
[nix-initial-commit]: https://github.com/NixOS/nix/commit/75d788b0f24e8de033a22c0869032549d602d4f6
[functional-programming]: https://en.wikipedia.org/wiki/Functional_programming
[lisp]: https://en.wikipedia.org/wiki/Lisp_(programming_language)
[lambda-calculus]: https://en.wikipedia.org/wiki/Lambda_calculus
[skeuomorph]: https://en.wikipedia.org/wiki/Skeuomorph
[multics]: https://en.wikipedia.org/wiki/Multics
[file-system-origin]: https://en.wikipedia.org/wiki/File_system#Origin_of_the_term
[unix]: https://dsf.berkeley.edu/cs262/unix.pdf
[unix-philosophy]: https://en.wikipedia.org/wiki/Unix_philosophy
[everything-is-a-file]: https://en.wikipedia.org/wiki/Everything_is_a_file
[unix-filenames]: https://lore.kernel.org/all/Pine.LNX.4.44.0206081523410.11630-100000@home.transmeta.com/T/#u
[unix-bytestream]: https://groups.google.com/g/fa.linux.kernel/c/Zm8SQ82bBYg/m/M2Xv-BfRv_YJ
[smalltalk]: https://en.wikipedia.org/wiki/Smalltalk
[plan9]: https://en.wikipedia.org/wiki/Plan_9_from_Bell_Labs
[plan9-namespace]: https://lore.kernel.org/all/Pine.LNX.4.44.0206091056550.13459-100000@home.transmeta.com/T/#u

## An intriguing shift in perspective

The key insight behind Nix is that the problem of **software deployment** can be seen through the lens of programming language theory.
The idea was first put forward by Eelco Dolstra et al. in [Imposing a Memory Management Discipline on Software Deployment][immdsd] (2004). In his PhD thesis [The Purely Functional Software Deployment Model][tpfsdm] (2005), Dolstra proposed that we can treat the file system in an operating system like memory in a running program, and equate package management to memory management.
With Nix, he showed how to apply proven solutions, such as garbage collection or disallowing arbitrary manipulation of pointers (also known as _pointer discipline_), to the perennial struggle of making software work reliably.

[immdsd]: https://edolstra.github.io/pubs/immdsd-icse2004-final.pdf
[tpfsdm]: https://edolstra.github.io/pubs/phd-thesis.pdf

The following figure illustrates the analogy of memory structures in programs and operating systems down to single objects.

![Memory Analogy](memory-analogy.svg)

As an example of this equivalence, take the following shell script:

```sh
#!/usr/bin/sh
echo hello
```

It contains a reference to `/usr/bin/sh`.
That file path is just like a mutable pointer to a mutable variable:

- The path itself can be changed to reference a different file or one that does not even exist.
- The contents of `/usr/bin/sh` can be changed or the file deleted entirely.

This makes it hard to reason about the overall system state – the same problem as for program state in an imperative programming language.

More details are elaborated in the table below (based on Figure 3.1 in [The Purely Functional Software Deployment Model][tpfsdm], p. 55).

| Programming Language Domain                                                                            | Deployment Domain                                                                  |
| ------------------------------------------------------------------------------------------------------ | ---------------------------------------------------------------------------------- |
| memory                                                                                                 | disk                                                                               |
| value, object                                                                                          | file                                                                               |
| address                                                                                                | path name                                                                          |
| pointer dereference                                                                                    | file access                                                                        |
| pointer arithmetic                                                                                     | string operations                                                                  |
| dangling pointer                                                                                       | path to absent file                                                                |
| object graph                                                                                           | dependency graph                                                                   |
|                                                                                                        |                                                                                    |
| calling constructed object with reference to other object                                              | runtime dependency                                                                 |
| calling constructor with reference to other object, not stored                                         | build-time dependency                                                              |
| calling constructor with reference to other object, stored                                             | retained dependency                                                                |
|                                                                                                        |                                                                                    |
| languages without pointer discipline (e.g. assembler)                                                  | typical Unix-style deployment                                                      |
| languages with enough pointer discipline to <br> support conservative garbage collection (e.g. C, C++) | Nix                                                                                |
| languages with full pointer discipline (e.g. Java, Haskell)                                            | as-yet unknown deployment style not enabled <br> by contemporary operating systems |

This notion was further refined by Andrey Mokhov et al. in [Build Systems à la Carte][bsalc] (2018), from a slightly different angle: distilling the essential features of build systems shows that **building software** can also be seen through the lens of programming language theory.
It is really about applying functions to arbitrary values, which happen to be files in a file system; some of these files end up being run as processes.
Again, proven solutions like memoization and self-adjusting computation offer themselves, this time, for tackling the perennial problem of long compilation times.

[bsalc]: https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf

![Dataflow Analogy](dataflow-analogy.svg)

Both building and deploying software components as if they were values in a program’s call graph clearly shows the benefits and power of [purely functional programming][pfup]: ensuring correctness while allowing flexible composition and automatic optimizations.

In case of Nix, this enables reproducible builds and deployments, comfortable construction of packages and their variants from reusable building blocks, and features such as transparent binary caching.

The following table shows equivalence in terminology between build systems and programming language theory:

| Nix              | Bazel                     | Build Systems à la Carte | programming language |
| ---------------- | ------------------------- | ------------------------ | -------------------- |
| store object     | artifact                  | value                    | value                |
| builder          | (depends on action type)  | function                 | function             |
| derivation       | action                    | `Task`                   | thunk                |
| derivation graph | action graph, build graph | `Tasks`                  | call graph           |
| build            | build                     | application of `Build`   | evaluation           |
| store            | action cache              | `Store`                  | heap                 |

What Nix has been doing successfully since 2004 is encoding the [place-oriented][plop] paradigm of files and processes in terms of a [dataflow-oriented][dfop] programming language, and hooking its evaluation results back into the operating system.

Maybe surprisingly, that programming language is _not_ the Nix language, but what is called the _derivation language_ — a key mechanism in the Nix store.
The purely functional, lazily evaluated Nix language is merely a user interface to declare objects and their relations (values and functions) as expressions in the derivation language.
The Nix package manager’s command line tools in turn allow running operations on the store, such as deriving new values by evaluating these expressions, and exposing values to the Unix environment.

NixOS pushes this idea to the limit by capturing as much operating system state as possible into the realm of [declarative programming][dclp].

[pfup]: https://en.wikipedia.org/wiki/Purely_functional_programming
[plop]: https://www.youtube.com/watch?v=-6BsiVyC1kM&t=123s
[dfop]: https://en.wikipedia.org/wiki/Dataflow_programming
[dclp]: https://en.wikipedia.org/wiki/Declarative_programming

The following diagram shows a drastic simplification of how Nix interacts with the operating system:
It uses files as function inputs, and outputs are files again.
On the operating system, files can be run as processes, which, in turn, operate on files.
A build function also amounts to an operating system process (not depicted).


![Functional Mapping](functional-mapping.svg)

Since its inception, Nix development has been primarily occupied with imposing the abstraction of functional programming onto the messy, real world of our Unix lineage:
encoding and correctly dealing with object references in the file system, ensuring purity of function application, and working around built-in assumptions behind the mechanisms of different language ecosystems and build procedures — all while keeping performance acceptable.

Despite numerous rough edges remaining due to the enormous scope of the undertaking, Nix, `nixpkgs`, and NixOS have been working products for many years.
Currently there is much work in progress to improve the user experience by presenting a more consistent command line interface and better error messages.

However, something much more interesting lives in the long-term.
Which other results from programming language theory and mathematics will we be able to leverage to make software build quickly, work reliably, and further tame Unix?
Nix is begging the question:
what if everything on our computers was, in fact, a computer program?

[^1]: Special thanks to: John Ericson (@Ericson2314) from Obsidian Systems and Attila Gulyas (@toraritte) for taking time to discuss research papers and collaborating on explanations and terminology table; Neil Mitchell for checking that the results from "Build Systems à la Carte" are represented correctly; my colleague Théophane Hufschmitt (@regnat), who asked for a proper introduction to the leading motif of this article, and prompted a more detailed research on the history of ideas in computing; my colleague Jackline Yim (@JacklineYim) for her tireless support with wielding the English language.
[^2]: Under the most favorable conditions, where file names can be completely arbitrary, file systems can only accurately model static trees with labeled nodes. A similar attempt will fail for directed acyclic graphs, as directories cannot have more than one parent. Even when encoding structure as file paths in symlinks and file contents, changes to that structure cannot be accounted for by the file system itself.
[^3]: Even though relational databases and object stores are ubiquitous in application development, there exists no widely used computing environment which is not ultimately based on files in a file system. Interestingly, [non-programmers are increasingly confronted with user interfaces that do not mention files at all][students-files].
[^4]: Eric Raymond offers a critical view on hierarchical file systems in [The Art of Unix Programming][taoup] (2003), and mentions research taking different directions. See [capability-based operating systems][capos] for examples.

[taoup]: http://www.catb.org/~esr/writings/taoup/html/ch20s03.html#id3019140
[capos]: https://en.wikipedia.org/wiki/Capability-based_operating_system

[^5]: Even the term "file descriptor" for a reference to an object in a computer system shows its historic roots in associating digital electronic data with the contents of paper files.

[students-files]: https://www.theverge.com/22684730/students-file-folder-directory-structure-education-gen-z
