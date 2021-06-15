---
title: An AsciiDoc processor and Pandoc front-end in Haskell
shortTitle: AsciiDoc in Pandoc and Haskell
author: Guillem Marpons
description: |
  Announcing asciidoc-hs, a new AsciiDoc parser and converter in
  Haskell, adapted for use with Pandoc.
tags: [haskell, fellowship]
---

[AsciiDoc](https://docs.asciidoctor.org/asciidoc/latest/) is a plain-text
writing format that tries to combine the readability and intuitiveness of
Markdown with the rigorous requirements of technical authoring and publishing.
It offers power and flexibility without resorting to HTML or tool-specific
extensions for essential syntax such as tables, description lists, footnotes,
or features like automatically generating a table of contents.

This post presents a new AsciiDoc parser and converter in Haskell, called
[`asciidoc-hs`](https://github.com/gmarpons/asciidoc-hs). It has been developed
with the support of a Tweag Open Source Fellowship, and is BSD licensed. It
aims to be the first AsciiDoc processor that can be directly used as a Pandoc
front-end[^1]. It is still [far from complete](https://github.com/gmarpons/asciidoc-hs/wiki/AsciiDoc-Compatibility-Matrix)
in terms of AsciiDoc syntax and features, but it hopefully establishes a solid
foundation to build upon.

## A vision for a new AsciiDoc processor

The de facto definition of modern AsciiDoc is the language recognized by
Asciidoctor, a processor written in Ruby. Fortunately, this will change soon,
as a [standardization process](https://www.eclipse.org/org/workinggroups/asciidoc-charter.php)
has recently started. Within a year or so, we should have a proper
specification of what AsciiDoc really is, mostly based on what Asciidoctor does
today. The [AsciiDoc Working Group](https://projects.eclipse.org/working-group/asciidoc)
is hosted by the Eclipse Foundation and companies like Couchbase, Red Hat,
VMware and OpenDevise (the maintainers of Asciidoctor) are supporting the
initiative.

With this growing interest in AsciiDoc, new implementation projects (in
[Java](https://projects.eclipse.org/proposals/eclipse-austen),
[Go](https://github.com/bytesparadise/libasciidoc), or
[Rust](https://gist.github.com/jamesmunns/06f70b68bde8e1394b79e936a8599718))
are being announced or released. Haskell has a reputation of being
particularly well equipped for all kinds of formal language processing tasks,
including parsing and AST transformation. Furthermore, AsciiDoc is probably the
only major plain-text format not fully supported by Pandoc even if it is a
[popular request](https://github.com/jgm/pandoc/issues?q=is%3Aissue+is%3Aopen+sort%3Areactions-%2B1-desc)
from Pandoc users. This gave rise to the idea of a processor in Haskell that
targets Pandoc’s intermediate representation (in JSON format).

`asciidoc-hs` draws on [`commonmark-hs`](https://github.com/jgm/commonmark-hs)
to offer both an executable that can be used right away in combination with
Pandoc and a library for building more advanced tools on top (and which leaves
the door open to future release integrated as a Pandoc reader).

Targeting Pandoc is a major strength of the project -- it enables many
conversions and transformations already implemented and maintained by a
wide community -- but other use cases have also been considered. Special
attention has been devoted to features expected from a modern technical writing
toolchain that are difficult to fulfill by current tools such as Asciidoctor,
resulting in the following project goals.

### Compatibility with current documents

Our tool needs to recognize AsciiDoc as it is used in today's documents, and
this means that a reasonable degree of compatibility with Asciidoctor is
necessary. Full compatibility would be prohibitive to achieve and probably
undesirable, as many Asciidoctor behaviors are undocumented and forced by the
implementation.

### Commitment to the future standard

To be future-proof we need to embrace the [goals and
vision](https://www.eclipse.org/lists/asciidoc-lang-dev/msg00001.html) arising
from the standardization process. The future definition of the language is
going to depart from the implementation-based definition we have today, and we
need a software architecture that can adapt to the planned changes. At some
point there will be a Technology Compatibility Kit (TCK) that we want to comply
with.

### IDE-enabling architecture

Docs-as-code is already mainstream in technical writing communities. Authors
(and, needless to say, developers) increasingly expect the edition of documents
to make the best use of all the facilities found in current code editors and
IDEs: live linters, syntax highlighting, text completion, live preview,
contextual information and actions, fast navigation and modification of
document structure, etc. A Language Server Protocol implemented on top of an
AsciiDoc parser would be a very interesting addition to the AsciiDoc landscape,
and `asciidoc-hs` could fill this gap.

I have tried to define an architecture that can accommodate an efficient and
[feature-complete implementation](https://toastui.medium.com/the-need-for-a-new-markdown-parser-and-why-e6a7f1826137)
of the aforementioned facilities. They require, among others, easy AST search
and update, and AST nodes with source range mappings and concrete syntax
(including space)[^2][^3].

In the long run I would like to try implementing incremental parsing (i.e.,
avoid parsing the whole document when only small editions have been performed).

### Semantically-rich scriptability

Pandoc compatibility also allows for easily writing document transformations in
the form of filters. Filters can be used, for example, to adapt AsciiDoc
cross-references or citations to the format required for popular static site
generators. But AsciiDoc is a semantically richer language than the Pandoc AST
can hold. In the long run, I want a similar extensibility mechanism at the
AsciiDoc level, ideally with the possibility of source-to-source
transformations with optional exact-print.

## Some challenges and corresponding design decisions

Writing a parser for AsciiDoc -- with the ambition of becoming a complete
AsciiDoc parser at some point -- has been far more difficult than anticipated.
AsciiDoc is large, complex, and mostly implementation-defined. I have found
many parsing-technology related advice -- very often inspired by programming
language implementation needs -- not easily applicable to AsciiDoc, if at all,
since the needs of markup languages are different.

### Inlines, blocks, and all the rest

The structural elements of the language can be split, like in HTML, into inline
elements and block elements. Early in the project I took the decision of
writing two independent parsers for inlines and blocks (with different parser
types, and maybe different token types). I think the decision has paid off in
terms of modularity and simplicity. I have since discovered that both parsers
demand different features (e.g., block parsing needs to occur in the IO monad,
as we will see). Furthermore, having a separated parser for inlines opens the
opportunity for parallel processing of inlines of different blocks.

In the end I have implemented three parsers and a half:

- A [parser for inlines](https://github.com/gmarpons/asciidoc-hs/blob/main/src/Text/AsciiDoc/Inlines.hs).
- A [parser for blocks](https://github.com/gmarpons/asciidoc-hs/blob/main/src/Text/AsciiDoc/Blocks.hs).
- A (simpler) [parser for element attribute lists](https://docs.asciidoctor.org/asciidoc/latest/attributes/element-attributes)[^4]
  that can be called by both inline and block parsers.
- A small library of [line parsers](https://github.com/gmarpons/asciidoc-hs/blob/main/src/Text/AsciiDoc/LineParsers.hs),
  for easily describing common patterns in block parser definitions, like a
  repeated char at the beginning of a line.

All of them have been written with the parser combinator library Parsec because
it is the parsing tool used all across the Pandoc ecosystem.

### Inline parsing

Inline parsing is quite challenging. Despite the absence of a formal
definition, current users have a sense of what can be written and expect a
particular response. Let’s see an example of AsciiDoc source and its
interpretation:

```asciidoc
[.green]*a sentence in bold*
```

Text enclosed in asterisks (\*) is meant to be in boldface. The attribute
enclosed in square brackets specifies that it must be colorized in green as
well. The asterisks and the bracketed fragment affect formatting, but they are
not part of the content of the text.

Now, if we remove the closing asterisk, we still have to accept the sequence of
characters, but the interpretation changes completely: there is no text in
boldface, the first and only asterisk is now simply an asterisk, and the list
of attributes and its brackets are a string “[.green]” printed verbatim as part
of the contents. The parser cannot try to fix our broken format because, as
happens with Markdown, AsciiDoc users expect any Unicode sequence to be
accepted[^5].

Things can be more involved with nested styles, format escaping sequences, and
the like. The result is a language that cannot be parsed [deterministically](https://en.wikipedia.org/wiki/Deterministic_parsing)[^6]
and that demands to be treated as context-sensitive if we want to avoid much
complexity and repetition in our grammar and parser. Most current AsciiDoc
processors side-step these difficulties using a battery of regex-based
substitutions for inline formatting, instead of constructing a proper AST[^7].
Even the standardization working group has not committed to deliver a formal
grammar as part of AsciiDoc Spec v1.0.

Having a complete AST is instrumental for many use cases thus, unaware of how
hard it could be, I tried many different grammars[^8] (and grammar formalisms)
for inline parsing, until I defined one I am moderately satisfied with (and
that I plan to contribute to the standardization effort). Some of my
(provisional) findings on grammar formalisms are:

- EBNF is expressive and intuitive, but defining an _unambiguous grammar_ for
  AsciiDoc -- even ignoring the context-sensitive bits -- could be prohibitively
  hard.
- [PEGs](https://en.wikipedia.org/wiki/Parsing_expression_grammar) are
  attractive because they are expressive (but not as intuitive as EBNF),
  unambiguous by definition, and can be easily converted into (efficient)
  executable code[^9]. The problem is that choices about how to resolve the
  inherent ambiguities of the language are sometimes implicit, rather than
  explicit, and can easily remain hidden to both language designers and
  users[^10].

My [grammar for inlines](https://github.com/gmarpons/asciidoc-hs/wiki/Inline-parsing)
is EBNF-based, but augmented with extra-syntactic predicates to explicitly
resolve ambiguities (similar to the [semantic predicates](https://github.com/antlr/antlr4/blob/master/doc/predicates.md)
found in many parser generators). Those predicates need supporting data
structures to be defined and implemented, but we need them for the
context-sensitive parts anyways.

Even if not as elegant as a pure-EBNF definition, I think my formalism has
already provided important benefits:

- The extra-syntactic predicates are in fact predicate placeholders that can be
  filled-up differently to explore possible designs. For example, the following
  AsciiDoc fragment mixes boldface and italics without proper nesting:
  ```asciidoc
  *a _b* c_
  ```
  Different disambiguation strategies are possible: e.g., boldface always takes
  precedence, or the first style to be correctly closed takes precedence, etc.
  It is easy to implement a new strategy by tweaking one or two predicates.
  Moreover, the ambiguity resolution is explicitly stated in code and easily
  linked to high-level design choices that can be communicated to AsciiDoc
  writers.
- I have been able to (informally) prove that my parser accepts any Unicode sequence as
  input (without resorting to catch-all cases).
- The grammar and accompanying predicates are compact enough to be easily
  extended and still be amenable to reason about ambiguity and other
  properties, and can be used to discover corner cases.

### Block parsing

Block parsing is line-oriented. This means that the document is first sliced
into lines, and full lines accepted or rejected by individual block parsers. In
some sense lines can be seen as tokens, but block parsers still have access to
the sequence of characters they contain. As said, I've developed a separate module of line
parsers to easily describe common patterns.

Block parsing is easier than inline parsing. Backtracking can be avoided
entirely if two conditions are met: the appropriate data structures for
context-sensitive parsing are in place, and we allow for
[delimited blocks](https://docs.asciidoctor.org/asciidoc/latest/blocks/build-basic-block/#delimited-blocks)
to end without a closing delimiter (which mimics Asciidoctor’s behavior).

An example of context-sensitivity is the following. An unordered list item is
marked using a sequence of asterisks (\*) of any length. The first item of a
list determines the mark of the subsequent items, and we can start a sub-list
with a mark of different length (i.e., a different number, not necessarily
greater, of asterisks). So, the number of asterisks at the beginning of a list
item does not determine the list nesting structure. We need to keep track of
the used marks to discover the nesting structure. Something similar happens
with delimited blocks and its nesting. As a consequence, our parser uses a
stack of open blocks -- with a stack of open list items inside -- as a supporting
data structure.

### Include expansion

AsciiDoc supports C-preprocessor-like directives:
[includes](https://docs.asciidoctor.org/asciidoc/latest/directives/include/) and
[conditional processing](https://docs.asciidoctor.org/asciidoc/latest/directives/conditionals/).
They suggest some kind of, well, preprocessing, or phase
distinction in addition to that of blocks and inlines. But includes in AsciiDoc
are not really pre-processable: their expansion is deeply entangled with block
parsing.

AsciiDoc also features variables in the form of
[document attributes](https://docs.asciidoctor.org/asciidoc/latest/attributes/document-attributes/),
and there is a circular dependency between include expansion, attribute
resolution and block parsing:

- A line with contents `:key: value` is considered an attribute entry (aka
  variable definition) in some places, but not others (e.g., inside
  a [source code block](https://docs.asciidoctor.org/asciidoc/latest/verbatim/source-blocks/)),
  and this circumstance is only known during (block) parsing.
- Include directives can receive attributes whose value affects how the
  include is expanded.
- Include expansion affects both attribute resolution and parsing:

  - An included file can leave open any number of delimited blocks or other
    constructions, thus affecting subsequent parsing, or the context in which
    a line `:key: value` is parsed.
  - New attributes can be defined in includes.

So, a separated preprocessing pass seems to complicate things for no clear
benefit. I haven't implemented preprocessor directives yet, but I've
designed and tested a solution to be
integrated shortly with block parsing.

As explained, block parsing is line oriented: each block parser is defined
combining some parsers that operate on a single input line. To run the
resulting combination the following function is called, where `Parser m a` is
the type for block parsers:

```haskell
lineP :: MonadIO m => LineParser a -> Parser m a
```

Function `lineP` is the only place in the code base where include expansion is
handled. The function needs to be run on top of the IO monad to have access to
the filesystem, and it relies on Parsec’s `getInput`/`setInput` functions.[^11]

## Conclusion

`asciidoc-hs` is probably the most serious attempt to date at an AsciiDoc parser
and converter in pure Haskell. It can be used as it is, as a Pandoc front-end,
to convert AsciiDoc source files to any output format supported by Pandoc, thus
filling an important gap in an AsciiDoc ecosystem that is in full bloom.

The tool does not yet cover the [entire AsciiDoc syntax](https://github.com/gmarpons/asciidoc-hs/wiki/AsciiDoc-Compatibility-Matrix) (not even close), but
the foundations for a tool that can be incrementally improved have been laid
out. Some of the most challenging problems posed by AsciiDoc processing have
been tackled -- and hopefully solved in a solid way -- so that adding [new
syntactic features](https://github.com/gmarpons/asciidoc-hs/labels/new%20syntax)
should be relatively straightforward. It goes without saying that all
contributions are very welcome.

I want to end by thanking Tweag IO for the funding, technical validation of the
proposal and supervision in the initial phases of this project.

[^1]:
  There have been former attempts in the past, but they have been
  abandoned. It is also possible to feed Pandoc with an AsciiDoc source by
  first converting to Docbook.

[^2]:
  As a test, I have implemented full inline syntax storage in the AST
  following a similar approach to what the [sv](http://hackage.haskell.org/package/sv)
  package does for CSV. When more features are implemented I will evaluate if
  extending the same pattern to block nodes.

[^3]:
  Those are difficult to implement with major current AsciiDoc processors
  because, to begin with, they do not generate a complete AST of the document,
  as explained in the section about inline parsing.

[^4]:
  AsciiDoc inlines and blocks can be annotated with a number of attributes
  for built-in and user-defined styles, behavior and metadata.

[^5]:
  Linters can try to find plausible formatting mistakes, but this is
  another story.

[^6]:
  I.e., we need backtracking, or another technique to evaluate
  different, arbitrarily long parsing alternatives.

[^7]:
  As a consequence, processors like Asciidoctor sometimes generate invalid
  HTML when different format styles are mixed.

[^8]:
  I explored all the grammars I could find on the Internet. All of them
  were very incomplete with the exception of the one used by the project
  [libasciidoc](https://github.com/bytesparadise/libasciidoc), which is a large
  and difficult to extend PEG that mixes inline and block parsing.

[^9]:
  Translating a PEG to an efficient Parsec parser is difficult, but this is
  not the main reason why we have discarded PEGs.

[^10]:
  I find [this post](https://blog.reverberate.org/2013/09/ll-and-lr-in-context-why-parsing-tools.html)
  very informative in this respect.

[^11]:
  A non-IO implementation would be possible with a parsing library that
  supports online parsing, like attoparsec. It would need to interrupt parsing
  when an include directive is found, take the continuation of the partial
  parsing and pass the included file to it. Then, get another continuation in
  return and pass it the lines following the include.
