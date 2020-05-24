---
title: "How to make your papers run:  Executable formal semantics for your language"
shortTitle: "How to make your papers run"
author: Teodoro Freund
tags: [internship, formal-methods]
description: "How to use the Makam metalanguage to implement an executable formal semantics for a simple language."
---

This post will show a simple example of how to use [Makam][makam-ref],
a very powerful, not yet widely-known, programming language
to specify executable semantics of a simply typed lambda calculus
with recursion and numbers ([PCF][pcf-ref]).

I'm using Makam during my internship at Tweag to specify and experiment with
the design of a new configuration language. Makam allows me to try new ideas
quickly, without worrying about low-level details. In the long run, it'll also
work as documentation for the language.

[makam-ref]: https://github.com/astampoulis/makam/#readme
[pcf-ref]: https://en.wikipedia.org/wiki/Programming_Computable_Functions

## The Problem

> _Scene: EXTERIOR, BEACH, SUNNY_
>
> A woman is relaxing on a chair on the beach, clearly on vacation,
> drinking an ice-cold drink.
> Her phone beeps. The text message asks _'Can you come back to the office?
> We upgraded the compiler and the program you wrote last week stopped working'_.

Ok, I might have exaggerated a bit, but every programmer
has likely found themself, at least once, trying to understand why a program
that was working with compiler A, suddenly stopped working
when switching to compiler B. Or even worse, maybe it was working with compiler A,
version `4.5.64`, and it stopped after an update to version `4.6.23`.

Ideally, programming language designers and implementors would like to keep
this kind of situation to a minimum. And, when it does happen,
they'd like to have a definitive answer as to how the compiler should behave.
A specification. A ground truth.

There are many ways to specify this kind of document; two are commonly used.
A _reference implementation_ allows a skeptical programmer to check what the
expected behavior of a program is. However, it does not give an explanation for the reference behavior.
On the other hand, using _plain English_ can be pretty useful since it's understandable
by a human. However, it may be really hard to understand how a program should behave by
trying to run it in your head. Ideally we'd like something simple enough so a person can get
something out of it, but that is still executable and unambiguous.

We should ask ourselves, what would a PLT researcher do? A favorite when writing about
λ-calculus and the like is to use [operational semantics][op-sem]. The basic idea is to specify the
semantics of a program by stating what a given term evaluates to, assuming we know how to
evaluate its subterms.

Operational semantics seem to be an amazing deal: it's simple enough that a human can understand
it quite easily, while at the same time having a clear, unambiguous, computer-readable
syntax. But if you came across this kind of thing before, you probably know that it has a trick;
it assumes that the really difficult stuff (like variable substitution) is taken care of by the
_meta_-theory, thus relieving the researcher from the burden of writing a whole interpreter for it.
However, our goal is to have an executable specification, so we can't
just expect the _meta_-theory to take care of everything.

[python-bitwise]: https://docs.python.org/3/reference/expressions.html#binary-bitwise-operations
[op-sem]: https://en.wikipedia.org/wiki/Operational_semantics

## A solution!

What if I told you that there's a language out there that can take care
of abstractions, variable substitution, the transformation of programs—and
that its name is a palindrome?

_Enter Makam_

Makam is a dialect of λ-Prolog designed as a tool for rapid
language prototyping. I won't go into details,
but it's based on the idea of higher-order [logic programming][log-pro].

With Makam, we can write relations between higher-order terms,
stating what shape (or properties) different terms should have to be considered related.
Then, we can ask the Makam interpreter to try to come up with terms
that make a given proposition true, and we can even add restrictions to these terms.

In this paradigm, computing something is searching for a proof of a given proposition,
i.e., showing terms exists that make true all the requested constraints.
Add to this that Makam can work with higher-order terms, and you have a powerful enough
language to manipulate abstractions in a really high-level way.

[log-pro]: https://en.wikipedia.org/wiki/Logic_programming

## An example

Let's dive into the implementation of PCF on Makam.
PCF is a simply typed lambda calculus,
with Peano-like numbers and a `case` destructor for them,
as well as a primitive recursion operator. We'll work under the assumption
that we only care about closed terms (i.e., with no free variables).

In what follows, I'll go through the main parts we'd like to specify
(syntax, type checking, and operational semantics), first showing a
research-paper-like specification, followed by the Makam implementation.

### Syntax

We can think of PCF terms and types as:

$$
A, B ≔ Num ~|~ A → B \\\\[5pt]
s, ~ t, ~ u ≔ x ~|~ s ~ t ~|~ λx.s ~|~ fix ~ x.s ~|~ zero ~|~ succ(s) ~|~ \text{case} ~ s ~ \text{of} ~ \\{ 0 ⇒ t ~ ; ~ succ(x) ⇒ u \\}
$$

... where a type (noted \\(A\\), \\(B\\), ...) can be either a \\(Num\\),
representing numbers, or an arrow \\(→\\) from one type to another, representing functions.
On the other side, a term (noted \\(s\\), \\(t\\), ...) can be a variable (\\(x\\)),
an application of two terms, a lambda abstraction (\\(λ\\)), a recursive term (\\(fix\\)),
zero, the successor of a term or a case over a term to handle numbers.

Now, let's see how to write this in Makam:

```haskell
tp, term : type.

num : tp.
arrow : tp -> tp -> tp.

app : term -> term -> term.
lam : (term -> term) -> term.
fix : (term -> term) -> term.
zero : term.
succ : term -> term.
case : term -> term -> (term -> term) -> term.
```

On the first line, we're defining two new types, `tp` is the type of types and
`term` is the type of terms.
The next block is showing how `tp`s are constructed, `num` is a `tp` by itself,
but `arrow`s require two `tp` parameters to make a `tp`.
The last six declarations show how `term`s are constructed.
However, variables seem to be missing.

The answer lies in the constructor for `lam` (aka the binder for variables), notice
how in order to construct a λ-abstraction we don't take a `term` with some nebulous notion of free variable to bind, but rather a function from `term` to `term`.
But this is a Makam function, not a PCF function. Remember how PLT researchers
depended a lot on _meta_-theory? Well, Makam is like an executable _meta_-theory, so
we pass the responsibilities to the _meta_-language.

### Types

On top of that, we want to define a type system over these terms.

$$
\frac{Γ ⊢ s: A → B \hskip 1em Γ ⊢ t: A}{Γ ⊢ s ~ t: B} \hskip 1em \tau\text{-} app \\\\[15pt]
\frac{Γ, x \text{:} A ⊢ s: B}{Γ ⊢ λx.s: A → B } \hskip 1em \tau\text{-}lam \\\\[15pt]
\frac{Γ, x \text{:} A ⊢ s: A}{Γ ⊢ fix ~ x.s: A} \hskip 1em \tau\text{-}fix \\\\[15pt]
\frac{x \text{:} A ∈ Γ}{Γ ⊢ x: A} \hskip 1em \tau\text{-}var \\\\[15pt]
\frac{}{Γ ⊢ zero: Num} \hskip 1em \tau\text{-}zero \\\\[15pt]
\frac{Γ ⊢ n: Num}{Γ ⊢ succ(n): Num} \hskip 1em \tau\text{-}succ \\\\[15pt]
\frac{Γ ⊢ n: Num \hskip 1em  Γ ⊢ z: A \hskip 1em  Γ ⊢ λp.s: Num → A}
{Γ ⊢ \text{case} ~ n ~ \text{of} ~ \\{ 0 ⇒ z ~ ; ~ succ(p) ⇒ s \\} : A} \hskip 1em \tau\text{-}case
$$

These are pretty standard, so there should be no surprises here.
Let's implement them on Makam:

```haskell
typeof: term -> tp -> prop.


typeof (app S T) B :- typeof S (arrow A B), typeof T A.

typeof (lam S) (arrow A B) :-
    (x: term ->
     typeof x A ->
     typeof (S x) B).

typeof (fix S) T :-
    (x: term ->
     typeof x T ->
     typeof (S x) T).

typeof zero num.

typeof (succ N) num :- typeof N num.

typeof (case N Z P) Ty :-
    typeof N num,
    typeof Z Ty,
    (x: term ->
     typeof x num ->
     typeof (P x) Ty).
```

On the first line, we define a new relation called `typeof`, which defines a relation between
`terms`s and `tp`s. Notice how `typeof` is a constructor of a proposition (`prop`),
which is a primitive datatype on Makam representing relations,
over which the Makam interpreter can look for a proof. Let's go over a few of them:

- First, notice how the rule for `zero` doesn't have a `:-` part. This means that
  we don't need any hypothesis to type the term `zero`.
- Also, check the rule for `succ N`. There are two kinds of identifiers, lower and upper case.
  The lower case are used for the constructors we've been declaring, but the upper case are unification
  variables, which means they'll match with anything that makes the bit after `:-` true. In this
  case, `N` must be a `term` with type `num`.
- Now, look at `typeof (lam S) (arrow A B)`. Since we don't have variables anymore, we can't
  have a context like \\(Γ\\); we need something else. Let's read it together: `x: term ->` introduces
  a fresh `term`, called `x`. You can assume that `x` has type `A`
  (`typeof x A ->`). And you need to show that `S x` has type `B`.

You should be able to understand the remaining ones by yourself, but the important part
is to compare how similar these definitions are to the judgment ones
at the beginning of this section. In fact, they are arguably simpler
since we don't have to deal with the context bureaucracy at all in Makam.

Now, in case you were wondering why this is useful, if you load this into the Makam interpreter,
you can ask it for the type of any expression.
Let's try with \\(λx. \text{case} ~ x ~ \text{of} ~ \\{ 0 ⇒ succ(zero) ~ ; ~ succ(y) ⇒ zero \\} \\):

```haskell
typeof (lam (fun x => case x (succ zero) (fun _ => zero))) Ty ?
```

And get answered back:

```haskell
Ty := arrow num num.
```

### Evaluation

Finally, we can give meaning to our PCF formalization
by defining operational semantics rules. Remember that only closed terms
that actually type check reach this point. The final result is always a value,
which can either be a number of the form \\(succ(succ(...succ(zero)))\\)
(with zero or more \\(succ\\)s) or a λ-abstraction.

<!-- TODO: Reorder this figure once it's reviewed -->

$$
\frac{s ⇓ λx.s' \hskip 1em t ⇓ t' \hskip 1em s'[x/t'] ⇓ v}{s ~ t ⇓ v} \hskip 1em ε\text{-} app \\\\[15pt]
\frac{}{λx.s ⇓ λx.s} \hskip 1em ε\text{-}lam \\\\[15pt]
\frac{t[x/fix ~ x.t] ⇓ v}{fix ~ x.t ⇓ v} \hskip 1em ε\text{-}fix \\\\[15pt]
\frac{}{zero ⇓ zero} \hskip 1em ε\text{-}zero \\\\[15pt]
\frac{n ⇓ m}{succ(n) ⇓ succ(m)} \hskip 1em ε\text{-}succ \\\\[15pt]
\frac{n ⇓ zero \hskip 1em s ⇓ v}
{\text{case} ~ n ~ \text{of} ~ \\{ 0 ⇒ s ~ ; ~ succ(x) ⇒ t \\} ⇓ v} \hskip 1em ε\text{-}czero \\\\[15pt]
\frac{n ⇓ succ(m) \hskip 1em t[x/m] ⇓ v}
{\text{case} ~ n ~ \text{of} ~ \\{ 0 ⇒ s ~ ; ~ succ(x) ⇒ t \\} ⇓ v} \hskip 1em ε\text{-}csucc
$$

Again, this definition is quite standard, so let's go straight to the Makam implementation:

```haskell
eval : term -> term -> prop.


eval (app S T) V :-
    eval S (lam S'),
    eval T T',
    eval (S' T') V.

eval (lam S) (lam S).

eval (fix S) V :- eval (S (fix S)) V.

eval zero zero.

eval (succ E) (succ V) :- eval E V.

eval (case N Z _) V :- eval N zero, eval Z V.

eval (case SN _ P) V :- eval SN (succ N), eval (P N) V.
```

Similarly to `typeof`, `eval` will relate a `term` with the value it evaluates
to. This value is itself a `term`, so we should try to do a special `value` type for values—but that's out of the scope of this post. This is actually much easier than the `typeof` proposition,
but let's still go through some of the cases:

- `eval (fix S) V` tells us that `fix S` evaluates to a value `V`,
  when `S`, substituting its abstracted variable with `fix S`, evaluates to `V`,
  very similarly to \\(ε\text{-}fix\\) above.
- Notice how we have two cases for `case` expressions, one that only applies
  if the number evaluates to `zero`, and the other if the number evaluates to
  the successor of another number.

Then, you can go ahead and ask the Makam interpreter to evaluate something:

```haskell
>>> eval (lam (fun x => case x (succ zero) (fun _ => zero))) V?
 Yes:
V := lam (fun x => case x (succ zero) (fun _ => zero)).


>>> eval (app (lam (fun x => case x (succ zero) (fun _ => zero))) zero) V?
 Yes:
V := succ zero.
```

The first example doesn't change, since a function is already a value.
But the second example applies this same function to `zero` and gives back 1 (`succ zero`).
If we interpret `zero` as false and `succ zero` as true, then we could
probably name this function as `isZero`.

## Final (and further) thoughts

What I showed here is only the tip of the iceberg of what Makam is perfectly capable of handling.
I suggest reading
[this paper][makam-paper] by [Originate's][makam-creatorworks] [Antonis Stampoulis](http://astampoulis.github.io/), the creator of Makam, for more complete _(and way more complex)_ examples.
Be sure to also check out [Makam's repo][makam-github]—especially the [`examples`][makam-ex] directory—to learn plenty more.

Also, I only showed a tiny bit of what specifying a language means,
and there are many more challenges (that I don't even know of).
I found Makam to be a quite useful tool, hopefully
this post whetted your appetite and you'll start to specify every language out there.

[makam-paper]: https://www.researchgate.net/publication/326725343_Prototyping_a_functional_language_using_higher-order_logic_programming_a_functional_pearl_on_learning_the_ways_of_lPrologMakam
[makam-ex]: https://github.com/astampoulis/makam/tree/master/examples
[makam-creator]: http://astampoulis.github.io/
[makam-creatorworks]: https://www.originate.com/
[makam-github]: https://github.com/astampoulis/makam/
