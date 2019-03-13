---
title: Capability is about free monads<br/>It's a bird… It's a plane… It's a free monad!
shortTitle: Capability is about free monads
author: Arnaud Spiwack
---

The subject of free monads is [resurfacing of
late][mcguire-free-monads], as it does from time to time. What
prompted this write-up is that Sandy McGuire [followed
up][mcguire-impredicative-free-monads] his post with a discussion about
an impredicative encoding (aka final encoding) of free monads:

```haskell
newtype Freer f a = Freer (forall m. Monad m => (forall t. f t -> m t) -> m a)
```

As far as I know, the subject of impredicative encoding of free monads
was first tackled, as many good things, [by Russell
O'Connor][oconnor-vl-free-monad], who calls them van Laarhoven
free monads. This is a fairly mathy read. But the key bit is this:

```haskell
-- (ops m) is required to be isomorphic to (Π n. i_n -> m j_n)
newtype VLMonad ops a = VLMonad { runVLMonad :: forall m. Monad m => ops m -> m a }
```

If you have any doubt: yes, it's precisely the same type; only spoken with
a different accent.

# Less is more

The comment, in Russell O'Connor's snippet, is crucial. Without it you
can't establish the isomorphism between `VLMonad` and the traditional
`Freer` monad.

That's because not all effects can be represented in free monads. The
prime example is exception handlers. You can make a function

```haskell
handle :: Freer MyEffect a -> Freer MyEffect a -> Freer MyEffect a
```

But it would have the property that `(handle s f) >>= k = handle
(s >>= k) (f >>= k)`: that is exceptions raised after exiting the
handler would still be caught by the handler. This is not a useless
function, but it is not an exception handler.

Being isomorphic to `Freer` is very good, but maybe you don't mind
having effects which are not representable. So let's drop the
constraint on the type and see where it gets us:

```haskell
newtype MyFreeMonad ops a = MkFree { runFree :: forall m. Monad m => ops m -> m a}
```

# Towards capability

Functions in our newfangled `MyFreeMonad` will look, as functions in a monad do, like
 
```haskell
somefunction :: A -> MyFreeMonad Ops B
```

But, after all, `MyFreeMonad` is only a newtype: we could very well
inline its definition.

```haskell
somefunction :: A -> forall m. Monad m => Ops m -> m B
```

The ordering of types and argument is not too idiomatic. So let's
rearrange them a little:

```haskell
somefunction :: Monad m => Ops m -> A -> m B
```

This may look like a familiar style of structuring effect, it is, for
instance the style [advertised by Éric Torreborre in a recent blog
post][torreborre-capabilities-as-records]. It's not really so much an
alternative to free monads as a slightly more liberal version of free
monads (remember: we have dropped a restriction along the way).

Personally, I find it rather tiresome to explicitly carry around the
capabilities (the `Ops` thing) at every function call. I'd rather keep
my function arguments for the program logic, and leave all the
plumbing to the monad. Therefore, I make `Ops` a type class (or
several):

```haskell
somefunction :: (Monad m, Ops m) => A -> m B
```

This is precisely the style of programming supported by the
[capability][hackage-capability] library (see also our [announcement
blog post][capability-announcement]).

# Closing thoughts

Free monads, capabilities-as-records and capabilities-as-type-classes,
are, essentially, three different flavours of the same thing (with free
monads technically being the more restrictive of the three).

Choosing between the three is, ultimately, a matter of taste. I really
like capabilities-as-type-classes because it pushes the boilerplate
outside of the program logic.

At the end, what really matters is the core idea shared by these three
approaches: capabilities should be expressed in terms of the program
logic, not in terms of implementation details.

[mcguire-free-monads]: https://reasonablypolymorphic.com/blog/freer-monads/
[mcguire-impredicative-free-monads]: https://reasonablypolymorphic.com/blog/too-fast-too-free/index.html
[oconnor-vl-free-monad]: http://r6.ca/blog/20140210T181244Z.html
[torreborre-capabilities-as-records]: https://medium.com/barely-functional/freer-doesnt-come-for-free-c9fade793501
[hackage-capability]: http://hackage.haskell.org/package/capability
[capability-announcement]: https://www.tweag.io/posts/2018-10-04-capability.html
