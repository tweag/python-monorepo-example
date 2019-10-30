---
title: "Here You See the Small Porcupine Perched in Its Tree,<br/> Preparing and Crunching Some Data with Me"
shortTitle: "Porcupine: Announcing First Release"
author: Yves Parès
tags: haskell, data-science
description: "We're happy to announce the first release of Porcupine, an open source framework to express portable and customizable data pipelines."
---

*<span class="dropcap">P</span><span style="font-variant:
small-caps;">orcupines</span> are large rodents with coats of sharp spines, or
quills, that protect them against predators. The term covers two families of
animals: the Old World porcupines of family Hystricidae, and the New World
porcupines of family Erethizontidae.*

*The New World porcupines [...] live in wooded areas and can climb trees, where
some species spend their entire lives.*

*Porcupines have a relatively high longevity and had held the record for
being the longest-living rodent, with one individual living to 27 years, until
the record was broken in 2002 by a naked mole-rat living to 28
years. [(source)](https://en.wikipedia.org/w/index.php?title=Porcupine&oldid=922581516)*

So, long story short,
[Porcupine](https://hackage.haskell.org/package/porcupine-core) is a framework
aimed at making
<span class="tooltip" title="I know it makes the connection with actual porcupines kind of far-fetched, and I know they no longer hold the record for the longest-living rodent anyway, but I wasn't really going to name my library 'naked mole-rat', was I?">long-lived</span>
(in the sense that they are robust, shareable, and reusable), **por**table and
**cu**stomizable data **pi**peli**ne**s. That is, Porcupine provides a team
writing a data-hungry analytics application the tools to write a directed
graph of tasks, where each task can depend on a tree of resources (think "a
filesystem tree"). Porcupine does this while remaining oblivious of the actual physical locations of
these resources, of their format, and of the protocols used to address them
(that's the “portable” part), and can expose any configuration parameter to the
outside world (that's the “customizable” part).

Porcupine provides tools for three different professions:
software developers, data scientists, and DevOps engineers. But by itself,
`porcupine-core` (the main package) is neither a data science nor a DevOps tool. Instead, it implements a central, principled basis for data applications to be
used by people with these different skill sets. It unites their worlds by _cleanly separating them_: the software developer can stay focused on data serialization and managing different data sources, the data scientist can stay focused on analytics, while the DevOps engineer can see, administer, and
modify the entire pipeline's configuration and inputs/outputs.

I'm going to focus on the developer part of the API in this blog post. For a broader overview of Porcupine (notably from an analyst's point of view),
you can watch [my talk at the latest Haskell
Exchange](https://skillsmatter.com/skillscasts/14236-porcupine-flows-your-rows-with-arrows).

## Abstracting over data sources and sinks

Cleanly separating sources of data from the code that processes them while
maintaining memory consumption guarantees is something that has
[already](https://hackage.haskell.org/package/io-streams)
[been](https://hackage.haskell.org/package/iteratee)
[extensively](http://hackage.haskell.org/package/conduit)
[studied](http://hackage.haskell.org/package/pipes)
[in](http://hackage.haskell.org/package/streamly)
[Haskell](http://hackage.haskell.org/package/streaming), including on
[this](https://www.tweag.io/posts/2017-07-27-streaming-programs.html)
[very](https://www.tweag.io/posts/2017-10-05-streaming2.html)
[blog](https://www.tweag.io/posts/2017-11-01-streaming-and-foldl.html).
Porcupine relies on [`streaming`](http://hackage.haskell.org/package/streaming)
quite extensively. So let's see where these streams originate and end in
Porcupine:

```haskell
class ( MonadMask m, MonadIO m, TypedLocation (LocOf l) ) =>
    LocationAccessor m (l::Symbol) where
  data LocOf l :: *
  locExists :: LocOf l -> m Bool
  writeBSS :: LocOf l -> BSS.ByteString m r -> m r
  readBSS :: LocOf l -> m (BSS.ByteString m ())
```

This `LocationAccessor` class is a slightly simplified version of the one
actually in use, but it shows the main idea. The `LocationAccessor` is, in the
end, a monad `m`, but since the same final monad stack can act as several
location accessors, we also need this type-level String (the `Symbol`) to
disambiguate the implementation we are targeting at a given moment. So each
implementation defines a new backend for opening and writing data [byte
streams](http://hackage.haskell.org/package/streaming-bytestring).

This is the instance for local resources (i.e., local files):

```haskell
instance (MonadResource m, MonadMask m) => LocationAccessor m "resource" where
  newtype LocOf "resource" = L URL
    deriving (Functor, Foldable, Traversable, ToJSON, TypedLocation)
  ...
```

Here we declare that `"resource"` is a `LocationAccessor` in any monad that can
provide [`MonadResource`](http://hackage.haskell.org/package/resourcet). The
`LocOf` type is a data family
[associated](http://amixtureofmusings.com/2016/05/19/associated-types-and-haskell/)
with the class. This gives every backend the capacity to declare its own
resource identifier type as long as this type satisfies the `TypedLocation`
constraint (mostly, that this type is representable in JSON and that it contains
some notion of "filetype", like an extension). A filepath-based or URL-based
backend (like `"resource"`) can just wrap the `URL` type provided by
Porcupine. The other location accessors currently provided are
[`"aws"`](http://hackage.haskell.org/package/porcupine-s3) (to read and write S3
objects) and [`"http"`](http://hackage.haskell.org/package/porcupine-http), and
other backends could be added in their specific packages in the future.

## Tasks

Let's now take a little detour to talk about the _tasks_. The main type here is
`PTask`, which is the frontier between the developer part of the API and that of
the data scientist. Here's how you could write a simple task in Porcupine:

```haskell
oneTask :: (LogThrow m) => PTask m () ()
oneTask =
  loadData myInput >>> arr processData >>> writeData myOutput
```

`PTask m i o` means that our task runs on some base monad `m`, takes `i` as an
input, and returns `o` as an output. The [`(>>>)`
operator](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Category.html)
sequentially composes two tasks, and
[`arr`](https://wiki.haskell.org/Arrow_tutorial) lifts a pure function to a
`PTask`. So `oneTask` fetches data from `myInput`, feeds it into `processData`,
whose result is in turn fed to `writeData` to store the result in
`myOutput`. The `loadData` and `writeData` calls are the ones which internally pull and write byte streams via the `LocationAccessor` typeclass.

The type of `oneTask` indicates that our task needs to run on a base monad
that supports [throwing
exceptions](http://hackage.haskell.org/package/exceptions-0.10.3/docs/Control-Monad-Catch.html#t:MonadThrow)
and [logging](http://hackage.haskell.org/package/katip). Eventually, it's the same `m` as the one we had in the instances of `LocationAccessor`: it is the final monad in which our application runs. However, you can notice that
the constraints are not the same, namely that our task doesn't have to care at
all that in the end, `m` needs to implement `MonadResource` and `MonadMask`
if we want to access local files: tasks and sources are decorrelated.

The `myInput` and `myOutput` bits are called `VirtualFiles`. They are the
frontier between the data scientist part of the API and that of the DevOps engineer. They
identify and expose a resource to the outside world so that resource can be
mapped to an actual physical resource before the pipeline runs. Both `myInput`
and `myOutput` have a _virtual path_ decided by the user (say, for the sake of
simplicity, `"/myInput,json"` and `"/myOutput.json"`), and they contain a collection of
possible serialization and deserialization functions, that are selected at
execution depending on the type of the file to which a `VirtualFile` is mapped.

We showed here the sequential composition of tasks with `(>>>)`, but you can also compose them in a parallel fashion with
[`(***)`](http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Arrow.html#v:-42--42--42-).
You can also use the [arrow
notation](https://www.haskell.org/arrows/syntax.html) for some syntactic sugar
akin to the monad `do` notation.

## Configuration and execution

So what happens when `oneTask` finally runs? This is done via a call to
`runPipelineTask`:

```haskell
main = runPipelineTask
       (FullConfig "simple-stuff" "myconfig.yaml" "." ())
       (baseContexts "")
       oneTask
       ()
```

We are not going to describe every parameter here (though check out the
documentation or the repository's
[GitHub pages](https://tweag.github.com/porcupine/)), but this is
where everything gets glued together. The `baseContexts` call is where the final
monad is created, by composing together the various capabilities our application
will need. `baseContexts` is where the implementations of logging and
`MonadResource` comes from. So it provides the `"resource"` location accessor,
and you can stack extra location accessors like `"http"` or `"aws"` on top of
it.

Once compiled, this pipeline can output its default configuration in the
`myconfig.yaml` file. It looks like this:

```yaml
locations:
  /: .
  /myInput:  _.csv
  /myOutput: _.json
```

This gives you a view of the [_Porcupine
Tree_](https://www.youtube.com/watch?v=WE_tMWD1QFo) (the tree of resources)
required by `oneTask`. Here you can see the _virtual paths_ that we mentioned
before, associated to some mappings. `runPipelineTask` is able to just expose
these virtual paths to the outside world via this configuration file, no extra
work is needed. For concision, mappings can start with an underscore token, in
which case the physical resource full path is derived from the mapping of the
root virtual path and from the mapped virtual path itself. Here, the root
virtual path is mapped to `"."`, the current working directory. So the
configuration here above tells the pipeline to look for `myInput` in
`./myInput.csv`, and to write `myOutput` to `./myOutput.json`.

So what happens if we change the configuration? For instance, if we say:

```yaml
locations:
  /: .
  /myInput:  http://someapi.org/api/v1/input.csv
  /myOutput: _.json
```

Well, we mentioned that `runPipelineTask` puts together a stack of
`LocationAccessors`, and that each one of these accessors has one `LocOf`
associated datatype which is parsable from JSON (or YAML). Well, for every
physical path or URL mapped to a virtual path in the YAML config, Porcupine goes through the stack of `LocationAccessors` and takes the first one that can
parse this physical location. If some location cannot be parsed by any of the
`LocationAccessors` in place, then Porcupine stops with an error before trying
to run anything. This is also true if a `VirtualFile` is mapped to a file type
for which it doesn't know any serialization/deserialization function (say, if
the end user mapped it to a `*.json` file but the only serialization function we
know is for `csv`). This way, you get an _early failure_ with an error message
that tells you which mapping is problematic, and your pipeline doesn't even
start running. This detection is invaluable when running long pipelines that run
CPU-intensive operations for minutes or hours: without Porcupine's early failure feature this computation time would be
wasted should the program crash because of an invalid URL to write the result
to.

## Closing words

Porcupine is built on top of our
[`funflow`](https://www.tweag.io/posts/2018-04-25-funflow.html) open source
library. `funflow` is also an arrow-based framework that gives us cacheable
tasks and a remote cache. Porcupine has been developed as part of a project
funded by [Novadiscovery](https://www.novadiscovery.com), which uses it
internally to express systems biology simulation pipelines.

`porcupine-core` is released at the same as two companion libraries:
[`reader-soup`](http://hackage.haskell.org/package/reader-soup), which provides
an automated [ReaderT
pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) and is
the machinery `runPipelineTask` uses to automatically create the Reader monad it runs the application in, and
[`docrecords`](http://hackage.haskell.org/package/docrecords), an extensible
records library based on [`vinyl`](http://hackage.haskell.org/package/vinyl)
that permits us to express records of parameters with docstrings and default
values which the pipeline can then expose to the outside world.

I hope you enjoyed this quick introduction to Porcupine's API for developers. Please have a look at the [GitHub
pages](https://tweag.github.io/porcupine/) and the [examples on the GitHub
repository](http://www.github.com/tweag/porcupine/tree/master/porcupine-core/examples), such as [these for the `"http"` location
accessor](http://www.github.com/tweag/porcupine/tree/master/porcupine-http/examples), and watch my [Haskell Exchange
talk](https://skillsmatter.com/skillscasts/14236-porcupine-flows-your-rows-with-arrows)
to learn more. We have a
[Gitter](https://gitter.im/tweag/porcupine) for Porcupine's users and
developers where any issues or questions are welcome. Please hop in and say hi! :)
