---
redirect_from: [/posts/2018-12-20-asterius-todomvc.html]
title: "Asterius GHC WebAssembly backend reaches TodoMVC"
shortTitle: TodoMVC in Haskell via WebAssembly
author: Cheng Shao
tags: [haskell, asterius]
---

Getting your browser to print "Hello World!" is fun, and a milestone
for compiler writers targeting the web. Soon expectations change. Can
you call existing code? Can you write a moderately complex web app?
How fast does it run? [Previously][asterius-ffi], we showed how
Haskell can call JavaScript and vice versa using a foreign function
interface. Today we demonstrate that writing web apps in Haskell
compiled to WebAssembly works well enough that TodoMVC, the more
intricate "Hello World!" of web apps, works in your
browser. [Try it out!][tweag-todomvc]. (Since our emitted code uses the
`BigInt` feature, currently you need a recent version of Chromium to
run it.)

[asterius-ffi]: https://www.tweag.io/posts/2018-09-12-asterius-ffi.html
[tweag-todomvc]: https://asterius.netlify.app/demo/todomvc/index.html

## Trying this at home

The complete source code of TodoMVC is included in asterius source
tree
[here](https://github.com/tweag/asterius/tree/master/asterius/test/todomvc).
You can try it [online][tweag-todomvc], or run it locally. As usual,
the simplest way to do that is using our pre-built Docker image:

```
$ docker run -it --rm -v $(pwd):/mirror terrorjack/asterius
root@16758a0232ba:~/asterius# ahc-link --input asterius/test/todomvc/todomvc.hs --browser
[INFO] Loading boot library store from "/root/asterius/.stack-work/install/x86_64-linux/ghc-8.7/8.7.20181115/share/x86_64-linux-ghc-8.7.20181115/asterius-0.0.1/.boot/asterius_lib/asterius_store"
[INFO] Populating the store with builtin routines
[INFO] Compiling asterius/test/todomvc/todomvc.hs to Cmm
[INFO] Marshalling from Cmm to WebAssembly
[INFO] Marshalling "WebAPI" from Cmm to WebAssembly
[INFO] Marshalling "TodoView" from Cmm to WebAssembly
[INFO] Marshalling "Main" from Cmm to WebAssembly
[INFO] Marshalling "ElementBuilder" from Cmm to WebAssembly
[INFO] Attempting to link into a standalone WebAssembly module
[INFO] Converting linked IR to wasm-toolkit IR
[INFO] Writing WebAssembly binary to "asterius/test/todomvc/todomvc.wasm"
[INFO] Writing JavaScript to "asterius/test/todomvc/todomvc.js"
[INFO] Writing HTML to "asterius/test/todomvc/todomvc.html"
```

After running the above commands, copy the `asterius/test/todomvc`
directory somewhere else, run `npm install` in it to fetch common
artifacts shared by all TodoMVC implementations, then you can fire up
a local HTTP server and browse `index.html` to evaluate it.

Next, we'll highlight some parts of our TodoMVC implementation,
explaining improvements of asterius itself along the way.

## Improved Haskell/JavaScript data marshalling

In our previous [post][asterius-ffi], we demonstrated how to write
functions to convert between Haskell types (e.g. `String`) and their
JavaScript counterparts. Since the marshaling of certain types is such
a common task, we now include them as a part of our standard
libraries, and even implemented some of them as runtime builtins for
better performance.

Check
[`Asterius.Types`](https://github.com/tweag/asterius/blob/master/ghc-toolkit/boot-libs/ghc-prim/Asterius/Types.hs) for
conversion functions between
`JSArrayBuffer`/`JSString`/`JSArray`/`JSObject`/`JSFunction` types and
their Haskell
counterparts.
[`Asterius.ByteString`](https://github.com/tweag/asterius/blob/master/ghc-toolkit/boot-libs/bytestring/Asterius/ByteString.hs) includes
functions for converting between `JSArrayBuffer` and strict
`ByteString`s.

Previously, `JSRef` was still a magic type that isn't defined
anywhere. Now we renamed it to `JSVal` and made it less magical: it's
defined in `Asterius.Types`, and whenever you need to use it in
a `foreign import javascript` declaration, you need to import that
module. Also, it's now possible to define and use `newtype`s for
`JSVal` and other types in `Asterius.Types`; automatic
wrapping/unwrapping in foreign declarations doesn't work yet, but
we're actively working on that front.

## DOM tree nodes as a datatype

An essential task in writing a TodoMVC implementation is building
a part of the DOM tree that corresponds to the current app state. In
vanilla JavaScript, we have stateful interfaces for this purpose.
Since we are using Haskell, it would be nicer to add a layer of
abstraction here: use a plain old datatype to model the DOM tree and
encapsulate the logic of converting a "pure" tree to a "real" tree
into one single function.

The [`ElementBuilder`](https://github.com/tweag/asterius/blob/master/asterius/test/todomvc/ElementBuilder.hs) module contains our modeling:

```
data Element
  = Element { className :: String
            , attributes :: [(String, String)]
            , children :: [Element]
            , hidden :: Bool
            , eventHandlers :: [(String, JSObject -> IO ())] }
  | TextNode String

emptyElement :: Element
buildElement :: Element -> IO JSVal
```

The `Element` type allows us to create a DOM tree node, specify its
class name and attributes, attach child nodes, and insert Haskell
callbacks as event handlers. After using `buildElement` to convert it
to a real JavaScript node, we can use interfaces like
`replaceChild`/`replaceWith` to attach that node to the webpage.

## Modeling and persisting TodoMVC app state

Another task is modeling and persisting app state. When the browser
tab is reopened, we don't want to forget our previous todo entries!
Similar to how we model DOM tree nodes with a datatype, we'd also like
to model the whole app state as a datatype:

```
data Todo = Todo
  { key, text :: String
  , editing, completed :: Bool
  } deriving (Generic)

instance Binary Todo

newtype TodoModel = TodoModel
  { todos :: [Todo]
  } deriving (Generic)

instance Binary TodoModel

loadModel :: String -> IO (Maybe TodoModel)
saveModel :: String -> TodoModel -> IO ()
modifyTodo :: TodoModel -> String -> (Todo -> Todo) -> TodoModel
```

Each todo entry is indexed by a randomly generated `key`, so later we
can modify the content of a single `Todo` using `modifyTodo`. The todo
model is simply a list of `Todo`s, and it can be serialized using the
`binary` package. Of course, we really want to use `FromJSON`/`ToJSON`
here, and we can assure you that `aeson` support is definitely
planned!

The `loadModel`/`saveModel` functions load/save a `TodoModel` value in
`localStorage`. When our app starts, it tries to load the previous
model first, and upon failure, falls back to an empty todo list as the
initial state. We perform a `saveModel` whenever the current app state
is changed.

## Applying app state to the real world

Now, we're equipped with datatypes to model our app state and the DOM
tree, the remaining task is: perform the plumbing between the
functional world and the real world.

Our method of applying pure state is quick and dirty: we implement
a global store of our app state with `IORef TodoModel`, and a `render`
callback which is invoked whenever the app state changes. `render`
queries the current `TodoModel`, builds the DOM tree of the new todo
list and replaces the old one, and also saves the `TodoModel` to
`localStorage`.

We also need to create event handlers to process certain events: e.g.
`"click"` events on buttons or `"keypress"` events on text input bars.
To add handlers to DOM tree nodes created from our `Element` datatype,
we just need to insert the Haskell callbacks into the `eventHandlers`
field. As for the nodes that already exist as a part of the TodoMVC
project skeleton, we implement a `TodoView` module containing bindings
to the widgets we're interested in, then we apply `addEventListener`
on them in our main module. See the complete code
of
[`todomvc.hs`](https://github.com/tweag/asterius/blob/master/asterius/test/todomvc/todomvc.hs) for
details.

Putting it all together, we arrive at a complete implementation of
TodoMVC in Haskell: first, we perform a pure modeling of app state and
the DOM tree, then we seek a way of receiving input from the real
world and applying these pure models to it.

Of course, this implementation of TodoMVC is not yet satisfactory: we
didn't properly implement a virtual DOM with a decent diff algorithm
to minimize actual mutation on the real DOM tree; we didn't use
a fancy FRP framework, and instead threaded some global state along
the app using an `IORef`. However, it does show that it's already
possible to apply asterius to frontend development, enjoying both the
performance of WebAssembly and the nice developing experience of
Haskell.

## Next steps

It's been nearly a year since the asterius project started. Despite
various difficulties, asterius grew from nothing to printing Cmm, to
compiling `fib`, to handle JavaScript interop, and eventually, to
a functioning TodoMVC. We're on the edge of graduating from just
research prototype. That means it's now time to start communicating
updates more regularly, with reports updated weekly and
roadmaps reviewed quarterly, both included in the docs and available
to the public.

And here is a list of improvements to wait for in the next quarter :)

- Proper GC and exception handling. For garbage collection,
  a reasonable starting point would be a single-generation copying GC,
  from which we can gradually improve. Keep in mind that eventually
  WebAssembly runtimes will be [exposing their own GC's][wasm-gc]. For
  exception handling, we'll enable users to handle Haskell and
  JavaScript exceptions in a uniform manner: a Haskell exception can
  be caught in JavaScript, and vice versa.
- More comprehensive regression tests on CI, so we know with greater
  confidence what primops/runtime interfaces/standard library
  functions are working and what aren't. We'll provide that
  information as a status page so people can set reasonable
  expectations on the status quo of this project, and see how it
  improves over time.
- Support for even more packages, up to `aeson`.

[wasm-gc]: https://github.com/WebAssembly/gc/blob/master/proposals/gc/Overview.md
