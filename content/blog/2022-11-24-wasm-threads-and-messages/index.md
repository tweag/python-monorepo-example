---
title: Threads and messages with Rust and WebAssembly
author: Joe Neeman
description: "How and why to share threads in WASM workers (and when not to)"
tags: [rust, webassembly]
---

On most systems, you can implement concurrency using either threads or processes,
where the main difference between the two is that threads share memory and
processes don't. Modern web browsers support concurrency through the [Web
Workers API][web-workers-api]. Although Web Workers are by default closer to a
multi-process model, when used with WebAssembly you can opt-in to a more
thread-like experience. Just like in systems programming, the choice of
threads vs. processes comes with various trade-offs and performance
implications; I'll be covering some of them in this post. These examples will be in
Rust, but similar trade-offs should apply to other languages compiled to WASM.

[web-workers-api]: https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API

## The Web Workers API (multi-processing on the web)

When used from JavaScript, the Web Workers API is very simple: call `new Worker("/path/to/worker.js")` and your browser will fetch `worker.js` and start
running it concurrently. Inter-worker communication works in a very JavaScripty
way, by setting message handler callbacks and then sending messages. To use
Web Workers from compiled WASM code, you'll need to go "through" JavaScript:
you need a little JavaScript glue for spawning the worker, and you need to do
the message sending and callback handling using some JavaScript bindings.
Here's a little example that spawns a worker, sends a message, and gets a
reply:

```rust
// Spawn a worker and communicate with it.
fn spawn_worker() {
  let worker = web_sys::Worker::new("./worker.js");
  let callback = wasm_bindgen::Closure<FnMut(web_sys::MessageEvent)>::new(|msg| {
    assert_eq!(msg.data.as_f64(), Some(2.0));
  }));
  // Set up a callback to be invoked whenever we receive a message from the worker.
  // .as_ref().unchecked_ref() turns a wasm_bindgen::Closure into a &js_sys::Function
  worker.set_onmessage(callback.as_ref().unchecked_ref());

  // Send a message to the worker.
  worker.post_message(&JsValue::from(1.0)).expect("failed to post");

  // Did you notice that `set_onmessage` took a borrow? We still own `callback`, and we'd
  // better not free it too soon! See also
  // https://rustwasm.github.io/wasm-bindgen/reference/weak-references.html
  std::mem::forget(callback); // FIXME: memory management is hard
}

// An entry point for the JavaScript worker to call back into WASM.
#[wasm_bindgen]
pub fn worker_entry_point(arg: i32) {
  // Add 1 to our argument and send it back to the main thread.
  // Yeah, the js_sys/web_sys bindings are ... low-level.
  js_sys::global()
    .dyn_into::<web_sys::DedicatedWorkerGlobalScope>()
    .unwrap()
    .post_message(&JsValue::from(arg + 1))
    .unwrap();
}
```

And here's the JavaScript glue code in `worker.js`, which receives messages and
calls `worker_entry_point`:

```js
importScripts("./path/to/wasm_bindgen/module.js")
self.onmessage = async event => {
  const { child_entry_point } = await wasm_bindgen(
    "./path/to/wasm_bindgen/module_bg.wasm"
  )
  worker_entry_point(Number(event.data))
}
```

Note that when using the Web Workers API, all of the messages you send are
`JsValue`s. This is fine for sending primitive types, but it becomes annoying
if you want to send structured types, which must be converted into `JsValue`s
and back. You can simplify this process by using a helper crate like
[`gloo-worker`][gloo-worker], which provides a convenient way to send
structured data between workers. Under the hood, it serializes and deserializes
data to and from a `js_sys::ArrayBuffer`.

[gloo-worker]: https://crates.io/crates/gloo-worker

Dealing with large data can also be tricky, because `post_message` requires
that you copy the data. To avoid large data copies, you can use a
[`SharedArrayBuffer`][sharedarraybuffer] (a buffer that can be accessed by
multiple workers) or the [`post_message_with_transfer`][pmwt] function, which
allows for transferring the ownership of certain JavaScript objects from one
worker to another without copying. The downside of this workaround is that it
doesn't work directly with objects living in WASM memory. For example, if you have a
`Vec<u8>` that you want to send to another worker, you'll need to either copy
it to an `ArrayBuffer` and transfer it, or copy it to a `SharedArrayBuffer` and
share it.

[sharedarraybuffer]: https://rustwasm.github.io/wasm-bindgen/api/js_sys/struct.SharedArrayBuffer.html
[pmwt]: https://rustwasm.github.io/wasm-bindgen/api/web_sys/struct.Worker.html#method.post_message_with_transfer

## Shared memory in WebAssembly (multi-threading on the web)

Workers that share an address space can communicate with less boilerplate and
minimal data-copying. To create shared memory workers, note that `wasm_bindgen`'s
auto-generated initialization function takes a second (optional) parameter: a
[WASM memory object][wmo] for the module to use. Memory chunks can be shared
between WASM modules, so we can instantiate a new module using the same memory
as the first one, and the two modules will share it.

[wmo]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/WebAssembly/Memory

Having two WASM workers sharing the same memory opens the door to more
expressive inter-worker communication. For example, we can easily write a
function for executing a closure in another worker, just like how the
`std::thread::spawn` function works. The trick is to create a closure and send
its address to the other worker. Since the memory space is shared, the
receiving worker can cast that address back into a closure and execute it.

```rust
// A function imitating `std::thread::spawn`.
pub fn spawn(f: impl FnOnce() + Send + 'static) -> Result<web_sys::Worker, JsValue> {
  let worker = web_sys::Worker::new("./worker.js")?;
  // Double-boxing because `dyn FnOnce` is unsized and so `Box<dyn FnOnce()>` is a fat pointer.
  // But `Box<Box<dyn FnOnce()>>` is just a plain pointer, and since wasm has 32-bit pointers,
  // we can cast it to a `u32` and back.
  let ptr = Box::into_raw(Box::new(Box::new(f) as Box<dyn FnOnce()>));
  let msg = js_sys::Array::new();
  // Send the worker a reference to our memory chunk, so it can initialize a wasm module
  // using the same memory.
  msg.push(&wasm_bindgen::memory());
  // Also send the worker the address of the closure we want to execute.
  msg.push(&JsValue::from(ptr as u32))
  worker.post_message(&msg);
}

#[wasm_bindgen]
// This function is here for `worker.js` to call.
pub fn worker_entry_point(addr: u32) {
  // Interpret the address we were given as a pointer to a closure to call.
  let closure = unsafe { Box::from_raw(ptr as *mut Box<dyn FnOnce()>) };
  (*closure)();
}
```

The JavaScript worker glue must be changed slightly, to use the received
memory chunk when initializing its WASM module.

```js
importScripts("./path/to/wasm_bindgen/module.js")
self.onmessage = async event => {
  // event.data[0] should be the Memory object, and event.data[1] is the value to pass into child_entry_point
  const { child_entry_point } = await wasm_bindgen(
    "./path/to/wasm_bindgen/module_bg.wasm",
    event.data[0]
  )
  child_entry_point(Number(event.data[1]))
}
```

And now we can spawn closures on another thread just like in native
multi-threaded code, using the `spawn` function above instead of
`std::thread::spawn`. You can even use Rust's native inter-thread communication
tools, like `std::sync::mpsc`, to transfer data between threads without
copying! Our first worker example becomes as simple as:

```rust
let (to_worker, from_main) = std::sync::mpsc::channel();
let (to_main, from_worker) = std::sync::mpsc::channel();
spawn(move || { to_main.send(from_main.recv().unwrap() + 1.0); });
to_worker.send(1.0);
assert_eq!(from_worker.recv().unwrap(), 2.0);
```

Ok, there are some caveats. Shared memory WASM modules need some features that
weren't in the first iteration of the WASM spec, so you'll need to build with
some extra [target-features][targer-features]. You'll also need to rebuild the
standard library with those features, which requires a nightly compiler and
unstable flags. Something like this will do the trick:

[target-features]: https://rust-lang.github.io/packed_simd/perf-guide/target-feature/features.html

```sh
RUSTFLAGS="-C target-feature=+atomics,+bulk-memory,+mutable-globals" cargo build --target=wasm32-unknown-unknown --release -Z build-std=panic_abort,std
```

And then you'll need to configure your web server with some [special
headers][special-headers], because shared WASM memory builds on
[`SharedArrayBuffer`][sharedarraybuffer-2].

[special-headers]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer#security_requirements
[sharedarraybuffer-2]: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer

But there's a more serious issue with shared-memory workers: our example called
`from_worker.recv()` in the main thread, and most browsers will throw an
exception if you try to block the main thread, even for a very short time.
Since Rust doesn't have any tooling for checking non-blockingness (see
[here][issue] for some discussion), this might be difficult to ensure.

[issue]: https://github.com/rust-lang/wg-async/issues/19

If the extra discipline is just too onerous or unreliable, you can guarantee a
non-blocked main thread by moving all shared-memory WASM modules off of it:
from the main thread, use the JavaScript message-passing methods to communicate
with one or more workers, which are free to communicate _amongst each other_
using whichever (possibly blocking) methods they want.

![](./thread-architecture.svg)

## How much does all of this actually matter?

To measure the performance implications of the various options, I made some
buffers and sent them back and forth repeatedly between workers while measuring
the round-trip time. I repeated the experiment with two different buffer sizes
(a large 20 MB buffer, and a small 16 B one) and three different
message-passing methods. The timings were done on Firefox 101, and the code is
available [here](https://github.com/tweag/rust-wasm-threads).

|                              | 20MB buffer | 16B buffer |
| ---------------------------- | ----------- | ---------- |
| `post_message`               | 28ms        | 0.028ms    |
| `post_message_with_transfer` | 0.033ms     | 0.033ms    |
| `std::sync::mpsc::channel`   | 0.0062ms    | 0.0062ms   |

You'll notice that Rust-native shared memory is the fastest by a substantial
_factor_ but not a very large _absolute amount_, unless you really need to send
a lot of messages. Between the JavaScript methods, `post_message_with_transfer`
has some small overhead compared to `post_message` for small buffers, but this
is dwarfed by the copying time if you have substantial data to send.

At Tweag, we've been working with a client on an optimized WASM library that
caches and doles out largish (around 20MB each) chunks of data. We tried
various different threading architectures and ended up making do _without_
shared memory. Our heavy use of non-lock-free primitives made it hard to keep
the main browser thread happy when using shared memory, while the hybrid
architecture depicted above forced us into too many expensive copies (we
couldn't just transfer the data to the main thread because we needed a copy in
cache). With a separate-memory architecture, we arranged our data processing
so that large buffers are only ever transferred, never copied. And the small
overhead of `post_message_with_transfer` was negligible compared to the other
processing we were doing.

Your ideal architecture might be different from ours. By explaining some of the
trade-offs involved, I hope this post will help you find it!
