---
title: "Improving Criterion.rs"
author: "David Himmelstrup"
tags: [rust, fellowship]
---

TLDR; Benchmarking in Rust is now faster, easier, and runs in your browser.

My name is David and for the past three months, Tweag has generously sponsored my work on criterion.rs[^criterion-rs]. Criterion.rs is a widely used (with nearly 1.4 million downloads in the last 90 days) benchmarking library for the Rust programming language which helps you measure the performance of your code and was inspired by its Haskell namesake[^criterion-hs].

I'm a heavy user of criterion.rs as well as one of its maintainers, and three particular pain points have been sticking out like a sore thumb to me. These pain points affect a significant number of people while still being relatively easy to fix, making them, in my opinion, a great fit for Tweag's Fellowship[^fellowship] programme which aims to fund Open Source contributions. Luckily Tweag thought the same and sent me on a three month journey to solve the three pain points:

- **Running benchmarks takes too much time.**
- **Comparing results is awkward.**
- **No first-class WASM support.**

## Trading accuracy for speed: Quick Mode

Benchmarking is notoriously difficult to get right and criterion.rs takes many precautions to tease every bit of signal out of very noisy data. Unfortunately, this means spending at least 8 seconds per benchmark which adds up when you have hundreds of benchmarks. This problem is not a new one, though, and there are many ways to sacrifice a little accuracy for better speed. In particular, the tasty-bench[^tasty] library uses an approach that terminates early when it deems the results to be sufficiently reliable. This seems to work great in tasty-bench so I've implemented the same in criterion.rs.

So, how much faster is the new quick-mode? Well, using the `hex` crate[^hex] for testing, running in quick mode is 50 times faster:

| `cargo bench --bench=hex` | `cargo bench --bench=hex -- --quick` | Speed-up |
| ------------------------- | ------------------------------------ | -------- |
| 92.9 s                    | 1.8 s                                | ~50x     |

Running benchmarks 50 times faster sounds great but it would be useless if the results are completely inaccurate. Let's compare the two sets of results and see how much they differ. In the table below, the absolute runtime and relative performance are given for each benchmark in both `normal` and `quick` mode:

```
group                            normal                      quick
-----                            ------                      -----
faster_hex_decode                1.00     41.2±0.44µs        1.00     41.1±0.06µs
faster_hex_decode_fallback       1.00      9.9±0.05µs        1.00      9.9±0.00µs
faster_hex_decode_unchecked      1.00      9.9±0.05µs        1.00      9.9±0.02µs
faster_hex_encode                1.01      8.2±0.11µs        1.00      8.1±0.01µs
faster_hex_encode_fallback       1.00     10.2±0.06µs        1.00     10.2±0.01µs
hex_decode                       1.00     90.4±0.49µs        1.00     90.3±0.07µs
hex_encode                       1.02     66.6±0.36µs        1.00     65.3±0.08µs
rustc_hex_decode                 1.00     90.3±0.37µs        1.00     90.3±0.12µs
rustc_hex_encode                 1.00     66.5±0.69µs        1.01     67.1±0.12µs
```

As you can see, the difference between the two modes is tiny and falls within the normal variance on the computer I'm using. Does this mean you can rely on quick-mode always being accurate? No, unfortunately not. There's no free lunch and quick-mode makes your benchmarks much more susceptible to frequency-scaling, heat throttling, context switches, and JIT/interpreter overhead.

To summarise, quick-mode can significantly shorten your iteration time but you should be aware of the trade-offs/pitfalls.

## Tabulating and Comparing Results

Notice how the results in the [previous section](##trading-accuracy-for-speed-quick-mode) are neatly tabulated? Yeah, that is actually a new feature. Previously, comparing results would look like this:

```
faster_hex_decode       time:   [41.061 us 41.083 us 41.105 us]
                        change: [-0.2499% +0.0044% +0.2702%] (p = 0.67 > 0.05)
                        No change in performance detected.
Found 5 outliers among 100 measurements (5.00%)
  2 (2.00%) high mild
  3 (3.00%) high severe

[snipped 56 lines]
```

This format isn't all that readable and it's difficult to quickly get an idea of how two sets of results differ. [BurntSushi](https://github.com/BurntSushi) was so annoyed by this that he wrote a tool for formatting results in a more concise manner: [critcmp](https://github.com/BurntSushi/critcmp). This tool is excellent but few people know about it, so, with the author's permission, I've integrated it into criterion.rs and documented it in the user's guide.

## First-class WASM support

Rust can easily be compiled to WebAssembly and executed in interesting environments such as wasmer, nodejs, or even a browser. The performance in these environments can be wildly different from native execution, though, and predicting how fast your code will run is nearly impossible.

The solution, of course, is to compile criterion.rs to WebAssembly and measure the performance empirically. While this was easier said than done, the WASM support is finally complete and it works nearly out of the box. The only requirement is to disable criterion's default features as some of them (eg. `rayon`) are not supported on WASM. To do this, simply modify your `Cargo.toml` file like this:

```diff
[dev-dependencies]
-criterion = "0.4"
+criterion = { version = "0.4", default-features = false }
```

With that out of the way, we can now start benchmarking a wide range of WebAssembly environments. Let's compare a native binary on my AMD Zen 2, [wasmer](https://wasmer.io), [wasmtime](https://wasmtime.dev), [nodejs](https://nodejs.org/en/), [firefox](https://www.mozilla.org/en-US/firefox/new/), and [chromium](https://www.chromium.org/chromium-projects/). Note: These results are purely indicative and should be taken with a pinch of salt. Your mileage may vary and should do your own benchmarking to draw your own conclusions. That being said, let's see the results for the `hex` crate:

```
hex_decode
----------
native       1.00     244.6±2.36µs       ? ?/sec
firefox      1.66    405.7±14.22µs       ? ?/sec
wasmer       1.72     421.4±9.65µs       ? ?/sec
wasmtime     1.73     423.0±3.00µs       ? ?/sec
nodejs       2.00     490.3±3.49µs       ? ?/sec
chromium     2.81    688.5±12.23µs       ? ?/sec

hex_encode
----------
native       1.00      69.2±0.40µs       ? ?/sec
wasmtime     1.18      81.7±0.38µs       ? ?/sec
wasmer       1.46     100.9±1.22µs       ? ?/sec
nodejs       2.20     152.5±1.93µs       ? ?/sec
firefox      3.25     224.8±7.53µs       ? ?/sec
chromium     4.08     282.7±4.19µs       ? ?/sec
```

The native executable is the fastest but not by much. I'm too often surprised by how relatively efficient WASM can be.

There are a bunch of pitfalls that can cause you grief when benchmarking WASM code. For one, you definitely have to avoid using quick-mode when targeting WASM. JIT compilers benefit enormously from a warm-up phase and you're all but guaranteed to get incorrect results in quick-mode. Furthermore, browser support for the WASI standard is not 100% robust and you may have to reload your browser window from time to time. All in all, the vision for running Rust in your browser is there but the implementations are lacking a bit behind.

## Conclusion

PRs for solving the three pain points (faster benchmarks, comparing results, WASM support) have been submitted and will hopefully make it into the next release of criterion.rs. If you want to test these features early, you can do so by depending directly on the release branch:

```diff
[dev-dependencies]
-criterion = "0.3"
+criterion = { git = "https://github.com/bheisler/criterion.rs", branch = "version-0.4" }
```

Before the release of `criterion-0.4`, documentation can be found here: [quick-mode](https://github.com/bheisler/criterion.rs/blob/version-0.4/book/src/user_guide/quick_mode.md), [tabulating results](https://github.com/bheisler/criterion.rs/blob/version-0.4/book/src/user_guide/tabulating_results.md), and [WASM support](https://github.com/bheisler/criterion.rs/blob/version-0.4/book/src/user_guide/wasi.md). After the upcoming release, those pages will be available in the user's guide.

I'd like to thank Tweag for giving me the freedom to work on what I felt was important, and I'd like to thank Ilya and Yann for guiding me throughout this process. I can wholeheartedly recommend the Tweag Fellowship program.

[^criterion-rs]: https://github.com/bheisler/criterion.rs
[^criterion-hs]: http://www.serpentine.com/criterion/
[^fellowship]: https://boards.greenhouse.io/tweag/jobs/4638654002
[^tasty]: https://github.com/Bodigrim/tasty-bench
[^hex]: https://crates.io/crates/hex
