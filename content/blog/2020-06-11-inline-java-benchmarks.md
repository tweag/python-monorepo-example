---
title: "Calling to the JVM from Haskell: Some benchmarks"
shortTitle: "Inline-java benchmarks"
author: Facundo Domínguez
tags: [haskell, inline-java]
description: "Benchmarks of inline-java and a discussion of its performance trade-offs."
---

In our [previous posts][previous-posts] about [inline-java][inline-java], we
presented it as a tool for interoperating with the huge Java ecosystem. Indeed,
the Haskell community being smaller, it is not uncommon to face problems for
which there are no libraries in the ecosystem. In these situations, a
multi-language solution can be a good compromise.

One trade-off is performance, keeping in mind that communicating across language boundaries always
has costs. In this post, I want to argue that `inline-java` can be a good
solution for integrating Haskell and Java from a performance standpoint. We do
this by benchmarking a concrete example, and discuss in which situations this
kind of integration becomes affordable as compared to projects that go full
Haskell or full Java for their implementations.

## The benchmarks

[FrameworkBenchmarks][framework-benchmarks] is a project that gathers
benchmarks of HTTP servers for a large set of implementations in different
languages. At the time of this writing, there are six types of tests, but a
given implementation doesn't need to implement all six of them.

I provided an implementation of a fast HTTP server implemented in Java which
invokes a handler implemented in Haskell for every request. The handler, in
turn, uses `inline-java` to interact
with the HTTP server. The implementation is called
[wizzardo-inline][wizzardo-inline] and it is based on
[wizzardo-http][wizzardo], a fast full-Java HTTP server.

I implemented three of the test types:

- The _Plaintext_ test, which returns a fixed plaintext response for each
  request, such as `"Hello, World!"`.

- The _JSON serialization_ test, returns a fixed JSON response, where each
  request must encode over and over again the same JSON object, for example,
  `"{ \"message\" : \"Hello, World!\" }"`.

- And finally, the _Single query_ test, which for every request provides a
  random record retrieved from a database, encoded again as a JSON object, such
  as `"{ \"id\" : 1234, \"randomNumber\" : 6678 }"`.

These benchmarks measure the throughput of the server, that is, the number of
requests per second that each implementation could deliver. That means that
higher numbers are better.

Because the benchmarks are run multiple times with varying
amounts of concurrency, the table shows the highest throughput achieved
by each implementation. Additionally, it shows the throughput as a
percentage of the one performed by _wizzardo-http_.

<center>

| Throughput (requests/sec) | wizzardo-http | wizzardo-inline |
| ------------------------- | ------------: | --------------: |
| **Plaintext**             |        669946 |    203347 (30%) |
| **JSON serialization**    |        136293 |     86638 (63%) |
| **Single query**          |         51646 |     38772 (75%) |

</center>

To understand the trend in the table, it must be noted that `inline-java` is
expected to add a constant overhead per request, since we need to invoke a
couple of methods of the JVM to marshal the response from Haskell to Java.

In all cases, one has to convert a Haskell `ByteString` to a Java `byte`
array. The code to convert the `ByteString` to a `byte` array is in the
[jvm][jvm-package] package, and does the following:

```Haskell
instance Reflect ByteString where
  reflect bs = BS.unsafeUseAsCStringLen bs $ \(content, n) -> do
      arr <- newByteArray (fromIntegral n)
      setByteArrayRegion arr 0 (fromIntegral n) content
      return arr
```

After obtaining a C-style buffer with the bytes from the ByteString,
we create a byte array object with newByteArray, and then ask the
JVM to copy the bytes from our buffer with setByteArrayRegion.

The JVM does some extra bookkeeping when copying bytes. The destination
could be moved by the Java garbage collector during copying, so
arrangements are necessary for the operation to complete safely.
After obtaining the `byte` array, a Java method is invoked to feed
it back to the server framework. In the case of the _Single query_ test, an
extra Java method must be called in order to kick the query to the database.

The overhead of these conversions and method calls amounts to 4 microseconds,
give or take. This constant overhead is very noticeable for fast requests. For
instance, lets say that `wizzardo-http` can execute a _Plaintext_ request in 3
microseconds. Therefore `wizzardo-inline` needs 7 microseconds, which is more
than twice the time.

A more interesting test is _Single query_ where one has an additional database
call. Supposing that `wizzardo-http` takes 15 microseconds to serve a request,
`wizzardo_inline` needs 19 microseconds, which is not nearly as onerous as it
was in the _Plaintext_ test.

In general, the shorter it takes to serve a request, the more weight
the overhead of making calls to the JVM has on the comparisons. Fast
requests like those of _Plaintext_ have durations which are close to
those of JVM calls from Haskell. The
bottleneck in the requests of _Single query_ is in the database
access, and calling to the JVM becomes more affordable in that case.

Provisionally, we ran the benchmarks on an `m4.2xlarge` instance on
Amazon's EC2 service. But we expect the `FrameworkBenchmarks`
to provide definitive measures when the next round of benchmarks is
executed by [TechEmpower][round18], the software consultancy firm running
the project.

## Final remarks

While preparing these benchmarks, I kept in mind all along the fact that
the FrameworkBenchmarks include an implementation for _warp_,
a full-Haskell implementation of an HTTP server. The following table
compares it with `wizzardo-inline`.

<center>

| Throughput (requests/sec) | wizzardo-inline |          warp |
| ------------------------- | --------------: | ------------: |
| **Plaintext**             |          203347 | 205573 (101%) |
| **JSON serialization**    |           86638 | 107381 (124%) |
| **Single query**          |           38772 |   20444 (53%) |

</center>

As we can see, the throughput of `warp` is comparable or better on the
simplest tests, but worse on the _Single query_ one.
I don't claim `wizzardo-http` to be superior to `warp`, because I haven't
analyzed the performance differences. But replace _warp_ with whatever
complex full-Haskell technology _X_ you need to consider for your
project, and `inline-java` can be a factor in deciding when or if to
engage in a detailed analysis.

The more complex a solution is, the more expensive it is to implement
and to optimize it for the relevant cases. When the budget and the time cannot
afford a reimplementation, `inline-java` can be a cheaper
alternative. Furthermore, depending on the bottlenecks of the solution
at hand, the overhead of crossing language runtimes may become
negligible, turning the multi-language integration path into the ideal one.

[previous-posts]: https://www.tweag.io/tag/inline-java.html
[framework-benchmarks]: https://www.techempower.com/benchmarks
[inline-java]: https://github.com/tweag/inline-java
[jvm-package]: https://github.com/tweag/inline-java/tree/master/jvm
[round18]: https://www.techempower.com/blog/2019/07/09/framework-benchmarks-round-18/
[wizzardo]: https://github.com/wizzardo/webery
[wizzardo-inline]: https://github.com/TechEmpower/FrameworkBenchmarks/tree/master/frameworks/Haskell/wizzardo-inline
