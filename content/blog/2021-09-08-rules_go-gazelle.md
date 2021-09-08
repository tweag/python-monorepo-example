---
title: "Building a Go project using Bazel"
author: Tanya Bouman
tags: [bazel, golang]
description: "Building a Go project using Bazel, generating build files with Gazelle"
---

In this post, I'll show how to build a Go project with [Bazel](https://bazel.build),
using [Gazelle](https://github.com/bazelbuild/bazel-gazelle) to generate the build files.
Why Bazel?
With Bazel, I get incremental, reproducible, and cacheable builds.
It's designed for large, polyglot monorepos
and can be extended to support more languages than the many already available.
Why Gazelle?
Gazelle looks at the Go source files and generates Bazel `BUILD` files for me.

I used [tendermint](https://github.com/tendermint/tendermint)
as an example Go project, because it's large
enough to include several interesting problems:

- protocol buffers
- external dependencies
- runtime dependencies
- flaky and exclusive tests

How will we be able to tell when it's finished?
At the end, we'll be able to build targets for all of the Go sources,

```bash
$ bazel build //...
```

run tests,

```bash
$ bazel test //...
```

and execute the tendermint binary.

```bash
$ bazel run //cmd/tendermint
```

To see the final results, check out
[my fork of tendermint](https://github.com/tweag/tendermint-bazel).

## Let's get started!

But where should we start? As it is usual for Bazel, we start with the
`WORKSPACE` file, which lives at the project root and describes it, and the
`BUILD` files, which define packages within the project.
The documentation for
[rules_go](https://github.com/bazelbuild/rules_go/blob/5d306c433cebb1ae8a7b72df2a055be2bacbb12b/README.rst#initial-project-setup)
and [Gazelle](https://github.com/bazelbuild/bazel-gazelle/blob/15828e8077542449fea13491bcb2e404a12412fd/README.rst#running-gazelle-with-bazel)
helpfully gives some code snippets for these.
Starting with a fresh clone of tendermint, I create a
`WORKSPACE` file that looks like this:

```python
workspace(name = "com_github_tendermint_tendermint")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "8e968b5fcea1d2d64071872b12737bbb5514524ee5f0a4f54f5920266c261acb",
    url = "https://github.com/bazelbuild/rules_go/releases/download/v0.28.0/rules_go-v0.28.0.zip",
)

http_archive(
    name = "bazel_gazelle",
    sha256 = "62ca106be173579c0a167deb23358fdfe71ffa1e4cfdddf5582af26520f1c66f",
    url = "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.23.0/bazel-gazelle-v0.23.0.tar.gz",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")
load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

go_rules_dependencies()

go_register_toolchains(version = "1.16.5")

gazelle_dependencies()
```

The first line of the `WORKSPACE` file gives the project a name,
and the rest loads the dependencies on `rules_go` and Gazelle.
Gazelle generates Bazel `BUILD` files, so that I don't have to write them all myself,
and updates the Bazel files after changes are done to Go source files.

Even though there aren't any Go sources to build in the root directory,
let's write a `BUILD` file in the root directory.
What is the point of that?
The root `BUILD` file contains a Gazelle target, so that we can use Bazel
to run Gazelle.
The `gazelle:prefix` directive tells Gazelle how other Go projects can import
this one.

```python
load("@bazel_gazelle//:def.bzl", "gazelle")

# gazelle:prefix github.com/tendermint/tendermint
gazelle(name = "gazelle")
```

Let's try it out and see what happens.

```bash
$ bazel run //:gazelle
```

Wow, `BUILD` files everywhere!
There is now one generated `BUILD` file
in every folder which contains Go source files.
And I'm really glad that
I didn't have to write those all myself.

## Proto problems

What happens if we try building the targets in those `BUILD` files?
Let's find out.

```bash
$ bazel build //...
ERROR: /tendermint/third_party/proto/gogoproto/BUILD.bazel:5:14: no such package '@com_google_protobuf//': The repository '@com_google_protobuf' could not be resolved and referenced by '//third_party/proto/gogoproto:gogoproto_proto'
ERROR: /tendermint/third_party/proto/gogoproto/BUILD.bazel:5:14: every rule of type proto_library implicitly depends upon the target '@com_google_protobuf//:protoc', but this target could not be found because of: no such package '@com_google_protobuf//': The repository '@com_google_protobuf' could not be resolved
ERROR: Analysis of target '//third_party/proto/gogoproto:gogoproto_proto' failed; build aborted: Analysis failed
```

Uh oh, there's a problem.

Here we've run into a dependency problem about using the `proto_library` rule for
the protocol buffers in tendermint.
Could I add that dependency and move on?
Well yes, [it can be done](https://github.com/bazelbuild/rules_go/blob/5d306c433cebb1ae8a7b72df2a055be2bacbb12b/README.rst#protobuf-and-grpc).
However, according to the [`rules_go` documentation](https://github.com/bazelbuild/rules_go/blob/5d306c433cebb1ae8a7b72df2a055be2bacbb12b/proto/core.rst#option-2-use-pre-generated-pbgo-files),
it's probably easier to use the existing .pb.go files generated by the protocol buffer compiler,
since tendermint already has those files.
By default, Gazelle makes `go_proto_library` and `proto_library`
to compile protos and generate .pb.go files.
Instead of using that default, I include a Gazelle directive in the root `BUILD` file
to disable the protos.

```python
# gazelle:proto disable_global
```

I remove the generated `BUILD` files, generate new ones, and attempt to build everything again.

```bash
$ bazel run //:gazelle
```

## Depending on the Outside World

```bash
$ bazel build //...
ERROR: /tendermint/internal/blockchain/v2/BUILD.bazel:3:11: no such package '@com_github_go_kit_kit//metrics/prometheus': The repository '@com_github_go_kit_kit' could not be resolved and referenced by '//internal/blockchain/v2:blockchain'
ERROR: Analysis of target '//internal/blockchain/v2:blockchain' failed; build aborted: Analysis failed
```

Oh no!
What happened now?

To see why it's failing, let's take a look at the definition of the
target `//internal/blockchain/v2:blockchain`
in `internal/blockchain/v2/BUILD.bazel`.

```python
go_library(
    name = "blockchain",
    srcs = [
        "io.go",
        "metrics.go",
        "processor.go",
        "processor_context.go",
        "reactor.go",
        "routine.go",
        "scheduler.go",
        "types.go",
    ],
    importpath = "github.com/tendermint/tendermint/internal/blockchain/v2",
    visibility = ["//:__subpackages__"],
    deps = [
        "//internal/blockchain",
        "//internal/blockchain/v2/internal/behavior",
        "//internal/consensus",
        "//internal/libs/sync",
        "//internal/p2p",
        "//libs/log",
        "//proto/tendermint/blockchain",
        "//state",
        "//types",
        "@com_github_go_kit_kit//metrics:go_default_library",
        "@com_github_go_kit_kit//metrics/discard:go_default_library",
        "@com_github_go_kit_kit//metrics/prometheus:go_default_library",
        "@com_github_gogo_protobuf//proto:go_default_library",
        "@com_github_prometheus_client_golang//prometheus:go_default_library",
        "@com_github_workiva_go_datastructures//queue:go_default_library",
    ],
)
```

The target's name is `blockchain`, and it builds a library from the Go
sources listed.
The import path tells us how another project can import it in Go, and the
[visibility](https://docs.bazel.build/versions/main/visibility.html) says
that only other targets within tendermint can depend on blockchain.

<!--
For reference:
https://github.com/bazelbuild/bazel-gazelle/blob/9f72eed9f79bfc18a04e8ac6204751998c7cba4a/language/go/generate.go#L715-L718
-->

Finally, each dependency is explicitly listed.
The dependencies that begin with `//` refer to packages within tendermint, and
the rest are external dependencies.
The error message points to one of those external dependencies:
`@com_github_go_kit_kit//metrics/prometheus`.
Why can't Bazel find it?
I haven't given any definition of `com_github_go_kit_kit`, or any of the
other external dependencies, so Bazel can't figure out where to find it.
Do I have to tell Bazel exactly where to find all of those dependencies?
Bazel needs exact instructions about where to find external
dependencies, but Gazelle's update-repos command can use the `go.mod` file from the Go build
to figure out the external dependencies,
so I don't have to write them all out myself.
In the root BUILD file, I make another `gazelle` target, and this one uses
the `update-repos` command.

```python
gazelle(
    name = "gazelle-update-repos",
    args = [
        "-from_file=go.mod",
        "-to_macro=deps.bzl%go_dependencies",
        "-prune",
        "-build_file_proto_mode=disable_global",
    ],
    command = "update-repos",
)
```

And I run it the same way as the other `gazelle` target.

```bash
$ bazel run //:gazelle-update-repos
```

What exactly did update-repos do?
There's now a new file in the root of the project, called `deps.bzl`.
Along with many other external dependencies, it gives a definition of
`com_github_go_kit_kit`.

```python
    go_repository(
        name = "com_github_go_kit_kit",
        build_file_proto_mode = "disable_global",
        importpath = "github.com/go-kit/kit",
        sum = "h1:dXFJfIHVvUcpSgDOV+Ne6t7jXri8Tfv2uOLHUZ2XNuo=",
        version = "v0.10.0",
    )
```

How does Bazel know to look at `deps.bzl`?
The update-repos command also adds a few lines in the `WORKSPACE`
file.

```python
load("//:deps.bzl", "go_dependencies")

# gazelle:repository_macro deps.bzl%go_dependencies
go_dependencies()
```

Note that the flag `-build_file_proto_mode=disable_global` sets the
`go_repository` attribute `build_file_proto_mode`.
This indicates that I don't want to have `proto_library` and `go_proto_library`
targets in my external dependencies either, and instead follow the
recommendation to use pre-generated .pb.go files.

And, with all that, everything builds.

```bash
$ bazel build //...
```

One more quick note about the build:
The current convention is that Gazelle generates names for `go_library` targets
using the last component of the path.
However, the convention used to be that all `go_library` targets
were called `go_default_library`.
When other projects depend on tendermint, they might use either of those
conventions, so to accommodate both, we include another directive in the
root `BUILD` file.

```python
# gazelle:go_naming_convention import_alias
```

After running Gazelle again, there are now `alias` targets to the
`go_library` target.
For example the alias to `//internal/blockchain/v2:blockchain`
is:

```python
alias(
    name = "go_default_library",
    actual = ":blockchain",
    visibility = ["//:__subpackages__"],
)
```

It refers to the same `blockchain` library, but allows it to use another name.

## Testing... Testing...

What next?
On to the tests!
Let's run them all.

```bash
$ bazel test //... --test_output=errors
```

### Finding Data

Most of them pass, but a few fail.
Let's look at one of them in particular.

```bash
$ bazel test //... --test_output=errors
==================== Test output for //state/indexer/sink/psql:psql_test:
--- FAIL: TestType (14.49s)
    psql_test.go:39:
        	Error Trace:	psql_test.go:364
        	            				psql_test.go:39
        	Error:      	Expected nil, but got: &fmt.wrapError{msg:"failed to read sql file from 'schema.sql': open schema.sql: no such file or directory", err:(*fs.PathError)(0xc0005f5f50)}
        	Test:       	TestType
```

The test needs the `schema.sql` file, but can't find it.
Why not?
The file definitely exists in the folder along with the `BUILD` file
and the Go sources.
However, Bazel needs to know explicitly about dependencies, and Gazelle
doesn't know about these runtime dependencies, so we have to add them
ourselves.

```python
go_test(
    name = "psql_test",
    srcs = ["psql_test.go"],
    data = ["schema.sql"],
    embed = [":psql"],
    deps = [
        "//abci/types",
        "//state/indexer",
        "//types",
        "@com_github_adlio_schema//:schema",
        ...
    ],
)
```

Since `schema.sql` is only necessary at runtime, we
list it as a `data` dependency.
Note that there is another type of dependency in this target: an `embed` dependency.
[Embedding](https://github.com/bazelbuild/rules_go/blob/a71cafbe48cd5e4d791f869965b77737897a6803/go/core.rst#embedding) is a specific type of dependency in rules_go which is usually
used to include a library in a binary.
In this case, it is a library embedded in a test target.

<!-- FIXME: I don't deeply understand why this is, so I'm just linking the docs. -->

Now we can run the test again:

```bash
$ bazel test //state/indexer/sink/psql:psql_test --test_output=errors
```

And it passes!
Onwards.

```bash
$ bazel test //... --test_output=errors
==================== Test output for //state/indexer:indexer_test:
--- FAIL: TestIndexerServiceIndexesBlocks (50.58s)
    indexer_service_test.go:55:
        	Error Trace:	indexer_service_test.go:184
        	            				indexer_service_test.go:55
        	Error:      	Expected nil, but got: &fmt.wrapError{msg:"failed to read sql file from './sink/psql/schema.sql': open ./sink/psql/schema.sql: no such file or directory", err:(*fs.PathError)(0xc0000aaea0)}
        	Test:       	TestIndexerServiceIndexesBlocks
```

Oh look!
Here's another test that also requires the same `schema.sql` file.

```python
    data = ["schema.sql"],
```

It might be tempting to add the dependency exactly the same way as before,
but that fails, because `schema.sql` is in a different package.
Each BUILD file defines a different package, and
`state/indexer/sink/psql/BUILD.bazel` defines the package that
contains `schema.sql`.
Instead, we need to explicitly mention the package name.

```python
    data = ["//state/indexer/sink/psql:schema.sql"],
```

But that's not all.

```bash
$ bazel test //... --test_output=errors
ERROR: /tendermint/state/indexer/BUILD.bazel:21:8: in go_test rule //state/indexer:indexer_test: target '//state/indexer/sink/psql:schema.sql' is not visible from target '//state/indexer:indexer_test'. Check the visibility declaration of the former target if you think the dependency is legitimate. To set the visibility of that source file target, use the exports_files() function
ERROR: Analysis of target '//state/indexer:indexer_test' failed; build aborted: Analysis of target '//state/indexer:indexer_test' failed
```

By default, files are only available to targets within the package.
How can we change this default?
An explicit declaration using
[exports_files](https://docs.bazel.build/versions/main/be/functions.html#exports_files)
in the package `//state/indexer/sink/psql` will do the trick.

```python
exports_files(
    ["schema.sql"],
    visibility = ["//state/indexer:__subpackages__"],
)
```

And finally, this test passes too.

```bash
$ bazel test //state/indexer:indexer_test --test_output=errors
```

### Exclusive Tests

Let's try something weird.
Let's add the flag `--nocache_test_results` so that Bazel doesn't re-use
the cached test results from before.

```bash
$ bazel test //... --test_output=errors --nocache_test_results
==================== Test output for //state/indexer/sink/psql:psql_test:
--- FAIL: TestType (114.24s)
    psql_test.go:39:
        	Error Trace:	psql_test.go:365
        	            				psql_test.go:39
        	Error:      	Expected nil, but got: &fmt.wrapError{msg:"Error unlocking while returning from other err: Migration '2021-07-09 20:30:37.648524208 +0000 UTC db schema' Failed:\ndriver: bad connection\nread tcp 127.0.0.1:51614->127.0.0.1:49364: read: connection reset by peer", err:(*fmt.wrapError)(0xc000527cc0)}
        	Test:       	TestType
```

Yuck!
What is this?
Wasn't this test passing before?
It's a network error, and
it turns out that this test does pass when run by itself,
and passes over and over again, even with `--nocache_test_results`.

```bash
$ bazel test //state/indexer/sink/psql:psql_test --test_output=errors --nocache_test_results
```

Running the test separately is really annoying,
but is necessary to guarantee that the network connections are available.
Is there a better way to do it?
Yes, there's an [attribute](https://docs.bazel.build/versions/main/be/common-definitions.html)
`tags = ["exclusive"]`, that ensures no other tests run in parallel with it.
Is there any way that those tests could run in parallel?
Yes, it is possible to change the tests, but that goes beyond building with Bazel,
so I work around the issue by not running them in parallel.

### Flaky Tests

After running all of those tests so many times,
I notice something strange.
What is it?
The test `//light:light_test` sometimes fails and sometimes passes.
In fact, this test was [known to be flaky](https://github.com/tendermint/tendermint/issues/5999).
Is there anything we can do about it?
Testing rules have a `flaky` attribute, and adding `flaky = True`
tells Bazel to run the test up to three times before failing it.

<!--
FIXME: and here I gloss over the fact that not all of the tests actually pass for me
Is this OK?
Making explicit that I'm ignoring some of the tests
-->

Are all the tests passing now?
There are a few failing tests left,
but these also fail on my machine with `go test`, so it's not a Bazel-specific issue.
To prevent those tests from running with `bazel test //...`,
I add `tags = ["manual"]` to those test targets.

## Are we done yet?

The final thing that we'll check in this post is the main
tendermint binary.

```bash
$ bazel run //cmd/tendermint
```

And it runs, printing out help for using tendermint.

<!--
FIXME: ok, so it works to spit out the help, but that's not much compared to the actual
program, and I don't know how to run that
-->

## Yes! It's finished

Now I have a Bazel build of tendermint, and I can run tests and the tendermint command.
For more examples of using Bazel,
check out posts which build projects with
[Haskell](https://www.tweag.io/blog/2020-05-06-convert-haskell-project-to-bazel) and
[OCaml](https://www.tweag.io/blog/2021-07-01-obazl).
