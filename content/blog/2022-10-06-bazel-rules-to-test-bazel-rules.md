---
title: "Bazel rules to test Bazel rules"
author: Ilya Polyakovskiy
tags: [bazel]
description: "Looking for a way to test Bazel-rules"
---

As frequent Bazel rule writers, at Tweag we need ways to test our rulesets.
In this post I will describe the approach that [I've recently introduced][integration_testing_pr] in [`rules_haskell`][rules_haskell]
to achieve clear, maintainable and efficient tests for Bazel rules.

## Why even bother?

All software needs to be tested and verified, and Bazel rules are no exception. Some of them are
popular public libraries with a lot of projects depending on them, and they need a proper test set
to allow every contributor, frequent or occasional, to propose a change with little risk of breaking the
public API those rules represents. The same applies to the rules that live in huge monorepos growing every day,
where they often act as a glue between different components and can become complex, tricky and fragile.

Also every Bazel rule introduces an API and a proper test-suite provides a clear and valid example of its usage.
The usual practice among rule writers is to include an `examples/` folder in their package with a bunch of
example workspaces and test on CI that all their targets are built and run successfully. Look at [`rules_rust`][rules_rust]
and [`rules_nixpkgs`][rules_nixpkgs] for example. There are some serious flaws in this approach:

- It's non-hermetic: since Bazel is not executed inside a sandbox those test doesn't benefit
  from Bazel's approach of tests sandboxing: any local files, specifics of the shell and environment
  variables can influence the execution of examples
- It's non-reproducible: in addition to lack of sandboxing this approach doesn't pin the Bazel version which
  should be used to execute those examples
- It's inconvenient: unlike any other tests you may have in your project, these tests
  are not invoked by Bazel and lose all benefits of Bazel's tests runner.

## An overview of existing tools

There are several projects addressing this problem. I will concentrate on the two I draw inspiration
from. I will first show how to apply each of them to the
`rules_haskell` project. Then I'll explain the solution that I've
eventually used for testing `rules_haskell`.

### `rules_go` and `go_bazel_test`:

This rule was created in the [`rules_go`][rules_go] repository to do integration testing of `go_*`-rules. It consists
of a Starlark macro and a Go library with some helper functions to set up a test workspace and run
Bazel commands inside it. This allows writing test scenarios in the form of a Go program. Lets see how
it would look like if we tried to write a simple test for the `haskell_binary` rule from `rules_haskell`.

Every Bazel rule requires its sources to be declared explicitly. In this case we want to test our
rules as a package, so we need to define a target which would represent the `rules_haskell` distribution
to pass it to the test. Let's create a `filegroup` in the top-level `BUILD.bazel` file (we can use
[the example from `rules_go`][rules_go_all_files] as an inspiration):

```python
filegroup(
    name = "all_files",
    src = [
        # All top-level rules files
        # and all sub-level "all_files" filegroups
        ...
    ],
)
```

Using this filegroup we can create a test target in a `BUILD.bazel` file:

```python
load("@rules_go//go/tools/bazel_testing:def.bzl", "go_bazel_test")

go_bazel_test(
    name = "sample",
    srcs = ["sample_test.go"],
    rule_files = "//:all_files",
)
```

Now let's create the actual test scenario. In our case it would be located in the `sample_test.go`
file:

```go
package sample_test

import (
        "testing"
        "github.com/bazelbuild/rules_go/go/tools/bazel_testing"
)
```

We need to define our test workspace. The `bazel_testing` library requires it to be specified as
a string using a specific format:

```go
const workspace = `
-- WORKSPACE --
local_repository(
    name = rules_haskell,
    # I will explain where this path came from a bit later
    path = "../rules_haskell",
)

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()
load(
    "@rules_haskell//haskell:toolchain.bzl",
    "rules_haskell_toolchains",
)

rules_haskell_toolchains(
    version = "8.10.7",
)

-- BUILD.bazel --
load(
    "@rules_haskell//haskell:defs.bzl",
    "haskell_binary",
    "haskell_toolchain_library",
)

haskell_toolchain_library(
    name = "base",
)

haskell_binary(
    name = "Sample",
    srcs = ["Sample.hs"],
    deps = [":base"],
)
-- Sample.hs --
main = putStrLn "Hello World!"
`
```

Now we can use `bazel_testing` functions to unpack this workspace as a real directory and run
some Bazel commands in it.

```go
func TestMain(m *testing.M) {
        bazel_testing.TestMain(m, bazel_testing.Args{
                Main: workspace,
        })
}

func SampleTest(t *testing.T) {
        if err := bazel_testing.RunBazel("build", "//:sample"); err != nil {
                t.Fatal(err)
        }
}
```

There is one more important thing about the `go_bazel_test` implementation worth mentioning:
`bazel_testing.TestMain` unpacks the test workspace and its dependencies in a specific place outside
of the sandbox in Bazel's `outputUserBase` directory

Here is an example of the directory structure created by `go_bazel_test` on my Linux machine:

```
 /home/user/.cache/bazel/_bazel_user/852e945491923423601e3c06e84323e3/
 |
 | # This is the directory where bazel creates "sandboxed" folders for every test
 | #   tests are encouraged to keep all their data inside those directories
 +-- execroot/
 |
 | # This is created by `go_bazel_test`
 +-- bazel_testing/
    |
    +-- bazel_go_test/
        |
        +-- main/
        |   |
        |   +-- WORKSPACE
        |   |
        |   +-- BUILD.bazel
        |   |
        |   +-- Sample.hs
        |
        +-- rules_haskell/
        |   |
            # files included in "all_files" filegroup
```

<a name="rules_go_workspace_path"></a>

So why is `go_bazel_test` not using the "sandboxed" directory which Bazel creates for every test? To answer this question
we need to understand some aspects of how Bazel does caching. If we look at the [Bazel output directory
layout][bazel_output_directory_layout] description, we'll see that for every project which Bazel builds, it
creates its own `outputBase` directory. The name of this directory is based on the md5sum of the project's absolute path, so it's unique for
every project you build on your machine. Every target built is stored in a directory corresponding
to this project's absolute path.

If we create a new Go project to build it with `rules_go`, when we run `bazel build //...` for the
first time it will spend a lot of time fetching and configuring the `go`-toolchain. For the following build
executions this toolchain would be reused from the cache and the overall build time significantly smaller.

Returning to `go_bazel_test`, if it used sandbox directories for every test workspace, their
absolute path would differ for every test and Bazel would be forced to build every test workspace from
scratch. So it will do fetching and configuration of the same toolchain every time, which would
take a lot of time and disk space.

To avoid this, `go_bazel_test` unpacks all tests workspaces into the same directory, keeping
the same absolute path across all test workspaces. In our example it's:

`/home/user/.cache/bazel/_bazel_user/852e945491923423601e3c06e84323e3/bazel_testing/bazel_go_test/main`

This will make Bazel reuse the same build cache between tests and significantly improve tests performance.
This approach of choosing a fixed directory weakens hermeticity, because tests are sharing some context between each other which
makes them less deterministic. Also it makes it impossible to run such tests in parallel, so they
have to use the `exclusive` tag. The `rules_haskell` test bench will
inherit these downsides.

### `rules_bazel_integration_test`:

There is a [project][rules_bazel_integration_test] created specifically for the purpose of testing Bazel
rules and was recently added into the [bazel-contrib][bazel-contrib] organization. It provides the
`bazel_integration_test` rule which supports:

- Creating test workspaces as nested directories inside a parent workspace
- Using arbitrary executable targets as test scenarios
- Using multiple Bazel versions to run tests against

With this ruleset we can rewrite our test in a clearer and more straightforward manner. We can create
a directory somewhere inside our project that looks like this:

```
WORKSPACE
|
test-scenarios/
|
+-- BUILD.bazel
|
+-- sample_test.sh
|
+-- sample_test/
|   |
|   +-- WORKSPACE
|   |
|   +-- BUILD.bazel
|   |
|   +-- Sample.hs
```

The contents of the `sample_test` directory would be the same as what was contained in our go-string in the
previous paragraph, except for `sample_test/WORKSPACE`. The path to parent workspace in `local_repository` would
differ since there is no specific optimizations moving everything outside of the sandbox anymore:

```python
local_repository(
    name = rules_haskell,
    path = "../",
)
```

`sample_test.sh` could look somewhat like:

```shell
# BIT_BAZEL_BINARY and BIT_WORKSPACE_DIR are environment variables set up by the test suite
# BIT is an acronym for Bazel Integration Testing

cd "$BIT_WORKSPACE_DIR"
output=$("$BIT_BAZEL_BINARY" run //:sample)
test "$output" == "Hello World!\n" || exit 1
```

and `test-scenarios/BUILD.bazel` like:

```python
load("@rules_bazel_integration_test//bazel_integration_test:defs.bzl", "bazel_integration_test")

sh_binary(
    name = "sample_test_scenario",
    srcs = "sample_test.sh",
)

bazel_integration_test(
    name = "sample",
    bazel_version = "4.2.0",
    test_runner = ":sample_test_scenario",
    workspace_files = integration_test_utils.glob_workspace_files("sample_test") + [
        "//:all_files",
    ],
    workspace_path = "sample_test",
)
```

This can be enough for a lot of projects, but without the optimizations from `go_bazel_test`, the average
integration test in `rules_haskell` would take about 5-7 minutes and huge amounts of disk space.

## Implementing a test rule for rules_haskell

Considering all of the above and with the specific needs of `rules_haskell` let's formulate the
requirements for the test rule we'd like to have in `rules_haskell`.

#### Creating a test in the form of a nested workspace

`rules_basel_integration_test` offers a way to express testcases in form of directories with Bazel
workspaces. In contrast `go_bazel_test` expects the workspace to be specified as a string of a specific
format inside a test scenario. I find the former much clearer and easy to read and
understand. In addition it provides clear and valid example of rules usage which the ruleset can refer to
in its documentation.

#### Testing multiple Bazel versions

Since `rules_haskell` is a public library which supports of a variety of Bazel versions it
would be very useful to be able to run integration tests using different Bazel versions and distributions.
Specifically `rules_haskell` supports [`Bazel >= 4.0`][rules_haskell_readme_bazel_versions] and
[Bazel installed using Nixpkgs][rules_haskell_readme_nixpkgs].

#### Picking the right Bazel configuration

`rules_haskell` [demands][rules_haskell_configurations] some configurations from the user depending on
the OS or the way the Haskell toolchain is distributed. For our tests we should be able to pick the proper
configuration automatically depending on the system this test is ran on.

#### Reuse the Bazel cache between tests runs

As I've mentioned before, we want to use the Bazel cache for
`rules_haskell` tests, in the same style as `go_bazel_test`. Otherwise
every test would require huge amount of time and disk space.

#### Writing test scenarios in Haskell

It's not that necessary but besides the fact that `rules_haskell` is about Haskell and most of its
contributors are proficient with Haskell, it has a [collection][rules_haskell_run_tests] of test
scenarios already written in Haskell and [waiting][rules_haskell_tests_issue] to be reimplemented as
an integration tests. So it would be very convenient if this new test rule would allow us to reuse
these test scenarios.

In order to achieve the first three points we can create a macro on top of `bazel_integration_test`.
Here is an example of how it looks like:

```python
load(
    "@rules_bazel_integration_test//bazel_integration_test:defs.bzl",
    "bazel_integration_test",
    "integration_test_utils",
)

def haskell_bazel_integration_test(
        name,
        srcs,
        deps,
        bazel_binaries,
        workspace_path,
        rule_files):

    test_scenario_name = "%s_scenario" % name
    haskell_binary(
        name = test_scenario_name,
        srcs = srcs,
        deps = deps,
        testonly = True,
    )

    for bazel_id, bazel_binary in bazel_binaries:
        bazel_integration_test(
            name = "%s_%s" % (name, bazel_id),
            test_runner = test_scenario_name,
            bazel_binary = bazel_binary,
            workspace_files = integration_test_utils.glob_workspace_files(workspace_path) + rule_files,
            workspace_path = workspace_path,
        )

```

The `bazel_binaries` argument exists because in `rules_haskell` we need to pass both
Bazel's official binary distributions and a Bazel binary from Nixpkgs. There is `bazel_versions` in
`bazel_integration_tests` which manages versions of official Bazel's binary and could be
entirely sufficient for many projects, but we need something more flexible to allow arbitrary
Bazel distributions. So here we expect `bazel_binaries` to be a dictionary with keys
representing a short descriptive name for a Bazel binary (e.g. `bazel_4` if it came from Nixpkgs' `bazel_4` package, or `4.2.1` if it's binary distribution of a specific version) and values which are
labels for the Bazel binaries.

To address the remaining two points we can borrow some ideas from `rules_go`. We can implement the same
approach and create a Haskell library for testing with a similar test setup procedure. It will find
a specific place and set up a test workspace in it, while also providing functions to invoke Bazel with
the proper configuration. For example they could look something like this:

```haskell
-- Sets up the workspace and Bazel's outputBaseDir. Returns paths to
-- workspace and to outputBaseDir as a tuple
setupWorkspace :: IO (String, String)

-- Creates a function which generates Bazel commands for a specific
-- workspaceDir and outputBaseDir, picks a bazel config depending on the OS as well
bazelCmd :: String -> String -> IO ([String] -> Process.CreateProcess)
```

In this case we can rewrite our test scenario like this:

```haskell
-- Imagine we have a function to assert a predicate on (stdout, stderr) of an arbitrary process
outputSatisfy
  :: ((String, String) -> Bool)
  -> Process.CreateProcess
  -> IO ()

main :: IO ()
main = do
  (workspaceDir, outputBaseDir) <- setupWorkspace
  bazel <- bazelCmd workspaceDir outputBaseDir
  let p (stdout, _stderr) = lines stdout == ["Hello World!"]
   in
     outputSatisfy p (bazel ["run", "//:sample"])
```

There is still one unsolved problem from mixing these two approaches. As mentioned above,
`rules_go`'s rule creates a special directory to keep the same absolute path of the test workspace.
`bazel_integration_test`'s rule on the other hand allows us to run different versions of Bazel in this
workspace. If you look at the code we wrote you can see we've created a separate test target for every
pair of test and Bazel version. Bazel doesn't guarantee any test execution order so without additional changes
the test cache will not be reused because although the absolute path of the test workspace is
the same, if the Bazel version differs from the previous test, Bazel
will reconstruct the cache from scratch.

In order to solve this, we will create a separate directory for every Bazel binary we are using for testing.
We need to communicate to the test suite which Bazel version we are going to use for a specific test, so that it
can prepare a proper directory for this test. We can use `bazel_id` - the short name we've introduced already -
to distinguish the Bazel binaries in test names.

```python
   for bazel_name, bazel_binary in bazel_binaries:
        bazel_integration_test(
            name = "%s_%s" % (name, bazel_id),
            test_runner = test_scenario_name,
            bazel_binary = bazel_binary,
            workspace_files = integration_test_utils.glob_workspace_files(workspace_path) + rule_files,
            workspace_path = workspace_path,
            env = {"BAZEL_ID": bazel_id},
        )
```

Now our `setupWorkspace` function can use the `BAZEL_ID` environment variable to create a directory, and
end up with something like:
`/home/user/.cache/bazel/_bazel_user/852e945491923423601e3c06e84323e3/bazel_testing/$BAZEL_ID`
instead of the [one created by `rules_go`](#rules_go_workspace_path).

With all this, we've got a rule, which we useg in `rules_haskell` to
create tests to be run with Bazel versions
4.0.0, 4.2.2, 5.0.0, 5.2.0 and also with Bazel binary provided by the `bazel_4` package from Nixpkgs.
With all the optimizations the average test duration is about 25-30 seconds.

## Conclusion

This was a brief story about how we developed an approach to test our rules in `rules_haskell`. If
you want to see the complete implementation you can find it in [this PR][integration_testing_pr], and
some instructions on how to use it [here][integration_testing_readme]. I hope you can borrow
from our experience to test your rules, and that together we will find a way to have a simple, clean and
safe process of developing Bazel rules. Stay tuned for more content for rule writers!

[rules_haskell]: https://github.com/tweag/rules_haskell
[rules_rust]: https://github.com/bazelbuild/rules_rust/tree/main/examples
[rules_go]: https://github.com/bazelbuild/rules_go
[rules_nixpkgs]: https://github.com/tweag/rules_nixpkgs/tree/master/examples
[rules_go_all_files]: https://github.com/bazelbuild/rules_go/blob/31d17212968b472032035a45353a1751f1c0f529/BUILD.bazel#L153
[rules_bazel_integration_test]: https://github.com/bazel-contrib/rules_bazel_integration_test
[bazel-contrib]: https://github.com/bazel-contrib
[integration_testing_pr]: https://github.com/tweag/rules_haskell/pull/1766
[integration_testing_readme]: https://github.com/tweag/rules_haskell/tree/master/tests/integration_testing#readme
[bazel_output_directory_layout]: https://bazel.build/docs/output_directories
[rules_haskell_readme_bazel_versions]: https://github.com/tweag/rules_haskell#setup
[rules_haskell_readme_nixpkgs]: https://github.com/tweag/rules_haskell#nixpkgs
[rules_haskell_configurations]: https://github.com/tweag/rules_haskell#configuring-your-platform
[rules_haskell_tests_issue]: https://github.com/tweag/rules_haskell/issues/951
[rules_haskell_run_tests]: https://github.com/tweag/rules_haskell/blob/master/tests/RunTests.hs
