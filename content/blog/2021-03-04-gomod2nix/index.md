---
title: "Announcing Gomod2nix"
author: Adam Hoese
description: "Better Go packaging with Gomod2nix"
tags: [nix, golang]
---

I'm very pleased to announce [Gomod2nix](https://github.com/tweag/gomod2nix), a new tool to create Go packages with Nix!

Gomod2nix is a code generation tool whose main focus is addressing the correctness and usability concerns I have with the current Go packaging solutions.
It offers a composable override interface, which allows overrides to be shared across projects, simplifying the packaging of complex packages.
As a bonus, it also boasts much better cache hit rates than other similar solutions, owing to not abusing fixed-output derivations.

I also took the opportunity of this new package to address some long-standing annoyances with existing Go Nix tooling. For instance Gomod2nix disables [CGO](https://dave.cheney.net/2016/01/18/cgo-is-not-go) by default, this let me enable static Go binaries by default.
Changing the defaults in existing tooling would be very difficult as it would break a lot of existing packages, especially those maintained outside of Nixpkgs, which depend on the present behavior.

In order to motivate this new tool, let's take a look at how Go dependency management evolved.

The developmentent of [Trustix][trustix] (which Gomod2nix was developed for) is funded by [NLNet foundation][nlnet] and the European Commission's [Next Generation Internet][ngi] programme through the [NGI Zero PET][pet] (privacy and trust enhancing technologies) fund.

# A history of Go packaging

In Go you don't add dependencies to a manifest, but instead you add a dependency to your project by simply adding an import to a source file:

```go
package main

import (
    "fmt"
    "github.com/tweag/foo"
)

func main() {
    fmt.Println(foo.SomeVar)
}
```

and the `go` tool will figure out how to fetch this dependency.

From the beginning Go didn't have package management in the traditional sense.
Instead it enforced a directory structure that mimics the import paths.
A project called `github.com/tweag/foo` that depends on `github.com/tweag/bar` expects to be located in a directory structure looking like:

```
$GOPATH/github.com/tweag/foo
$GOPATH/github.com/tweag/foo/main.go
$GOPATH/github.com/tweag/bar
$GOPATH/github.com/tweag/bar/main.go
```

This may not look so bad in this very simple example, but since this structure is not only enforced for _your_ packages but also your dependencies, this quickly becomes messy.
The `$GOPATH` mechanism has been one of the truly sore spots of Go development.
Under this packaging paradigm you are expected to always use the latest Git master of all your dependencies and there is no version locking.

[Dep](https://golang.github.io/dep/) was the first official packaging experiment for Go.
This tool improved upon `$GOPATH` not by removing it, but by hiding that complexity from the user entirely.
Besides that, it added lock files and a SAT dependency solver.

Finally, armed with the learnings and some critique of Dep, the Go team decided to develop a new simpler solution -- Go modules.
It addressed a number of perceived problems with Dep, like the fact that the use of semver and SAT solvers are far too complicated for the requirements Go has.
As of now, Dep is deprecated, and Go modules is the solution I developed against.

# A tale of two lock files

Originally, I set out to design `gomod2nix` in the same way as [poetry2nix](https://github.com/nix-community/poetry2nix), a Python packaging solution for Nix.
In `poetry2nix` one refers directly to a Poetry lock file from Nix, and `poetry2nix` does all the job needed to create the Nix package, which is very convenient.

However, this wasn't possible here because of the design of Go modules, for reasons that I will explain below.
As a consequence, `gomod2nix` is a Nix code generation tool.
One feeds lock files to it, and it generates Nix expressions defining the corresponding packages.

In the following, I will compare the Poetry and Go lock files, and show which limitations the Go file format and import mechanism imposes upon us.

### Exposed dependency graphs

First, let's look at an excerpt of [Poetry](https://python-poetry.org/)'s lock file:

```toml
[[package]]
name = "cachecontrol"
version = "0.12.6"
description = "httplib2 caching for requests"
category = "main"
optional = false
python-versions = ">=2.7, !=3.0.*, !=3.1.*, !=3.2.*, !=3.3.*"

[package.dependencies]
lockfile = {version = ">=0.9", optional = true, markers = "extra == \"filecache\""}
msgpack = ">=0.5.2"
requests = "*"
```

And also at `go.sum`:

```
github.com/davecgh/go-spew v1.1.0 h1:ZDRjVQ15GmhC3fiQ8ni8+OwkZQO4DARzQgrnXU1Liz8=
github.com/davecgh/go-spew v1.1.0/go.mod h1:J7Y8YcW2NihsgmVo/mv3lAwl/skON4iLHjSsI+c5H38=
github.com/pmezard/go-difflib v1.0.0 h1:4DBwDE0NGyQoBHbLQYPwSUPoCMWR5BEzIk/f1lZbAQM=
github.com/pmezard/go-difflib v1.0.0/go.mod h1:iKH77koFhYxTK1pcRnkKkqfTogsbg7gZNVY4sRDYZ/4=
github.com/stretchr/objx v0.1.0 h1:4G4v2dO3VZwixGIRoQ5Lfboy6nUhCyYzaqnIAPPhYs4=
github.com/stretchr/objx v0.1.0/go.mod h1:HFkY916IF+rwdDfMAkV7OtwuqBVzrE8GR6GFx+wExME=
gopkg.in/check.v1 v0.0.0-20161208181325-20d25e280405 h1:yhCVgyC4o1eVCa2tZl7eS0r+SDo693bJlVdllGtEeKM=
gopkg.in/check.v1 v0.0.0-20161208181325-20d25e280405/go.mod h1:Co6ibVJAznAaIkqp8huTwlJQCZ016jof/cbN4VW5Yz0=
gopkg.in/yaml.v3 v3.0.0-20200313102051-9f266ea9e77c h1:dUUwHk2QECo/6vqA44rthZ8ie2QXMNeKRTHCNY2nXvo=
gopkg.in/yaml.v3 v3.0.0-20200313102051-9f266ea9e77c/go.mod h1:K4uyk7z7BCEPqu6E+C64Yfv1cQ7kz7rIZviUmN+EgEM=
```

After squinting at these files for a while we can already see some radical differences in semantics, most notably that `go.sum` is structured as a flat list rather than a graph.
Of course, all dependencies to the build are listed in `go.sum`, but we don't know what depends on what.
What this means for us in Nix is that we have no good unit of incrementality -- everything has to be built together -- while Poetry can build dependencies separately.

### Bespoke formats

Go modules use its own Go-like file format, while Poetry uses [TOML](https://en.wikipedia.org/wiki/TOML) to serialize both its manifest and lock files.

While this is format is simple and writing a parser for it isn't hard, it makes the lives of tooling authors harder.
It would be much easier if a standard format for data interchange was used, rather than a custom format.

### Bespoke hashes

The next problem with Go modules is its use of a custom hashing mechanism that's fundamentally incompatible with how Nix hashes paths.

As explained in [Eelco Dolstra's](https://github.com/edolstra) thesis [_The Purely Functional Software Deployment Model_](https://edolstra.github.io/pubs/phd-thesis.pdf) Nix uses it's own reproducible archive format NAR, which is used for both uploading build results and for directory hashing.

The Go developers faced with similar concerns created their own directory hashing scheme, which unfortunately is fundamentally incompatible with Nix hashes.
I don't see how Go modules could have done this any better, but the situation is unfortunate.

### Dynamic package name resolving

In the previous example, I showed how a Go import path looks like.
Sadly it turns out that the surface simplicity of those paths hide a lot of underlying logic.

Internally, these import paths are handled by the [`RepoRootForImport`](https://godoc.org/golang.org/x/tools/go/vcs#RepoRoot) family of functions in the `vcs` package, which maps import paths to repository URLs and VCS types.
Some of these are matched statically using regex but others use active probing.

This is a true showstopper for a pure-Nix Go packaging solution, and the reason why Gomod2nix is a code generation tool -- we don't have network access in the Nix evaluator, making it impossible to correctly resolve VCS from a Nix evaluation.

# Solutions to go modules packaging

The points above make our limitations clear.
With these in mind, let's discuss how Go packaging solutions were conceived.

### Code generation: vgo2nix

My first attempt at creating a tool for packaging Go modules was [vgo2nix](https://github.com/nix-community/vgo2nix), another code generation tool.
It was written very shortly after modules were announced, and at the time the tooling support for them wasn't good.
For example, there wasn't a parser for `go.mod` published back then.

Being based on the older Nixpkgs Go abstraction `buildGoPackage` this method emulates a `$GOPATH` based builds that make some assumptions that are not true.

Let's again look at an excerpt from `go.sum`:

```
github.com/Azure/go-autorest/autorest v0.9.0/go.mod h1:xyHB1BMZT0cuDHU7I0+g046+BFDTQ8rEZB0s4Yfa6bI=
github.com/Azure/go-autorest/autorest v0.9.3/go.mod h1:GsRuLYvwzLjjjRoWEIyMUaYq8GNUx2nRB378IPt/1p0=
github.com/Azure/go-autorest/autorest/adal v0.5.0/go.mod h1:8Z9fGy2MpX0PvDjB1pEgQTmVqjGhiHBW7RJJEciWzS0=
github.com/Azure/go-autorest/autorest/adal v0.8.0/go.mod h1:Z6vX6WXXuyieHAXwMj0S6HY6e6wcHn37qQMBQlvY3lc=
github.com/Azure/go-autorest/autorest/adal v0.8.1/go.mod h1:ZjhuQClTqx435SRJ2iMlOxPYt3d2C/T/7TiQCVZSn3Q=
github.com/Azure/go-autorest/autorest/date v0.1.0/go.mod h1:plvfp3oPSKwf2DNjlBjWF/7vwR+cUD/ELuzDCXwHUVA=
github.com/Azure/go-autorest/autorest/date v0.2.0/go.mod h1:vcORJHLJEh643/Ioh9+vPmf1Ij9AEBM5FuBIXLmIy0g=
```

These packages are all developed in the same repository but have different tags, and in this case `vgo2nix` would incorrectly only clone one version of the repository and sacrifice the correctness we get from modules because of how `$GOPATH` is set up.

### Fixed-output derivations: buildGoModule

The [buildGoModule](https://nixos.org/manual/nixpkgs/stable/#ssec-go-modules) tool is the most popular solution for Go packaging right now in Nixpkgs.
A typical buildGoModule package looks something like:

```
{ buildGoModule, fetchFromGitHub, lib }:

buildGoModule {
  pname = "someName";
  version = "0.0.1";

  src = fetchFromGitHub { ... };

  vendorSha256 = "1500vim2lmkkls758pwhlx3piqbw6ap0nnhdwz9pcxih4s4as2nk";
}
```

The `buildGoModule` package is designed around fixed-output derivations, which means that a single derivation is created where all the dependencies of the package you want to build are wrapped, and only a single hash of the derivation is specified.
It fetches all dependencies in the fixed output, creating a [vendor](https://golang.org/ref/mod#vendoring) directory which is used for the build.

This has several issues, most notably there is no sharing of dependencies between packages that depends on the same Go module.

The other notable issue is that it forces developers to remember to edit the `vendorSha256` attribute separately from the already existing `hash`/`sha256` attribute on the derivation.
Forgetting to do so can not only lead to incorrect builds but also be frustrating when working with larger packages that takes a long time to build, and only very late in the build notice that something was broken so you have to start over from scratch.

Because of the lack of hash granularity the build needs to clone _every_ dependency every time the `vendorSha256` is invalidated, and cannot use the cache from previous builds.

Fixed-output derivations can also be considered an impurity, and there is [a push to restrict them](https://github.com/NixOS/nix/issues/2270).

### My solution: gomod2nix

Approach-wise [`gomod2nix`](https://github.com/tweag/gomod2nix) positions itself right between `vgo2nix` and `buildGoModule`.
It's still a code generation tool like `vgo2nix`, but fully embraces the Go modules world and only supports Go modules based builds -- the old `GOPATH` way is unsupported.
It uses the same vendoring approach that `buildGoModule` uses, but instead of vendoring the actual sources in a derivation, it uses symlinks instead.
In that way, dependencies can be fetched separately, and identical dependencies source trees can be shared between multiple different packages in the Nix store.

From a user perspective the workflow is largely similar to `vgo2nix`:

- You write a basic expression looking like:

```nix
pkgs.buildGoApplication {
  pname = "gomod2nix-example";
  version = "0.1";
  src = ./.;
  modules = ./gomod2nix.toml;
}
```

- Run the code generation tool: `$ gomod2nix`

# Conclusion

Go packaging looks very simple on the surface, but murky details lure around underneath, and there are lots of tiny details to get right to correctly create a Go package in a sandboxed environment like Nix.

I couldn't get the best-in-class user experience I was hoping for and gotten used to with Poetry2nix.
Code generation adds extra steps to the development process and requires either a developer or a test pipeline to keep the Nix expressions in sync with the language specific lock files, something that requires discipline and takes extra time and effort.
Despite that, it turned out there were major wins to be had regarding creating a new packaging solution.

The development of [gomod2nix](https://github.com/tweag/gomod2nix) is funded by [NLNet](https://nlnet.nl/) through the [PET](https://nlnet.nl/PET/)(privacy and trust enhancing technologies) fund.
[gomod2nix](https://github.com/tweag/gomod2nix) is being developed as a part of [Trustix](https://nlnet.nl/project/Trustix/).

<a href="https://nlnet.nl/" style="width=40%;margin=2%;">![NLNet](./nlnet-banner.png)</a>

<a href="https://nlnet.nl/NGI0" style="width=40%;margin=2%;">![NGI0](./NGI0_tag.png)</a>

[trustix]: https://github.com/tweag/trustix
[nlnet]: https://nlnet.nl/project/Trustix
[pet]: https://nlnet.nl/PET
[ngi]: https://ngi.eu
