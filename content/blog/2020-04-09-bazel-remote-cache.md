---
title: "Setting up a shared build cache <br/>using Bazel"
shortTitle: "Shared Bazel cache"
author: Mark Karpov
tags: bazel, devops
description: "How to set up a remote Bazel cache, and why it is a good idea."
---

Long build times are a productivity killer. Having to wait an hour or more
for a large project to build after an initial checkout, having to wait again
when checking out a new branch, and wait yet more after returning to the
original branch, can be outright demoralizing. If there was a silver bullet,
few would talk much about build times. You'll want to bring to bear multiple
strategies to solve this tough problem. I'll talk today about just one
strategy: make it so that build artifacts only need to be built *once* by
anyone on your team and by the build bots. To achieve this, you'll need a
shared build cache. In this post I'm going to show why Bazel may be a good
solution if having a robust cache shared among all developers is important
to you, and how to setup such a cache.

## Granularity of cache

Why bother with a Bazel cache if you are already using other caches,
like the [Nix binary cache][tweag-untrusted-ci], or [Docker
registries][docker-registries]?

Nix is a great solution for caching system dependencies, that is, the code
that your developers do not modify. Since Nix provides a per-package cache, a
build may run for one hour only to fail because of a syntax error. In that
case there will be no caching because the build did not succeed. Next time
you would have to wait another hour to get to the same point in the
compilation process and to learn whether you have fixed the problem or not.

On the other hand, Bazel caching is much more granular. Typically the cache
works on the level of components or modules. That means that if you have a
syntax error in the module `X` and all dependencies of `X` compiled
successfully, recompilation will start from `X` or close to `X`.

[tweag-untrusted-ci]: https://www.tweag.io/posts/2019-11-21-untrusted-ci.html
[docker-registries]: https://docs.docker.com/registry/

## Local vs shared caching

[Bazel][bazel] breaks a build into discrete steps, which are called
[actions][bazel-actions]. Each action has required inputs and expected
outputs. Action outputs can be cached, so that running another build
does not require executing all actions again. There are two levels of
caching in Bazel:

* a "local" cache for the currently checked out
  [workspace][bazel-workspace],
* an optional "remote" cache, which can be reused across all
  workspaces and branches no matter where and how many times they are
  checked out. If this remote cache is stored on a server, it's
  possible to share this remote cache among all developers in your
  team and even with build drones used during continuous integration
  (CI).

The local cache can be sufficient for small projects. But you'll
eventually want to setup a remote cache as well. Having a remote
cache, and sharing it, has multiple advantages:

* After completing a build of branch `mybranch1`, all subsequent
  builds are instantaneous, even if you checked out `mybranch2` in the
  meantime.
* If CI and developers share the build cache, then builds of the
  master branch are always fully cached and therefore very fast, even
  after an initial checkout of the project by a new developer.
* CI build drones can be stateless. They all pull from a shared cache
  to accelerate the build. If our cache is not stored on the file
  systems of individual nodes but instead is kept in a storage service
  that is re-used by everyone, we can switch to short-lived,
  preemptable CI nodes and cut the costs significantly. In other
  words, a remote cache allows us to separate computing resources from
  storage resources.
* Initial build times after submitting a PR can be very low indeed. If
  you allow developers to write to the shared cache, then by the time
  the PR is submitted, the build will have been already cached if the
  developer ran a build on their laptop first.

Sharing a cache across multiple machines and users is usually
a dangerous proposition. Shared caches have a knack for exacerbating
the effects of any bugs in your build targets specification. For
example, omitting to explicitly declare an input to a build action can
lead to failed builds. But in the presence of a shared cache this can
moreover lead to *cache poisoning*, which can lead to builds
succeeding but producing corrupted build artifacts. Say a CI build
drone executes a build action that needs `A` and `B` as inputs, but
only `A` is declared. Then the output of the build action could go
into the shared cache, and be reused by all developers running a build
on their own machine, even if the content of `B` happens to be
different on their system. But thanks to
[sandboxing][bazel-sandboxing] of build actions, Bazel guarantees that
if a build action succeeds, the set of inputs was correctly declared.
So cache poisoning is much less of a concern when using Bazel.

Below I describe two ways to setup a remote Bazel cache in the [Google
Cloud Platform (GCP)][gcp]. Both methods use GCP because Bazel has
more extensive support for it. The first method uses a bucket for
storage, while the second uses the experimental remote build execution
(RBE) service of GCP. It is worth mentioning that it is also possible
to use AWS for a [Bazel remote cache][bazel-remote] and it has [been
done][bazel3cache]. More information about the flags used below can be
found in the [Bazel docs][bazel-docs].

[bazel-actions]: https://docs.bazel.build/versions/master/bazel-overview.html#what-is-the-action-graph
[bazel-workspace]: https://docs.bazel.build/versions/master/build-ref.html#workspace
[bazel-sandboxing]: https://blog.bazel.build/2015/09/11/sandboxing.html

## Remote cache with a GCP bucket

The first method amounts to setting up a [GCP bucket][gcp-bucket] and granting CI nodes
access to that bucket. Bazel can then get access to the bucket in two ways:

* When Bazel is run inside of a Google Compute Engine instance the
  `--google_default_credentials` flag can be used for authentication.
* Otherwise `--google_credentials=/path/to/your/secret-key.json` can tell
  Bazel which secret key to use. The `secret-key.json` file can be generated
  for a service account using the GCP console, in the IAM & Admin section.

Then, the following configuration options should be used to tell Bazel to
use the remote cache:

* `--remote_http_cache=https://storage.googleapis.com/<bucket-name>` where
  `bucket-name` is the name of the cache Bucket.
* `--remote_upload_local_results=true` makes Bazel upload the results of
  locally executed actions to the remote cache.

These flags can both be set on the command line, or in your
[`user.bazelrc`][user-bazelrc].

## Remote cache using remote build execution

[Remote build execution (RBE) for GCP][rbe] is an experimental service
in alpha mode. It provides:

* A shared cache that eliminates duplication of effort across the entire
  team.
* A scalable worker pool that reduces latency by parallelizing the build execution
  across thousands of nodes.

The second feature is outside of the scope of this blog post and can be
challenging to use in practice because RBE requires the build rules you use
to support remote execution, which is not always the case.

In order to enable and use RBE, one must [join the
`RBE-Alpha-Customers`][rbe-join] Google group and follow the steps described
in [the official documentation][rbe-docs]. Once it is done, the following
options should be passed to Bazel. Again, these flags
can be set on the command line, or in your [`user.bazelrc`][user-bazelrc].

* `--remote_cache=grpcs://remotebuildexecution.googleapis.com`
* `--host_platform_remote_properties_override='properties:{name:"cache-silo-key"
  value:"<silo-key>"}'` where `silo-key` should be different for every
  unique configuration/machine.
* `--remote_instance_name=projects/<project-id>/instances/<instance-id>`
  where `project-id` is the id of the project you created, and `instance-id`
  is the id of the instance.

For authentication, the same two methods work (either with
`--google_default_credentials` or with `--google_credentials`), depending on
where you run your builds.

## Conclusion

Shared caching is an example where "correct" is a prerequisite to "faster".
Bazel's focus on build system correctness pays back in spades when
your project becomes large, because more aggressive optimization
strategies to speed up your builds, like shared caching, become viable
when they would otherwise cause a lot of pain.
Comparing the two approaches we presented here for setting up a shared cache, the first one
represents a more “manual” setup where you have more control (since you
create and manage a bucket yourself) while the other is more like a service
that is managed for you (RBE). Using paid services can be cheaper than
having your engineers spend their time setting up and maintaining the
infrastructure, even though in this particular case creating a bucket is
also quite simple. When going with RBE it is also worth remembering that it
currently works only in one region, `us-central1` and so in certain cases
you won't be able to benefit from co-location between build cache and
build drones.

[bazel]: https://bazel.build
[bazel-docs]: https://github.com/bazelbuild/bazel/blob/master/site/docs/remote-caching.md#user-content-google-cloud-storage
[bazel-remote]: https://github.com/buchgr/bazel-remote
[bazel3cache]: https://github.com/Asana/bazels3cache
[gcp]: https://console.cloud.google.com/getting-started
[rbe]: https://groups.google.com/forum/#!forum/rbe-alpha-customers
[rbe-join]: https://groups.google.com/forum/#!forum/rbe-alpha-customers
[rbe-docs]: https://blog.bazel.build/2018/10/05/remote-build-execution.html
[gcp-bucket]: https://cloud.google.com/storage/docs/creating-buckets
[tweag-untrusted-ci]: https://www.tweag.io/posts/2019-11-21-untrusted-ci.html
[docker-registries]: https://docs.docker.com/registry/
[user-bazelrc]: https://docs.bazel.build/versions/master/best-practices.html#bazelrc
