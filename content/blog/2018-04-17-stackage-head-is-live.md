---
redirect_from: [/posts/2018-04-17-stackage-head-is-live.html]
title: Stackage HEAD is now live!
author: Mark Karpov, Manuel Chakravarty
tags: [haskell]
---

We are happy to announce that Stackage HEAD is now functional. For those who
had missed [the original blog post][original-post], or already forgot it,
let's refresh the idea behind Stackage HEAD.

Each Stackage Nightly is a build plan that includes a large set of
real-world packages that are known to build and play nicely together.
By picking a nightly plan and building it using development versions
of GHC at different commits, we can detect changes in the build status of
packages contained in the plan, which are caused by changes in GHC, thereby detecting
potential regressions in the compiler.

## Current workflow

The current workflow (which is slightly different from what was described in
the original blog post) is the following:

1. Our CI script runs in CircleCI inside a container instantiated
   from [a Docker image][our-container] derived
   from [`snoyberg/stackage:nightly`][snoyberg-container]. There are
   several reasons to use a separate image but all of them have to do
   with the same thing: hermeticity. The image `snoyberg/stackage:nightly`
   changes fairly regularly, but we want our results to be determined
   only by GHC and the current build plan, not by other factors. So we
   have to make sure that nothing else can affect our builds.

2. To avoid re-compiling GHC (which takes some time), we fetch bindists and
   some associated metadata (such as commit, branch, tag, etc.) from S3.
   The new tool [`ghc-artifact-collector`][ghc-artifact-collector] currently uploads the
   bindists and metadata from GHC's CI processes run on CircleCI and
   AppVeyor. This tool was created to work around some limitations of
   AppVeyor and to provide a uniform interface to the artifact collection mechanism used on CircleCI and
   AppVeyor, but it came in handy for saving us time with Stackage
   HEAD, too.

   The tool probably deserves its own blog post. For example you can fetch the
   bindist created at the latest commit on GHC `master`, like so:

   ```
   curl https://ghc-artifacts.s3.amazonaws.com/nightly/validate-x86_64-linux/latest/bindist.tar.xz --output latest-bindist.tar.xz
   ```

   You can also fetch metadata of this bindist:

   ```
   curl https://ghc-artifacts.s3.amazonaws.com/nightly/validate-x86_64-linux/latest/metadata.json --output latest-metadata.json
   ```

   Which is literally what we do in our script.

3. We pick a plan from the [`stackage-nightly`][stackage-nightly]
   repository. This plan should not change between tests, because we're
   interested in catching changes introduced by GHC, not by packages
   themselves or their dependencies.

4. We execute the build plan using `stackage-curator` and save the log.

5. The build log is parsed by a helper app called `stackage-head` and
   transformed into a simple CSV file that we refer to as the _build report_.
   We'll discuss its contents in the next section. Build reports are persisted
   in CircleCI caches at the moment. We accumulate more of them as more
   builds are attempted.

6. `stackage-head` also maintains a history of build results. It has a
   command called `diff` that picks the two latest build reports and detects any
   changes, such as if a package stopped building or its test suites started
   to fail.

7. If the changes are considered “suspicious” `stackage-head diff` fails
   with non-zero exit code and thus the whole CircleCI job fails and an email
   notification is sent (currently only to us). The `stackage-head` tool also automatically
   generates a Trac ticket with instructions on how to reproduce the issue and
   stores it as a build artifact on CircleCI. If we then decide that the
   failure is worth attention of GHC team, we just copy and paste the ticket.

8. The CircleCI script is configured to run 4 times per day for a start.

## Build results

Build results are currently modelled as follows:

```haskell
-- | Results of building a build plan by Stackage curator typically obtained
-- through parsing of its log or by loading prepared build results file in
-- CSV format.

newtype BuildResults = BuildResults
  { unBuildResults :: HashMap Text BuildStatus
    -- ^ 'BuildStatus'es per package
  } deriving (Show, Eq)

-- | Build status.

data BuildStatus
  = BuildFailure
    -- ^ The package failed to build
  | BuildUnreachable
    -- ^ The package could not be built for some reason (e.g. its dependency
    -- failed to build—the most common case)
  | BuildSuccess !Int !Int
    -- ^ Success, 'Int's are:
    --     * the number of passing test suites
    --     * the number of failing test suites
  deriving (Show, Eq)
```

This is serialized into CSV files (the `s` tag means success, there is also
`f` for build failures and `u` for unreachable packages):

```csv
semigroupoids,s,1,0
mtl-compat,s,0,0
cereal-text,s,0,0
modern-uri,s,1,0
rest-stringmap,s,0,0
regex-pcre,s,0,0
structs,s,2,0
etc.
```

Using `nightly-2018-04-13`, we got the following distribution:

- Failing packages: 28
- Unreachable packages: 826
- Packages that build: 928

Some failing packages:

- `exceptions`, blocking 118 packages
- `doctest`, blocking 52 packages
- `cabal-doctest`, blocking 36 packages
- `tagged`, blocking 32 packages
- `generic-deriving`, blocking 10 packages

In the future, we could perhaps open issues and ask maintainers of these packages
to make them compile with the development version of GHC.

The full build takes about 1 hour. So if all packages start to build, we can
expect the total time to be something about 2 hours with this plan.

Another interesting topic is what to consider a “suspicious” change. Quite
conservatively, most changes are suspicious:

```haskell
isChangeSuspicious :: Maybe BuildStatus -> Maybe BuildStatus -> Bool
isChangeSuspicious Nothing Nothing  = True -- this can't be, so suspicious
isChangeSuspicious (Just _) Nothing = True -- a package disappeared, not good
isChangeSuspicious Nothing (Just _) = True -- also strange
isChangeSuspicious (Just old) (Just new) =
  case (old, new) of
    -- There is no change, so this case won't be evaluated. But let's
    -- consider it not suspicious (nothing changes after all).
    (BuildFailure,     BuildFailure)     -> False
    -- A package became unreachable, this is suspicious.
    (BuildFailure,     BuildUnreachable) -> True
    -- Something was fixed, good. Still, we should look if any of its test
    -- suites fail.
    (BuildFailure,     BuildSuccess _ b) -> b > 0
    -- New failure, always suspicious.
    (BuildUnreachable, BuildFailure)     -> True
    -- There is no change, so this case won't be evaluated. Still
    -- unreachable, so not suspicious.
    (BuildUnreachable, BuildUnreachable) -> False
    -- This packages now builds, so really we should look if any of its test
    -- suites fail.
    (BuildUnreachable, BuildSuccess _ b) -> b > 0
    -- Now the package fails to build, suspicious.
    (BuildSuccess _ _, BuildFailure)     -> True
    -- A package became unreachable, suspicious.
    (BuildSuccess _ _, BuildUnreachable) -> True
    -- Here we should look carefully at the results of running test suites.
    (BuildSuccess p0 b0, BuildSuccess p1 b1) ->
      let moreTestSuitesFailToBuild = (p0 + b0) > (p1 + b1)
          moreTestSuitesFail        = b1 > b0
      in moreTestSuitesFailToBuild || moreTestSuitesFail
```

The `Maybe`s are used to represent the possibility that a package has
appeared/disappeared between in two build results which should not normally
happen when we use the same build plan, but theoretically possible if we
diff arbitrary build reports (which we should be able to do).

## In action

Build diff is split into two sections: suspicious and innocent. Suspicious
changes are those for which `isChangeSuspicious` returns `True`. A bit
unexpectedly, the tool detected such a change even before we quite finished
working on Stackage HEAD:

```
==== SUSPICIOUS CHANGES

stm-delay:
  at nightly-2018-04-05-5161609117c16cb7b29b2b8b1cd41e74341d4137.csv
    build succeeded, 2 test suites passed, 0 test suites failed
  at nightly-2018-04-05-3cfb12d8adac37e5565d66fd173e4648cc041e65.csv
    build succeeded, 1 test suites passed, 0 test suites failed

There are changes that need attention of GHC team.
```

Since the sum of succeeding and failing test suites decreased, this means
that some test suites stopped to build. On further inspection, we determined
that this change was not caused by GHC, and that forced us to improve
hermeticity of the setup to prevent this from happening in the future.

Even if the change were caused by GHC, it'd not necessarily indicate a
regression. Still, it's good to be able to catch these changes instead of
discovering them when a release candidate is out, or a new version of GHC is
published.

CircleCI logs are publicly available here:
<https://circleci.com/gh/tweag/stackage-head>. The `stackage-head` project
can be found here: <https://github.com/tweag/stackage-head>.

## Running as a job in GHC's CircleCI script

The script is currently run independently of GHC's CI processes. There are
some advantages to running Stackage HEAD as a job in GHC's CircleCI script:

1. We can test on a per-commit basis and precisely detect which commit introduces a
   change.

2. We can see the effect of a commit before it's merged into `master`.

Reasons to stick to running Stackage HEAD as a separate CircleCI script:

1. Failure of Stackage HEAD is not necessarily a regression of GHC so it
   probably should not contribute to CI failures of GHC automatically.

2. Build times may increase in the future as more packages start to build with the
   development version of GHC and more packages are added to Stackage.

3. Right now: with separate setup we can configure email sending on failures
   whereas GHC CI is currently still always red due to failing tests.

## Conclusion

Only time can tell how useful Stackage HEAD really is. But it looks like the
experiment is worth it and may allow us to improve QA of the GHC development
process. What do you think? Please let us know, or even better, please consider contributing to this new and exciting project.

[original-post]: https://www.tweag.io/posts/2017-10-27-stackage-head.html
[our-container]: https://hub.docker.com/r/mrkkrp/stackage-head/
[snoyberg-container]: https://hub.docker.com/r/snoyberg/stackage/tags/
[ghc-artifact-collector]: https://github.com/tweag/ghc-artifact-collector
[stackage-curator]: https://github.com/fpco/stackage-curator
[stackage-nightly]: https://github.com/fpco/stackage-nightly
