---
title: Stackage HEAD is live!
author: Mark Karpov
---

We are happy to announce that Stackage HEAD is now functional. For those who
had missed [the original blog post](original-post), or already forgot it,
let's refresh the idea behind Stackage HEAD.

Stackage nightly plans include a large set of real-world packages that are
known to build and play nicely together. By picking and fixing a nightly
plan and building it using development versions of GHC at different commits,
we can detect changes in build status of various packages in the plan, and
so detect regressions and/or other suspicious changes.

## Current workflow

The current workflow (which is slightly different from what was described in
the original blog post) is the following:

1. We run the script on Circle CI by using the
   [`snoyberg/stackage:nightly`](docker-container) docker container which is
   already prepared for running Stackage nightly build plans.

2. To avoid re-compiling GHC (which takes some time) we fetch bindists and
   some associated metadata (such as commit, branch, tag, etc.) from S3.
   [`ghc-artifact-collector`](ghc-artifact-collector) now uploads the
   bindists and metadata from GHC's CI processes run on CircleCI and
   AppVeyor. This tool was created to work around some limitations of
   AppVeyor and make artifact collection mechanism used on CircleCI and
   AppVeyor uniform, but it came in handy for saving us time with Stackage
   HEAD too.

   The tool probably deserves its own blog post. For example you could fetch
   bindist created at latest commit on GHC `master` like so:

   ```
   curl https://ghc-artifacts.s3.amazonaws.com/nightly/validate-x86_64-linux/latest/bindist.tar.xz --output latest-bindist.tar.xz
   ```

   You could also fetch metadata about this bindist:

   ```
   curl https://ghc-artifacts.s3.amazonaws.com/nightly/validate-x86_64-linux/latest/metadata.json --output latest-metadata.json
   ```

   Which is literally what we do in our script.

3. Next we download [`stackage-curator`](stackage-curator), which is a tool
   that can execute a Stackage build plan.

4. We pick a plan from the [`stackage-nightly`](stackage-nightly) repo. This
   plan should not change between tests, because we're interested in
   catching changes introduced by GHC, not by packages themselves or their
   dependencies.

5. We execute the build plan using `stackage-curator` and save the log.

6. The build log is parsed by a helper app called `stackage-head` and
   transformed into a simple CSV file that we refer to as “build report”.
   I'll get to its contents a bit later. Build reports are persisted in
   CircleCI caches at the moment. We accumulate more of them as more builds
   are attempted.

7. `stackage-head` also maintains a history of build results. It has a
   command called `diff` that picks two latest build reports and detects any
   changes, such as if a package stopped building or its test suite started
   to fail.

8. If the changes are considered “suspicious” `stackage-head diff` fails
   with non-zero exit code and thus the whole CircleCI job fails and email
   notification is sent.

9. The CircleCI script is configured to run 4 times per day for a start.

## Build results

Build results are currently modelled like so:

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

This is serialized like so (the `s` tag means success, there are also `f`
(failure), and `u` (unreachable)):

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

Using `nightly-2018-04-05` we got the following distribution:

* Failing packages: 5
* Unreachable packages: 794
* Packages that build: 897

The full build takes about 1 hour. So if all packages start to build we can
expect total time to be something about 2 hours with this plan.

Another interesting topic is what to consider “suspicious” change. Quite
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
happen, but theoretically possible if we diff arbitrary build reports (which
we should be able to do).

## In action

Build diff is split in two sections: suspicious and innocent. Suspicious
changes are those for which `isChangeSuspicious` returns `True`. A bit
unexpectedly, we detected such a change even before we quite finished
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

Does it indicate a regression? Not necessarily. Still it's good to be able
to catch these changes instead of discovering them when a release candidate
is out, or new version of GHC is published.

CircleCI logs are publicly available here:
https://circleci.com/gh/tweag/stackage-head. The `stackage-head` project can
be found here: https://github.com/tweag/stackage-head.

## Running as a job in GHC's CircleCI script

Some advantages to this approach:

1. We can test on per-commit basis and detect which commit introduces a
   change precisely.

2. We can see the effect of a commit before it's merged into `master`.

Reasons to stick to running Stackage HEAD as a separate CircleCI script:

1. We need to figure out how to persist caches containing build results with
   build results that would work nicely with arbitrary branching without
   messing up the caches. This requires using name of parent branch when
   naming CircleCI caches, which may be impossible.

2. Failure of Stackage HEAD is not necessarily a regression of GHC so it
   probably should not contribute to CI failures of GHC automatically.

3. Build times may increase in future as more packages start to build with
   development version of GHC and more packages are added to Stackage.

4. Right now: with separate setup we can configure email sending on failures
   whereas GHC CI is currently always red due to failing tests.

## Conclusion

Only time can tell how useful Stackage HEAD is. But it looks like the
experiment is worth it and may allow us to improve QA of GHC development
process.

[original-post]: https://www.tweag.io/posts/2017-10-27-stackage-head.html
[docker-container]: https://hub.docker.com/r/snoyberg/stackage/tags/
[ghc-artifact-collector]: https://github.com/tweag/ghc-artifact-collector
[stackage-curator]: https://github.com/fpco/stackage-curator
[stackage-nightly]: https://github.com/fpco/stackage-nightly
