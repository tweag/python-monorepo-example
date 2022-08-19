---
title: "Functional data pipelines with funflow2"
shortTitle: "Functional data pipelines with funflow2"
author: Dorran Howell, Guillaume Desforges, and Vince Reuter
tags: [data-science, haskell]
description: "Introducing a library for writing data pipelines which compose well and fail early"
---

As the data science and machine learning fields have grown over the past decade,
so has the number of data pipelining frameworks. What started out largely as a
set of tools for extract-transform-load (ETL) processes has expanded into a
diverse ecosystem of libraries, all of which aim to provide data teams with the
ability to move and process their data. Apache Airflow, Beam, Luigi, Azkaban -- the list [goes on and
on](https://github.com/pditommaso/awesome-pipeline).

As users of several of the above frameworks, Tweag has a special interest in
data pipelines. While working with them, we have observed a few common
shortcomings which make writing and debugging data pipelines more complex than
it needs to be. These shortcomings include limited
composability of pipeline components, as well as minimal
support for static analysis of pipelines. This second issue can be especially
annoying when executing pipelines in a machine learning context, where pipelines
are often defined as a Directed Acyclic Graph (DAG) of long-running tasks (e.g.
training a model). In these situations, early identification of a pipeline
doomed to fail due problems like configuration errors can spare great waste of
compute time and resources.

Additionally, while many data pipeline frameworks provide some support for
choosing the execution environment of the pipeline (e.g. running locally vs. on
a Spark or Kubernetes cluster), not all provide control over the execution logic
of the tasks themselves. This is a common limitation of workflow-oriented
frameworks, like Apache Airflow, that couple a task's definition to its
execution logic by combining them in a single class. This lack of
modularity makes it difficult to do things like write integration
tests for pipeline components.

## Enter: funflow2

`funflow2` is the successor of
[`funflow`](https://www.tweag.io/blog/2018-04-25-funflow/), a Haskell library
for writing workflows in a functional style. `funflow2` makes use of
[`kernmantle`](https://github.com/tweag/kernmantle/) to
offer several advantages over the original `funflow` including greater
extensibility, additional type-level controls, and a more feature-rich
interpretation layer for analyzing pipelines before execution.

An [ICFP Haskell Symposium 2020 paper](https://doi.org/10.1145/3406088.3409023)
provides a deep dive into `kernmantle`'s design and features.

`funflow2` aims to address the limitations we have observed in other data
pipeline frameworks while providing a simple, high-level API to express
pipelines in a functional style. Let’s take a closer look.

## Composability

In `funflow2`, a pipeline is represented by a `Flow` data type. Flows have the
nice property of being composable, which allows pipeline subcomponents to be
recombined with other components to create new pipelines.

To illustrate this point, we will compare two simple Apache Airflow and
`funflow2` pipelines, each with three sequential tasks that execute a function.
In Airflow, this can be written as follows.

```python
from datetime import datetime

from airflow import DAG
from airflow.operators.python import PythonOperator

dag = DAG(
   "example1",
   schedule_interval=None,
   default_args={"start_date": datetime(2021, 10, 1)},
)

gen_data = PythonOperator(
   python_callable=lambda: 1,
   op_args=None,
   task_id="task-gen-data",
   dag=dag,
)

add_one = PythonOperator(
   python_callable=lambda x: x + 1,
   op_args=[gen_data.output],
   task_id="task-add-one",
   dag=dag,
)

print_result = PythonOperator(
   python_callable=lambda x: print(x),
   op_args=[add_one.output],
   task_id="task-print-result",
   dag=dag,
)
```

In Airflow it's simple to define a DAG of tasks, but it's tricky to re-use it
among pipelines. Airflow provides support for a
[SubDagOperator](https://airflow.apache.org/docs/stable/concepts.html?highlight=subdag#subdags)
to trigger other DAGs, but in the end we end up with two completely separate DAG
objects, forcing us to manage them individually.

With Funflow2 reusability is much simpler. Since Flows are designed to be
composable [with
Arrows](https://hackage.haskell.org/package/base-4.14.0.0/docs/Control-Arrow.html),
we can just write:

```haskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

import Funflow (pureFlow, ioFlow)
import Control.Arrow ((>>>))

-- Note: the >>> operator is used to chain together two Flows sequentially
genData = pureFlow (const 1)
addOne = pureFlow (\x -> x + 1)
printResult = ioFlow (\x -> print x)
example1 = genData >>> addOne >>> printResult

anotherFlow = example1 >>> pureFlow (\_ -> "anotherFlow")
```

It’s as simple as that!

## Identifying errors early

One of the most frustrating things that can happen when executing a pipeline of
long-running tasks is to discover that one of the tasks towards the end of the
pipeline was misconfigured - leading to wasted developer time and compute
resources. Our original `funflow` package sought to alleviate this pain point by
[emphasizing _resumability_](https://www.tweag.io/blog/2018-04-25-funflow/).
Resumability was achieved through _caching_ results as pipeline execution
proceeded. `funflow2` supports resumability using the same caching approach as
the original `funflow.`

The potential time and resource savings provided by caching are limited,
however, in at least two ways. First, since pipeline tasks are structured as
atomic units, a failed pipeline cannot recover the work that was successfully
completed _within_ a failing task before the failure occurred. Perhaps even more
importantly, with resumability a failed pipeline may still cause lost efficiency
due to mental context switching. For instance, maybe you start a pipeline run, switch to
another project (perhaps over multiple days), and then find that your pipeline
has failed. You might find yourself asking the question, "what was I trying to
achieve in the task that failed?" Earlier discovery of errors is less wasteful
with mental resources and less stressful for a pipeline user.

As a pipeline author it is useful to identify misconfigurations early in the
development process, either through compilation errors or through early failures
at run-time. While many pipelining frameworks divide a program's lifecycle into
compile-time and run-time, `kernmantle` (and thus funflow) distinguishes three
phases: compile-time, config-time and run-time. In the intermediate config-time
phase, the pipeline is interpreted and configured which allow for static
analysis. As we'll see below, `funflow2` makes use of both compile- and
execution-time phases to ensure early failure.

### Type errors, detectable at compilation time

There are a number of ways in which a pipeline author can accidentally
misconfigure a pipeline. One example of misconfiguration is when there's a
mismatch between a task's input type and the type of argument passed by an
upstream task. This kind of issue can plague pipelines built with a library
written in a dynamically typed language like Python (e.g. Airflow). It's less
common of an issue, though, for pipelines written in statically typed languages
like Java or Scala (e.g. `scio`). Since `funflow2` pipelines are written in
Haskell, the language's static type checking allows us to catch issues like this
at compile time.

For example, the following pipeline will fail to compile since `flow3` attempts
to pass the string output of `flow1` as an input to `flow2`, which expects an
integer.

```haskell
-- Note: `const` is a built in function which just returns the
-- specified value
flow1 :: Flow () String
flow1 = pureFlow (const "42")

flow2 :: Flow Integer Integer
flow2 = pureFlow (+1)
flow3 = flow1 >>> flow2
```

```
Couldn't match type ‘Integer’ with ‘String’
```

<a name="config-time-errors"></a>

### Value errors, detectable at config time

A more subtle and less easily detectable kind of misconfiguration occurs when a
task is configured in a way that passes compile-time type checks but is
guaranteed to be invalid at runtime. Funflow2 is built on top of `kernmantle`,
which provides us with a convenient layer for extracting _static information_
from tasks in a pre-execution phase called config-time, after the pipeline has
been loaded but before any task has run. This layer can be used for early
detection of errors in a pipeline and ensuring early failure if something is
awry. For example, let's try running a Flow which attempts to run a Docker
container with an image tag that does not exist. This pipeline can be compiled
without complaint but is doomed to fail at run-time.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Funflow (dockerFlow)
import Funflow.Tasks.Docker (DockerTaskConfig (..), DockerTaskInput (..))


failingFlow = dockerFlow $
  DockerTaskConfig {
    image = "alpine:this-tag-does-not-exist",
    command = "echo",
    args = ["hello"]
  }

-- Flows created with dockerFlow expect a DockerTaskInput,
-- and we can just create an empty value for the sake of this
-- example.
emptyInput = DockerTaskInput {inputBindings = [], argsVals = mempty}


flow = ioFlow (\_ -> print "start of flow")
  >>> pureFlow (const emptyInput)          -- Constructs dockerFlow input
  >>> failingFlow
  >>> ioFlow (\_ -> print "end of flow")
```

Attempting to run this pipeline gives us the following error.

```haskell
runFlow flow ()
```

```
Found docker images, pulling...
Pulling docker image: alpine:this-tag-does-not-exist
ImagePullError "Request failed with status 404: \"Not Found\" …"
```

Note that "start of flow" is never printed; the first task in the pipeline,
which would print that, never actually executes. This is because `funflow2`'s
default pipeline runner extracts the identifiers of all images required to run a
pipeline at config-time and attempts to pull them before starting the actual
pipeline run.

For another example of this _fail-fast_ behavior when a pipeline is
misconfigured, have a look at the [configuration
tutorial](https://tweag.github.io/funflow/tutorials/ExternalConfig.html).

## Aside: workflows and build systems

The connection between build systems and the kinds of task-based workflows /
pipelines mentioned here is no secret and is a topic we have mentioned in
previous [blog posts](https://www.tweag.io/blog/2018-04-25-funflow/). Despite
serving different domains, workflow tools like `funflow2` and build tools like
[Bazel](https://bazel.build/) or
[`mill`](https://www.lihaoyi.com/post/BuildToolsasPureFunctionalPrograms.html)
all seek to provide users with the ability to execute a graph of tasks in a
repeatable way. Here we briefly consider a couple of key parallels between build
systems and `funflow2`.

Early cutoff is a common property of build systems and refers to a build
system's ability to halt once it has determined that no dependency of a
downstream task has changed since the most recent build[^a-la-carte]. While
`funflow2` is not a build system, it can [be used to mimic
one](https://github.com/tweag/funflow/blob/master/funflow-examples/makefile-tool/)
and even exhibits early cutoff, owing to its caching strategy using
content-addressable storage. If the hash determined by the hash of a task and
its inputs has not changed since a previous pipeline run, work need not be
repeated.

Another property which disinguishes build systems is whether or not they support
dynamic dependencies, or those for which the relationships themselves may vary with task output(s). This property depends on the build systems' approach to
modeling graphs of tasks, and in a functional paradigm is determined by whether
or not tasks are modeled using monadic effects[^a-la-carte]. `funflow2` uses
arrows and not monads for composing its task graph and therefore falls within
the camp of tools which do not support dynamic dependencies such as `Bazel`. The
use of a static task graph is the key to pre-execution dependency collection and
is what allows `funflow2` to support the execution-free interpretation of tasks
to detect invalid configuration. If tasks were linked in a monadic fashion, this
would be impossible.

## Modularity

In addition to limited composability and fail-late behavior, tight coupling
between a task and its execution logic is another common weakness among existing
task-based data pipelining frameworks: the data defining a task is inseparably
bound to the specific logic for how the task is executed. In a closely coupled context, altering task
execution logic requires entire redefinition of the task itself, e.g. by
subclassing. This confounds granular testing and nudges pipeline validation
toward an end-to-end style. The resulting coarse resolution view of a test
failure complicates diagnosis and remedy of a bug.

In `funflow2`, an **interpreter** defines task execution logic and is separate
from the task definition itself. Effectively, an interpreter transforms a task,
which is treated as data, into an executable action. Separating concerns this way
allows development of custom interpreters for integration testing,
project-specific execution requirements, and more. Using interpreters, you can
do things like flip between test and deployment execution contexts while working
with the same pipeline definition. Furthermore, this modularity allows for a
greater static analysis, yielding enhanced fail-fast behavior.

While an example implementation is out of scope for this post, an example of
custom tasks and interpreters is available in the [extensibility
tutorial](https://tweag.github.io/funflow/tutorials/Tutorial2.html) on the
`funflow2` project website.

## Similarities with funflow

`funflow2` uses a completely new backend (`kernmantle`) and therefore has major
API differences from the original `funflow`. Nonetheless, many of the useful
features of `funflow` are available in `funflow2`:

<a name="arrow-syntax"></a> **Arrow syntax**

The most immediately evident similarity between the original `funflow` and
`funflow2` is syntactic. Since `funflow2` still models flows using arrows,
[Arrow syntax](https://www.haskell.org/arrows/syntax.html) can still be used to
define and compose flows.

**Caching**

Like its predecessor, the `funflow2` project uses the `cas-store` package which
provides caching functionality using content-addressable
[storage](https://en.wikipedia.org/wiki/Content-addressable_storage).

**Compatibility**

While `funflow2`'s API differs significantly from `funflow`, it provides [a
module with
aliases](https://tweag.github.io/funflow/api/funflow/html/Funflow-Compat.html)
for some of the core functions from `funflow` such as `step` and `stepIO` to
help simplify migration. Many of the core example from `funflow` have also been
ported to `funflow2` such as the C compilation tutorial or custom [make
example](https://github.com/tweag/funflow/blob/master/funflow-examples/makefile-tool/src/Main.hs)
discussed in an [earlier blog
post](https://www.tweag.io/blog/2018-07-10-funflow-make/).

## Next steps

Interested in trying out some hands-on examples? We’ve prepared a set of
tutorials on the [`funflow2`
website](https://tweag.github.io/funflow/tutorials/). Alternatively, each
tutorial notebook can be run locally using the [provided Nix
shell](https://github.com/tweag/funflow/tree/master/funflow-tutorial). You may
also initialize a `funflow2` project using our [cookiecutter
template](https://github.com/tweag/funflow/tree/master/cookiecutter-funflow).

`funflow2` is still in its early stages, and so far most of the development on
it has focused on building the `Flow` API, along with a few tasks and
interpreters. One area in which other data pipeline frameworks excel is in
providing a high level of interoperability through a wide range of predefined
tasks for common operations, including running database queries and uploading
data to cloud storage. We would welcome external contributions to provide these
and other common task types, so please open an issue or pull request if this
interests you.

Thanks for reading, and stay tuned for future updates on `funflow2`!

[^a-la-carte]:
    Refer to the paper ["Build Systems a la
    Carte"](https://www.microsoft.com/en-us/research/uploads/prod/2018/03/build-systems.pdf)
    for much more discussion of minimality, early cutoff, and other properties of
    build systems and pipelines.
