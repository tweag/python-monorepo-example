---
redirect_from: [/posts/2016-06-20-haskell-compute-paas-with-sparkle.html]
title: Haskell compute PaaS with sparkle
author: Alp Mestanogullari, Mathieu Boespflug
featured: yes
tags: [haskell, data-science]
---

Maintaining a compute cluster for batch or stream data processing is
hard work. Connecting it up to storage facilities and time sharing
resources across multiple demands even more so. Fortunately cloud
service providers these days typically upscale their offering to not
just providing infrastructure as a service (IaaS), but also fully
baked platforms (PaaS) on top for specific verticals or workflows.
Amazon offers Elastic MapReduce for creating on-demand Hadoop clusters
with just a few clicks. Google, Microsoft and others provide similar
services for creating cookie-cutter clusters on-demand. These services
make the common case easy: running a Java/Python/R distributed batch
job, written using Hadoop API's, sourcing data from S3. In this post
we'll show you how to turn these robust existing PaaS offerings into
interactive distributed Java/Python/R/_Haskell_ platforms.

<!--more-->

To illustrate, we ran a topic analysis Haskell/Scala app applied to
_all of English Wikipedia_ on 16 nodes (64 cores and 480GB RAM in
total) simultaneously.

## The application

We'll use topic modeling with [Latent Dirichlet Allocation (LDA)][lda]
from our [last post][hello-sparkle] as a running example. The full
code for this distributed application is available
[here][sparkle-lda]. It's a short 35 line Haskell program, with
a _bona fide_ `main` entry point, leveraging the [Spark][spark] API
for transparently distributing data parallel operations over as many
nodes in the cluster as are currently available. Much of the heavy
lifting is performed by canned natural language processing code, also
part of Spark, in their "machine learning" library.

In this post, like in the original [Scala version][databricks-lda],
we'll perform topic modeling on all of the English language Wikipedia.
The nice thing about building a distributed app using the Spark API is
that it can run anywhere. You can try performing topic modeling over
such a large dataset on your laptop, like we did in the last post
using a much smaller dataset, but you'll likely run out of patience
long before it finishes. So instead, let's run this in the Cloud,
where we can rent one-off clusters of any size on-demand for as long
as we need one.

There are many Cloud services available to create such on-demand
clusters easily, but the [Databricks managed Spark][databricks]
service is particularly convenient to use. First, let's build and
package our Haskell app as a JAR archive that can be uploaded to
Databricks:

```
$ git clone https://github.com/tweag/sparkle.git
$ cd sparkle
$ stack --nix build
$ stack --nix exec -- sparkle package sparkle-example-lda
```

As always, the [`--nix` flag][stack-nix] to all Stack commands means
the build will use a locally provisioned, auto-downloaded JVM and
Spark class files. This sequence of commands produces a file called
`sparkle-example-lda.jar`, a self-contained archive containing our
Haskell application as well as _all of the Haskell dependencies and
system shared libraries_, plus some Java glue code to allow Haskell
and the JVM to interoperate nicely. You can think of a `.jar` as
a universal file format for running our application anywhere.

In the next section, we'll run the app on a cluster using the
Databricks paid service, but read on for how to do that on plain old
Amazon EMR too.

[databricks]: https://databricks.com
[databricks-lda]: https://databricks.com/blog/2015/09/22/large-scale-topic-modeling-improvements-to-lda-on-apache-spark.html
[hello-sparkle]: 2016-02-25-hello-sparkle.html
[lda]: https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation
[spark]: http://spark.apache.org/
[sparkle-lda]: https://github.com/tweag/sparkle/tree/master/apps/lda
[stack-nix]: http://stack.readthedocs.io/en/stable/nix_integration/

## On Databricks

Think of Databricks as an IDE for analytics. You get,

- an online **file explorer** to easily manage and share your datasets,
- interactive **notebooks** for conveniently combining data, code and
  visualizations,
- **dashboards** for managing clusters to evaluate notebook cells on
  as quickly as possible and configuring new batch and streaming jobs.

Databricks currently only supports Scala, Python and R in notebooks.
But for full-fledged batch or streaming "jobs" (i.e. standalone apps),
Haskell is supported too, via the
[sparkle][sparkle] bindings. Let's walk
through the process of running a distributed Haskell app in
Databricks.

![Jobs dashboard](./databricks-jobs.png)

We'll want to,

1. create a new job,

![New job](./db-new-job.png)

2. upload the `.jar` we created in the previous section,

![JAR form](./db-jar-upload.png)

3. select a cluster size and press "Run".

When you get to the upload step, just make sure to mention
`io.tweag.sparkle.SparkMain` as the "main" class for your app (Amazon
EMR recognizes the main class automatically but Databricks doesn't
yet).

When a job run starts, Databricks creates a new cluster specially for
that job, which it tears down once the run is over. For the Wikipedia
dataset, which we can represent (together with an integer index
identifying each article) as a Haskell value using,

```haskell
do docs <- textFile sc "dbfs:/databricks-datasets/wiki/part-*"
   return (zipWithIndex docs)
```

we created a large 16 node cluster to **process all 4M+ pages that
make up the English language Wikipedia in 27 minutes**. And the
[results][sparkle-lda-wiki-results] are in!

[sparkle]: https://github.com/tweag/sparkle
[sparkle-lda-wiki-results]: https://gist.github.com/mboes/8634b4e2ae1bd7e1002c98695694905d

## On Amazon Elastic MapReduce

Alternatively, you can run the same `.jar` on Amazon Elastic MapReduce
(EMR). Getting started with Amazon's EMR just requires an AWS account.
With those credentials in hand, the process is a matter of:

1. uploading our application `.jar` to S3,
2. configuring a new cluster (click _Create cluster_ on the [EMR home
   page][aws-emr]),

![Enable Spark support](./emr-cluster-config.png)

3. adding the `.jar` to the cluster as a new "step" (click _Add step_
   in the task list and paste in the S3 location where you uploaded
   the JAR).

[aws-emr]: https://console.aws.amazon.com/elasticmapreduce/home

## What's next?

What we've shown in this post is that by packaging Haskell
applications as standalone `.jar` files binding to industry standard
JVM based API's for distributed computing, we can now leverage
a wealth of existing tools, environments and cloud services to
transparently run Haskell on a very large scale. The key enabler here
is that standalone `.jar`'s is the basic unit of distribution that
enterprise and data analytics tools expect, so that's the form factor
we targeted for our Haskell app.

While the 4 million pages of Wikipedia stand at just 51 GB to process,
50 iterations of LDA topic modeling represents close to 30 hours of
single core processing time, which we were able to bring down to just
27 minutes by provisioning a bunch of worker nodes at the click of
a button. We didn't have to build any of the infrastructure to support
that feat ourselves: we got to reuse it for free.

In a future post, we'll explore some of the technical details that
made it possible to ship unmodified GHC-compiled Haskell as
a standalone `.jar` that looks like any other Java program to the JVM.
In the meantime, there's plenty to do to fully realize the vision of
seamless, statically typed, purely functional data analytics in the
cloud. In particular, while in this post we focused on standalone
Haskell apps, it would be great if support for Haskell extended to
interactive _notebooks_ with mixed Java/R/Haskell as well.
