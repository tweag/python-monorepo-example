---
title: Construction and analysis of the build and runtime dependency graph of nixpkgs
author: Eloi Wang
tags: [internship, nix]
description: "A nixpkgs content database with graph building!"
---

During my internship under the mentorship of Guillaume Desforges at Tweag, I worked on creating and analyzing a graph of the contents of [Nixpkgs](https://github.com/NixOS/nixpkgs).
I received help and advice from many talented colleagues on Tweag's Nix Slack channel during this process.

[Nix](https://nixos.org/) is a build system and package manager with a focus on reproducible, declarative and reliable packages.
Nixpkgs is an enormous collection of software packages that can be installed with the [Nix](https://nixos.org/) package manager.
Due to the way Nix works, all packages must define precisely all of their dependencies (their dependency closure) down to the operating system's kernel.
This rigor-by-design with respect to dependencies is what makes Nix packages highly reproducible, and as a side effect, it gives us a fantastic dataset:
the full dependency network that the more than 80000 packages in this collection form.

The interdependence between software packages forms a very complex network.
Seemingly insignificant programs maintained by only a handful of people may be the pillar of applications we use every day.
Looking at the dependency graph of software allows us to identify such libraries or programs.
As a software collection that is also used to build a complete operating system called NixOS, Nixpkgs is not limited to an ecosystem and contains packages for many languages and programs.
Its dependency graph can help us understand the relationship between different software ecosystems and capture some macro features that they have.

Besides, if you plan to contribute to Nixpkgs, knowing some its basic characteristics in advance can help you get a feeling for how this intricate system works.

In contrast to an [earlier blog post](https://www.tweag.io/blog/2019-02-06-mapping-open-source/) that extracted the graph in an ad hoc way directly from Nix's database of packages, the main purpose of this project is to provide a command line tool that simplifies the extraction of derivations (Nix's name for packages) and their dependencies programmatically from Nixpkgs, and injecting them as nodes and edges in a graph database for further examination.

I thus sincerely encourage you to visit the [GitHub repository `tweag/nixpkgs-graph`](https://github.com/tweag/nixpkgs-graph/) after reading this post. Now let's get into it!

## How to get nodes and edges from Nixpkgs?

Before we start, let's see how Nixpkgs can be seen as a graph.

A graph in computer science is a structure made of nodes and edges, a set of node pairs.
A directed graph (digraph), is a graph in which the edges have a direction, from the first to the second node in an edge's node pair.

![Digraph](./DiGraph.png)

In the context of Nixpkgs, we can interpret nodes as derivations (packages) identified by their name, in the form of `<pname>-<version>`, where `<pname>` is a package's name and `<version>` its version.
In addition, given two derivations A and B, an edge from A to B can be understood as a dependency of A on B, meaning B is in either a build-time or runtime dependency of A [^In Nix derivations, dependencies are specified in the `buildInputs` and `propagatedBuildInputs` attributes].
For example, in this interpretation, the Nix derivation `chromium` has four dependencies, one of them being `glib`.

### How to get a list of all derivations in Nixpkgs?

One of the easiest and most direct ways to get the list of all derivations under Nixpkgs is to use the [nix search](https://nixos.org/manual/nix/unstable/command-ref/new-cli/nix3-search.html) command:

```shell
$ nix search --json nixpkgs
```

The json output is a list of all derivations in Nixpkgs containing each package's full path in the Nixpkgs collection, it's `pname` (package name), `version`, and `description` attributes:

```json
"legacyPackages.aarch64-darwin.zzuf":{"pname":"zzuf","version":"0.15","description":"Transparent application input fuzzer"}
```

This is the list of _nodes_, with names and various properties, in the graph that we want to build.

### How to list the dependencies of a derivation?

To find the dependency relationships between those nodes, we need to get the dependencies for all derivations from Nixpkgs.
To do this, we can go through the full tree of Nixpkgs paths (similar to `legacyPackages.aarch64-darwin.zzuf`), access each of the derivations and extract their dependencies programmatically.
In short, we need to map over Nixpkgs.

#### Mapping over the full Nixpkgs attribute set

The command `nix search` is a good start for an initial look at the content of Nixpkgs，but it is not enough for what we need because it doesn't output dependencies.
To go further, we will use the Nix language to inspect Nixpkgs, instead of using Nix's command-line tools. This will give us more liberty.
The key is to correctly understand the structure of Nixpkgs, then obtain the dependencies of derivations under Nixpkgs without building them.
Because the build of derivations is quite time consuming and we just need to evaluate them.

In Nix, the data type of Nixpkgs is [attribute set](https://nixos.org/manual/nix/unstable/language/values.html?highlight=attribute%20set#attribute-set) (similar to the notion of a dict in Python).
And the Nix language contains two special functions: the Nix builtin function [`mapAttrs`](https://nixos.org/manual/nix/stable/expressions/builtins.html#builtins-mapAttrs) and the Nixpkgs function [`concatMapStrings`](http://ryantm.github.io/nixpkgs/functions/library/strings/).

With the help of the above two tools, we can iterate through each package in Nixpkgs, get its basic information and dependencies directly and integrate the results into the output.
To avoid the time-consuming problem of build, we will use [`nix-instantiate`](https://nixos.org/manual/nix/unstable/command-ref/nix-instantiate.html) instead of `nix-build`.
The `--eval` flag will allow the `nix-instantiate` command to evaluate Nix expressions without instantiations of store derivations taking place.
And this is just what we need.

Then, for each attribute in Nixpkgs, we first check whether it is a derivation using the Nixpkgs library function [`isDerivation`](http://ryantm.github.io/nixpkgs/functions/library/attrsets/#function-library-lib.attrsets.isDerivation).
If so we extract the information in it with [`tryEval`](https://nixos.org/manual/nix/unstable/language/builtins.html?highlight=tryEval#built-in-functions).
We use `tryEval` because not all derivations can be evaluated.
`tryEval` prevents the program from stopping because of the evaluation failure of some derivations.
Otherwise, we check if it can recurse (this means that it is not a derivation but a attribute set that contains derivations, and we need to re-apply the extraction function on this set) or is in the whitelist. If so, we recurse, else we stop.
For determining whether or not to recurse, we can rely on [`recurseForDerivations`](https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/release-lib.nix#L151) and [`recurseForRelease`](https://github.com/NixOS/nixpkgs/blob/master/pkgs/top-level/release-lib.nix#L151) attributes.
In particular, there are important sets of derivations that are not derivations while their both recurse attributes are false, such as `python3Packages`.
Therefore, a whitelist is added for these sets.

#### Inconsistent result hierarchy due to nested structure

With the above steps we get the `pname`, `version` and dependencies of the derivations in Nixpkgs in JSON format.
We get something like `{n1, n2, {n3, {n4}}}`, while what we hope to get is `{n1, n2, n3, n4}`.
All derivations should be in the same level of the JSON file to be readable to other software.

Once again, Nix provides the ace we need: [`lib.collect`](https://teu5us.github.io/nix-lib.html#lib.attrsets.collect).
Using `collect`, we can both flatten a nested structure and select which elements to take.
In order to properly filter the packages, we need the previous mapping step to flag the packages that we have evaluated.
This can be done for instance by adding a `type` attribute with value `node`, and filtering with a function `selectNodes = x: (x.type or null) == "node"`.
Then the function `collectNodes = pkgs.lib.collect selectNodes` will give us all the previously evaluated packages as a flat list.

In the end, we get the result in the following format:

```json
...
{
  "buildInputs": "/nix/store/c1pzk30ksbff1x3krxnqzrzzfjazsy3l-gsettings-desktop-schemas-42.0 /nix/store/mmwc0xqwxz2s4j35w7wd329hajzfy2f1-glib-2.72.3-dev /nix/store/64mp60apx1klb14l0205562qsk1nlk39-gtk+3-3.24.34-dev /nix/store/6hdwxlycxjgh8y55gb77i8yqglmfaxkp-adwaita-icon-theme-42.0 ",
  "id": "chromium-103.0.5060.134",
  "package": [
    "nixpkgs",
    "chromium"
  ],
  "pname": "chromium",
  "propagatedBuildInputs":"",
  "type":"node",
  "version": "103.0.5060.134"
}
...
```

## Log the graph to NetworkX

For the graph generation and processing, this project uses the [NetworkX](https://networkx.org/) Python package.
NetworkX is a powerful Python package for the creation, manipulation, and study of complex networks.
It also has output functions for multiple formats (`.csv`, `.gexf`, etc.), which is very helpful for the subsequent analysts of the graph.

Based on the data in json format obtained in the previous section, the generation of the graph consists of the following main steps:

- Read data and pre-process
- Create a new graph in `networkx.DiGraph` format and add nodes and edges to it
- Complete data
- Output data

Preprocessing consists mainly of reading data using [pandas](https://pandas.pydata.org/) and cutting `buildInputs` and `propagatedBuildInputs` from one single string to a list.
And each item in this list contains only the required `id` part.
In addition, depending on the package set the node belongs to (e.g. `pythonPackages`), we add a `group` attribute to it.

Nodes and edges can be added using NetworkX built-in functions.
In particular, NetworkX allows us to add various labels to nodes and edges (e.g. a node can contain its `id`, `pname`, `version`, `group`; an edge can contain the category it belongs to).

Specifically, since not all packages in Nixpkgs can be evaluated, there are some edges involved in nodes that are not evaluated.
In turn, in NetworkX's database, these nodes only have `id`.
So additional `group` attributes need to be added for them.
Here, the group attribute of all nodes that cannot be evaluated is set to `"nixpkgs"`.

Finally, we can first export the data in CSV format using pandas.
Besides, we can use NetworkX's built-in functions output the graph in PNG format, which can allow us to have a general idea of the graph.
However, if we want to go deeper into the visualization features, NetworkX also allows us to export the graph in GEXF format and then we can process it with Gephi as well as in GraphML format which could be treated by Neo4j.

## Analyze the relationships in Nixpkgs

Now let's make use of this data.

The command line interface of this project allows the user to customize the version of Nixpkgs to be used.
Simply provide the full 40-character SHA-1 hash of a commit and the SHA256 of its tree.
The commit of Nixpkgs used in this blog is `481f9b246d200205d8bafab48f3bd1aeb62d775b`.

### Some basic information

The final directed graph consists of 64205 nodes and 217579 edges.
Among them, the top 3 packages with the most direct dependencies are: `pleroma-2.4.3`: 124, `azure-cli-2.34.1`: 117, `libreoffice-7.3.3.2`: 94.
And the most cited 3 nodes are: `python3-3.10.6`: 7697, `texinfo-6.8`: 5626, `emacs-28.1`: 5553.
On average, a node has 3.35 direct dependencies.
And the longest chain of dependencies in Nixpkgs consists of 41 nodes.

### Use Gephi for visualization

Using the GEXF format file provided by default, we can draw the following image with Gephi.
As shown below, we set the color of the nodes according to the group they belong to.
And the size of each node is nonlinearly and positively related to its [in degree](https://en.wikipedia.org/wiki/Directed_graph) (the number of edges coming into a node in a directed graph).

For the layout of the graph, the [ForceAtlas2](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0098679) algorithm is used here.
It is a force simulation algorithm that contains both gravitational and repulsive forces.
The attractive forces pull the nodes toward their dependencies while the repulsive forces push high-degree nodes away from the nodes around them.
Thus we can see that packages belonging to the same ecosystem are clustered together because they have similar dependencies.
The nodes with high degree form a blank area around them.
In addition, just like the celestial bodies, there is a gravitational force that makes all the nodes clump together to form a circle.

![Gephi0](./Gephi_00.png)

### Cycles in the graph

When I first designed the algorithm to calculate the longest chains in Nixpkgs, the algorithm always failed to run.
After some analysis, I found that there are some simple cycles in Nixpkgs.
Some cycles are of length 1, which means that some derivations have `buildInputs` or `propagatedBuildInputs` that contain themselves.
There are also some cycles of length 2 or 3. There are six cycles in total:

```python
['chicken-5.3.0']
['chicken-4.13.0']
['mlton-20180207']
['gvfs-1.50.2', 'libgdata-0.18.1', 'gnome-online-accounts-3.44.0']
['gvfs-1.50.2', 'gnome-online-accounts-3.44.0']
['pipewire-0.3.51', 'ffmpeg-4.4.2', 'SDL2-2.0.20']
```

Specifically, to confirm if the error occurred when fetching the Nixpkgs data, I accessed the raw Nixpkgs data:

```nix
nix show-derivation nixpkgs#chicken
```

In the results given by Nix there is the following information:

```json
"/nix/store/1qlyycams6q39ll5r4p1sq57gcvhvgmn-chicken-5.3.0.drv": {
    ...
    "env": {
      ...
      "buildInputs": "/nix/store/c4ha2dqj3a1jp2dn962wdfq5wqy0gikv-chicken-5.3.0",
      ...
    }
    ...
}
```

This means that cycles do exist in the raw data of Nixpkgs.

How is this possible?
If a derivation's dependencies contained itself, then an infinite loop would occur during the build.
That is, building `A` would require that the environment had already `A`. Of course this is not possible.

When checking the full `hash-pname-version` entry in the store, we can see it's not actually the same package.
See example above for `chicken`, it is what we call [bootstrapping](<https://en.wikipedia.org/wiki/Bootstrapping_(compilers)>).
Nix will first build an initial version of `chicken` without `chicken` in its dependencies.
Then use this initial version to install the final `chicken`.
This is why we can see two `chicken` with different hashes.
But since our identifiers have the form `<pname>-<version>`, they are identified as one node, thus forming a cycle.

### Query the graph with [Neo4j](https://neo4j.com/)

Gephi provides us with a nice visualization, but sometimes you may need some precise queries.
For this reason, we also provide the appropriate solution.

Neo4j is a tool to manipulate graphs with additional information, such as node and edge labels and properties.
More importantly, it allows querying these graphs through a query language called Cypher.
It is possible to query the graph by using three main keywords: `MATCH`, `WHERE`, `RETURN`.
The first keyword allows to match some nodes and edges following their types and their edges, the second one allows to check data property and the last one allows to return some result.

First let's see how to export data from NetworkX to Neo4j.
The [import file format](https://neo4j.com/docs/operations-manual/current/tools/neo4j-admin/neo4j-admin-import/) supported by Neo4j is mainly CSV, which needs to read nodes and edges line by line.
But we have a simpler solution to make: let NetworkX output the graph in GraphML format and then install the [APOC](https://neo4j.com/labs/apoc/4.4/installation/) plugin for Neo4j to read it.
This plugin can be installed on both the desktop and server versions of Neo4j and is very easy to use.
If you happen to need to transfer data between networkx and Neo4j, you can also refer to this method.

Now we can start playing with some commands to demonstrate the benefits of Neo4j.
For example if we want to know who the Python ecosystem directly relies on most, we can do this.

```cypher
MATCH (n)-[e]->(m)
WHERE n.group STARTS WITH 'python' AND NOT (m.group STARTS WITH 'python')
RETURN DISTINCT m.group AS group, COUNT(e) AS times
ORDER BY times DESC
LIMIT 10
+-------------------------------+
| group                 | times |
+-------------------------------+
| "nixpkgs"             | 8966  |
| "gnuradio3_8Packages" | 5409  |
| "xorg"                | 86    |
| "libsForQt5"          | 41    |
| "gst_all_1"           | 18    |
| "gnome2"              | 9     |
| "driversi686Linux"    | 6     |
| "gnome"               | 5     |
| "haskellPackages"     | 4     |
| "llvmPackages"        | 3     |
+-------------------------------+
```

From the results we can see that Python ecosystem relies mainly on some separate derivations (`nixpkgs` is the group for separate software).
This indicates that Python may rely on many separate programs or libraries, as we mentioned at the very beginning.
Secondly Python mainly references `Gnuradio` (a software development toolkit that provides signal processing blocks to implement software radios).
This indicates that Python is widely used for signal processing applications.

To take this a step further, we can compare the differences between Python 3.9 and Python 3.10 replacing the 'python' string by 'python39' and 'python310' in the previous query.
The results show that Python's references to `Gnuradio` are mainly a Python 3.10 thing.

The above is just a preliminary use of Neo4j for the Nixpkgs graph.
But with this example we can see that the Nixpkgs graph is able to show some macro features of the software world that is invisible when we're just in parts of it.

## Conclusion

The graph of Nixpkgs on the one hand allows us to visualize through Gephi and thus show the interactions between different software ecosystems. On the other hand, it allows us to perform precise queries through Neo4j.
In addition, with the help of Python's modules NetworkX and Pandas, we can obtain a lot of quantitative results, such as the average dependency of a software of 3.35.
This project, [tweag/nixpkgs-graph/](https://github.com/tweag/nixpkgs-graph/), provides users with raw materials and some tools that they can explore according to their needs to explore.

In closing, I would like to thank Tweag for giving me this internship opportunity.
The value of what I have learned here far outweighs the salary.
I would also like to thank all the Tweagers, including my mentor Mr. Guillaume Desforges, who have helped me in this internship program.
I hope to have the opportunity to work with you again.
