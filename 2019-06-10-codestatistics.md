---
title: "Code Line Patterns: <br/>Two-dimensional representations of Stackage and PyPi"
shortTitle: "Code Line Patterns"
author: "Simeon Carstens, Matthias Meschede"
tags: data-science
---

We all know that the code we write is not random, but follows patterns. Can we detect these patterns in a database of code? How do these patterns differ between programming languages? Can we somehow exploit these patterns? In this blog post, we try to explore these questions using databases of Haskell and Python source code.

## The data

Our datasets come from Haskell's and Python's associated package repositories: In the case of Haskell, we use a current snapshot of all packages on the [Stackage](http://www.stackage.org) server. For Python, we downloaded a random subset of approximately 2% of all packages on the [Python Package Package Index](http://www.pypi.org).
Based on our sample, we estimate the total size of all (compressed!) packages on PyPi to approximately 19 Gb, so this sampling allows us to load all of our Python data set in memory and keeps the size of our data set more or less comparable to the amount of Haskell code on Stackage.  
A subject related to our analysis, from which we will borrow techniques later on, is [natural language processing (NLP)](https://en.wikipedia.org/wiki/Natural_language_processing). NLP is concerned with patterns in *natural* human languages. In contrast, we are analyzing *constructed* languages.

Let's look at some characteristics of our data set and compared to a data set that is used for state-of-the-art NLP models, specifically the number of packages, total lines of code (LOC), per package lines of code, number of words, and the most common word:

|                        | Python corpus | Haskell corpus | English Wikipedia    |
| ---------------------- | ------------- | -------------- | -------------------- |
| **# of packages**      | 3414          | 2312           | 5,800,000 (articles) |
| **LOC**                | 6,048,755     | 3,862,107      | n/a                  |
| **avg. LOC / package** | 1772          | 1760           | n/a                  |
| **# of words**         | 36,577,867    | 23,174,821     | 2,500,000,000        |
| **most common word**   | "x" (6,7%)    | "NUL" (4,5%)   | "the"                |

Hold on. "NUL" is the most common word in Haskell stackage packages? Sad, but true (we checked).
If you want to know why, try a quick [search on Github](https://github.com/search?l=&p=2&q=NUL+extension%3Ahs+language%3AHaskell&ref=advsearch&type=Code&utf8=%E2%9C%93):
turns out people love to hard-code byte strings...
FYI: the next common Haskell word is "a".
It is also interesting to see that the average number of LOC is very, very similar in the Haskell and the Python data sets!
In state-of-the-art NLP, data sets are one to two orders of magnitude larger:
The model [BERT](https://arxiv.org/pdf/1810.04805.pdf), for example, is trained on the English Wikipedia (2,500M words) and the Google BooksCorpus (800M words), which is on the order of 3 billion words, whereas our datasets are on the order of 30 million words.
Working with these amounts of data requires hardware significantly more powerful than what our feeble Dell XPS 13 offers, on which the following analyses were run within a few minutes.
Based on our random sample of Python packages, we can estimate the number of words in all of PyPi to be about 1,800M - which is not too far from the size of the English Wikipedia.
With a total number of projects on PyPi of almost 181,000, the total number of LOC in PyPi is, based on our sample, approximately 314M.

Let's take a closer look, and filter the LOC in our data sets for some specific keywords:

If you write code with more than the most basic features, you usually need to import additional functionality from a standard library or 3rd-party code.
This means that you need to type something like `import [...]` or `from [...]`.
We thus expect `import` statements to be a common pattern in the source code data sets.
In Haskell, we imagine `language` pragmas to be another common pattern.

Let's consider import and language pragmas as 'boilerplate' code for a moment:
It is straightforward to determine a package's LOC fraction that corresponds to boilerplate.
This fraction is just the LOC with import and language pragma keywords divided by the number of all LOC.
The following histograms show the results:

![Histograms of fractions of LOC with boilerplate code for both the Python and Haskell data set](../img/posts/codestatistics_histogram_importfractions_both.png)

Haskell tends to have more `import` and `language` statements, our definition of boilerplate code, per package than Python, as indicated by the average percentage (dashed lines):
for Python, it's about 6%, while for Haskell it's about 9.5%.
Interestingly, in both languages, a few packages have a very high boilerplate fraction.
Those can be found from the 50% mark on but they are not visible in the figure because of their low package count.
In case of Python, such packages often are `setuptools` scripts, while for Haskell, they are module exports and setup files.

We can also ask which packages are imported most often.
Given a LOC with an `import` statement, it is straightforward to extract the name of the imported package.
For Python, we first look at basic `import [...]` statements:

![Most frequent Python packages imported via basic import [...] statements](../img/posts/codestatistics_py_basic_imports.png)

![Most frequent Python packages imported via from [...] import [...] statements](../img/posts/codestatistics_py_from_imports.png)


Few surprises for Python's basic `import`s - `os` and `sys` are the most frequently imported modules.
In fact, they make up 27% and 19% of all basic imports.
But things change dramatically when considering `from [...] import [...]` statements:


 40% of all `from [...] import [...]` statements import stuff from TensorFlow, a popular machine learning library. [TODO: so what's up with TensorFlow?]

Onwards to Haskell: here we find an unexpectedly high occurence of `prelude` and `network` imports:

<img title="Most frequently imported Haskell modules" src="../img/posts/codestatistics_hask_boilerplate.png" style="max-width: 100%;max-height: 100%;"/>


Imports from the `data` namespace make up 34% of all import statements, which matches our intuition that its contents are very frequentlu used.
We finally take a look at language pragmas and ask what the most frequently used ones are:

Perhaps unsurprisingly, the `OverloadedStrings` extensions leads the field:
40% of all Haskell packages in our data set use this extension.
Given the popularity of this extension, this makes a good case for it entering the Haskell standard.
Furthermore, it's surprising that `TypeFamilies` is the third most common language pragma.

## Visualizing more advanced LOC patterns

Let's dive into less obvious patterns in our data sets:
how about extracting patterns from the data sets without specifying a priori what keywords they correspond to?

Such code patterns provide insight into the coding habits of a community in a programming language ecosystem.
Beyond the scope of this post, they can be exploited to help programmers via bug detection, refactoring, intelligent code completion, or even to synthesize complete programs.

A pattern is characterized by a large number of similar LOCs.
While similarity between two LOC might be very easy to assess for us humans, we don't have time to print out 100,000 LOC on little snips of paper and sort them on piles according to their similarity.
So obviously, we want to use a computer for that.
Unfortunately, computers *a priori* don't know what similarity between two LOC means.
A first step to teaching it is to represent each LOC as a vector, which is a quantitative representation of what we think are the most important characteristics ("features") of a LOC.
In our case, we use a 500-dimensional feature vector, and each dimension encodes the occurence of one of the 500 most common data set words: it's either one (if the word is present in that LOC) or zero (if it is not).

We could now race off and look at the LOCs in the 500-dimensional feature space.
But how would we visualize such a high-dimensional space?
And do all of these dimensions really contain valuable information which sets apart one pattern from another?

It seems like a smart move to reduce the number of dimensions significantly while keeping as much information about the structure of the data as possible.
For the purpose of visualization, we would love to have a two-dimensional representation so that we can draw a map of points in which points with similar features are placed close to each other and form groups ("clusters").
To achieve that, we need a measure of "closeness", that is, a notion of distance.
On a map, this is easy; we simply take a ruler and measure the (Euclidean) distance between two points. 
But we require a distance not only on the map, but also in feature space:
after all, if two points close on the map represent similar LOC with similar features, they should also be similar (or "close") in feature space.
We choose to measure distance (or, equivalently, similarity) in feature space by the Hamming distance (see Appendix).

A popular technique to reduce the number of dimensions while trying to maintain the structure of the data or, equivalently, the distances between similar data points, is UMAP.
UMAP is a non-linear technique, which means it's well suited for data sets which can not easily be projected on flat manifolds. We first calculate the UMAP for both the Python and the Haskell data set and color points depending on whether the LOC they represent contain certain keywords:

<a href="../img/posts/codestatistics_py_umap_embedding_words_large.png">
<img title="UMAP embedding of the Python code data set" src="../img/posts/codestatistics_py_umap_embedding_words_smaller.png" style="max-width: 100%;max-height: 100%;"/>
</a>

We immediately notice the complex structure of the data set, which seems to consist of a large number of clusters comprising very similar LOC. We expect many LOCs containing the keywords we chose to be close together on the UMAP which is indeed what we observe, although they no not necessarily form connected clusters. Furthermore, one LOC could contain several of the keywords, which would not be visible in our representation.

To find out what kind of code other clusters represent, we perform clustering on the UMAP embedding. Starting with the Python data set, we annotate the twenty most dense and reasonably large clusters with the two words which co-occur most frequently in a cluster:

<a href="../img/posts/codestatistics_py_umap_embedding_clusters_large.png">
<img title="UMAP embedding of the Python code data set" src="../img/posts/codestatistics_py_umap_embedding_clusters_small.png" style="max-width: 100%;max-height: 100%;"/>
</a>

It is reassuring to find clusters with the most frequent words being `for` and `in`, which corresponds to Python `for`-loops, or `assertEqual` and `self`, which stems from the `unittest` framework. Some other clusters, though, do not remind us of common Python idioms. The most common words in cluster 17 are `xd` and `xc`. Looking up what kinds of LOC contain both of these words, we find that these occur very often in binary strings.

Performing the same analysis for the Haskell data set, we find clusters such as a very well-defined one containing often both `text` and `maybe` and clusters corresponding to popular imports:

<a href="../img/posts/codestatistics_hask_umap_embedding_clusters_large.png">
<img title="UMAP embedding of the Haskell code data set" src="../img/posts/codestatistics_hask_umap_embedding_clusters_small.png" style="max-width: 100%;max-height: 100%;"/>
</a>

Furthermore, the big blue cluster (#16) seems to contain mostly auto-generated code from the `amazonka` package, which implements communication with Amazon Web Service-compatible APIs.

## Conclusion

We found several interesting patterns in both Python and Haskell code - clusters of common language idioms, but also unexpected clusters stemming from byte strings.
With these results, we could now build a very basic code completion tool:
while you type, that tool would continuously check to which cluster the line you're typing most likely belongs to and suggest you words from that cluster.
An obvious limitation, though, is the complete absence of context-sensitivity, meaning that proposed words neither depend on the order of previous words in the same line nor on adjacent LOCs.

Other projects have taken the application of machine learning techniques far further, resulting in tools which can significantly facilitate programmers' lives.
[Kite](https://kite.com/) performs context-sensitive code completion for Python and is available as a plug-in for several popular IDEs.
The [Learning from Big Code website](http://http://learnbigcode.github.io/) lists several other interesting projects.
For example, the [Software Reliability Lab](http://www.sri.inf.ethz.ch/) at ETH Zurich developed [JSNice](http://jsnice.org/), a tool to automatically rename and deobfuscate JavaScript code and to infer types.
Finally, [Naturalize](http://groups.inf.ed.ac.uk/naturalize/) learns coding conventions from an existing codebase to improve consistency. 

And if you're eager to explore yourself:
the analysis in this blog post was performed in a strictly reproducible pipeline built using [Nix](https://nixos.org).
This means you can [download a single file](http://www.tweag.io/linkgoeshere) and then rerun our analysis by typing just one single line.
Reproducible pipelines for data science using Nix will be discussed in a forthcoming blog post - stay tuned!

## Appendix: data preprocessing and technical details

After downloading the source code in the form of archives, we unpack them and all source files within a given package are concatenated and written to one single big corpus file.
Some of the above analyses require us to know from which package a certain LOC originates.
We thus also write a single file for each package containing a concatenation of all source files of that package.
As a final step in the data preprocessing pipeline, we tokenize all LOC, meaning we replace all types of whitespace by a single space, retain only letters and convert upper case letters to lower case ones.

To perform a dimensionality reduction of our data sets, it is neccessary to create informative feature vectores from each line of code.
We use count vectorization as implemented in `scikit-learn` to turn each LOC in a binary vector, whose dimensions correspond to the 500 most frequent words in our Python / Haskell code corpus.
We don't care how often a word occurs in a LOC, only whether it's there (1) or not (0).
Furthermore, single-letter words are neglected.

Having built these feature vectors, we apply a popular dimensionality reduction techniques, [UMAP](https://arxiv.org/pdf/1802.03426.pdf).
UMAP is a manifold embedding technique, meaning that it tries to represent each data point in the high-dimensional feature space by a point on a lower-dimensional manifold in a way that similar points in the feature space lie close together on the lower-dimensional manifold.
UMAP requires a measure of similarity in feature space. 
There are many possibilities to construct such a similarity.
We choose to count the number of unequal entries of the two vectors - called the Hamming distance.
In other words, we count the number of times one of the 500 most frequent words appears in only one LOC, but not in the other.
A smaller Hamming distance indicates less differences and therefore that the LOC are more similar.

To assign points in the two-dimensional representations of our data sets to clusters, we use the [Python implementation](https://github.com/scikit-learn-contrib/hdbscan) of the recent clustering algorithm [HDBSCAN](https://link.springer.com/chapter/10.1007/978-3-642-37456-2_14) (paywalled).
A big advantage of HDBSCAN over many other clustering algorithms is that it automatically determines the number of clusters and allows to classify data points as noise.
