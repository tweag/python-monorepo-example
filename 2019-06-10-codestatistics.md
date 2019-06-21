---
title: "Source code headers: diverse repetition unveiled"
shortTitle: "Source code headers: diverse repetition unveiled"
author: "Simeon Carstens, Matthias Meschede"
tags: data-science
---

Given the fact that you stumbled on the blog of a software consulting company, chances are that you spend a significant amount of your time writing code or with people doing exactly that.
A part of this code is quite repetetive and usually found at the beginning of your source files:
`import` statements (or your language's equivalent of them) which add additional functionality from standard or third-party libraries.
If you're programming in Haskell, you probably also add several extensions to the Haskell language by using `LANGUAGE` pragmas.
You are not alone - everyone else is doing it, but... how often, really?
Which packages are imported most?
And are there differences between programming languages?
In this blog post, we explore these questions using data sets of Python and Haskell code.
With the results, we can learn about community-wide habits, hope to whet your appetite for further analyses of our data sets and finally realize that after all, repetitive does not necessarily mean uninteresting!

## A first look at the data

Our data sets come from Haskell's and Python's associated package repositories: 
In the case of Haskell, we use a current snapshot of all packages on the [Stackage](http://www.stackage.org) server. For Python, we downloaded a random subset of approximately 2% of all packages on the [Python Package Package Index](http://www.pypi.org).
Based on our sample, we estimate the total size of all (compressed!) packages on PyPi to approximately 19 Gb,
The quick runtime of a few minutes on a standard laptop and easy handling of the data are the reasons why we chose only a small sample from PyPi.
This sampling allows us to load all of our Python data set in memory and keeps the size of our data set more or less comparable to the amount of Haskell code on Stackage.

Let's first look at a few key characteristics of our data sets, namely the number of packages, total number of lines of code (LOC), LOC per package, number of words, and the most common word: 

|                        | Python        | Haskell        |
| ---------------------- | ------------- | -------------- |
| **# of packages**      | 3414          | 2312           |
| **LOC**                | 6,048,755     | 3,862,107      |
| **avg. LOC / package** | 1772          | 1760           |
| **# of words**         | 36,577,867    | 23,174,821     |
| **most common word**   | "x" (6,7%)    | "NUL" (4,5%)   |

Hold on. "NUL" is the most common word in Haskell stackage packages? Surprising, but true: `\NUL` is the quotation of the null character, and a small number of packages (2.7%) have inline bytestrings with many, many copies of `\NUL` in them.
FYI: the next common Haskell word is "a", which is a common type and term variable.
It is also interesting to see that the average number of lines of code is very, very similar in the Haskell and the Python data sets!
With a total number of projects on PyPi of almost 181,000, the total number of lines of code in PyPi is, based on our sample, approximately 314M.

## Import statements and language extensions - how many are there...

Now let's take a closer look and see what we can learn from this data.
As you might know, Python and Haskell have in common that files start with a list of import statement. In Haskell, file headers also contain a list of `LANGUAGE` pragmas, which add extensions to the language.
We thus expect `import` statements to be a common pattern in the source code data sets.
In Haskell, we imagine `LANGUAGE` pragmas to be another common pattern.

Let's find out whether there are any differences in the frequency of these patterns between Python and Haskell code.
We can easily determine a package's LOC fraction that corresponds to `import` statements and `LANGUAGE` pragmas:
This fraction is just the number of lines of code with import and language pragma keywords divided by the number of all lines of code.
The following histograms show the results:

<a href="../img/posts/codestatistics_swearwords.png">
<img title="Histograms of fractions of lines of code with import statements or LANGUAGE pragmas" src="../img/posts/codestatistics_histogram_importfractions_both.png" style="max-width: 100%;max-height: 100%;"/>
</a>

Haskell tends to have more `import` and `language` statements per package than Python, as indicated by the average percentage (dashed lines):
for Python, it's about 6%, while for Haskell it's about 9.5%.
Interestingly, in both languages, a few packages have a very high fraction of this specific kind of boilerplate code.
Those can be found from the 50% mark on but they are not visible in the figure because of their low package count.
In case of Python, such packages often are `setuptools` scripts, while for Haskell, they are module exports and setup files.

## ... and which are the most frequently used?

Answering this questions amounts to extracting the name of the imported package / the used `LANGUAGE` pragma for each line of code.
For Python, we first look at basic `import [...]` statements:

<img title="Most frequently imported Python packages" src="../img/posts/codestatistics_py_imports.png" style="max-width: 100%;max-height: 100%;"/>

Few surprises for Python's basic `import`s - `os` and `sys` are the most frequently imported modules.
In fact, they make up 27% and 19% of all basic imports.
But things change dramatically when considering `from [...] import [...]` statements:


40% of all `from [...] import [...]` statements import stuff from TensorFlow, a popular machine learning library.
We know that TensorFlow is popular, but *that* popular?
It turns out that our random sample of Python packages happens to contain a complete version of TensorFlow and that "self-imports" within that package account for 83% of all TensorFlow imports.
It is thus a single, big package which leads to this surprisingly high percentage of TensorFlow imports. 
Disregarding that package, around 2.5% of all `import` statements are concerned with TensorFlow, which would still crack the top 10.

Onwards to Haskell: here we find an unexpectedly high occurence of `prelude` and `network` imports:

<img title="Most frequently imported Haskell modules" src="../img/posts/codestatistics_hask_boilerplate.png" style="max-width: 100%;max-height: 100%;"/>


Imports from the `Data` namespace make up 34% of all import statements, which matches our intuition that its contents are very frequently used.

When considering the most frequently used language pragmas, perhaps unsurprisingly, the `OverloadedStrings` extensions leads the field: 
40% of all Haskell packages in our data set use this extension.
Given the popularity of this extension, this makes a good case for it entering the Haskell standard.
Furthermore, it's surprising that `TypeFamilies` is the third most common language pragma, given that type families are a fairly advanced subject and one would thus expect them not to be that commonly used.
We can also compare our results to a [previous analysis of Haskell source on Github](https://gist.github.com/atondwal/ee869b951b5cf9b6653f7deda0b7dbd8), which, too, finds that `OverloadedStrings` is the most popular extension. 
The ten most popular extensions listed in the figure above also feature in that analysis' list of the 20 most frequently used language extensions, although not necessarily in the same order.
The reason for that is not immediately clear - it might be that our Haskell data set simply is not representative of all Haskell code on Github; after all, at the time of writing, there are around 45,000 Haskell projects on Github, while our data set contains only 2,312 packages.

## Conclusion

In this blog post, we took a first look at our data sets and investigated `import` statements and `LANUAGE` pragmas in Python and Haskell.
While our data sets offer the potential for much deeper analysis (think: automatic code completion, refactoring, ...), already by just filtering the lines of codes for certain keywords we were able to answer interesting questions about the frequency with which these basic coding patterns occur and how their frequency differs between Python and Haskell code.
Thinking about this further, we might wonder what other patterns occur commonly in code.
A pattern would be some set of very similar lines of code, so we could expect, e.g., control structures such as `for` loops to form a pattern.
But are there maybe unknown patterns to be discovered we didn't think of?
How do we discover them in our data sets?
How do they differ between programming languages?
And can we somehow exploit these patterns?
We're thus excited to see what other insights these data have to offer - so stay tuned!