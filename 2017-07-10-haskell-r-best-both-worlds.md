---
title: R and Haskell:<br>best of both worlds with HaskellR
author: Sigrid Keydana
featured: yes
---

*A guest post by [Sigrid Keydana][sigrid-keydana] telling us the backstory behind the
very cool trading app notebook she put together... [Keras][keras], [ggplot2][ggplot2],
Haskell and R all in one Jupyter notebook! Post originally appeared
[here][original-post].*

[keras]: https://keras.io/
[ggplot2]: http://ggplot2.org/
[sigrid-keydana]: https://recurrentnull.wordpress.com/about/
[trading-notebook]: http://nbviewer.jupyter.org/github/skeydan/haskellR-intro/blob/master/stockmarket_demo.ipynb
[original-post]: https://recurrentnull.wordpress.com/2017/07/07/haskellr/

Earlier today, I presented at [UseR! 2017][user2017]
about [HaskellR][haskellr]: a great piece of software, developed by
Tweag I/O, that allows to seemlessly use R from Haskell.

[haskellr]: https://tweag.github.io/HaskellR/
[user2017]: https://user2017.brussels/

It was my first UseR!, it was a great experience, and if I had the
time I’d like to write a separate blog post about it, as there were
things that did not quite align with my prior expectations… Stuff for
thought, but not the topic of this post. (Mainly this would be about
how the academic talks compared to the non-academic ones.)

So, why HaskellR? If you allow me one personal note… For the
ex-psychologist, ex-software-developer, ex-database administrator, now
“in over my head” data scientist and machine learning/deep learning
person that I am (see [this post][doing-data-science] for that story),
there has always been some fixed point of interest (ideal, you could
say), and that is the elegance of functional programming. It all
started with [SICP][sicp], which I first read as a (Java) programmer
and recently read again (partly) when
preparing [R 4 hackers][r4hackers], a talk focused to a great part on
the functional programming features of R.

[sicp]: https://mitpress.mit.edu/sicp/
[r4hackers]: https://recurrentnull.wordpress.com/2017/03/20/r-4-hackers/
[doing-data-science]: https://recurrentnull.wordpress.com/2016/09/05/doing-data-science/

For a database administrator, unless you’re very lucky, it’s hard to
integrate use of a functional programming language into your work. How
about deep learning and/or data science? For deep learning, there’s
Chris Olah’s wonderful blog post linking deep networks to functional
programs, but the reality (of widely used frameworks) looks different:
TensorFlow, Keras, PyTorch… it’s mostly Python around there, and
whatever the niceties of Python (readability, list comprehensions…)
writing Python certainly does not feel like writing FP code at all
(much less than writing R!).

So in practice, the connections between data science/machine
learning/deep learning and functional programming are scarce. If you
look for connections, you will quickly stumble upon the Tweag I/O
guys’ work: They’ve not just created HaskellR, they’ve also made
Haskell run on Spark, thus enabling Haskell applications to use
Spark’s MLLib for large-scale machine learning.

What, then, is HaskellR? It’s a way to seemlessly mix R code and
Haskell code, with full interoperability in both directions. You can
do that in source files, of course, but you can also quickly play
around in the interpreter, appropriately called H (no, I was not
thinking of its addictive potential here ;-)), and even use Jupyter
notebook with HaskellR! In fact, that’s what I did in the demos.

If you’re interested in the technicalities of the implementation,
you’ll find that documented in great detail on the HaskellR website
(and even more, in their [IFL 2014 paper][ifl2014]), but otherwise
I suggest you take a look at the demos from my talk: First, there’s
a notebook showing how to use HaskellR, how to get values from Haskell
to R and vice versa, and then, there’s
the [trading app scenario notebook][trading-notebook]: Suppose you
have a trading app written in Haskell – it’s gotta be lightning fast
and as bug-free as possible, right? But, how about nice
visualizations, time series diagnostics, all kinds of sophisticated
statistical and machine learning algorithms… Chances are, someone’s
implemented that algorithm in R, already! Let’s take ARIMA – one line
of code with R.J. Hyndman’s auto.arima package! Visualization?
ggplot2, of course! And last not least, an easy way to do deep
learning with R’s keras package (interfacing to Python Keras).

[ifl2014]: https://ifl2014.github.io/submissions/ifl2014_submission_16.pdf
[haskellr-intro]: http://nbviewer.jupyter.org/github/skeydan/haskellR-intro/blob/master/haskellR_demo.ipynb

Besides the notebooks, you might also want to check out the [slides][slides],
especially if you’re an R user who hasn’t had much contact with
Haskell. Ever wondered why the pipe looks the way it looks, or what
the partial and compose functions are doing?

[slides]: http://rpubs.com/zkajdan/289817

Last not least, a thousand thanks to the guys over at Tweag I/O,
who’ve been incredibly helpful in getting the whole setup to run (the
best way to get it up and running on Fedora is using nix, which
I didn’t have any prior experience with… just at a second level of
parentheses, I think I’d like to know more about nix, the package
manager and the OS, now too ;-)). This is really the great thing about
open source, the cool stuff people build and how helpful they are! So
thanks again, guys – I hope to be doing things “at the interface” of
ML/DL and FP more often in the future!
