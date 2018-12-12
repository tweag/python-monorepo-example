---
title: "Towards Interactive Datascience in Haskell:<br/>Harnessing the Power of Jupyterlab"
shortTitle: "Harnessing the Power of Haskell and Jupyterlab"
author: Matthias Meschede
---

## Introduction ##

Haskell and data science - on first sight a great match: native function
composition, lazy execution, fast execution times, and lots of code checks.
These sound like ingredients for scalable, production-ready data transformation
pipelines. What is missing then? Why is Haskell not widely used in data
science?

One of the reasons is that Haskell lacks a standardized data analysis
environment.  For example, Python has a *de facto* standard library set with
`numpy`, `pandas` and `scikit-learn` that form the backbone, and many other
well-supported specialized libraries such as `keras` and `tensorflow` that are
easily accessible. These libraries are distributed with user friendly package
managers and explained in a plethora of tutorials, Stack Overflow questions and
millions of Jupyter notebooks. Most problems from beginner to advanced level
can be solved by adapting and combining these existing solutions.

This post shows how Jupyter, in particular with the Jupyterlab frontend, can
be used for interactive data analysis from Haskell as well.


## Jupyter and exploratory data analysis ##

For those who don't know it, Project Jupyter revolves around a messaging
protocol that standardizes interactions between Jupyter *frontends* and Jupyter
*kernels*. In a common use case, the kernel receives a code message from the
frontend, executes some computation, and responds with a rich media message.
The frontend can render such responses as text, images, videos or small
applications. Kernels for many different languages exist, like Python,
Haskell, R, C++, Julia, etc, and since the standardized message protocol is
language agnostic, frontends can communicate with any kernel.

The quick REPL-like interaction with a compute kernel is very useful for
exploratory data analysis that largely turns around a dialogue between the user
and the data. This dialogue is expressed as little code snippets and rich media
answers. Different algorithms (expressed as short code snippets) can rapidly be
tested and visualized. Longer dialogues with the kernel, with multiple steps,
can be assembled into a notebook. Since notebooks interlace explanatory text,
code and media elements, they can also be used as human-readable reports (such
as this blogpost). Given these characteristics, notebooks and their associated
workflows became one of the most popular ways to exploration data and
communicate comprehensive insights.

## Conversations with a Jupyter kernel ##

IHaskell is the name of the Jupyter kernel for Haskell. It contains a little
executable `ihaskell` that can receive messages in the Jupyter protocoll (via
ZeroMQ for those who know it), and responds with messages once executed. Here
is a little dialogue with `ihaskell`. Let's send the following code snippet
from the notebook frontend to `ihaskell`:


```haskell
take 10 $ (^2) <$> [1..]
```

And here is the answer that the frontend received from the kernel:

`[1,4,9,16,25,36,49,64,81,100]`

In Jupyter parlance, the above dialogue corresponds to an `execute_request`:

```
>> shell.execute_request (8be63d5c-1170-495d-82da-e56272052faf) <<

header: {username: "", version: "5.2",
         session: "32fe9cd0-8c37-450e-93c0-6fbd45bfdcd9",
         msg_id: "8be63d5c-1170-495d-82da-e56272052faf",
         msg_type: "execute_request"}
parent_header: Object
channel: "shell"
content: {silent: false, store_history: true, user_expressions: Object,
          allow_stdin: true, stop_on_error: true,
          code: "take 10 $ (^2) <$> [1..]"}   <<<<< LOOK HERE
metadata: Object
buffers: Array[0]
```

A `display_data` message is received as a response:

```
<< iopub.display_data (68cce1e7-4d60-4a20-a707-4bf352c4d8d2) >>

header: {username: "", msg_type: "display_data", version: "5.0"
         msg_id: "68cce1e7-4d60-4a20-a707-4bf352c4d8d2",
         session: "32fe9cd0-8c37-450e-93c0-6fbd45bfdcd9",
         date: "2018-08-02T08:14:10.245877Z"}
msg_id: "68cce1e7-4d60-4a20-a707-4bf352c4d8d2"
msg_type: "display_data"
parent_header: {username: "", msg_type: "execute_request", version: "5.0",
                msg_id: "8be63d5c-1170-495d-82da-e56272052faf",
                session: "32fe9cd0-8c37-450e-93c0-6fbd45bfdcd9"}
metadata: Object
content: {data: {text/plain: "[1,4,9,16,25,36,49,64,81,100]"},  <<<<< LOOK HERE
          metadata: {output_type: "display_data"}}
buffers: Array[0]
channel: "iopub"
```

`ihaskell` can import other Haskell libraries dynamically and has some special
commands to enable language extensions, print type information or to use
Hoogle.

## Jupyterlab ##

Jupyterlab is the newest animal in the Jupyter frontend zoo, and it is arguably
the most powerful: console, notebook, terminal, text-editor, or image viewer,
Jupyterlab integrates these data science building blocks into a single web
based user interface. Jupyterlab is a a modular system. New modules can be
added that change every functionality of the main application. The base modules
can be assembled in a variety of ways, resembling an IDE, a classical notebook
or even a GUI where all interactions with the underlying execution kernels are
hidden behind graphical elements.

How can Haskell can take advantage of these capacities of jupyterlab? What are
the potential gains? To begin with, jupyterlab provides plenty of
out-of-the-box renderers that could be used for free by Haskell. From the
[default renderers](https://jupyterlab.readthedocs.io/en/stable/user/file_formats.html),
the most interesting is probably Vega plotting (declarative `d3.js`). But also
geojson, plotly or and many others are available from the list of extensions,
that will certainly grow.

The second point is, that Jupyterlab is easily extensible. Extensions can use
and modify everything that comes with the base packages. Building simple UI's
that interact with an execution environment is therefore relatively easy.

The third point is that Jupyter and associated workflows are known by a large
community. Using Haskell through these familiar environments softens the
barrier that many encounter when starting to explore Haskell for serious data
science.

Let's get into a small example that shows how to use VEGA rendering from
Haskell in Jupyterlab.

## Setting up IHaskell ##

Unfortunately, setting up `IHaskell` correctly is not easy yet.
In order to make things easier, we created a repository [`ihaskell-setup`](https://github.com/tweag/ihaskell-setup) with instructions for its installation in regular Linux distributions, as well as NixOS.

The following steps suppose that the setup described on the repository is done.

## Wordclouds using Haskell, Vega and Jupyterlab ##

We will use here the word content of all blog posts of `tweag.io`, which are
written in markdown. Here is a little code cell that reads all `.md` files
the `posts` folder and concatenates them in a single long string from which we
remove some punctuation characters. The code cell is send to the `ihaskell`
kernel, which responds to the last `take` function with a simple text response.


```haskell
:ext QuasiQuotes
import System.Directory
import Data.List

fnames <- getDirectoryContents "../../posts"
paths = ("../../posts/"++) <$> fnames
md_files = filter (isSuffixOf ".md") paths
text <- mconcat (readFile <$> md_files)
cleanedText = filter (not . (`elem` "\n,.?!-:;\"\'")) text
take 50 cleanedText
```

```
"title Were hiring<br>(Software engineer / devops)p"
```


Now let's define a Vega JSON as a string and fill it up with our text. A
convenient way to write longer multiline strings in Haskell are QuasiQuotes. We
use `fString` QuasiQuotes from the `PyF` package. Note that `{}` fills in template
data and `{{` corresponds to an escaped `{`.


```haskell
import Formatting
import PyF
import Data.String.QQ

let vegaString = [fString|{{
  "$schema": "https://vega.github.io/schema/vega/v4.json",
  "width": 800,
  "height": 400,
  "padding": 0,

  "data": [
    {{
      "name": "table",
      "values": [
         "{take 20000 cleanedText}"
      ],
      "transform": [
        {{
          "type": "countpattern",
          "field": "data",
          "case": "upper",
          "pattern": "[\\\\w']{{3,}}",
          "stopwords": "(\\\\d+|youll|looking|like|youre|etc|yet|need|cant|ALSO|STILL|ISNT|Want|Lots|HTTP|HTTPS|i|me|my|myself|we|us|our|ours|ourselves|you|your|yours|yourself|yourselves|he|him|his|himself|she|her|hers|herself|it|its|itself|they|them|their|theirs|themselves|what|which|who|whom|whose|this|that|these|those|am|is|are|was|were|be|been|being|have|has|had|having|do|does|did|doing|will|would|should|can|could|ought|i'm|you're|he's|she's|it's|we're|they're|i've|you've|we've|they've|i'd|you'd|he'd|she'd|we'd|they'd|i'll|you'll|he'll|she'll|we'll|they'll|isn't|aren't|wasn't|weren't|hasn't|haven't|hadn't|doesn't|don't|didn't|won't|wouldn't|shan't|shouldn't|can't|cannot|couldn't|mustn't|let's|that's|who's|what's|here's|there's|when's|where's|why's|how's|a|an|the|and|but|if|or|because|as|until|while|of|at|by|for|with|about|against|between|into|through|during|before|after|above|below|to|from|up|upon|down|in|out|on|off|over|under|again|further|then|once|here|there|when|where|why|how|all|any|both|each|few|more|most|other|some|such|no|nor|not|only|own|same|so|than|too|very|say|says|said|shall)"
        }},
        {{
          "type": "formula", "as": "angle",
          "expr": "[0, 90][~~(random() * 3)]"
        }},
        {{
          "type": "formula", "as": "weight",
          "expr": "if(datum.text=='VEGA', 600, 300)"
        }}
      ]
    }}
  ],

  "scales": [
    {{
      "name": "color",
      "type": "ordinal",
      "range": ["#3e4593", "#bc3761", "#39163d", "#2a1337"]
    }}
  ],

  "marks": [
    {{
      "type": "text",
      "from": {{"data": "table"}},
      "encode": {{
        "enter": {{
          "text": {{"field": "text"}},
          "align": {{"value": "center"}},
          "baseline": {{"value": "alphabetic"}},
          "fill": {{"scale": "color", "field": "text"}}
        }},
        "update": {{
          "fillOpacity": {{"value": 1}}
        }},
        "hover": {{
          "fillOpacity": {{"value": 0.5}}
        }}
      }},
      "transform": [
        {{
          "type": "wordcloud",
          "size": [800, 400],
          "text": {{"field": "text"}},
          "rotate": {{"field": "datum.angle"}},
          "font": "Helvetica Neue, Arial",
          "fontSize": {{"field": "datum.count"}},
          "fontWeight": {{"field": "datum.weight"}},
          "fontSizeRange": [12, 56],
          "padding": 2
        }}
      ]
    }}
  ]
}}|]
```

We display this JSON string with the native Jupyterlab JSON renderer here for
convenience. The `Display` function explicitly tells `ihaskell` to send a display
message to the frontend. The json function tells `ihaskell` to annotate the
content of the display message as `application/json`.


```haskell
import qualified IHaskell.Display as D
D.Display [D.json vegaString]
```

![Vega Wordcloud](../img/posts/jupyterlab-json.png)


Finally, we can plot this JSON with Vega:


```haskell
D.Display [D.vegalite vegaString]
```

![Vega Wordcloud](../img/posts/jupyterlab-wordcloud.png)
