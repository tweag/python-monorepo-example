---
title: "Funflow Example:<br/>Our `makefile-tool`"
shortTitle: Funflow Example
author: Divesh Otwani, Nicholas Clarke
---

Since [funflow](https://github.com/tweag/funflow) is a bit abstract, 
in this post we look at a neat little example to illustrate some of funflow's 
features. This post builds off of our 
[earlier post](https://www.tweag.io/posts/2018-04-25-funflow.html)
in which we introduced funflow and what it had to offer.
All the code for this example as well as some documentation
can be found [here](https://github.com/tweag/funflow/tree/master/funflow-examples/makefile-tool).



## What did we build?
Our example is a simple version of GNU's Make
restricted to building C files (though it could be generalized).
It takes a makefile that looks like this

```makefile
source-files: main.cpp factorial.cpp hello.cpp functions.h

hello: main.o factorial.o hello.o functions.h
    g++ main.o factorial.o hello.o -o hello

main.o: main.cpp functions.h
    g++ -c main.cpp

factorial.o: factorial.cpp functions.h
    g++ -c factorial.cpp

hello.o: hello.cpp functions.h
    g++ -c hello.cpp
```

and can build the `hello` executable:

```bash
$ ls
factorial.cpp  functions.h  hello.cpp  main.cpp  makefile
$ stack exec makefile-tool
$ ./hello
Hello World!
The factorial of 5 is 120
```

Because we used funflow, our tool has several desireable properties,
_both of the tool and of the code itself_:

 * **No repeated builds:** If we've built something before, we don't build it again. Period. 
   With `make`, if you make a tiny change and find out it isn't what you want, when you revert
   back `make` will rebuild something it had built before. Our tool won't.
 * **Enforced target dependencies:** We build each target file in a docker container
   with exactly the dependencies listed in the `Makefile`. There's no opportunity 
   for hidden dependencies or hidden requirements on the environment. Further, 
   such 'hidden' preconditions are caught early and flagged making it easy to fix
   the `Makefile`.
 * **Clean Sequencing Code:** The function that makes a target file sequences
   file processing, recusive calls for making the dependencies of the given target,
   and running docker containers. Usually, this sequencing is messy
   and difficult to follow.
   With funflow, however, we can inject these various forms of
   computation into `Flow`s and sequence them seamlessly with arrow notation.
 * **Concise Code:** Because of the abstractions funflow provides, we can focus on
   the essential algorithm and get some efficiency \& safety for free.


## No Repeat Builds: CAS Gives Us Free Dynamic Programming

The essential function is `buildTarget` which, given a `Makefile`
and a specific rule in that `Makefile`, creates a flow to build the
target specified by that rule. A rule specifies a target file by
a list of file dependencies and a command to build that target file.
Each dependency is either a source file or itself a target file.

Here is a slightly simplified version of
that function:


```haskell
-- For readability, we introduce a type alias for Flow
type a ==> b = Flow a b

buildTarget :: MakeFile -> MakeRule -> (() ==> (Content File))
buildTarget mkfile target@(MakeRule targetNm deps cmd) = 
  let
    srcfiles = sourceFiles mkfile
    neededTargets = Set.toList $ deps `Set.difference` srcfiles
    neededSources  = Set.toList $ deps `Set.intersection` srcfiles
    depRules = (findRules mkfile neededTargets :: [MakeRule])
    depTargetFlows = map (buildTarget mkfile) depRules
    countDepFlows = length depTargetFlows
    grabSources srcs = traverse (readFile . ("./" ++)) srcs
    grabSrcsFlow = stepIO grabSources   
   in proc _ -> do
     -- 1) Get source file contents
     contentSrcFiles <- grabSrcsFlow -< neededSources
     -- 2) Zip these with the names of each source file
     let fullSrcFiles = Map.fromList $ zip neededSources contentSrcFiles
     -- 3) Recursively build all dependent targets
     depFiles <- flowJoin depTargetFlows -< (replicate countDepFlows ())
     -- 4) Compile this file given
       -- a) the name of the target
       -- b) the dependencies that are source files 
       -- c) the dependencies that were targets
       -- d) the gcc command to execute
     compiledFile <- compileFile -< (targetNm, fullSrcFiles, depFiles,cmd)
returnA -< compiledFile
```

The code for building the flow is straightforward.

First, we do some file processing to get the dependent _source files_
(in steps 1 & 2).
Then, we recursively call `buildTarget` to create flows for each of the
dependent _target files_ and then run those flows to build the
dependent targets (in step 3). Finally, we put these dependencies
in a docker container in which we run the gcc command and extract the
produced target file (provided the compilation succeeded).

On the surface, this code appears inefficient.
Step 3, the recursive building of dependent target files, looks like it repeats
a lot of work since many of the recursive calls are identical.
For example, if the rule for `main.o` in our sample `Makefile`
listed `factorial.o` as a dependency, it looks like this code
would build `factorial.o` twice -once as a dependency of `hello` and once as
a dependency of `main.o`. If `factorial.o` took a long time to build,
this would be disastrous.

Yet, this code isn't inefficient? Why?
**This problem of 'overlapping' recursive calls is solved by _dynamic programming_,
the algorithmic design pattern of saving the values of function calls in a dictionary and
checking this dictionary to avoid repeated recursive calls.
Funflow's caching gives this to us for free.** Hence, we can write
DP-algorithms without dealing with the added complexity
of keeping track of the dictionary ourselves. This is precisely what happens in
`buildTarget`.

Each time a file is compiled with `compileFile`,
a hash of the inputs and output is cached. So, on a repeated
recursive call building say, `target4.o`, the use of `compiledFile`
is constant time.

However, this goes a step further.
Because of funflow's caching our tool, unlike GNU's `make`, doesn't re-build
targets _even after reverting back changes_.
Say `factorial.cpp` took a long time to build and was part of
a larger project. Suppose further that to fix a bug in this large project,
we tried something out in `factorial.cpp` and found out it didn't work.
When we revert back, our tool would build `factorial.cpp` in constant time
whereas plain ol' `make` would not.

<!--- Should I have an example? --->

## Enforced Dependencies

The `makefile-tool` also showcases funflow's docker capabilities.
We compile each target in its own docker container with exactly its
list of dependencies.

**This prevents hidden preconditions.** If some rule fails to mention a source file
it needs, the docker container in which the rule's target file is being compiled won't
have that missing source file. Hence, if a file dependency is missing from the dependency
list of a certain target, that target won't build. In the same vein, if there was a hidden
requirement on the environment, like a custom `gcc` executable, the build would fail
inside the docker container.

**Further, this approach doesn't merely prevent hidden preconditions;
it flags them.** For example, if our `Makefile` above had
the line `factorial.o : factorial.cpp` 
instead of `factorial.o : factorial.cpp functions.h`, we would get
an error from `gcc` indicating that it couldn't find `functions.h`. With
ordinary `make`, the build would succeed and this hidden precondition would last.
Within the scope of a large project, this could be maddening.

This dependency enforcement could work for more than just C projects.
As suggested earlier, the `makefile-tool` can be generalized. We could
extend this tool by having the user provide a docker container for the build command
and a way of directing the naming of the target file. (For C projects,
this would be the `gcc` container with the `-o` command line argument.)


## Clean Sequencing: Diverse Injection \& Arrow Notation

This example also demonstrates funflow's ability to inject various forms of
computation into `Flow`s and sequence them in fancy ways. This
makes the code readable and maintainable.

### Injecting IO and External Steps

At one point, we needed to inject the `IO` action 
```haskell
parseRelFile :: String -> IO (Path Rel File)
```
and could easily do so:
```haskell
flowStringToRelFile :: Flow String (Path Rel File)
flowStringToRelFile = stepIO parseRelFile
```

Then, inside the `compileFile` flow, we were able to sequence this
with `dockerFlow`, a flow that was made from an external step.
That is, `dockerFlow` was made from the funflow library function `docker`:

```haskell
import qualified Control.Funflow.External.Docker as Docker
import qualified Control.Funflow.ContentStore as CS

docker :: (a -> Docker.Config) -> Flow a CS.Item

dockerConfFn :: (Content Dir, Content File) -> Docker.Config

dockerFlow = docker dockerConfFn


data Config = Config
  { image      :: T.Text        --  e.g., "gcc", "ubuntu", "python".
  , optImageID :: Maybe T.Text
  , input      :: Bind          --  Files in the content store & locations to
                                --  put them in the docker container. This
                                --  includes a bash script to execute.
  , command    :: FilePath      --  A path to the bash script.
  , args       :: [T.Text]      --  Command line arguments to 'command'.
} deriving Generic

```

**Then by using [arrow notation](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial), 
a generalization of 'do notation', we were able to elegantly and easily 
sequence these `Flow`s. Without going into the details, we can see the 
sequencing is as readable and intuitive as 'do notation'**:


```haskell
compiledFile <- dockerFlow -< (inputDir,compileScript)
relpathCompiledFile <- flowStringToRelFile -< tf
returnA -< (compiledFile :</> relpathCompiledFile)
```

Further, these properties persist when we need more
sophisticated forms of sequencing.

### Recursive Calls & Linearly Sequencing Similar Flows

The `buildTarget` function calls itself and recursively builds
a list of `Flow`s for the other targets:

```haskell
depTargetFlows :: [Flow () (Content File)]
depTargetFlows = map (buildTarget mkfile) depRules
```
where
```haskell
buildTarget :: MakeFile -> MakeRule -> (Flow () (Content File))
mkfile :: MakeFile
depRules :: [MakeRule]
```

Now, to actually use these we need to linearly sequence these `Flow`s.
(We can't sequence them in parallel because then repeated recursive calls
would repeat work if flows were distributed.) 
In other words, we need the power to combine an arbitrary amount of
similar flows into one flow. Because funflow is embedded in haskell,
we can write the function `flowJoin`:


```haskell
flowJoin :: [Flow a b] -> Flow [a] [b]
flowJoin [] = step (\_ -> [])          -- step :: (a -> b) -> Flow a b
flowJoin (f:fs) = proc (a:as) -> do
  b <- f -< a
  bs <- flowJoin fs -< as
  returnA -< (b:bs)

joinedDepTargets :: Flow [()] [Content File]
joinedDepTargets = flowJoin depTargetFlows
```

A careful programmer might note that the base case is ill defined.
Indeed, the _right_ approach is to use length indexed vectors and have

```haskell
safeFlowJoin :: forall (n :: Nat) a b. Flow a b -> Flow (Vec n a) (Vec n b)
```

but even without this, **`flowJoin` is a clean, simple, and powerful way of sequencing
`Flow`s.**

## Summary

The `makefile-tool` is only 266 lines long _of which 180 pertain to
following a Makefile_ and yet,

- the CAS gives us free dynamic programming,
- we never re-build targets even when reverting changes
  (which is an improvement over `make`), and,
- the dependencies are checked and made explicit which
  lends to a crisp functional model.


