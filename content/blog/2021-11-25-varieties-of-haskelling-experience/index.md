---
title: The Varieties of the Haskelling Experience
author: Noon van der Silk
tags: [haskell]
description: "A collection of ways various Tweagers develop in Haskell"
---

Recently, a group of Haskellers within Tweag had a knowledge-sharing event
where we shared our varied Haskell setups with each other, and learned some
nice tricks and tips for every-day life.

The idea was raised of sharing these findings via the blog, so without further
ado, let's explore the varieties of the Tweag Haskelling experience by
covering a few of our personal setups!

### Nicolas Frisby

- **Editor**: emacs
- **Other main tools**: grep, cabal,
  inotify/[entr](https://github.com/eradman/entr) around `cabal build all`, my own prototype tags generator.
- **Explanation**: barebones emacs = syntax highlighting + basic Haskell mode for minimal indentation suggestions + a hack of `gtags.el` for querying my Tweag tags db
- **I love**: fast start-up, tags for instance declarations, tags only require parse not typecheck, never hangs, never crashes, all state is persistent, independence of all the tools
- **Could be better**: my tags prototype only handles `cabal.project`, just one at a time, not the `source-repository-package` entries (a cabal limitation, but fix on its way), and I still use grep for finding use sites, no Haddocks integration
- **Trivia**: aliases of any kind worry me because I won't be able to work in a fresh/foreign machine/login; these two are my only exceptions

  - `git config --global alias.lg log --first-parent --color --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit`
  - `git config --global alias.lgtree log --graph --color --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit`

### Richard Eisenberg

- **Editor**: Visual Studio Code
- **Other main tools**: [haskell-language-server](https://github.com/haskell/haskell-language-server), cabal, [cabal-env](https://github.com/phadej/cabal-extras), ghci, emacs's `smerge-mode`.
- **Explanation**: A recent convert from emacs, I'm enjoying the easier discoverability of commands in VSCode -- and the power of the Haskell Language Server.
- **I love**: Not worrying about directories when finding files in VSCode
- **Could be better**: The switch has made me realize I need to upgrade my 5-year-old laptop; VSCode's merge conflict resolution is not as good as `smerge-mode`.
- **Trivia**: `alias gitlog='git log --graph --full-history --all --color --pretty=format:"%x1b[31m%h%x09%x1b[32m%d%x1b[0m%x20%s"'`. Try it. Be happy.

### Facundo Domínguez

- **Editor**: Vim
- **Other main tools**: bash, [hasktags](https://hackage.haskell.org/package/hasktags), grep, [hoogle](https://hoogle.haskell.org/), [stackage.org](https://www.stackage.org/).
- **Explanation**: Every project has its own ways of building, and in my experience, figuring out IDE configuration for each one takes so long that I don't feel it helps me read or write code fast enough to deserve it. Vim is almost always already installed when I approach a new development environment.
- **I love**: I can focus on the task to solve as fast as possible.
- **Could be better**: hasktags sometimes sends you to the wrong definition. A more sophisticated indexing solution could give more precision, without necessarily complicating the setup.
- **Trivia**: I rely a lot on the undo history and the language-agnostic autocompletion that vim provides.

### Thomas Bagrel

- **Editor**: VS Code + Haskell extension pack (HLS, Hlint, formatter using Ormolu)
- **Other main tools**: nix, hoogle, stackage.org
- **Explanation**: I'm still a beginner with haskell, and I usually rely a lot on IDE features (especially autocompletion, real-time type inference, documentation at call site) to learn a new programming language. However, this strategy doesn't work that well with haskell (for various reasons I could detail if someone is interested). Anyway, VS Code with haskell extensions gives the closest experience to what I'm used to have. Previously, I extensively used JetBrains IDEs (for Java, Scala, Rust, JavaScript and Python dev).
- **I love**: the IDE is decently fast (compared to JetBrains IDEs for example)
- **Could be better**:
  - I still don't know how to navigate in the code quickly with VSC,
  - I still need to learn the keyboard shortcuts,
  - Refactoring capabilities and "find references" actions are nowhere near as
    good as IntelliJ for Rust dev for example,
  - Documentation popup (at call site) works well most of the time, but I
    can't jump to the documentation of a type itself in some function signature
    for example,
  - I can't jump to the definition of something defined in a library.
- **Conclusion**: I really like large (and slow) JetBrains IDEs, because I can often access everything from the IDE (I don't need to fetch documentation online most of the time, ease-of-discoverability is great, and most importantly, muscle memory is here). But because their Haskell support is not really good at the time (but maybe I need to try harder, and tweak/configure the haskell plugin better), I fallback to VSCode. I still feel quite lost to be honest.

### Clément Hurlin

- **Editor**: NeoVim
- **Other main tools**:
  - Plugins:
    - [fzf](https://github.com/junegunn/fzf) for fuzzing finding things (files, symbols),
    - [coc.nvim](https://github.com/neoclide/coc.nvim) as the LSP server,
      that delegated to haskell language server under the hood,
    - [neoformat](https://github.com/sbdchd/neoformat) for formatting with [ormolu](https://github.com/tweag/ormolu),
    - [vim-grepper](https://github.com/mhinz/vim-grepper) to search for text
      with `git ls-files` under the hood. Although alternatively I more and
      more rely on the LSP "search" feature (which opens a nice preview window).
  - Other tools:
    - Usually I have ghcid running in a terminal on the side, because the LSP
      sometimes doesn't list all errors (the error appears only when you open
      the file).
    - [hls-tactics-plugin](https://hackage.haskell.org/package/hls-tactics-plugin)
      for automatic case splitting (hole filling never works :-/)
    - [hoogle](https://hoogle.haskell.org/) in a browser nearby :-)
- **Trivia**: My history: Like Thomas, I used to exclusively use (and develop
  actually) large IDEs such as Eclipse, but I always used the vim mode in
  there. When starting Haskell I started with vscode but it started to be too
  slow when my project started growing. On the contrary, neovim with coc.nvim
  under the hood and the LSP is really fast and easy to setup 👍(and I use it
  with OCaml too).

### Torsten Schmits

- **Editor**: NeoVim
- **Other main tools**: nix, zsh, [tmux](https://github.com/tmux/tmux), [ghcid](https://github.com/ndmitchell/ghcid), HLS, [CoC](https://github.com/neoclide/coc.nvim), [tree-sitter](https://github.com/tree-sitter/tree-sitter)
- **Explanation**: I maintain my own haskell-nix toolkit that's rather
  specific to my workflow for the projects I work on mostly alone, and do some
  ad-hoc setup for those I collaborate on. Luckily, HLS is making it easier to
  hack on a new project. I've also built a few Neovim plugins in Haskell that
  implement some IDE-like features, like running ghcid in a tmux pane.
- **I love**: Nix's reproducibility is a total paradigm shift for setting up
  dev environments, and I'm very happy about the features that HLS does well.
- **Could be better**: Maybe it's my idiosyncratic workflow, but HLS still has
  significant problems. Before version 1.2 it kept crashing after 1-5 minutes on
  lots of my projects (it was an insane relief when this was fixed), and it
  still doesn't work well with multi-project setups.
- **Trivia**: Did you know I wrote the [Haskell grammar for tree-sitter](https://github.com/tree-sitter/tree-sitter-haskell)?

### Guillaume Desforges

- **Editor**: VS Code
- **Other main tools**: Nix, "Nix Environment Selector" (VSCode extension), "Haskell" (VSCode extension)
- **Explanation**: I make a shell.nix to get stack and haskell-language server.
  (and requirements for plugins), load it into the VSCode env, then create my
  project with stack. I write a small hie.yaml to explicit the cradle. You need
  to edit the settings.json so that Haskell (VSCode extension) actually uses the
  one from Nix.
- **I love**-: "it just works"™, ease of setup (at least relatively), get full HLS
- **Could be better**: first load of the nix shell can be a bit long, and Nix
  Environment selector has no output, so I'd advise loading it in shell before
  loading it using the extension.
- **Trivia**: if you are on NixOS, stack enables nix build automatically so
  you might need to define a `stack-shell.nix` and specify it in `stack.yaml`.

### Jeff Young

- **Editor**: [Spacemacs](https://www.spacemacs.org/) (emacs)
- **Other main tools**: Nix! I've used NixOS+Xmonad as a daily driver for 3
  years now. Fish shell. Rigrep and fzf for searching. Magit, a legendary git
  UX; I don't know how people live without it.
- **Explanation**: I mostly work on Haskell and GHC, but in my spare time I've
  been contributing and exploring
  [J](<https://en.wikipedia.org/wiki/J_(programming_language)>),
  [BQN](https://github.com/mlochbaum/BQN), and
  [Dyalog-APL](https://www.dyalog.com/). So I've been working
  on better package in nixpkgs for each of these and writing concomitant layers
  for Spacemacs. This means I'm in way too deep into emacs to switch now. I
  track all my goals in org-mode including clocking time, I have syncthing setup
  to sync my org files on a home server I built. This then syncs to my phone for
  notifications so the interface between all digital objects in my home is
  emacs+org. All of this to say that I'm probably a lifer when it comes to
  emacs. Although I am thinking of switching and contributing to Doom emacs
  (Spacemacs has been on version 0.3 for 3 years now?!)
- **I love**:
  - A small set of extremely powerful tools. Once you grok it it is _the_
    hammer for every problem (and therein lies the problem with emacs!).
  - Available everywhere and is free. Focus on backwards compatibility.
  - Built in documentation for everything. I simply hit `SPC h d f` over _any_
    function and get documentation for it. This integrates in varying ways with
    Haskell docs but nothing is as good as common lisp's SLIME or emacs-lisp's
    built in documentation.
  - The docs do what they say, say what they mean and are up to date. I can
    read some documentation that describes a configuration change, then go do
    that exact change and the predicted effect will occur.
  - Emacs is a lisp machine. Don't like something? Then sling some code and
    overwrite it with a hook, or if it is old then directly since emacs-lisp is
    dynamically scoped.
  - TRAMP mode allows my to `SPC f f` write `/sudo::/etc` and get a sudo'd
    shell into `/etc`. This same feature of emacs allows me to remote into _any_
    server and use my emacs configuration from my desktop. This means I never
    need to export my emacs configuration to the remote server, I simply need to
    use TRAMP to ssh in and I'm all set.
  - Copy paste with the kill ring allows me to hit `SPC r y` and see the last
    `n` things I've killed or copied. In contrast with most clip boards which
    just save the last thing copied.
  - [emacs-everywhere](https://github.com/tecosaur/emacs-everywhere)
- **Could be better**:
  - Latency, although this is improving with emacsGcc (a jit'd version of
    emacs)
  - No true Async!
  - Various packages are missing documentation. I'm sure this bites people
    trying to use emacs to work on Haskell. For me I've gotten used to reading
    source code in lieu of reading docs.
  - It is easy to get lost. There are a million ways to do the same thing and
    there are numerous packages that solve the same problem. Just understanding
    org-mode means reading docs for `org` `org-babel` `org-ox` `org-publish`
    etc.
  - Setting up HLS and LSP in spacemacs on NixOS is a very involved process
    and is not beginner friendly in any way. I have struggle with getting it
    working but now it is pretty good! I even have it setup for GHC which was a
    minor miracle.
- **Trivia**: I'd regularly lose workflow without fish's history autocompletion.

### Karol Czulkowski

- **Editor**: emacs + haskell-mode + haskell-language-server (lsp, lsp-ui)
- **Other main tools**: nix/nixos, [helm](https://emacs-helm.github.io/helm/),
  [key-chord](https://github.com/emacsorphanage/key-chord),
  [eno](https://github.com/enoson/eno.el),
  [projectile](https://github.com/bbatsov/projectile),
  [silver-searcher](https://github.com/ggreer/the_silver_searcher),
  [nix-direnv](https://github.com/nix-community/nix-direnv),
  [envrc](https://github.com/purcell/envrc) (thx Steve!),
  [yasnippet](https://github.com/joaotavora/yasnippet), hoogle search (from
  haskell-mode)
- **Explanation**: My journey with emacs started from terrible scala support
  in IntelliJ. Frontend compiler for this ide was showing fake errors which had
  confused me many, many times. Then I found
  [ensime](https://github.com/ensime/ensime-emacs) which unfortunately was
  abandoned but simultaneously [Metals](https://scalameta.org/metals/) was
  presented to the wider audience. This LSP experience was something that I was
  looking for. When it comes to Haskell & emacs, I started with
  [Intero](https://chrisdone.github.io/intero/) which was... inevitably
  abandoned and somehow not working on NixOs. Then I've found
  [Dante](https://github.com/jyp/dante) but the overall experience for me was
  worst in comparison to Scala Metals. I gave another chance to HLS and now this
  is really, really close my best LSP experience so far.
- **I love**:
  - emacs: Albeit, I'm not elisp hacker, I know there is always a way to setup
    anything I want in the way I want. I can use different plugins like jumping
    between windows/words/braces, regexp expressions, key-chords, dired-helm,
    projectile, multiple-cursors for all the projects or tech-stacks I work on.
  - projectile: all the boost it gives me when it comes to navigating among
    projects.
  - hls: Feedback it gives. Immediately.
- **Could be better**:
  - hls: jump to definition works only for local definitions
  - hls: haskell-language-server & bazel integration :)
- **Trivia**: I feel like I'm still not utilizing all the power that emacs
  gives (especially after reading Jeff's setup :) ).

### Adrian Robert

- **Editor**: VS Code + NixEnv + HLS.
- **Other main tools**: rg (ripgrep), Cabal, haskell.nix
- **Explanation**: I first tried IntelliJ but found it reminiscent of my
  experience with Eclipse for Scala – not good enough. So then became a
  first-time user of VS Code and liked it. Why not Emacs? Somehow the collective
  relative friction of setting up, the more specialized UI, and a general
  feeling of greater speed have me on the VS Code side now. I'll continue
  experimenting.
- **I love**: Quick feedback (underline + hover) on type mismatch problems. As
  a Haskell newbie I'd go through a lot more compile cycles without this.
- **Could be better**: The experience still feels pretty basic, say like using
  XCode for Objective-C ten years ago. I appreciate what's there, but nowadays
  one misses having things like refactoring and code templating. I don't know if
  this is a fundamental limitation in the LSP paradigm or in VS Code, or a lack
  in the Haskell implementation or weakness in the Haskell plugin.
- **Trivia**: For Scala I still haven't managed to get our Scala/Gradle project
  up and running properly in VS Code with Metals, which I suspect has to do with
  its multidirectory and subproject structure, together with too many parameters
  in Gradle versions and things it pulls in. Frustrating, though since I don't
  work on this very often it hasn't been frustrating enough for me to get it
  working. :-}

### Noon van der Silk

- **Editor**: NeoVim
- **Other main tools**: [XMonad](https://xmonad.org/), [nix](https://nixos.org/), [zsh](https://ohmyz.sh/), [rip-grep](https://github.com/BurntSushi/ripgrep), [fzf](https://github.com/junegunn/fzf)
- **Explanation**: I use a fairly simple vim setup, with a strong reliance on fuzzy-finding and grep.
- **I love**: How fast everything starts up; independence of all the tools.
- **Could be better**: I'm jealous of people using HLS.
- **Trivia**: I rely on a lot of zsh aliases to live a convenient command-line life.

### Julien Debon

- **Editor**: [Spacemacs](https://www.spacemacs.org/)
- **Other main tools**:
  - [Nix/NixOS](https://nixos.org/)
  - [Direnv](https://direnv.net/)
  - [nix-direnv](https://github.com/nix-community/nix-direnv)
  - [i3](https://i3wm.org/)
  - [ZSH](https://www.zsh.org/)
  - [Haskell Language Server](https://github.com/haskell/haskell-language-server)
  - [Hoogle](https://hoogle.haskell.org/)
  - [Magit](https://magit.vc/)
  - [Ripgrep](https://github.com/BurntSushi/ripgrep)
- **Explanation**: I have been a heavy user of IDEs like Eclipse and IntelliJ for many years, but after several frustrations about configuring/fine tuning, I moved to VS Code for a while, and then a year ago to Emacs, via Spacemacs. While the learning curve is steep, I would never go back: I can exactly customize my editor as I wish with minimal effort, and nearly all languages and features are already supported. On the Haskell front, a lot of changes have happened in the past few years. Using HLS is currently pretty good, though I still occasionally miss a good debugger or advanced refactoring tools.
- **I love**:
  - Emacs: Emacs extensibility and Spacemacs mnemonics: almost all shortcuts **just make sense**.
  - Hoogle: I am a user and abuser of Hoogle: I can't believe I have developed all these years in various languages without this game-changing tool which allows looking for functions by signature. Whatever function, type, typeclass or package I am looking for, Hoogle finds the perfect solution 99% of the time. It baffles me that most languages don't have an equivalent feature, including other languages I use on a regular basis.
  - HLS: I love code navigation, documentation display, instant typechecking, and call hierarchy.
- **Could be better**:
  - Emacs: every once in a while I would like to scroll a buffer without moving the cursor, but it's impossible in Emacs. I think it is the only feature from my past editors that is not simply possible in Emacs.
  - HLS: I learn a lot by browsing dependency code, so I really, really wish we could [navigate to dependency code](https://github.com/haskell/haskell-language-server/issues/708). Currently I browse dependency code via Hackage/Stackage, but this is nowhere near as comfortable as editor integration.
- **Trivia**: I only discovered Magit a year ago, but if anyone forced me to go back to using vanilla Git, the situation would escalate pretty quickly. Similarly to Hoogle, I just don't understand why it's not more widespread, considering how many people interact with Git every day.

---

What's your preferred way of working in Haskell? We'd love to hear from you!
Share your setup with us on [Twitter](https://twitter.com/tweagio)!
