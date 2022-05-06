# The Tweag website

## Cloning the website

This repo uses [Git Large File Storage](https://git-lfs.github.com/)
to avoid keeping around large blobs inflating the repository, instead
only keep a pointer in the repo and download blobs during checkout
when necessary. This is accomplished by git filter magic.

An up-to-date list of GIT LFS tracked files can be retrieved by
looking into `.gitattributes`, but this is mostly about large images
from blog post, or team member images.

To make sure you see the real files, and not pointers in your
checkout, please make sure to have git lfs enabled and configured before cloning:

```nix
# <home-manager> configuration
programs.git.lfs.enable = true;

environment.systemPackages = [ pkgs.git-lfs ];
```

or if you're using something else than NixOS, follow their installation
guide [on the website](https://git-lfs.github.com/).

If GIT LFS is installed and configured after cloning, run

```
git lfs pull
```

from inside the repo to get the images.

If you see a warning message such as:

```
Encountered 1 file(s) that should have been pointers, but weren't:
	app/assets/img/team/someone.jpg
```

this can be corrected with the following command, which creates a "add to lfs"
commit:

```
git lfs migrate import --no-rewrite -m "add to lfs" app/assets/img/team/someone.jpg
```

If `migrate` doesn't work, [an issue on the Github LFS
tracker](https://github.com/git-lfs/git-lfs/issues/1939#issuecomment-692292514)
recommends

```
git add --renormalize .
git commit -m "Fix broken LFS files"
```

## Building the website

For convenience, a [shell.nix](./shell.nix) file is provided to
provision a shell environment with Node.js installed, using
`nix-shell`.

To install and build the website (run from inside the `nix-shell` to
ensure you have Node.js installed):

```
$ npm install
```

To start the website in development mode locally:

```
$ npm run develop
```

The webserver's address will be shown in the commands' output.

To test the production build, just to be sure:

```
$ npm run build
```

## Submitting a change

Before committing a change, make sure to have installed
the website before with `npm install`, then run the code formatter and linter:

```
$ npm run format
```

CI builds will fail during the linting stage if you omit this step.

The site is deployed using [Netlify](https://www.netlify.com/). A new version
auto-deploys to production upon a push to the `master` branch.

Whenever you submit a new PR, Netlify creates a new instance of the production
site available at a temporary URL of the form:

```
https://<HASH>--tweag-www.netlify.app/
```

You can access this link in the details of the CI action `deploy/netlify`:
See the CI block that appears immediately above where you can add comments.
The last entry is `deploy/netlify`; clicking the "Details" link at right will
bring you to the rendered page.

It might take a few minutes for the deploy process to finish and for
the URL to be operational.

## Blog post guidelines

The management of blog post ideas and drafts is done through the
[board](https://github.com/tweag/www/projects/1). The general workflow
is the following:

- When you have an idea of a blog post which you would enjoy seeing on
  the blog, create a card, in the _ideas_ column, with a few lines
  explaining what you would like to see in the post. Be generous with
  ideas: ideas are cheap and we can always decide later, collectively,
  not to follow up.

- When you want to spend some time and pick up an idea (or start
  writing on something new). Pick up the card, move it to the _in
  progress_ column and tag yourself.

- When you are done writing the post, make a pull request against the
  blog’s repository. Add a link to the PR as a comment in the card,
  assign the card to a reviewer (if you don’t know whom to assign,
  assign @guaraqe who will dispatch the review requests) and move the
  card to the under review section. See also the reviewer's checklist
  below.

- When the review is done, the reviewer assigns the card to @guaraqe
  and moves the card to the staging section. It is @guaraqe's role to
  merge the pull request. When the blog post lands on master, the card
  will be marked as accomplished.

### Submitting a new blog post

Note that adding a new post is a simple matter of creating a new
Markdown file in [content/blog](./content/blog). The post should be
a CommonMark compliant Markdown file with a Jekyll-style YAML header.
The header should contain the following fields at a minimum:

```yaml
title: Your blog post title
author: Alan Turing
```

You can package arbitrary assets (like images) with your blog post by
creating a folder for it instead, with the Markdown file called
`index.md`.

### Writing a blog post

The blog posts written in this blog must always be put in the context
of the company. That means that they should offer a history that goes
beyond a personal learning experience.

In order to do this, it must be clear:

- _Who_ are the readers you want to target with your post. That should
  give you an idea on what you assume as background knowledge from
  your readers, and what you are going to show them that is
  interesting.

- _What_ the reader should expect by reading your post. Very often
  this is the solution of a problem. That should include the end goal
  you want to achieve by your process, and the constraints that you
  have when solving this problem. This may not apply strictly when the
  post has a teaching nature.

- _How_ we achieve our end goal. Very often this describes how the
  problem has been solved. That should include the steps of the
  process, which should constitute the main content of the text, but
  also what this solution offers in terms of advantages and
  disadvantages in relation to other solutions. This may not apply
  strictly when the post has a teaching nature.

The title should make clear what is achieved in the end. The intro
should cover the _What_ and _How_ points, with the conclusion
recalling _What_ was achieved, together with the qualities of the
_How_ with relation to others.

If the end goal cannot be described explicitly in the post (for
example, when trying to sell a technology like Haskell or Nix), this
should be explicit at least to the reviewers. One cannot judge
correctly a text if one does not know what is the reasoning behind it.

### Reviewing a blog post

Your role, as a reviewer, is to ensure the quality of the post both in
terms of language and in term of content. The main question you need
to answer are:

- Is this well-written?
- Is this exciting / Does this make a good read?

Make suggestions as a regular pull request review. If either question
above is answered by the negative, your review ought to point out why,
and suggest improvements to the post's author.

Don't hesitate to propose or ask for many changes: writing is hard, and
there will often be unclear or boring bits in the first round. Think
about what you would like being pointed out to you if you were the
author. You don't need the author's expertise: if you don't understand
a passage, neither will the target reader.

However, review of the content should take into account the goal of
the text and the way the writer wants to convey it. We do not want to
change completely a text in our own way, but rather make it most
effective in the way the writer conceived it. This may not apply if
you are the boss.

The phases of the reviewing process are roughly the following:

- Content review:
  - Comments on meaning and phrasing of sentences and code
  - add links to Tweag's other posts on related subjects.
  - _Responsibles_: author, editor and volunteer proofreaders.
- Grammar and spelling check:
  - _Responsibles_: author, editor and volunteer proofreaders.
- Code snippets review:
  - Make sure all the code snippets run and that nothing is missing.
  - _Responsible_: author.
- Style check
  - Markup, capitalization, meta-tags (description, image),
  - _Responsible_: editor.

When you are satisfied with the result, approve the PR (with Github's
semantical review) and move the card to Staging (see the README card).

## Implementation details

### Build process

We use the [GatsbyJS] framework to generate a static website from YAML
data and JSX pages. Ready to deploy HTML pages and CSS are built from
JSX source files consisting of [React] components. This gives us the
full power of a programming language (in this case JavaScript) when
authoring pages and makes it easy to reuse the plethora of React
components out there. Moreover, React components compose well - better
than mixins and file inclusions used in template engines, typically.
Most importantly, GatsbyJS is an incredibly well-documented project.

There are two modes in Gatsby:

- **development mode:** useful for iterating quickly on a new page
  design;
- **build mode:** takes longer, but produces assets optimized for
  production that can be deployed anywhere.

The two modes are documented extensively [here][gatsby-build-process].

[gatsbyjs]: https://gatsbyjs.org
[gatsby-build-process]: https://www.gatsbyjs.org/docs/overview-of-the-gatsby-build-process/

### Styling

Styles are assembled programmatically from JavaScript source embedding
CSS syntax as string literals and CSS properties as objects. This is
called [CSS-in-JS][gatsby-css-in-js]. We use the [Emotion
library][gatsby-emotion] for this. Global CSS rules are in
`src/styles/global.js`. Adding new global rules should be avoided.
Most styling should go in the same source file as the component it
pertains to.

Some basic global properties (fonts, colors, spacing, etc), used
pervasively on all pages of the website, are set as part of a _theme_.
See `src/gatsby-plugin-theme-ui/index.js`. Properties of the theme can
be accessed in any `sx` prop of a component. We use [Theme
UI][gatsby-theme-ui] for this.

[theme-ui]: https://theme-ui.com
[gatsby-css-in-js]: https://www.gatsbyjs.org/docs/css-in-js/
[gatsby-emotion]: https://www.gatsbyjs.org/docs/emotion/
[gatsby-theme-ui]: https://www.gatsbyjs.org/docs/theme-ui/
