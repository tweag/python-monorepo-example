# The Tweag website

If you find that watching this repository causes too many notifications,
it might be useful to join the [#www-notifications](https://tweag.slack.com/archives/C03N56TKD7Z) channel on slack.
A bot in that channel sends notifications only when PRs and issues are opened and closed,
rather than for every message in a discussion.

## Cloning the website

This repo uses [Git Large File Storage](https://git-lfs.com/)
to avoid keeping around large blobs inflating the repository, instead
only keep a pointer in the repo and download blobs during checkout
when necessary. This is accomplished by git filter magic.

An up-to-date list of GIT LFS tracked files can be retrieved by
looking into `.gitattributes`, but this is mostly about large images
from blog post, or team member images.

To make sure you see the real files, and not pointers in your
checkout, please make sure to have git lfs enabled and configured before cloning:

```nix
programs.git = {
 enable = true;
 lfs.enable = true;
};
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
$ yarn install
```

To start the website in development mode locally:

```
$ yarn develop
```

The webserver's address will be shown in the commands' output.

To test the production build, just to be sure:

```
$ yarn build
```

## Submitting a change

Before committing a change, make sure to have installed
the website before with `yarn install`, then run the code formatter and linter:

```
$ yarn format
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
