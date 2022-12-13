---
title: "Optimizing NixOS Search"
author: William Wang
description: "An internship report on my experience working on improving search.nixos.org and my internship at Tweag"
tags: [internship, nix]
---

With their introduction in Nix 2.4, [flakes](https://www.tweag.io/blog/2020-05-25-flakes/) are quickly becoming an integral part of the Nix ecosystem. For anyone unfamiliar, flakes exist to provide a standard format to package Nix-based projects.

To allow for better user experience and composability for existing flakes, discoverability of flakes is a necessary feature.

The site [search.nixos.org](search.nixos.org) is used to search for packages, options, and flakes. It also provides metadata information such as descriptions, maintainers, and compatible platforms.

Currently, search.nixos.org imports flake metadata through custom nix code that wraps flake evaluations. However, wrapping employs too much memory when used with large repositories such as `nixpkgs`. This causes a segmentation fault. As currently implemented, search.nixos.org cannot show all flakes.

My internship involved demonstrating a solution to this issue by updating the JSON schema for `nix flake show` to natively export the necessary metadata for packages.

## Metadata For Export

`nix flake show` reads in the file `flake.nix` which provides an interface to artifacts like packages. Packages contain metadata fields (also called meta-attributes) detailing information such as a description of the package (`meta.description`) and a list of supported Nix platforms (`meta.platforms`).
To showcase a working solution, I decided to implement support for three additional fields: `meta.homepage`, `meta.licenses`, and `meta.maintainers`.

These fields are more or less how `nixpkgs` defines them in their flake files. In theory, these changes to `nix flake show` should already be compatible.

Let's explore these three additional fields:

`homepage` is the package's homepage. It takes the form of a simple string:

```nix
meta.homepage = "https://code.visualstudio.com/";
```

`licenses` appears as an attribute set:

```nix
meta.licenses =
  {
    spdxId = "MIT";
    fullName = "MIT License";
  };
```

`maintainers` includes a list of package maintainers and is defined as a list of attribute sets which include basic identifying information:

```nix
meta.maintainers =
    [
        {
            email = "maintainer@nixos.org";
            matrix = "@user:matrix.org";
            name = "first last";
            github = "username";
            githubid = 0000000;
            keys = {
                fingerprint = "8w5w jc2b d0h8 q3l9 0tc4 m55v i87r 23zj 0fbj r30x"
            };
        }
       ...
    ]

```

For example, the [GNU Hello package](https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/misc/hello/default.nix) declares its meta fields as follows:

```nix
meta = with lib; {
  description = "A program that produces a familiar, friendly greeting";
  longDescription = ''
    GNU Hello is a program that prints "Hello, world!" when you run it.
    It is fully customizable.
  '';
  homepage = "https://www.gnu.org/software/hello/manual/";
  license = licenses.gpl3Plus;
  maintainers = [ maintainers.eelco ];
  platforms = platforms.all;
};
```

## Flake Show Command

The `nix flake show` command is implemented by `CmdFlakeShow`'s `run()` function. The `run()` function consists of a definition and call to the local function `visit()`. For recognized nested attributes, `visit()` will be recursively run until all recognized attributes have been processed. When applicable, another nested function `showDerivation()` will be run, printing a derivation's attributes to standard output.

When the `json` flag is passed into the program, output information and metadata will also be stored in a JSON object. Upon recursing, `showDerivation` will check for a `meta` symbol within the `flake.nix` file. If found, the program will continue to check for recognized symbols defined as elements under `meta`. To collect our metadata, we run `maybeGetAttr` which returns `std::shared_ptr<AttrCursor>`. As `AttrCursor` serves as a wrapper for an `Attr`, we can use one of its member functions to access each metadata value based on its type.

For `description` and `homepage`, this is straightforward. `AttrCursor` implements a `getString()` function that returns a `std::string` value for a given attribute.

`license` consists of an attribute set. Therefore, we can use the `getAttrs()` function to obtain a `std::vector<Symbol>`. We can then do a lookup of the symbols in our `EvalState` for our `std::string` values.

So far, the previous two fields are easy to implement. However, `maintainers` requires a bit more work. The main trouble comes from the fact that `maintainers` is defined as a list of attribute sets. There exists no getter function for this attribute value type. This requires us to implement our own `getListOfAttrs()` function for `AttrCursor`.

## Implementing getListOfAttrs

For performance reasons, Nix internally caches the evaluation results on-disk.

This cache uses a SQlite database with a custom schema designed for efficiently caching lazy recursive data-structures.

However, this format does not yet handle lists properly, and only has hard-coded support for lists of strings, which is not sufficient for our purposes, as some of the metadata fields are represented as lists of compound structures.

Adding such support is easy in principle, but the actual implementation would have taken more time than what was available for my internship. My solution instead was to fall back on an uncached evaluation for evaluating these attributes. However, this means that we will not benefit from the cache, making it a temporary solution only.

Nevertheless, with this prototype solution, `nix flake show` now shows all three metadata fields as JSON. This can now be used with nixos-search to deprecate our wrapped nix evaluations.

So what does the end result look like? Let us take a look at what `nix flake show --json` outputs when given a `flake.nix` file using the GNU Hello package:

```JSON
"homepage": "https://www.gnu.org/software/hello/manual/",
"license": {
  "deprecated": "false",
  "free": "true",
  "fullName": "GNU General Public License v3.0 or later",
  "redistributable": "true",
  "shortName": "gpl3Plus",
  "spdxId": "GPL-3.0-or-later",
  "url": "https://spdx.org/licenses/GPL-3.0-or-later.html"
},
"maintainers": {
  "edolstra": {
    "email": "edolstra+nixpkgs@gmail.com",
    "github": "edolstra",
    "githubId": "1148549",
    "name": "Eelco Dolstra"
  }
}
```

## Future Work To Be Done

- As mentioned above, `getListOfAttrs()` should utilize caching in production. This will require revamping how lists are internally represented. Ideally, there should be a generalized `getList()` function that behaves similarly to `getAttrs()`. One proposed solution is to implement lists exactly like attribute sets but with a different tag and increasing integer key values.
- There are many more metadata fields that can be queried. Implementing support for these in nix should be relatively straightforward.
- `nix flake check` should warn users about missing or improperly formatted metadata fields.
- Finally, we should implement our solution at search.nixos.org and benchmark it to verify that the import of nixpkgs is fast enough.

## Interning at Tweag

As my first internship experience, Tweag has been a fantastic work environment. Tweagers share a mutual love for discussion and group learning, which fosters the exploration and application of new ideas.

During my time here, I have had the chance to put common software engineering techniques into practice and immerse myself in all things Nix. My mentor Rok provided extensive advice throughout this process and taught me the value of avoiding premature optimizations.

My chapter at the Tweag Paris office gave me an inside glimpse into the brilliant minds behind all those GitHub commits. I would like to thank Tweag for this opportunity and my coworkers for their thought-provoking insights.

For a chance to experience working with a company where research and engineering intersect, I would highly recommend checking out Tweag.
