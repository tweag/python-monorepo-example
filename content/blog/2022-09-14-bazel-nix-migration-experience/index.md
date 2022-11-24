---
title: "Bazel and Nix: A Migration Experience"
shortTitle: A Bazel and Nix Migration

author: Ben Radford

description: |
  Getting an old Make based project building again after 10 years with the help of Bazel and Nix.

tags:
  - bazel
  - nix
  - build-systems
---

I’ve recently been learning how to use [Bazel][bazel-build] and [Nix][nixpkgs]
to build projects. Reading documentation and playing around with
[codelabs][codelabs] is a great way to get started, but when learning a new
technology there comes a point where you have to start applying it for real to
make further progress. To this end I decided to migrate an old hobby project of
mine to use Bazel. This post describes my experience and I hope serves as a
useful example for others.

Bazel is a fantastic build system which by itself provides [many
benefits][bazel-competencies]. However dependency management could be
considered an area of weakness for it. This is understandable given its
monorepo origins at Google but when not working in a monorepo, what is the best
way to provide the dependencies a project needs?

One approach is to simply assume the dependencies exist in the environment
(having been put there by `apt install` or the like), but this is just ignoring
the problem. Bazel has the concept of [external repositories][bazel-external]
and is able to fetch and build source code from a variety of places[^1]. While
this might be okay under some circumstances, we probably don't want to spend
significant amounts of time building a large dependency tree for a project that
itself only takes a few minutes to build.

Instead, Nix can be used to supply the dependencies to Bazel prebuilt. Nix and
Bazel are very similar technologies (both build code from source and both place
strong emphasis on properties like determinism and hermeticity), but without
[diverging into the details][blog-bazel-nix] it is perhaps best to think of Nix
as a package manager and Bazel as a build system. The advantage of Nix is that
while it ostensibly builds everything from source, in practice it usually just
downloads artifacts from a binary cache.

The [project][space-game] I migrated is an unfinished game with around fifteen
thousand lines of C++ code. It was originally built using some very convoluted
makefiles and is structured as follows:

<p align="center">
<img src=project-structure.svg alt="Project structure" style="width: 100% !important;" />
</p>

This game has a few attributes that make it an interesting case study:

- It has a non-trivial structure with modules[^2] shared between client and
  server executables.

- It depends on several third party libraries. Previously these were manually
  built from source. Now with the help of [`rules_nixpkgs`][rules_nixpkgs] they
  are provided by Nix. Typically this means they will be fetched prebuilt from
  the Nix cache, saving the time it would have taken to compile them.

- In addition to code that needs to be built, the game has assets like 3D
  meshes and textures that need to be transformed into a format that can be
  loaded at runtime.

- One particular transformation is handled by the custom `zonebuild` tool. If
  any of the source code that goes into this tool changes and it gets rebuilt,
  the data transformation it performs also needs to be rerun.

## Building the source code

Every Bazel project requires a [`WORKSPACE.bazel`][bazel-workspace] file at its
root. As the migration advances this file will grow in complexity, but to begin
with it simply specifies the workspace name:

```python
workspace(name = "space-game")
```

A Bazel workspace is organised into [packages][bazel-packages] by `BUILD.bazel`
files. There are many ways you might do this (e.g. a separate package for each
module and executable) but for a project of this scale it is reasonable to
build all the source code from a single package named `//common/src`.

As it has no dependencies, building the `core` module is a good place to start.
It just requires a [`cc_library`][bazel-cc-library] rule in
`common/src/BUILD.bazel`:

```python
cc_library(
    name = "core",
    hdrs = glob(["core/*.hpp"]),
    srcs = glob(["core/*.hpp", "core/*.cpp"]),
    copts = ["-std=c++17"],
    includes = ["."],
    visibility = ["//visibility:public"],
)
```

At this point it is possible to build the `core` module by running the following command:

```bash
bazel build //common/src:core
```

Bazel does not pollute the source tree with intermediate build files and output
files. These are instead created within the [`outputRoot`][bazel-output]
directory and some convenient symlinks are added to the workspace root. If you
follow the `bazel-bin` symlink you'll find that `cc_library` has produced both
static and shared libraries:

```bash
bazel-bin/common/src/libcore.a
bazel-bin/common/src/libcore.so
```

This is pretty cool. In the old days, linking even a moderately complex project
was quite a tricky affair. Even more so when shared libraries were involved
since there were compiler flags like [`-fPIC`][fpic-flag] to worry about. With
Bazel all those messy details have been abstracted away.

Next let's examine how the `net` module is built. If you check the graph above
you'll see that `net` depends on `core` and two third party libraries,
[`boost`][boost-lib] and [`enet`][enet-lib]. As mentioned in the introduction,
these libraries will be fetched from Nix. First we need a `nixpkgs.nix` file:

```nix
let nixpkgs = fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/ce6aa13369b667ac2542593170993504932eb836.tar.gz";
  sha256 = "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
};
in import nixpkgs
```

This pins the nixpkgs version to git commit [`ce6aa13`][nixpkgs-22-05], which
corresponds to the 22.05 release. You can pick any commit you like but if you
are unsure, picking the commit with the latest [release tag][nixpkgs-tags] is a
good place to start. Next some additions to `WORKSPACE.bazel` are required:

```python
load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

# Import the rules_nixpkgs repository.
http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "b01f170580f646ee3cde1ea4c117d00e561afaf3c59eda604cf09194a824ff10",
    strip_prefix = "rules_nixpkgs-0.9.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/archive/v0.9.0.tar.gz"],
)

# Import the transitive dependencies of rules_nixpkgs.
load("@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl", "rules_nixpkgs_dependencies")
rules_nixpkgs_dependencies()

# Import a repository for the version of nixpkgs we pinned in nixpkgs.nix above.
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_local_repository")
nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

# Configure a toolchain from the nixpkgs repository above.
load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_cc_configure")
nixpkgs_cc_configure(
    name = "nixpkgs_config_cc",
    repository = "@nixpkgs",
)

load("@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl", "nixpkgs_package")
```

The [`nixpkgs_package`][nixpkgs_package] macro loaded by the last line can be
used to import a Nix package as a Bazel repository. It has many optional
parameters, allowing its behaviour to be customised precisely as needed (though
this requires some familiarity with Nix and Bazel).

The [`attribute_path`][attribute-path] parameter indicates the particular Nix
package to be imported. You can find attribute paths by searching for a Nix
package with the [`nix-env`][nix-env-install] command:

```bash
$ nix-env -f nixpkgs.nix -qaP '.*enet.*'  # Note, the last argument is a regex.
nixpkgs.enet                                           enet-1.3.17
nixpkgs.freenet                                        freenet-build01480
```

If `attribute_path` is not given it defaults to the value given for the `name`
parameter, so for `enet` it can be left out. Once you've found a Nix package
you can examine what files it contains:

```
$ tree $(nix-build nixpkgs.nix -A enet)
/nix/store/75xz5q742sla5q4l0kj6cm90vgzh8qv3-enet-1.3.17
|-- include
|   `-- enet
|       |-- callbacks.h
|       |-- enet.h
|       |-- list.h
|       |-- protocol.h
|       |-- time.h
|       |-- types.h
|       |-- unix.h
|       |-- utility.h
|       `-- win32.h
`-- lib
    |-- libenet.la
    |-- libenet.so -> libenet.so.7.0.5
    |-- libenet.so.7 -> libenet.so.7.0.5
    |-- libenet.so.7.0.5
    `-- pkgconfig
        `-- libenet.pc
```

This information will be useful when crafting `build_file_content`. The value
given for this parameter is written to a `BUILD.bazel` file in the root of the
`@enet` repository. Again in `WORKSPACE.bazel`:

```python
nixpkgs_package(
    name = "enet",
    repository = "@nixpkgs",
    build_file_content = """\
cc_import(
    name = "enet-import",
    hdrs = glob(["include/**/*.h"]),
    shared_library = "lib/libenet.so.7",
    visibility = ["//visibility:public"],
)
cc_library(
    name = "enet",
    hdrs = glob(["include/**/*.h"]),
    includes = ["include"],
    deps = [":enet-import"],
    visibility = ["//visibility:public"],
)
""",
)
```

The [`cc_import`][bazel-cc-import] rule is necessary because `libenet.so` is
prebuilt by Nix. Note that the particular symlink chosen for `shared_library`
matters[^3]. You will get a runtime error if you choose wrong, but fortunately
the error will indicate the correct choice.

Next a `cc_library` rule is used to pull in the header files. This rule is the
one that the `net` module will depend upon directly and the `cc_import` rule
will be a transitive dependency. This general pattern is used for all the
third party dependencies, including `boost`.

Now that we have the necessary dependencies, let's see how the `net` module is
built:

```python
cc_library(
    name = "net",
    hdrs = glob(["net/*.hpp"]),
    srcs = glob(["net/*.hpp", "net/*.cpp"]),
    copts = ["-std=c++17"],
    includes = ["."],
    deps = [
        "//common/src:core",
        "@boost",
        "@enet",
    ],
    visibility = ["//visibility:public"],
)
```

Note that the `@enet` dependency is syntactic sugar for `@enet//:enet` and this
target is just the `cc_library(name="enet", ...)` rule defined a little
earlier. The remaining modules (`math`, `physics` and `script`) are built in a
similar fashion.

The final piece of the puzzle is to build the executables. Taking `client` as
an example:

```python
cc_binary(
    name = "client",
    srcs = glob(["client/*.hpp", "client/*.cpp"]),
    copts = ["-std=c++17"],
    includes = ["."],
    deps = [
        "//common/src:core",
        "//common/src:net",
        "//common/src:physics",
        "@cegui",
        "@ogre",
        "@ois",
    ],
    visibility = ["//visibility:public"],
)
```

Apart from using [`cc_binary`][bazel-cc-binary], this looks much the same as
the rules we have already seen.

## The asset transformation pipeline

Having seen how the code is built, let's turn to the game assets. An asset
could be a polygon mesh or a texture for a 3D model. It could be a music track,
a voice recording or a pre-rendered video. These assets are usually authored
and stored in a high resolution lossless format. They must be transformed into
a format more suitable for distribution and optimised for runtime use. For
proper games the asset pipeline can be quite complicated and might take hours
to run in full, but for this project it is simple:

<p align="center">
<img src=asset-transformations.svg alt="Asset transformations" style="width: auto !important;" />
</p>

Files with no incoming arrows are checked into source control while the rest
are produced by Bazel during the build. A brief description of each follows:

- `geometry.xml`: List of the triangles and faces comprising a mesh.
- `texture.tga`: [UV mapped][uv-map] texture for the mesh.
- `material.script`: A high level representation of a [shader program][shader-program].
- `mesh.bin`: An efficient binary format for meshes used by Ogre3D.
- `assets.zip`: Contains all the assets needed to render a 3D model.
- `collision.dat`: Stores map geometry in a [k-d tree][kdtree] for fast collision detection.

Bazel has no built-in rules for these transformations but
[`genrule`][bazel-genrule] can be used to run arbitrary Bash commands. To avoid
duplication I decided to define a couple of helper macros in a
[`resources.bzl`][resources-bzl] extension file. In Bazel a
[macro][bazel-macros] is a function that can instantiate rules. The
`assemble_assets` macro is used in `//common/data/ships` like this:





```python
load("//common/data:resources.bzl", "assemble_assets")

assemble_assets("bomber")
assemble_assets("spider")
assemble_assets("warbird")
```

The version of `assemble_assets` shown below has been simplified a little for clarity:

```python
load("@rules_pkg//pkg:zip.bzl", "pkg_zip")

def assemble_assets(name):
    # Convert xml mesh data to ogre binary format.
    mesh_converter = "@ogre//:bin/OgreXMLConverter"
    native.genrule(
        name = "convert_{name}_mesh",
        srcs = [":{name}-geometry.xml"],
        outs = ["{name}-mesh.bin"],
        tools = [mesh_converter],
        cmd = """$(execpath {mesh_converter}) \
            $(execpath {name}-geometry.xml) \
            $(execpath {name}-mesh.bin)
        """,
    )

    # Package the assets into a zip.
    pkg_zip(
        name = "zip_{name}_assets",
        out = "{name}-assets.zip",
        srcs = [
            "{name}-mesh.bin",
            "{name}-texture.tga",
            "{name}-material.script",
        ]
    )
```

The most interesting thing here is that the `OgreXMLConverter` tool is being
used to transform mesh data. This tool comes as part of the Nix package for
`ogre` and it must be [explicitly exported from `BUILD.bazel`][ogre-converter].
It also needs to be passed to the [`tools`][bazel-tools] parameter of the
`genrule` or else Bazel will complain that it has not been declared as a
prerequisite.

Ships are treated as spherical objects by the physics engine so they do not
need their geometry to be transformed into a k-d tree for collision detection,
but for maps this extra step is needed:

```python
def assemble_map(name):
    assemble_assets(name)

    # Build k-d tree for collision detection.
    native.genrule(
        name = "{name}_collision",
        srcs = [":{name}-geometry.xml"],
        tools = ["//common/src:zonebuild"],
        cmd = """$(execpath //common/src:zonebuild) \
            $(execpath {name}-geometry.xml) \
            $(execpath {name}-collision.dat)
        """,
        outs = ["{name}-collision.dat"],
    )
```

Maps have meshes, textures and materials and the transformation of those is
delegated to `assemble_assets`. Then the k-d tree is built by invoking
`zonebuild` from another `genrule`. This is essentially the same as invoking
`OgreXMLConverter` except this time the tool is built in the main repository
instead of coming prebuilt from an external repository.

Beyond what we have seen above there are some [`pkg_zip` rules][common-data]
for various other game resources like background textures, configuration files,
fonts, etc.

## Running the game

With both the code and data building we now turn to actually running the game.
For the `server` executable this is relatively straight forward. Its only data
dependency is the map collision data.

However the graphics engine used by `client` requires all assets to be listed
in a `resources.cfg` file. It also needs to dynamically load various plugins
with [`dlopen`][dlopen]. This is facilitated by a wrapper script which
enumerates all the resources and sets the plugin folder appropriately. To
preserve the locality of the logic, it is convenient to inline such scripts
directly in `BUILD.bazel` using a [`write_file`][write_file] rule:

```python
write_file(
    name = "write-run-client-sh",
    out = "run-client.sh",
    content = [
        "sed -i \"s|^PluginFolder=.*|PluginFolder=$1|\" plugins.cfg",
        "scripts/enum-resources.sh . >resources.cfg",
        "common/src/client",
    ]
)
```

This causes Bazel to generate the script file as needed and avoids the hassle
of creating and committing another file to version control. The script is
invoked by a [`sh_binary`][bazel-sh-binary] rule:

```python
sh_binary(
    name = "run-client",
    srcs = [":run-client.sh"],
    data = [
        "//common/data/maps:base03.dat",
        "//common/data/maps:base03.zip",
        "//common/data/ships:bomber.zip",
        "//common/data/ships:spider.zip",
        "//common/data/ships:warbird.zip",
        "//common/data:cegui",
        "//common/data:config",
        "//common/data:materials.zip",
        "//common/data:particles.zip",
        "//common/data:textures.zip",
        "//common/src:client",
        "//scripts:enum-resources.sh",
        "@ogre//:lib/OGRE",
    ],
    args = [
        "$(rootpath @ogre//:lib/OGRE)",
    ],
)
```

Bazel ensures that all the data dependencies are available in the
`run-client.runfiles` directory where they will be found by the
`enumerate-resources.sh` script. The full path to the plugin directory is
obtained by using [`location`][bazel-variables] to resolve the
`@ogre//:lib/OGRE` label. This path is then passed as an argument to
`run-client.sh` and there substituted into `plugins.cfg`.

Now the entire game can be [built and run][build-and-run] from a Nix shell with
just two commands:

```bash
bazel run //common:run-server
bazel run //common:run-client
```

## Conclusion

It took around three days to complete the migration from start to finish.
However the majority of that time was spent updating the code to work with
C++17 and fixing various issues that arose from upgrading to newer versions of
the third party libraries. The Bazel migration itself was fairly easy and
wouldn't have taken more than a day if the code had been otherwise up-to-date.
Of course this was a pretty small project so your mileage may vary.

I was initially concerned that there might not be Nix packages for some of the
more obscure dependencies but I was pleasantly surprised. Every single one of
the libraries I had chosen for this project in 2009 can now be found in Nix.
Having easy access to over 80,000 packages is reason enough to pair Bazel with
Nix, but if reproducible builds matter to you [it makes even more
sense][blog-bazel-nix].

Perfectly [correct and incremental][correct-incremental-builds] builds are
awesome. Bazel is very good at managing complex dependency graphs where some
build outputs (e.g. tools) are themselves used as part of the build. When you
change something Bazel will rebuild exactly what is necessary and it will do so
in the correct order.

Clearly, not building more than necessary saves time but not building less does
too. When a build system neglects to rebuild something it should have, the
result is confusion and wasted time because runtime behaviour does not match
code. After being bitten by this a few times developers will tend to clean the
build more frequently, incurring the cost of a full rebuild each time. So the
confidence Bazel provides here is great for developer productivity.

<!-- Footnotes -->

[^1]:
    For example, elsewhere on the filesystem or from a remote Git repository.

[^2]:
    The term _module_ is used here informally to refer to a grouping of related
    source files in a directory under `//common/src`. These are not [C++20
    modules][cpp-modules].

[^3]:
    The `shared_library` parameter should exactly match the shared library name
    in the dynamic section of the executable. You can use `readelf -d EXECUTABLE`
    to check this.

<!-- Links -->

[attribute-path]: https://github.com/tweag/rules_nixpkgs#nixpkgs_package-attribute_path
[bazel-build]: https://bazel.build/
[bazel-cc-binary]: https://bazel.build/reference/be/c-cpp#cc_binary
[bazel-cc-import]: https://bazel.build/reference/be/c-cpp#cc_import
[bazel-cc-library]: https://bazel.build/reference/be/c-cpp#cc_library
[bazel-competencies]: https://bazel.build/about/vision#bazel-core-competencies
[bazel-external]: https://bazel.build/docs/external
[bazel-genrule]: https://bazel.build/reference/be/general#genrule
[bazel-macros]: https://bazel.build/extending/macros
[bazel-output]: https://bazel.build/remote/output-directories#layout
[bazel-packages]: https://docs.bazel.build/versions/main/build-ref.html#packages
[bazel-sh-binary]: https://docs.bazel.build/versions/main/be/shell.html#sh_binary
[bazel-tools]: https://bazel.build/reference/be/general#genrule.tools
[bazel-variables]: https://bazel.build/reference/be/make-variables#predefined_label_variables
[bazel-workspace]: https://docs.bazel.build/versions/main/build-ref.html#workspace
[blog-bazel-nix]: https://www.tweag.io/blog/2018-03-15-bazel-nix/
[boost-lib]: https://www.boost.org/
[build-and-run]: https://github.com/benradf/space-game#building-and-running
[codelabs]: https://github.com/tweag/nix_bazel_codelab/tree/main#nixbazel-codelab
[common-data]: https://github.com/benradf/space-game/blob/master/common/data/BUILD.bazel
[common-src]: https://github.com/benradf/space-game/blob/master/common/src/BUILD.bazel
[correct-incremental-builds]: https://docs.bazel.build/versions/main/guide.html#correct-incremental-rebuilds
[cpp-modules]: https://en.cppreference.com/w/cpp/language/modules
[dlopen]: https://man7.org/linux/man-pages/man3/dlopen.3.html
[enet-lib]: http://enet.bespin.org/
[exports-files]: https://bazel.build/reference/be/functions#exports_files
[fpic-flag]: https://stackoverflow.com/questions/966960/what-does-fpic-mean-when-building-a-shared-library
[kdtree]: https://en.wikipedia.org/wiki/K-d_tree
[nix-env-install]: https://nixos.org/manual/nix/stable/command-ref/nix-env.html#operation---install
[nixpkgs-22-05]: https://github.com/NixOS/nixpkgs/commit/ce6aa13369b667ac2542593170993504932eb836
[nixpkgs-tags]: https://github.com/NixOS/nixpkgs/tags
[nixpkgs]: https://github.com/NixOS/nixpkgs#readme
[nixpkgs_package]: https://github.com/tweag/rules_nixpkgs#nixpkgs_package
[ogre-converter]: https://github.com/benradf/space-game/blob/master/WORKSPACE.bazel#L264
[resources-bzl]: https://github.com/benradf/space-game/blob/master/common/data/resources.bzl
[rules_nixpkgs]: https://github.com/tweag/rules_nixpkgs#nixpkgs-rules-for-bazel
[shader-program]: https://en.wikipedia.org/wiki/Shader
[space-game]: https://github.com/benradf/space-game#space-game
[uv-map]: https://en.wikipedia.org/wiki/UV_mapping
[write_file]: https://github.com/bazelbuild/bazel-skylib/blob/main/docs/write_file_doc.md
