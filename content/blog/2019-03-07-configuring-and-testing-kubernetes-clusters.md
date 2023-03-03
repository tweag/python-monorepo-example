---
redirect_from:
  [/posts/2019-03-07-configuring-and-testing-kubernetes-clusters.html]
title: "Configuring and testing Kubernetes clusters   with KubeNix and kind"
shortTitle: "Kubernetes, KubeNix and kind"
author: Tobias Pflug
tags: [nix, devops]
---

In a recent project I was tasked with creating a local testing environment for a Kubernetes cluster the client was actively working on.
The main requirements were:

- **Cross platform**: It should work on Linux and macOS.
- **Ease of use**: The setup should be easy to use.
- **Suitable for CI integration**: It should be possible to use the setup as part of a CI pipeline.

Two options came to mind, both of which had to be dismissed:

- **Minikube**: Minikube is a well established tool for local Kubernetes clusters but its reliance on a hypervisor rules it out.
- **NixOS/QEMU**: NixOS makes it easy to build and start arbitrary configurations via QEMU but this obviously is not cross-platform and thus not an option.

Instead we'll use [kind](https://github.com/kubernetes-sigs/kind), which:

- Depends on Docker only
- Works on Linux, macOS and even Windows
- Supports multi-node (including HA) clusters

In the following I will guide you through an [example project](https://github.com/gilligan/kind-kubenix) which will illustrate how kind can
be combined with [KubeNix](https://github.com/xtruder/kubenix) to develop correct, easy to maintain, and easy to test
Kubernetes deployments. This will include:

- implementing and nixifying a simple NodeJS service,
- using Nix tooling to build minimal Docker images,
- using kind to boot up a Kubernetes cluster with minimal effort,
- using KubeNix to catch common configuration errors early, eschew
  copypasta and make configurations composable,
- deploying the configuration created by KubeNix to the kind cluster.

**Note**: What I am presenting is for motivational purposes and you should certainly put more thought into your setup if you want to
take this approach to production. The full source code of this project is available on [GitHub](https://github.com/gilligan/kind-kubenix/tree/master).

## Implementing a simple nodejs service

The first thing we need is a service to actually deploy to Kubernetes. The service itself is mostly irrelevant for our purposes so
it will just be an Express based JavaScript app that returns "Hello World" on a port that can be configured via the environment
variable `APP_PORT`:

```js
#!/usr/bin/env node

const express = require("express")
const app = express()
const port = process.env.APP_PORT ? process.env.APP_PORT : 3000

app.get("/", (req, res) => res.send("Hello World"))
app.listen(port, () => console.log(`Listening on port ${port}`))
```

### Nixifying the service

In order to nixify the little [`hello-app`](https://github.com/gilligan/kind-kubenix/blob/master/hello-app/index.js) I am going to
use [yarn2nix](https://github.com/moretea/yarn2nix):

```nix
pkgs.yarn2nix.mkYarnPackage {
  name = "hello-app";
  src = ./.;
  packageJson = ./package.json;
  yarnLock = ./yarn.lock;
}
```

I made sure to add `"bin": "index.js"` to `package.json` so that `mkYarnPackage` will put `index.js` in the `bin` path of the resulting output.
Thanks to the shebang `#!/usr/bin/env node` in `index.js` Nix is able to figure out that Node is a runtime dependency of `hello-app`
all by its own.

### Using Nix to build Docker images

Kubernetes runs Docker images, so the little Express service has to be dockerized. The traditional way to achieve this would be to write
a `Dockerfile`. Nix, however, provides more convenient and declarative tooling for building Docker images.
The snippet below shows how to use [`dockerTools.buildLayeredImage`](https://nixos.org/nixpkgs/manual/#ssec-pkgs-dockerTools-buildLayeredImage)
to build a minimal docker image containing our app and nodejs:

```nix
  pkgs.dockerTools.buildLayeredImage {
    name = "hello-app";
    tag = "latest";
    config.Cmd = [ "${helloApp}/bin/hello-app" ];
  }
```

The snippet above generates a Docker archive - a `.tar.gz` file which could be loaded into the Docker daemon using `docker load`. Notice
how the only arguments given are the image name, the image tag and command to be executed in the container. There is no need to
provide a sequence of commands to populate the container and no explicit contents configuration either. Instead, the contents can
be automatically derived from `config.Cmd`: The image will include everything that is required to execute `hello-app` - i.e the
closure of the `helloApp` derivation. This also explains why there is no need to specify a base image (`FROM node:10` in a Dockerfile):
The `helloApp` derivation brings along nodejs as runtime dependency.

Much more could be said about the benefits of this way to build Docker images but it would go beyond the scope of this blog post.
Suffice to say that with 4 lines of code, no additional tooling and no reliance on external base images I'm able to create a Docker image
that contains only exactly what is needed.

## kind: Kubernetes clusters in Docker

kind is a portable (Linux, macOS and Windows) solution to running Kubernetes clusters locally, in a Docker container. The project
is still young but it is getting a lot of support and works very well already:

```
$ kind create cluster
Creating cluster "kind" ...
 ✓ Ensuring node image (kindest/node:v1.13.3) 🖼
 ✓ [control-plane] Creating node container 📦
 ✓ [control-plane] Fixing mounts 🗻
 ✓ [control-plane] Configuring proxy 🐋
 ✓ [control-plane] Starting systemd 🖥
 ✓ [control-plane] Waiting for docker to be ready 🐋
 ✓ [control-plane] Pre-loading images 🐋
 ✓ [control-plane] Creating the kubeadm config file ⛵
 ✓ [control-plane] Starting Kubernetes (this may take a minute) ☸
Cluster creation complete. You can now use the cluster with:

export KUBECONFIG="$(kind get kubeconfig-path --name="kind")"
kubectl cluster-info
```

The above command takes roughly 35sec on my Dell XPS laptop. After that, the cluster is up and running and you can interact with it
via `kubectl` once you set the `KUBECONFIG` as described in the output above. Clusters can of course also be deleted again using
`kind delete cluster`. This is almost everything that needs to be said about kind at this point - Later on I will also mention the
ability to preload Docker images using `kind load` but this isn't yet relevant.

What _is_ important is the fact that creating, deleting and interacting with Kubernetes clusters via kind is trivial, has no dependencies
beyond Docker and thus works on Linux, macOS and Windows.

## KubeNix: Validation for free and no YAML in sight either

The [KubeNix](https://github.com/xtruder/kubenix) parses a Kubernetes configuration in Nix and validates it against the official Swagger
specification of the designated Kubernetes version. Furthermore it changes the way in which you can work with, and organize your
deployment configuration.

With deployments configured in JSON or YAML, files are the smallest unit to work with. There is little that can be done in terms of
reuse, composition or abstraction in general. This leads to a lot of redundancy and often big files that are error-prone to work with.
I don't like repeating myself and I also hate typos in labels breaking references between services and pods. Using Nix I can avoid this
or at the very least turn runtime errors into compile-time errors:

```nix
{ type ? "dev" }:

let
  kubeVersion = "1.11";

  helloApp = rec {
    label = "hello";
    port = 3000;
    cpu = if type == "dev" then "100m" else "1000m";
    imagePolicy = if type == "dev" then "Never" else "IfNotPresent";
    env = [{ name = "APP_PORT"; value = "${toString port}"; }];
  };
in
{
  kubernetes.version = kubeVersion;

  kubernetes.resources.deployments."${helloApp.label}" = {
    metadata.labels.app = helloApp.label;
    spec = {
      replicas = 1;
      selector.matchLabels.app = helloApp.label;
      template = {
        metadata.labels.app = helloApp.label;
        spec.containers."${helloApp.label}" = {
          name = "${helloApp.label}";
          image = "hello-app:latest";
          imagePullPolicy = helloApp.imagePolicy;
          env = helloApp.env;
          resources.requests.cpu = helloApp.cpu;
          ports."${toString helloApp.port}" = {};
        };
      };
    };
  };

  kubernetes.resources.services."${helloApp.label}" = {
    spec.selector.app = "${helloApp.label}";
    spec.ports."${toString helloApp.port}".targetPort = helloApp.port;
  };
}
```

`configuration.nix` actually contains a function that takes a `type` argument which is used for augmenting the requested resources of
the deployment. This is just a motivating example, but it would also be possible to split bigger configurations into
`production.nix` and `development.nix`, which both import settings from `generic.nix`. The best solution is the one that works best
for your setup and requirements. The very fact that there are now different options to pick from is an advantage over being restricted
to a bunch of YAML files.

## Applying a configuration

Despite all the benefits of using Nix to describe deployments that were outlined above, `kubectl` still only consumes JSON or YAML input.
The [default.nix](https://github.com/gilligan/kind-kubenix/blob/master/default.nix#L13) in the root of the example project defines a
function that uses `kubenix.buildResources` to generate schema-validated JSON output that can be fed into `kubectl`:

```nix
{
    buildConfig = t: kubenix.buildResources { configuration = import ./configuration.nix { type = t; }; };
}

```

The [deploy-to-kind](https://github.com/gilligan/kind-kubenix/blob/master/nix/deploy-to-kind.nix) script uses the output
of `buildConfig "dev"` and pipes it into `kubectl`. You can try it by entering the Nix shell at the root of the project:

```
$ nix-shell
$ deploy-to-kind
```

One thing worth mentioning about this: The dockerized `hello` service is a Docker archive, a local `.tar.gz` archive. When Kubernetes
is asked to apply a `hello-app:latest` image it will try to fetch it from somewhere. To avoid that from happening we have to do two things:

1. Tell Kubernetes to never pull the image: [configuration.nix](https://github.com/gilligan/kind-kubenix/blob/master/configuration.nix#L27)
2. Make the image available using `kind load image-archive`: [nix/deploy-to-kind.nix](https://github.com/gilligan/kind-kubenix/blob/master/nix/deploy-to-kind.nix#L13)

With that in place the deployment will work just fine.

## Finishing Up

The `default.nix` of the project exposes the following attributes:

- `app`: The nodejs service. It can be build via `nix-build -A app`.
- `deploy-to-kind`: A script that starts a kind cluster and deploys `configuration.nix`.
- `test-deployment`: A script that implements a very simplistic smoke test to check if our app is up and working.
- `deploy-and-test`: Running this shell via `nix-shell -A deploy-and-test default.nix` will deploy, wait for the deployment and finally test it.
- `shell`: Started via `nix-shell` this shell provides all required inputs for manually deploying and testing.

### Conclusions

- kind is an easy to use, cross-platform solution for running local Kubernetes clusters with minimal runtime dependencies.
- KubeNix makes working with Kubernetes configuration files more
  convenient and safer at the same time (more errors are caught early).
- Nix is a powerful framework with which all the moving parts of a project can be orchestrated. From the provisoning of all required run and build-time dependencies to the streamliend creation of Docker images, Nix is the underlying concept making KubeNix possible in the first place.

**Notes**:

- The version of `kind` used in this project is built from the master revision at the time of writing. The latest release doesn't include the `kind load` functionality.
- KubeNix currently doesn't have any documentation but a major overhaul with great features is in the works. Follow [KubeNix refactoring](https://github.com/xtruder/kubenix/issues/9) for details.
- I used [wait-for-deployment](https://github.com/timoreimann/kubernetes-scripts) - a nice little bash script - to wait for the completion of the deployment.
- yarn2nix might be [removed from Nixpkgs soon](https://github.com/NixOS/nixpkgs/issues/20637#issuecomment-466901820). Once that happens there are still various ways to continue using it, but the code presented here is not going to work as-is anymore.