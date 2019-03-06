---
title: "Playing With Kubernetes: Nix, Kind And Kubenix"
shortTitle: "Nix, Kind And Kubenix"
author: Tobias Pflug
---

<meta name="twitter:card" content="summary_large_image" />
<meta name="twitter:site" content="@tweagio" />
<meta name="twitter:creator" content="@tweagio" />
<meta property="og:url" content="https://www.tweag.io/posts/2019-03-05-playing-with-kubernetes.html" />
<meta property="og:title" content="Playing With Kubernetes: Nix, Kind And Kubenix" />

In a recent project I was tasked with creating a local testing environment for a kubernetes cluster the client was actively working on.
The main requirements were:

- **Cross platform**: It should work on Linux and macOS.
- **Ease of use**: The setup should be easy to use.
- **Suitable for CI integration**: It should be possible to use the setup as part of a CI pipeline.

Two options come to mind, both of which have to be dismissed:

- **minikube**: minikube is well established as a tool for local kubernetes clusters but its reliance on a hypervisor rules it out.
- **nixos/qemu**: NixOS makes it easy to build and start arbitrary configurations via qemu but this obviously is not cross platform and thus not an option.

Instead I discovered [kind](https://github.com/kubernetes-sigs/kind):

- Depends on docker only
- Works on Linux, macOS and even Windows
- Supports multi-node (including HA) clusters

In the following I will describe how to use kind in combination with [kubenix](https://github.com/xtruder/kubenix) to create a validated
kubernetes configuration, deploy it to a kind cluster and finally execute a simple smoke test. A simple "hello world" node service will
run on the cluster and I will also demonstrate how to nixify and dockerize it. Simply to showcase how easy and convenient this is with Nix.

**Note** that what I am presenting is for motivational purposes and you should certainly put more thought into your setup if you want to
take this approach to production. The full source code of this project is available on [GitHub](https://github.com/gilligan/kind-kubenix/tree/master).

### A service to deploy: hello

In order to deploy something to kubernetes we first need some service. The service itself is mostly irrelevant for our purposes so we
just write a little express based JavaScript app that returns "Hello World" on a port that can be configured via the environment
variable `APP_PORT`:

```js
#!/usr/bin/env node

const express = require('express');
const app = express();
const port = process.env.APP_PORT ? process.env.APP_PORT : 3000;


app.get('/', (req, res) => res.send('Hello World'));
app.listen(port, () => console.log(`Listening on port ${port}`));
```

#### Can I get a nix package please?

In order to nixify the little [hello-app](https://github.com/gilligan/kind-kubenix/blob/master/hello-app/index.js) I am going to 
use [yarn2nix](https://github.com/moretea/yarn2nix):

```nix
pkgs.yarn2nix.mkYarnPackage {
  name = "hello-app";
  src = ./.;
  packageJson = ./package.json;
  yarnLock = ./yarn.lock;
}
```

I made sure to add `"bin": "index.js"` to `package.json` so `mkYarnPackage` will put `index.js` in the `bin` path of the resulting output.
Thanks to the shebang (`#!/usr/bin/env node`) in `index.js` Nix is able to figure out that node is a runtime dependency of "hello-app"
all by its own. 

#### Creating a docker image

Kubernetes runs docker images so the little express service has to be dockerized. Time to start writing a Dockerfile? No, I prefer using  
[`dockerTools.buildLayeredImage`](https://nixos.org/nixpkgs/manual/#ssec-pkgs-dockerTools-buildLayeredImage):

```nix
  pkgs.dockerTools.buildLayeredImage {
    name = "hello-app";
    tag = "latest";
    config.Cmd = [ "${helloApp}/bin/hello-app" ];
  }
```
`${helloApp}` is the derivation that was created above using `mkYarnPackage`.

### Cluster in a box: kind

kind is a portable (linux, osx and windows) solution to running kubernetes clusters locally, in a docker container. The project
is still young but it is getting a lot of support and works very well already:

```
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

All it takes is `kind create cluster` and setting the correct KUBECONFIG environment variable and we can interact with the cluster 
via `kubectl`.


### kubenix: validation for free and no yaml in sight either

The [kubenix](https://github.com/xtruder/kubenix) parses a kubernetes configuration in Nix and validates it against the official swagger
specification of the designated kubernetes version. But the free compile-time validation isn't the only benefit: With deployments configured 
in JSON or YAML we have a file as smallest unit to work with. There is little that can be done in terms of reuse, composition or abstraction
in general. This leads to a lot of redundancy and often big files that are error-prone to work with.

In my [configuration.nix](https://github.com/gilligan/kind-kubenix/blob/master/configuration.nix) i am making some use of this. I don't 
like repeating myself and I also hate typos in labels breaking references between services and pods. Using Nix I can avoid this or at 
the very least turn runtime errors into compile-time errors:

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
the deployment. Obviously this is just a motivating example. It would also be possible to split bigger configurations into 
`production.nix` and `development.nix` which both import settings from `generic.nix`. The best solution is the one that works best 
for your setup and requirements. The very fact that there are now different options to pick from is an advantage over being restricted 
to a bunch of YAML files. Creating a json output which can be fed into `kubectl` can be created using `kubenix.buildResources`:

```nix
buildConfig = t: kubenix.buildResources { configuration = import ./configuration.nix { type = t; }; };

```

### Applying our configuration

kubenix gives us a validated k8s configuration (try to add some nonesense and you will see that it will actually yell at you) and with
kind we can pull up a k8s cluster without any effort. Time to actually apply the configuration. [deploy-to-kind](./nix/deploy-to-kind.nix)
does just that.

One thing to worth mentioning about this: The dockerized `hello` service is a docker archive, a local .tar.gz archive. When kubernetes 
is asked to apply a `hello-app:latest` image it will try to fetch it from somewhere. To avoid that from happening we have to do two things:

1. Tell kubernetes to never pull the image: [configuration.nix](https://github.com/gilligan/kind-kubenix/blob/master/configuration.nix#L27)
2. Make the image available using `kind load image-archive`: [nix/deploy-to-kind.nix](https://github.com/gilligan/kind-kubenix/blob/master/nix/deploy-to-kind.nix#L13)

With that in place the deployment will work just fine.

### Finishing Up

The `default.nix` of the project exposes the following attributes:

- `app`: The nodejs service. It can be build via `nix-build -A`.
- `deploy-to-kind`: A script that starts a kind cluster and deploys `configuration.nix`.
- `test-deployment`: A script that implements some very simplistic smoke test to check if our app is up and working.
- `deploy-and-test`: Running this shell via `nix-shell -a deploy-and-test default.nix` will deploy, wait for the deployment and finally test it.
- `shell`: Started via `nix-shell` this shell provides all required inputs for manually deploying and testing.

**Notes**:
- The version of `kind` used in this project is built from the master revision at the time of writing. The latest release doesn't include the `kind load` functionality.
- kubenix currently doesn't have any documentation but a major overhaul with great features is in the works. Follow [kubenix refactoring](https://github.com/xtruder/kubenix/issues/9) for details.
- I used [wait-for-deployment](https://github.com/timoreimann/kubernetes-scripts) - a nice little bash script - to wait for the completion of the deployment.
