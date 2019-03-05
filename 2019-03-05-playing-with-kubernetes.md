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

In a project I was working on recently I was tasked with implementing local testing of a kubernetes setup the client was working on. I ended up using [kind](https://github.com/kubernetes-sigs/kind) for this and it worked out nicely. Another tool that I have been meaning to try is [kubenix](https://github.com/xtruder/kubenix). In this post I will give a short overview on a couple of topics:

- Nixifying a small nodejs service
- Creating a docker image the nix way
- Using kind to easily boot up a k8s cluster
- Describing k8s deployments with kubenix

Note that what I am presenting is for motivational purposes and you should certainly put more thought into your setup if you want to take this approach to production.

### A service to deploy: hello

In order to deploy something to kubernetes we first need some service. The service itself is mostly irrelevant for our purposes so we just write a little express based JavaScript app that returns "Hello World" on a port that can be configured via the environment variable `APP_PORT`:

```js
#!/usr/bin/env node

const express = require('express');
const app = express();
const port = process.env.APP_PORT ? process.env.APP_PORT : 3000;


app.get('/', (req, res) => res.send('Hello World'));
app.listen(port, () => console.log(`Listening on port ${port}`));
```

Granted we could just deploy some random public docker image but hey, where would be the fun in that :)

#### Can we nixify this please? Yes we can!

In order to nixify our little [hello-app](./hello-app/index.js) we are going to use
[yarn2nix](https://github.com/moretea/yarn2nix) which makes everything really for us:

```nix
pkgs.yarn2nix.mkYarnPackage {
  name = "hello-app";
  src = ./.;
  packageJson = ./package.json;
  yarnLock = ./yarn.lock;
}
```

We just have to make sure that we add `"bin": "index.js"` to our `package.json` and `mkYarnPackage` will put
`index.js` in the `bin` path of our output. Since we added `#!/usr/bin/env node` to `index.js`, node will also be
added to closure of our app derivation.

#### Creating a docker image of our app

Next we want to create a docker image of our app using [`dockerTools.buildLayeredImage`](https://nixos.org/nixpkgs/manual/#ssec-pkgs-dockerTools-buildLayeredImage):

```nix
  pkgs.dockerTools.buildLayeredImage {
    name = "hello-app";
    tag = "latest";
    config.Cmd = [ "${helloApp}/bin/hello-app" ];
  }
```
`${helloApp}` is of course the derivation we created above using `mkYarnPackage`. Easy as pie.

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

All it takes is `kind create cluster` and setting the correct KUBECONFIG environment variable and we can interact with the cluster via `kubectl`.


### kubenix: validation for free and no yaml in sight either

The [kubenix](https://github.com/xtruder/kubenix) parses a kubernetes configuration in Nix and validates it against the official swagger specification of the designated kubernetes version. Apart from getting a compile-time validation for free, writing kubernetes configurations in Nix allows for much better abstraction and less redundancy which otherwise creeps in all to easy.

For the most part the [configuration.nix](./configuration.nix) is analogous to what would otherwise be written in YAML or JSON. Yet `configuration.nix` actually defines a function and introduces a small let binding:

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
  # ...
}

```

Function takes a `type` argument which is used for augmenting the requested resources of the deployment. Obviously this is just a motivating example. It would also be possible to split bigger configurations into `production.nix` and `development.nix` which both import settings from `generic.nix`. The best solution is the one that works best for your setup and requirements. The very fact that there are now different options to pick from is an advantage over being restricted to a bunch of YAML files. Creating a json output which can be fed into `kubectl` can be created using `kubenix.buildResources`:

```nix
buildConfig = t: kubenix.buildResources { configuration = import ./configuration.nix { type = t; }; };

```

### Applying our configuration

kubenix gives us a validated k8s configuration (try to add some nonesense and you will see that it will actually yell at you) and with kind we can pull up a k8s cluster without any effort. Time to actually apply the configuration. [deploy-to-kind](./nix/deploy-to-kind.nix) does just that.

One thing to worth mentioning about this: The docerized `hello` service is a docker archive, a local .tar.gz archive. When kubernetes is asked to apply a `hello-app:latest` image it will try to fetch it from somewhere. To avoid that from happening we have to do two things:

1. Tell kubernetes to never pull the image: [configuration.nix](./configuration.nix#L27)
2. Make the image available using `kind load image-archive`: [nix/deploy-to-kind.nix](nix/deploy-to-kind.nix#13)

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
