---
title: "First release of Nickel"
shortTitle: "First release of Nickel"
author: Yann Hamdaoui
tags: [nix, nickel]
---

I am excited to announce the first release of [Nickel][nickel-repo]! In the
original introductory [blog post][nickel-1] blog post, I've written about why we, at
Tweag, are developing yet a new configuration language. Our goal is to empower
people to write correct, programmable and maintainable configurations. Nickel
targets Nix, Infrastructure-as-Code, build systems, and more. We think such a
tool would greatly help taming the growing complexity of configuring systems.

Since the first blog post, we have made progress. We backed up and changed our
mind sometimes. We had a lot of discussions, and we also experimented quite a
bit. There is still a lot to do and many imperfect aspects of the language to
improve.

Still, I think we reached a point where Nickel is consistent enough for people
to try it more seriously. That way we can get our target users involved and not
lose sight of their actual issues and wishes.

Beware: this is a `0.1` release. There is no backward-compatibility guarantees
for this time, because we don't want to tie our hands too early and be stuck
with retrospectively unfortunate decisions. Of course, we'll try to minimize
breaking changes as much as possible. You shouldn't use this version in anything
remotely in production yet, however, we encourage you to try Nickel wherever you
think makes sense: personal projects, experimenting outside of the production
environment, etc.

## What's here

To try the examples from this section, you can use the
[online playground][playground].

### Basics

Apart from a few syntax differences, basic Nickel is close to JSON. Here is
an example of a service configuration:

```nickel
{
  kind = "Service",
  apiVersion = "v1",
  route = "/api/v1",
  metadata.name = "phabricator",

  service = {
    selector.name = "phabricator",
    ports = [
      {port = 80, protocol = "http"},
      {port = 443, protocol = "https"}
    ]
 }
}
```

Nickel has the same primitive data types than JSON: numbers, string, lists and
records (objects in JSON).

At the difference of JSON, Nickel is programmable. You can use variables to have
a single source of truth for your data. You can use functions to transform and
describe dependencies between data. This example has several repetitions, and
while it is fabricated, those repetitions are actually quite frequent in
real-life configurations, for example in Kubernetes. Repetitions can lead to
inconsistencies and make modifying data a burden. Let's see how we can get rid of them
in Nickel:

```nickel
let app_name = "phabricator" in

let protocol_from_port = fun port =>
  if port == 80 then "http"
  else if port == 443 then "https"
  else "" in

{
  kind = "Service",
  apiVersion = "v1",
  route = "%{app_name}/api/%{apiVersion}",
  metadata.name = app_name,

  service = {
    selector.name = app_name,
    ports = array.map
      (fun p => {port = p, protocol = protocol_from_port p})
      [80, 443],
  }
}
```

`let app_name = "phabricator" in ...` defines an immutable variable `app_name`
with value `"phabricator"`. `fun port => ...` defines a function of one argument
named `port`. The string interpolation `%{app_name}` substitutes `app_name` by
its value inside the string.

In this new version,

- The name `phabricator` is now centralized in `app_name`. It ensures that all
  the occurrences are the same, and that changing the name only amounts to
  updating `app_name`.
- The array of records `ports` is now generated, and each `protocol` field is
  deduced automatically from the corresponding port number using the function
  `protocol_from_port`. Generating `ports` ensures each port number and its
  associated protocol are consistent, and make modification easy (just modify
  directly the port list).
- The `route` field refers to another field of the same record, `apiVersion`.
  In Nickel, records are _recursive by default_, meaning that fields can freely
  refer to each others. This makes it simple to express inter-dependencies of
  fields. Here, indeed, the `route` depends on the `apiVersion`. If we change
  the api version, the route will automatically stay in sync.

### Typing

The previous code is dynamically typed, which is the default in Nickel. This
makes it easy to write a configuration. However, dynamic type
errors can be quickly hard to debug, especially when using functions.

To help in writing bug free code, Nickel features a gradual type system. In
practice, this means you have the possibility to leverage static typing by
simply annotating a particular block, typically a function, with a type. The
typechecker will then rigorously verify this block. Here is a tweak of our
previous `protocol_from_port` (I changed the `""` for `null` on the last line)
with a type annotation:

```nickel
# In file wrong.ncl
let protocol_from_port : Num -> Str = fun port =>
  if port == 80 then "http"
  else if port == 443 then "https"
  else null in
```

If we try to evaluate this program, the typechecker will rightfully reject it.
Indeed `null` is not a valid value for a string:

```
$ nickel -f wrong.ncl
error: incompatible types
  ┌─ repl-input-7:4:8
  │
4 │   else null in
  │        ^^^^ this expression
  │
  = The type of the expression was expected to be `Str`
  = The type of the expression was inferred to be `Dyn`
```

### Contracts (schemas)

The end result of the evaluation of a Nickel program is typically a JSON file
(or YAML, or TOML). This file is then fed to a system (e.g. Kubernetes). This
file must certainly follow the requirements imposed by this system. Such
requirements can usually be expressed as data schemas, specifying which fields
are allowed, which are required and what kind of data can go in each field.

Part of such requirements can be enforced using types. However, a large number
of them are out of reach of static typing. For example, take a [random field
from the kubernetes openAPI specification][kubernetes-openapi-serveraddr]:

```json
"serverAddress": {
  "default": "",
  "description": "[..] This can be a hostname, hostname:port, IP or IP:port.",
  "type": "string"
}
```

The last part of the description is a good example: being a string containing a
hostname, hostname:port, IP or IP:port. Not only this is out of reach of most
static type systems, but openAPI is not able to express this property either!
The `type` field just indicates `string`, which is much less precise.

Nickel can express the full actual specification of `serverAddress` using contracts.
Contracts are a validation system that can enforce arbitrary properties (you can
provide your own validation functions). Contracts are designed to be written in an intuitive way,
like schemas:

```nickel
# We assume the boolean functions is_hostname and is_ip are defined before
let Address = contract.from_predicate (fun value =>
  builtin.is_string value &&
  (value == "" ||
   is_hostname value ||
   is_ip value) in

# Defines a simple schema for our configuration
let Schema = {
    serverAddress
      | Address
      | doc "[..] This can be a hostname, hostname:port, IP or IP:port."
      | default = "",
    # ... rest of the contract
} in

{
  serverAddress = "192.168.0.0.0",
  # ... rest of the configuration
} | Schema
```

If we try to export this program, we get a contract violation, because our IP is
not valid (there is one extra number):

```
error: contract broken by a value
   ┌─ :1:1
   │
 1 │ Address
   │ ------- expected type
   │
   ┌─ repl-input-2:11:19
   │
11 │   serverAddress = "192.168.0.0.0",
   │                   ^^^^^^^^^^^^^^^ applied to this expression
   │
[..]

note:
  ┌─ repl-input-2:3:9
  │
3 │       | Address
  │         ^^^^^^^ bound here

```

I went for a simple example, but I could have used custom messages to make the
error even more helpful. Thanks to contracts, invalid configurations can be
caught early, instead of downstream the pipeline when trying to deploy our
services. Beyond just making our deployment work, we can even imagine using
contracts for enforcing additional properties, such as security-related rules.

### Merging

On last important aspect of Nickel is the merge operation, written `&`. Merging
combines records recursively and provides a way of writing modular
configurations:

```nickel
# file service.ncl
{
  name = "phabricator",
  kind = `Service,
  firewall.openPorts = [80, 443],
}

# file security.ncl
{
  useFirewall = true,
  firewall = {
    use = "iptables",
    openPorts | default = [],
    allowedProtocols = array.map protocol_from_port openPorts,
  }
}

# file main.ncl
let service = import "service.ncl" in
let security = import "security.ncl" in
service & security
```

Here we split security options from the service definition. The merge operator
recursively combines the two resulting in the following JSON:

```json
"{
  "firewall": {
    "use": "iptables",
    "openPorts": [
      80,
      443
    ],
    "allowedProtocols": [
      "http",
      "https"
    ]
  },
  "kind": "Service",
  "name": "phabricator",
  "useFirewall": true
}"
```

`firewall` has been specified by pieces in the two different files and combined
by merging. The service needs to open some ports: the `firewall.ncl` closes them
all by default (by defining `openPorts = []`, but makes this option overridable
thanks to the `default` annotation. What's more, merging plays well with
recursive records, as it automatically updated the value of `allowedProtocols`,
which depends on `openPorts`.

### Going further

You can look at the main [README][nickel-repo] for a general description of the
project. The first [blog post series][nickel-1] explains the inception of
Nickel, and the following posts focus on specific aspects of the language. For a
(very) condensed version of this blog post, see the
[release notes][release-notes]. Finally, the most complete source remains the
[user manual][user-manual].

## What's not here (yet)

We've been mostly focusing on designing and implementing the core language.

### Tooling and documentation

Tooling and documentation are here but not full-fledged yet. Still, check out
the [vim plugin][vim-nickel] or the [VSCode][nickel-vscode] plugin, and the
editor-agnostic [LSP server][nickel-lsp]. For documentation, you can look at the
[website][nickel-website], and in particular the [user manual][user-manual].

### Performance

Not much effort has been put into making the interpreter fast or memory savvy.
This is a subject we plan to work on in the future.

### Nickel and Nix

Although Nickel is at heart a generalist configuration language, Nix has been a
distinguished application from day one and one of the original motivation. We
think Nickel could make an impact there and really improve the user experience.

There are several possible approaches to bring integration of Nickel within Nix,
with varying power, ergonomics and required effort. Some approaches would
require to modify either Nix itself, Nickel, or both.

For the time being, we haven't yet worked out one robust, practical and powerful
solution to use Nickel as a front-end for Nix development. However, we have been
actively thinking about it. And now, Nix integration is the very next step on
the roadmap, the aspect we want to fully focus on, in parallel to collecting
feedbacks and usage reports to help building the future of Nickel.

In the meantime, we open-sourced the result of our experiments of writing a
simple Nix shell in Nickel. The idea has been to find the more direct route,
involving only pure Nix and Nickel, and without augmenting either the Nix or the
Nickel interpreter. The current code is experimental at best, but may serve as a
basis for more Nickel in Nix, for the more adventurous among you:
[nickel-nix][nickel-nix].

## Conclusion

We are happy to announce the first release of the Nickel configuration language.
You can use it wherever you would use JSON, YAML or TOML, but felt the limit of
using static text or ad-hoc and limited templating languages.

This release is both usable and not ready for production yet. We are seeking for
feedbacks, ideas and opinions. Yours are invaluable: please use Nickel, break
it, do cool things we haven't even imagined, and most importantly, please let us
know about it!

[nickel-1]: https://www.tweag.io/blog/2020-10-22-nickel-open-sourcing/
[nickel-repo]: https://github.com/tweag/nickel/
[user-manual]: https://nickel-lang.org/user-manual/
[nickel-lsp]: https://github.com/tweag/nickel/tree/master/lsp
[nickel-vscode]: https://github.com/tweag/nickel/tree/master/lsp#vs-code
[nickel-nix]: https://github.com/nickel-lang/nickel-nix
[nickel-website]: https://www.nickel-lang.org
[release-notes]: https://github.com/tweag/nickel/blob/master/RELEASES.md
[vim-nickel]: https://github.com/nickel-lang/vim-nickel
[vim-vscode]: https://github.com/tweag/nickel/tree/master/lsp#vs-code
[kubernetes-openapi-serveraddr]: https://github.com/kubernetes/kubernetes/blob/a41f9e976da10af28169cbbfebbce5ad4ba965f0/api/openapi-spec/v3/apis__networking.k8s.io_openapi.json#L83
[kubernetes-example]: https://github.com/kubernetes/examples/blob/6a762deb16a2625584fe3d85ce5194ef58c14429/staging/phabricator/phabricator-service.json
[playground]: https://nickel-lang.org/playground/
