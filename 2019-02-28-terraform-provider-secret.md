---
tags: terraform, secrets
title: Securely storing secrets in Terraform<br/> with terraform-provider-secret
shortTitle: Storing secrets in Terraform
author: Jonas Chevalier
---

[digital-asset]: https://www.digitalasset.com
[encryption-at-rest]: https://www.terraform.io/docs/state/sensitive-data.html
[vault]: https://www.vaultproject.io
[terraform-provider-secret]: https://github.com/tweag/terraform-provider-secret
[nix]: https://nixos.org/nix
[remote-state]: https://www.terraform.io/docs/state/sensitive-data.html

I want to present you a small Terraform plugin for securely managing secrets that was written for [Digital Asset][digital-asset], who kindly allowed me to open source it.
The plugin is simple, but fills an important gap on infrastructure deployment with Terraform.
The general idea is to protect secrets by making use of Terraform's _state_.

Terraform maintains the state of the world at the moment of a deployment in _state files_.
In the case of multi-seat deployments, where multiple people work with the same infrastructure, these files are often synced into a [remote storage](https://www.terraform.io/docs/state/remote.html) like S3 or GCS.
Every time Terraform is invoked, the state is fetched and maintained in memory.
When the run is finished, the new state is pushed back into the remote store.

That state might contain sensitive information such as RDS initial passwords, TLS certificates or Kubernetes certificates from a `google_container_cluster`.
It is therefore a good practice to enable [encryption at rest][encryption-at-rest] and restrict access to the remote store only to the people responsible for deployment.

Ideally, one should store secrets in a dedicated tool like [HashiCorp Vault][vault] that allows fine-grained access control, auditing and dynamic secrets.
However, managing a Vault instance takes time and operational knowledge, which the team might not have.
In this case, having the secrets stored inside of the Terraform state is actually a good thing since it is strictly better than having them stored inside of a repository and passed as environment variables.

This is where the [`terraform-provider-secret`][terraform-provider-secret] plugin comes in.
The idea is to introduce a value-holding resource whose role is to store secrets.
The resource is declared in code, and the `terraform import` command is used to import the secrets into the store.

This plugin brings many benefits:

* No secrets stored in code. This means that we can share the infrastructure code with all the employees, which is nicely in line with the self-help principle of devops.
* No secrets stored on the developer's laptop. If it gets stolen, it's just a matter of resetting their GCS or S3 bucket access.
* Secrets are encrypted at rest in the remote state, if configured properly.
* Auditable access. Each access to the GCS or S3 bucket can be logged for out-of-band breach correlation.
* No need to send secrets over Slack.
* Easy secret rotation.

## Installation

The plugin depends on Go being installed on your system.
Make sure to have `$GOPATH/bin` in your `$PATH` and run `go get -u github.com/tweag/terraform-provider-secret`.

If you are fortunate enough to be using [Nix][nix], the `terraform-full` package already includes the plugin.

## Usage example

Before using this plugin, make sure to properly secure the remote state.
More instructions on how to do so is available [here][remote-state].

To add a new secret, first declare a `secret_resource` resource in the Terraform code.
It has no configuration options.
In this example we want to store the DataDog API key:

```tf
resource "secret_resource" "datadog_api_key" {}
```

To actually import the secret, run:

```sh
terraform import secret_resource.datadog_api_key TOKEN
```

where `TOKEN` is the value of the secret token.

Now the secret is imported into the remote state and everybody with access to it can now read the value.
The resource can also be accessed in the Terraform code with `secret_resource.datadog_api_key.value`.

```tf
locals {
  datadog_api_key = "${secret_resource.datadog_api_key.value}"
}
```

That's it!

## Secret rotation

In case where the secret gets leaked or an employee leaves the company, rotating the secret is also quite simple.

```sh
terraform state rm secret_resourc.datadog_api_key
terraform import secret_resourc.datadog_api_key NEW_TOKEN
```

_Note_: This operation is unfortunately not atomic.

## Misc gotchas

To import multi-line secrets, make sure to escape them properly:

```sh
terraform import secret_resourc.my_cert "$(cat my_cert.pem)"
```

If the secret contains binary data, use `base64` to store the information:

```sh
terraform import secret_resource.gce_service_account_key "$(base64 service_account.key)"
```

and use the `base64decode()` interpolation function in the Terraform code to get back the binary value:

```tf
locals {
  gce_service_account_key = "${base64decode(secret_resource.gce_service_account_key.value)}"
}
```

These values are marked as "sensitive" which means that they won't turn up in the Terraform logs by default.
However, the "sensitive" attribute doesn't propagate though references, therefore some precaution is required when referring to secrets.

## Conclusion

Terraform is not designed as a security tool, unlike for example HashiCorp Vault.
But depending on your security requirements, using it in conjuction with this plugin is already a step up from having secrets recorded into code.
All the secrets are now stored in a single place which makes it easier to handle and audit.

I hope that this little plugin will prove to be useful in your toolbox.

## Sponsorship

Thank you to [Digital Asset][digital-asset] for sponsoring this work and releasing it under an open source license for the benefit of everyone.
