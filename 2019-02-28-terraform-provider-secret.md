---
tags: terraform, secrets
title: Storing secrets in Terraform with `terraform-provider-secret`
author: Jonas Chevalier
---

# Storing secrets in Terraform with `terraform-provider-secret`

Today I want to present to you a little Terraform plugin that I wrote. It was written for [Digital Asset][] and they kindly allowed me to open source it. The plugin does almost nothing but also fills, I believe, an important corner of infrastructure deployment.

But first, let me set a bit of context.

In a scenario where Terraform is being used to deploy cloud infrastructure, some state is maintained that records the view of the world during the last run. To facilitate multi-seat deployments, where multiple people touch the infratrucutre, it's possible to configure the tool to sync that state into a [remote storage](https://www.terraform.io/docs/state/remote.html) like S3 or GCS. In that case, every time Terraform is invoked, the state is fetched and only maintained in memory during the run. At the end of the run, the new state is pushed back into the remote store.

That state might already contain sensitive information such as RDS initial passwords, TLS certs from the TLS providers, Kubernetes certificates from the google_container_cluster. It is therefor a good practice to enable encryption at rest, restrict the access to the remote store to only the people responsible for deployment.

For a strong security stance, it would be best to store the secrets in a dedicated tool like HashiCorp Vault that allow fine-grained access control, audit and dynamic secrets. But security is not absolute. Running Vault takes time and operational knowledge which the team might not have.

In that case, having the secrets stored inside of the Terraform state is actually a good thing. It's better than having them stored inside of the repository and passed as variables.

This is where the [terraform-provider-secret][] plugin comes in. The idea is to introduce a value-hoding resource which only role: to hold secrets. The resource is declared, and then the `terraform import` command is used to import the secrets into the store.

## Quick benefits list

* No secrets stored in code. It means that we can share the infrastructure code with all the employees. Which is nicely in line with the self-help principle of devops.
* No secrets stored on the developer's laptop. It they get stolen, it's just a matter of resetting their GCS or S3 bucket access.
* Secrets encrypted at rest in the remote state (if configure properly)
* Auditable access. Each access to the GCS or S3 bucket can be logged for out-of-band breach correlation
* No need to send screts over Slack.
* Easy secret rotation.

## Installation

To install the plugin, install Go onto the system and then run `go get -u github.com/tweag/terraform-provider-secret`. Make sure to have `$GOPATH/bin` into the `$PATH`.

Or if you are fortunate enough to be using [Nix][], the `terraform-full` package already includes the plugin.

## Usage example

To add a new secret, first add the resource into the code. The resource has no configuration option. In this example we want to store the DataDog API key:

```tf
resource "secret_resource" "datadog_api_key" {}
```

The to actually import the secret, run:

```sh
terraform import secret_resource.datadog_api_key TOKEN
```
where `TOKEN` is the value of the secret token.

At this point the secret is now imported into the state and everybody with access to the remote state store can now read the value. The resource can now be further accessed in the terraform code with `secret_resource.datadog_api_key.value`.

```tf
locals {
  datadog_api_key = "${secret_resource.datadog_api_key.value}"
}
```

That's it.

## Secret rotation

In case where the secret gets leaked or an employee leaves the company, rotating the secret is also quite simple. Unfortunately the operation is not atomic:

```sh
terraform state rm secret_resourc.datadog_api_key
terraform import secret_resourc.datadog_api_key NEW_TOKEN
```

## Misc gotchas

To import multi-line secrets, make sure to escape the right hand side properly:

```sh
terraform import secret_resourc.my_cert "$(cat my_cert.pem)"
```

If the secret contains binary data, use base64 to store the information:

```sh
terraform import secret_resource.gce_service_account_key "$(base64 service_account.key)"
```

And then in the terraform code, use the `base64decode()` interpolation function to get back the binary value:

```tf
locals {
  gce_service_account_key = "${base64decode(secret_resource.gce_service_account_key.value)}"
}
```

The values are marked as "sensitive" which means that they won't turn up in the terraform logs by default. Still, it's easy to reference the value and the "sensitive" attribute doesn't propagate so some precaution is still required.

## Conclusion

Before using this plugin, make sure to properly secure the remote state. More instructions on how to do so is available at https://www.terraform.io/docs/state/sensitive-data.html

Terraform is not designed as a security tool, unlike for example HashiCorp Vault, but depending on your security requirements, is already a step up from having secrets recorded into code. All the secrets are now stored in a single place which makes it easier to handle and audit.

I hope that this little plugin will prove to be useful in your toolbox.

## Sponsorship

Thank you to [Digital Asset][] for sponsoring this work and releasing it under an open source license for the benefit of everyone.


[Digital Asset]: https://digitalasset.com
[nix]: https://nixos.org/nix/
