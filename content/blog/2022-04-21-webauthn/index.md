---
title: Announcing WebAuthn
author: Erin van der Veen and Silvan Mosberger
tags: [haskell]
description: "Announcement of a WebAuthn Haskell library, and a description of how the protocol works"
---

Tweag and [Mercury](https://mercury.com/) are happy to announce a server-side library for the the [WebAuthn](https://www.w3.org/TR/webauthn-2/) specification (part of the [FIDO2](https://fidoalliance.org/fido2/) project), available as [webauthn](https://hackage.haskell.org/package/webauthn) on Hackage!

This library has been developed by a team at Tweag, contracted by [Mercury](https://mercury.com/). Mercury felt that Webauthn support was a missing element in the Haskell ecosystem, and wanted to contribute an open-source library to fill this gap.
The library builds upon two previous prototypes, in concertation with their authors: a [hackathon project](https://github.com/arianvp/haskell-fido2) by [Arian van Putten](https://github.com/arianvp) and an [alternative implementation](https://github.com/fumieval/webauthn) by [Fumiaki Kinoshita](https://github.com/fumieval) (also known as [webauthn-0](https://hackage.haskell.org/package/webauthn-0) on Hackage).

In the rest of the blog post we will mostly focus on introducing WebAuthn itself.

## The problems with passwords, TOTP and SMS 2FA

Passwords are the dominant way to log into web services, but they have problems, both for users and developers.
Users have to choose a password that is not easy to guess and then remember it.
A password manager solves this problem, but introduces another attack vector.
Furthermore, passwords can be compromised with phishing attempts.

Developers have to handle passwords with care, using a good [key derivation function](https://en.wikipedia.org/wiki/Key_derivation_function) involving hashing and salts, only transferring the resulting key for storage in a secure database.
This can be very error prone, see the [OWASP cheat sheet for password storage](https://cheatsheetseries.owasp.org/cheatsheets/Password_Storage_Cheat_Sheet.html) for the current best practices.

[TOTP](https://datatracker.ietf.org/doc/html/rfc6238) and SMS-based two-factor authentication methods provide additional security over standalone passwords.
However, these also come with their respective downsides.
TOTP is a symmetric algorithm, and therefore provides no additional security against database leaks; SMS is plaintext, unreliable, and subject to local law surrounding automated messages.

## Enter WebAuthn

The [FIDO2](https://fidoalliance.org/fido2/) project, of which [WebAuthn](https://www.w3.org/TR/webauthn-2/) is a part of, attempts to solve these issues by using public key cryptography instead of passwords for authentication.
Public-private key pairs specific to each web service are generated and stored by [authenticators](https://www.w3.org/TR/webauthn-2/#authenticator) like a [YubiKey](https://www.yubico.com/) or [SoloKey](https://solokeys.com/), or platform specific hardware like [TPM](https://trustedcomputinggroup.org/resource/tpm-library-specification/) or Apple’s TouchID[^1].

While the main motivation for WebAuthn is authentication without passwords, it can also be used to add second-factor authentication to password-based authentication.

## Ceremonies

WebAuthn can be split into two [ceremonies](https://www.w3.org/TR/webauthn-2/#ceremony) (a ceremony is like a network protocol except that it involves human actions).
The first ceremony is one-time [registration](https://www.w3.org/TR/webauthn-2/#registration) of a credential, in which a new key pair is generated on an authenticator and its public component transferred to and stored by the web service.
The second ceremony is [authentication](https://www.w3.org/TR/webauthn-2/#authentication) using a previously registered credential, in which it is proven that the user is in possession of the authenticator with the private key corresponding to a given public key.

Since web services can only run sandboxed client-side code, connected authenticators cannot be used directly.
Instead, the browser-provided [WebAuthn API](https://www.w3.org/TR/webauthn-2/#sctn-api) needs to be called via JavaScript to indirectly interact with authenticators.
Both ceremonies also use the shared concept of a [public key credential](https://www.w3.org/TR/webauthn-2/#public-key-credential), which is something the user can present to the web service in order to be authenticated.

We will now look at the steps of these ceremonies in some detail.

**Registration**
The image below depicts the steps of the registration ceremony, showing how the web server, the browser and the authenticator interact with each other through the WebAuthn API.

![Web Authentication API registration component and dataflow diagram](./registration.png)
[This figure](https://developer.mozilla.org/en-US/docs/Web/API/Web_Authentication_API) depicting the registration component of WebAuthn by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/MDN/Community/Roles_teams#contributor) is licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

**Step 0**: The client informs the web server that a user wishes to register a credential.
This initial message is implementation-specific, but typically it contains the desired username.
For username-less login this message can be empty.

**Step 1**: The web server responds with [detailed](https://www.w3.org/TR/webauthn-2/#dictdef-publickeycredentialcreationoptions) [requirements](https://www.w3.org/TR/webauthn-2/#dictdef-publickeycredentialcreationoptions) of the to-be-created key pair and other information, of which the following are of particular interest:

- [`challenge`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredentialcreationoptions-challenge):
  A server-generated [cryptographic challenge](https://www.w3.org/TR/webauthn-2/#sctn-cryptographic-challenges), preventing replay attacks and proving to the web server that the key was attested for this specific session.

- [`pubKeyCredParams`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredentialcreationoptions-pubkeycredparams):
  The [signing algorithms](https://www.w3.org/TR/webauthn-2/#typedefdef-cosealgorithmidentifier) that are allowed and their order of preference.
  For example, EdDSA, ECDSA with SHA-256 and RSA with SHA-256.

- [`authenticatorSelection`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredentialcreationoptions-authenticatorselection):
  Allows selecting authenticators with specific properties, see [Authenticator Taxonomy](https://www.w3.org/TR/webauthn-2/#sctn-authenticator-taxonomy) for a more detailed discussion.

  - [`authenticatorAttachment`](https://www.w3.org/TR/webauthn-2/#dom-authenticatorselectioncriteria-authenticatorattachment):
    Whether the selected authenticator should be [platform](https://www.w3.org/TR/webauthn-2/#platform-attachment) specific (like a [TPM](https://trustedcomputinggroup.org/resource/trusted-platform-module-tpm-summary/)) or [cross-platform](https://www.w3.org/TR/webauthn-2/#cross-platform-attachment) (like a [YubiKey](https://www.yubico.com/)).

  - [`residentKey`](https://www.w3.org/TR/webauthn-2/#dom-authenticatorselectioncriteria-residentkey):
    Whether a [client-side discoverable credential](https://www.w3.org/TR/webauthn-2/#client-side-discoverable-credential) should be created, having the effect that the user doesn’t need to enter a username for authentication. This is needed for username-less login.

  - [`userVerification`](https://www.w3.org/TR/webauthn-2/#dom-authenticatorselectioncriteria-userverification):
    Whether [user verification](https://www.w3.org/TR/webauthn-2/#user-verification) (in addition to [user presence](https://www.w3.org/TR/webauthn-2/#test-of-user-presence), which is always done) should be performed, making the result signify multi-factor authentication.

- [`attestation`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredentialcreationoptions-attestation):
  Whether an [attestation](https://www.w3.org/TR/webauthn-2/#sctn-attestation) is desired.
  Enabling this makes it possible to know that the result comes from a trusted authenticator model.

**Step 2**: The client selects an authenticator based on the above requirements and relays the relevant information to it.

**Step 3:** From here, the authenticator verifies that a user is present (via a button for example) and generates a new public-private key pair scoped to the web service, optionally with a proof that it originates from a trusted and secure authenticator, which is relayed back to the client.

**Step 4:** The client combines this information with its own information and relays it back to the web server.

**Step 5**: The client takes the data created by the authenticator and constructs the [`PublicKeyCredential`](https://www.w3.org/TR/webauthn-2/#publickeycredential), the interesting parts of which are:

- [`identifier`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredential-identifier-slot):
  The unique identifier of the provided credential.

- [`response`](https://www.w3.org/TR/webauthn-2/#authenticatorattestationresponse):
  The response of the authenticator to the client’s request for a new credential.
  - [`clientData`](https://www.w3.org/TR/webauthn-2/#dom-authenticatorresponse-clientdatajson):
    Provides the context for which the credential was created. e.g. the challenge and perceived origin of the options.
  - [`attestationObject`](https://www.w3.org/TR/webauthn-2/#attestation-object):
    In case attestation was requested this will contain the attestation information.

**Step 6**: The web server performs validation.
This validation includes, but is not limited to:

1. Checking if the challenge matches the challenge originally sent to the client.
2. Checking that the origin is the expected origin.
   If this isn’t the case, the client might have been connected to another server.

In case the web server requested, and was provided with, an attestation object, it may also verify that the attestation is sufficient.
Different authenticators provide different methods of attestation.
Hence, the web server must be able to handle different formats.
WebAuthn Level 2 (the specification used for the implementation of the library) [defines 6](https://www.w3.org/TR/webauthn-2/#sctn-defined-attestation-formats) [verifiable](https://www.w3.org/TR/webauthn-2/#sctn-defined-attestation-formats) [attestation statement formats](https://www.w3.org/TR/webauthn-2/#sctn-defined-attestation-formats).

The web server can also choose to lookup further information on attested authenticators in the [FIDO Alliance Metadata Service](https://fidoalliance.org/metadata/) ([specification](https://fidoalliance.org/specs/mds/fido-metadata-service-v3.0-ps-20210518.html)).
This service provides up-to-date information on registered authenticators.
These fields are of particular interest:

- [`attestationRootCertificates`](https://fidoalliance.org/specs/fido-v2.0-id-20180227/fido-metadata-statement-v2.0-id-20180227.html#widl-MetadataStatement-attestationRootCertificates):
  The root certificates for many authenticators.
  For these authenticators, verifying the attestation using the metadata is essential to trust the attestation.
  For other authenticators (e.g. Apple), the root certificate is hardcoded in the library.
  Our library automatically handles looking up the authenticator and verifying the attestation if needed, if provided with the required data.

- [`AuthenticatorStatus`](https://fidoalliance.org/specs/mds/fido-metadata-service-v3.0-ps-20210518.html#authenticatorstatus-enum): The status of the authenticator.
  For instance, an authenticator might be compromised for physical attacks.

**Authentication**
When a user wishes to authenticate themselves, this happens through the [authentication ceremony](https://www.w3.org/TR/webauthn-2/#authentication-ceremony).
The goal of this ceremony is for the user to securely prove that they are in possession of the authenticator that holds the private key corresponding to a public key previously registered to the users account.

![WebAuthn authentication component and dataflow diagram](./authentication.png)
[This figure](https://developer.mozilla.org/en-US/docs/Web/API/Web_Authentication_API) depicting the authentication component of WebAuthn by [Mozilla Contributors](https://developer.mozilla.org/en-US/docs/MDN/Community/Roles_teams#contributor) is licensed under [CC-BY-SA 2.5](https://creativecommons.org/licenses/by-sa/2.5/).

**Step 0**: The client informs the web server that a user authenticate themselves.
This initial message is implementation specific.
For typical registration this message should contain the desired username.
For username-less login, this message may be void of any information.

**Step 1**: The web server generates a new challenge and constructs the [PublicKeyCredentialRequestOptions](https://www.w3.org/TR/webauthn-2/#dictdef-publickeycredentialrequestoptions).
For username-less login, the challenge is in fact the only field that has to be set.
The two fields that are interesting are:

- [`challenge`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredentialrequestoptions-challenge):
  The authenticator will sign this challenge in the following steps to prove the possession of the private key.

- [`allowCredentials`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredentialrequestoptions-allowcredentials):
  For the given username (if any), the server selects the credentials it has on record and relays them as this field (in order of preference).
  This will allow the client to select the credential for which it knows an authenticator with the correct private key.
  For passwordless login, this field is empty, it is then up to the authenticator to select the correct credential based on their scope.

- [`userVerification`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredentialrequestoptions-userverification):
  Whether [user verification](https://www.w3.org/TR/webauthn-2/#user-verification) (in addition to [user presence](https://www.w3.org/TR/webauthn-2/#test-of-user-presence), which is always done) should be performed, making the result signify multi-factor authentication.

**Step 2-4**: The client selects an authenticator and relays the relevant information to it.
From here, the authenticator verifies if the user is present (using a button for example), signs the challenge (and client data) with its private key, and returns the signature.
The authenticator also returns the [`authenticatorData`](https://www.w3.org/TR/webauthn-2/#authenticator-data) which contains information about the scope of the credential, the user interaction, and the authenticator itself.

**Step 5**: The client takes the data created by the authenticator and constructs the [`PublicKeyCredential`](https://www.w3.org/TR/webauthn-2/#publickeycredential), the interesting parts of which are:

- [`identifier`](https://www.w3.org/TR/webauthn-2/#dom-publickeycredential-identifier-slot):
  The unique identifier of the provided credential.

- [`response`](https://www.w3.org/TR/webauthn-2/#authenticatorassertionresponse):
  The response of the authenticator to the client’s request.

  - [`clientData`](https://www.w3.org/TR/webauthn-2/#client-data):
    Provides the context for which the signing took place. e.g. the challenge and perceived origin of the options.

  - [`authenticatorData`](https://www.w3.org/TR/webauthn-2/#authenticator-data):
    Contains information about the credential scope, user interaction with the authenticator for signing, and the signature counter.

  - [`signature`](https://www.w3.org/TR/webauthn-2/#dom-authenticatorassertionresponse-signature):
    Contains the signature over the [`clientDataJSON`](https://www.w3.org/TR/webauthn-2/#dom-authenticatorresponse-clientdatajson) and [`authenticatorData`](https://www.w3.org/TR/webauthn-2/#authenticator-data).

**Step 6**: The server verifies that the [client data](https://www.w3.org/TR/webauthn-2/#client-data) and [`authenticatorData`](https://www.w3.org/TR/webauthn-2/#authenticator-data) are as expected, and that the signature is valid.

## The Haskell library

The library implements almost the entire [WebAuthn Level 2 specification](https://www.w3.org/TR/webauthn-2), with [extensions](https://www.w3.org/TR/webauthn-2/#sctn-extensions) being the only major [missing part](https://github.com/tweag/webauthn/issues/35).
While the general design of the library isn't expected to change very much, it should still be considered an alpha version for now.
If you have a website with user accounts running on Haskell, we'd love for you to try it out and [tell us](https://github.com/tweag/webauthn/issues) what could be improved, contributions are welcome too!

To get started, here are our recommendations:

- Read the [introduction to the WebAuthn specification](https://www.w3.org/TR/webauthn-2/#sctn-intro)
- Read the documentation of the main [Crypto.WebAuthn](https://hackage.haskell.org/package/webauthn/docs/Crypto-WebAuthn.html) module, which gives an overview of the library
- Check out and run the code of our [demo server](https://github.com/tweag/webauthn/tree/master/server) implementation, which shows an example of how the library might be used

[^1]: More specifically, the [Secure Enclave](https://support.apple.com/guide/security/secure-enclave-sec59b0b31ff/web), which TouchID allows access to.
