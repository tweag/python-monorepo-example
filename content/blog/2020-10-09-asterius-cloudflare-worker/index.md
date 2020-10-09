---
title: Edge computing with servant on Cloudflare
author: Cheng Shao
tags: [haskell, asterius]
---

_Just a few days ago, Cloudflare and Stack builders published [a
excellent blog
post](https://blog.cloudflare.com/cloudflare-worker-with-webassembly-and-haskell/)
where they demonstrate how to deploy a Haskell program to Cloudflare
Worker with Asterius. In this post we go one step further and show
that you can deploy full-fledged servant applications just as well._

Suppose we're about to write the backend of yet another web app. The traditional
way of doing this is to implement a backend program that listens on a
port, waits for incoming requests, and sends responses back. The backend program
is deployed to a controlled server environment and runs persistently. This
is the most flexible approach since there's no limit on the tech stack: we can
use any programming language and rely on arbitrary runtime dependencies. But of
course, we're also responsible for provisioning the deployment server, which is
a hard and error-prone process. Even if we use cloud computing instead of
managing a physical machine, we're still in charge of jobs like monitoring the
instance state, performing security updates, or upgrading to larger instances
when necessary.

What if we could just forget about servers altogether? We just need to ensure the
backend program conforms to a certain request handler function interface, then
the cloud provider will run the handler function on a per-request basis without
our intervention. This also changes the programming paradigm of backends;
instead of long-running programs, they are now transient programs, started and
terminated frequently, and the lifetime of a running instance shouldn't be
assumed to be much longer than the time it takes to process a single request.
This is exactly what "serverless computing" promotes, and major cloud providers
have already been providing this service for some time, like [AWS
Lambda][aws-lambda], [Azure Functions][azure-functions], [Google Cloud
Functions][google-cloud-functions], etc.

Edge computing takes serverless computing even further, by running the deployed
code in the [CDN](https://en.wikipedia.org/wiki/Content_delivery_network)
servers. The end user's request reaches the CDN, computation occurs, and the
result can be returned directly instead of having to forward the
request to a further server and forward its response back to the end
user.
This means an improvement in overall latency since
computation occurs close to the end user.

Compared to regular serverless computing, edge computing brings an additional
challenge: tighter restrictions on the runtime environment. Ordinary serverless
computing platforms offer a variety of choices for the backend programming
language, and they sometimes even allow running native executables. Edge
computing platforms usually restrict developers to a single scripting language
for technical reasons: [Cloudflare Workers][cloudflare-workers] supports
JavaScript/WebAssembly, [Lambda@Edge][lambda-at-edge] supports Node.js and
[fastly][fastly] supports pure WebAssembly.

Asterius is a Haskell-to-WebAssembly compiler that supports a variety of
existing Haskell packages. Since it emits WebAssembly & JavaScript code, it can
be used to develop edge computing apps provided the target platform supports
WebAssembly and JavaScript. Why would we want to run Haskell on the edge?
Here are only a few of the possible answers:

- to wrap popular Haskell apps like [`pandoc`][pandoc] or
  [`ShellCheck`][shellcheck] into web services,
- to adapt existing Haskell web backend codebases to benefit from edge
  computing, or
- the same reason you choose Haskell for vanilla web backend projects,
  _e.g._ type-driven development.

As we explored potential use cases of Asterius, we've succeeded in deploying an
example [`servant`][servant] web app to Cloudflare Workers, and this post
shows how we did it. You're welcome to give it a try with your Worker
program or other providers like Lambda@Edge.

## What does a Worker function look like?

To use Cloudflare Workers, we need to write or generate a single JavaScript
program that implements the Worker function. The Worker function is a
[`fetch`][fetch-event] event handler. When there is an incoming HTTP request, a
`fetch` event is fired, then the handler is called to perform a user-defined
computation and produce the response:

```javascript
addEventListener("fetch", event => {
  event.respondWith(handleRequest(event.request))
})

async function handleRequest(request) {
  return new Response("Hello world")
}
```

To implement a Worker function in Haskell, the Haskell module needs to export
the request handler function and the JavaScript entry script needs to set up the
Asterius runtime, call the function, and get the result upon incoming requests.

## A `wai` app as a Worker function

The idea of `Request -> Response` functions reminds us of something similar in
the Haskell web development landscape. The [`wai`][wai] framework provides this
type:

```haskell
import Network.Wai

type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
```

The `Application` type defines a function type which consumes a `Request` and
uses a `Response` afterward.

Haskell web backend developers should be pretty familiar with `wai` and the
`Application` type. The type serves as a keystone in many Haskell web backends:

- High-level web frameworks like [servant][servant] and [yesod][yesod]
  encapsulate the business logic into a single `Application` which examines
  requests and reroutes to the appropriate handler based on the request URL.
- One can apply `Middleware`s, which are simply `Application -> Application`
  functions to add specific functionality, like gzip encoding, logging, caching,
  etc.
- An `Application` can be converted to a traditional web server program which
  polls requests on a port.

Given how central a role `Application` plays in Haskell web backend development,
we should use it to model Worker
functions. If it works, then in principle it should be possible to migrate
higher-level backend codebases to Workers as well. The next section demonstrates
adapting an `Application` as a Worker function, then we'll move on to implement
a minimal `servant` web app on top of it and deploy the example.

## Bridging the `wai` world and the JavaScript world

Before we turn a `wai`-based app into a Worker function, we need to implement the
conversion between the `Request`/`Response` data types in `wai` and the
JavaScript [`Request`][request]/[`Response`][response] classes as described in
the Worker documentation.

```haskell
newtype JSRequest = JSRequest JSVal
newtype JSResponse = JSResponse JSVal

parseRequest :: JSRequest -> IO Wai.Request
makeResponse :: Wai.Response -> IO JSResponse
fromWaiApplication :: Wai.Application -> JSRequest -> IO JSResponse
```

Using `parseRequest` and `makeResponse`, we can implement
`fromWaiApplication` which converts a Haskell `Application` to a `JSRequest -> IO JSResponse` event handler function to be exported and called in the
JavaScript world.

The functions above are the infrastructure for running Haskell code as Workers,
useful for any `wai`-based web framework. For the complete code, see
[here][cloudflare-new].

## A `servant` app as a Worker function

Let's move on to implement a minimal [`servant`][servant] app as a
proof-of-concept Worker function. The following module implements a small
fraction of [httpbin][httpbin]. The httpbin website provides a service for
testing HTTP client libraries. One can make requests to its different endpoints,
and it'll return JSON-formatted client information. For instance, accessing
`/ip` will return the end user's IP address as a string.

The example below implements 3 endpoints: `/ip`, `/country`, and `/user-agent`,
which return the visitor's IP address, country, or user agent. The Worker
runtime already encodes this information as HTTP request headers, so all we
need to do is declare the required header field and return the value. There
is no server or other third party services involved, therefore the
example is self-contained and can be deployed to the Worker preview service
without any credentials.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module HttpBin (handleFetch) where

-- The complete source code of the Cloudflare.* modules is available at
-- https://github.com/tweag/asterius/tree/master/asterius/test/cloudflare-new/Cloudflare
import Cloudflare.Application
import Cloudflare.Request
import Cloudflare.Response
import Data.Text (Text)
import Servant

type HttpBin =
  "ip" :> Header "cf-connecting-ip" Text :> Get '[JSON] Text
    :<|> "country" :> Header "cf-ipcountry" Text :> Get '[JSON] Text
    :<|> "user-agent" :> Header "user-agent" Text :> Get '[JSON] Text

httpBinServer :: Server HttpBin
httpBinServer = f :<|> f :<|> f
  where
    f (Just s) = pure s
    f _ = pure "Header not found."

httpBinApp :: Application
httpBinApp = serve (Proxy @HttpBin) httpBinServer

handleFetch :: JSRequest -> IO JSResponse
handleFetch = fromWaiApplication httpBinApp

foreign export javascript "handleFetch" handleFetch :: JSRequest -> IO JSResponse
```

The exported `handleFetch` function is the entry point of the Haskell world. For
each incoming `fetch` event, the following JavaScript code will initialize an
Asterius runtime instance, call `handleFetch` and respond with the result.

```JavaScript
import * as rts from "./rts.mjs";
import req from "./HttpBin.req.mjs";

addEventListener("fetch", (event) => {
  event.respondWith(
    rts
      .newAsteriusInstance(Object.assign(req, { module: wasm }))
      .then((i) => i.exports.handleFetch(event.request))
  );
});
```

## Compiling & deploying the Worker function

We'll use the official Worker CLI [wrangler][wrangler] to perform the
deployment. Two config files are required. The `wrangler.toml` file sets up a
[`webpack`][webpack]-based `wrangler` project:

```toml
name = "cloudflare-new"
type = "webpack"
webpack_config = "webpack.config.js"

account_id = ""
workers_dev = true
route = ""
zone_id = ""
```

The `webpack.config.js` file specifies the `HttpBin.mjs` entry script as the
starting point of code bundling:

```javascript
module.exports = {
  entry: "./HttpBin.mjs",
}
```

Now, we can finally compile the example and deploy it to the Worker preview
service. The following commands work with the prebuilt container image of
Asterius:

```
# --browser is required for Worker functions
# --yolo enables no-op GC, see next section for an explanation
# --gc-threshold=1 sets a smaller initial heap size
# --no-main is required since it's not a Main module

ahc-link                        \
  --browser                     \
  --yolo                        \
  --gc-threshold 1              \
  --input-hs HttpBin.hs         \
  --input-mjs HttpBin.mjs       \
  --export-function handleFetch \
  --no-main

# wrangler hard-codes the expected .wasm file path

mkdir -p worker
cp HttpBin.wasm worker/module.wasm

# Specify the /ip endpoint. Without --url the default endpoint is /

wrangler preview --url https://example.com/ip
```

The `wrangler` tool will bundle and deploy the JavaScript & WebAssembly code.
The result will be available in the current console and an opened browser
window:

```
Running preview without authentication.
Your Worker responded with: "149.62.159.244"
```

## Conclusion

The example in the last section is based on `servant` and `wai`. Other web
frameworks should work as well, as long as we can wrap them into `fetch` request
handler functions.

Though most frameworks should be straightforward to adjust, here are a couple
of tips you might find useful when developing Worker functions:

- Unlike ordinary web server programs which are expected to run for a long time,
  Worker scripts are started and stopped by need. Therefore, instance-level
  global state is discouraged since it's fragile. Instead, the application state
  should be explicitly provided by an external service, e.g. a database or a
  message queue.

- By not relying on instance-level global state, we're also able to use a no-op
  GC which can increase the throughput of the Worker function. As demonstrated
  in the example, for each incoming `fetch` request, a new Asterius runtime is
  initialized to run the handler function. In the outer JavaScript world, the
  used Asterius runtimes are garbage collected shortly, so the heap space used
  by Haskell will not leak.

- Here's a tip on generating smaller WebAssembly code for deployment. In most
  cases, we need to handle multiple endpoints in a Worker function. Conventional
  web frameworks allow us to combine the handlers of different endpoints into a
  single one which picks the right handler at runtime based on the request URL.
  Another way to handle different endpoints is wrapping their handlers into
  different Worker functions, then it is more likely to generate smaller
  WebAssembly code due to dead code elimination of Asterius.

We believe the combination of Haskell, WebAssembly and edge computing has much
potential. The initial Cloudflare Worker [test][worker-test-pr] was contributed
by external contributor Ento on Github, many thanks! Feel free to
reach us in case you find this use case interesting and want more in-depth
support.

[aws-lambda]: https://aws.amazon.com/lambda
[azure-functions]: https://docs.microsoft.com/en-us/azure/azure-functions
[cloudflare-new]: https://github.com/tweag/asterius/tree/master/asterius/test/cloudflare-new/Cloudflare
[cloudflare-workers]: https://workers.cloudflare.com
[cloudflare-workers-playground]: https://cloudflareworkers.com
[fastly]: https://www.fastly.com/products/edge-compute
[fetch-event]: https://developers.cloudflare.com/workers/runtime-apis/fetch-event
[google-cloud-functions]: https://cloud.google.com/functions
[httpbin]: https://httpbin.org
[lambda-at-edge]: https://aws.amazon.com/lambda/edge
[pandoc]: https://pandoc.org
[request]: https://developers.cloudflare.com/workers/runtime-apis/request
[response]: https://developers.cloudflare.com/workers/runtime-apis/response
[servant]: https://docs.servant.dev/en/stable
[shellcheck]: https://www.shellcheck.net
[stackbuilders-post]: https://blog.cloudflare.com/cloudflare-worker-with-webassembly-and-haskell
[wai]: https://www.yesodweb.com/book/web-application-interface
[webpack]: https://webpack.js.org
[worker-test-pr]: https://github.com/tweag/asterius/pull/413
[wrangler]: https://developers.cloudflare.com/workers/cli-wrangler
[yesod]: https://www.yesodweb.com/book
