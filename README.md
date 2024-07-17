# hs-opentelemetry-instrumentation-http-client

Provides a hook to add OpenTelemetry tracing to an HTTP Manager.

Unlike the upstream [hs-opentelemetry-instrumentation-http-client](https://github.com/iand675/hs-opentelemetry/instrumentation/http-client/),
this package does not require replacing every `Network.HTTP.Client` function with one that is wrapped with tracing.
