{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module OpenTelemetry.Instrumentation.HttpClient
  ( addTracerToManagerSettings,
    addTracerToManagerSettings',
  )
where

import Control.Concurrent.Thread.Storage (ThreadStorageMap)
import qualified Control.Concurrent.Thread.Storage as TS
import Control.Monad (forM_, when, (>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.IO.Exception (IOException)
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified OpenTelemetry.Context as Context
import OpenTelemetry.Context.ThreadLocal as Context
import OpenTelemetry.Propagator (extract, inject)
import OpenTelemetry.Trace.Core
import System.IO.Unsafe (unsafePerformIO)
import UnliftIO.Exception (SomeException, displayException, withException)

-- | Store the current parent span, if available. Used for cleanup.
type ThreadLocalSpans = ThreadStorageMap (Maybe Span, Span)

threadLocalSpans :: ThreadLocalSpans
threadLocalSpans = unsafePerformIO TS.newThreadStorageMap
{-# NOINLINE threadLocalSpans #-}

addTracerToManagerSettings :: (MonadIO m) => HTTP.ManagerSettings -> m HTTP.ManagerSettings
addTracerToManagerSettings settings = do
  tp <- getGlobalTracerProvider
  pure $ addTracerToManagerSettings' tp settings

addTracerToManagerSettings' :: TracerProvider -> HTTP.ManagerSettings -> HTTP.ManagerSettings
addTracerToManagerSettings' tp settings = do
  let tracer = makeTracer tp "hs-opentelemetry-instrumentation-http-client" tracerOptions

  settings
    { -- NOTE: do not use `managerModifyRequest`.
      -- It may be called multiple times per request.
      -- See https://hackage.haskell.org/package/http-client-0.7.17/docs/Network-HTTP-Client.html#v:managerModifyRequest
      HTTP.managerWrapException = \req e -> do
        (req', e') <- traceRequest tracer req e
        HTTP.managerWrapException settings req' e',
      HTTP.managerModifyResponse = HTTP.managerModifyResponse settings >=> traceResponse tracer
    }

traceRequest :: (MonadIO m) => Tracer -> HTTP.Request -> IO a -> m (HTTP.Request, IO a)
traceRequest t req onExceptionHandler = do
  let url =
        T.decodeUtf8
          ((if HTTP.secure req then "https://" else "http://") <> HTTP.host req <> ":" <> B.pack (show $ HTTP.port req) <> HTTP.path req <> HTTP.queryString req)
      args =
        defaultSpanArguments
          { kind = Client,
            attributes =
              H.fromList
                [ ("http.method", toAttribute $ T.decodeUtf8 $ HTTP.method req),
                  ("http.url", toAttribute url),
                  ("http.target", toAttribute $ T.decodeUtf8 (HTTP.path req <> HTTP.queryString req)),
                  ("http.host", toAttribute $ T.decodeUtf8 $ HTTP.host req),
                  ("http.scheme", toAttribute $ TextAttribute $ if HTTP.secure req then "https" else "http"),
                  ( "http.flavor",
                    toAttribute $ case HTTP.requestVersion req of
                      (HTTP.HttpVersion major minor) -> T.pack (show major <> "." <> show minor)
                  ),
                  ( "http.user_agent",
                    toAttribute $ maybe "" T.decodeUtf8 (lookup HTTP.hUserAgent $ HTTP.requestHeaders req)
                  )
                ]
          }
  ctxt <- getContext
  s <- createSpan t ctxt "http.request" args
  updateName s url
  Context.adjustContext (Context.insertSpan s)
  _ <- TS.attach threadLocalSpans (Context.lookupSpan ctxt, s)

  hdrs <- inject (getTracerProviderPropagators $ getTracerTracerProvider t) ctxt (HTTP.requestHeaders req)
  let req' = req {HTTP.requestHeaders = hdrs}
  pure (req', onExceptionHandler')
  where
    onExceptionHandler' =
      onExceptionHandler `withException` handleResponseException

    handleResponseException :: SomeException -> IO ()
    handleResponseException e = do
      mspans <- TS.lookup threadLocalSpans
      forM_ mspans $ \(_parent, s) -> do
        setStatus s $ Error $ T.pack $ displayException e
        recordException s mempty Nothing e
        endSpan s Nothing

      cleanupThreadLocalSpan

traceResponse :: (MonadIO m) => Tracer -> HTTP.Response a -> m (HTTP.Response a)
traceResponse t resp = do
  ctxt <- getContext
  ctxt' <- extract (getTracerProviderPropagators $ getTracerTracerProvider t) (HTTP.responseHeaders resp) ctxt
  _ <- attachContext ctxt'
  mspans <- TS.lookup threadLocalSpans
  forM_ mspans $ \(_parent, s) -> do
    when (HTTP.statusCode (HTTP.responseStatus resp) >= 400) $
      setStatus s (Error "")
    addAttributes s $
      H.fromList
        [ ("http.status_code", toAttribute $ HTTP.statusCode $ HTTP.responseStatus resp)
        ]
    endSpan s Nothing

  cleanupThreadLocalSpan

  pure resp

{-# INLINE cleanupThreadLocalSpan #-}
cleanupThreadLocalSpan :: forall m. (MonadIO m) => m ()
cleanupThreadLocalSpan =
  TS.detach threadLocalSpans >>= mapM_ cleanupSpan'
  where
    cleanupSpan' :: (Maybe Span, Span) -> m ()
    cleanupSpan' (parent, _s) = Context.adjustContext $ \ctx ->
      maybe (Context.removeSpan ctx) (`Context.insertSpan` ctx) parent
