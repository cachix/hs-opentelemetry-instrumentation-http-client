{-# LANGUAGE OverloadedStrings #-}

module OpenTelemetry.Instrumentation.HttpClient
  ( addTracerToManagerSettings,
    addTracerToManagerSettings',
  )
where

import Control.Monad (forM_, when, (>=>))
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import qualified OpenTelemetry.Context as Context
import OpenTelemetry.Context.ThreadLocal as Context
import OpenTelemetry.Propagator (extract, inject)
import OpenTelemetry.Trace.Core

addTracerToManagerSettings :: (MonadIO m) => HTTP.ManagerSettings -> m HTTP.ManagerSettings
addTracerToManagerSettings settings = do
  tp <- getGlobalTracerProvider
  pure $ addTracerToManagerSettings' tp settings

addTracerToManagerSettings' :: TracerProvider -> HTTP.ManagerSettings -> HTTP.ManagerSettings
addTracerToManagerSettings' tp settings = do
  let tracer = makeTracer tp "hs-opentelemetry-instrumentation-http-client" tracerOptions

  settings
    { HTTP.managerModifyRequest = traceRequest tracer >=> HTTP.managerModifyRequest settings,
      HTTP.managerModifyResponse = HTTP.managerModifyResponse settings >=> traceResponse tracer
    }

traceRequest :: (MonadIO m) => Tracer -> HTTP.Request -> m HTTP.Request
traceRequest t req = do
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
  hdrs <- inject (getTracerProviderPropagators $ getTracerTracerProvider t) ctxt (HTTP.requestHeaders req)
  pure $ req {HTTP.requestHeaders = hdrs}

traceResponse :: (MonadIO m) => Tracer -> HTTP.Response a -> m (HTTP.Response a)
traceResponse t resp = do
  ctxt <- getContext
  ctxt' <- extract (getTracerProviderPropagators $ getTracerTracerProvider t) (HTTP.responseHeaders resp) ctxt
  _ <- attachContext ctxt'
  forM_ (Context.lookupSpan ctxt') $ \s -> do
    when (HTTP.statusCode (HTTP.responseStatus resp) >= 400) $ do
      setStatus s (Error "")
    addAttributes s $
      H.fromList
        [ ("http.status_code", toAttribute $ HTTP.statusCode $ HTTP.responseStatus resp)
        ]
    endSpan s Nothing
  pure resp
