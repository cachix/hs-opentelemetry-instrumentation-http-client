{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.ByteString.Lazy as Lazy
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status200, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Context.ThreadLocal (attachContext, getContext)
import OpenTelemetry.Instrumentation.HttpClient (addTracerToManagerSettings)
import OpenTelemetry.Trace
  ( TracerProvider,
    defaultSpanArguments,
    inSpan,
    initializeGlobalTracerProvider,
    makeTracer,
    shutdownTracerProvider,
    tracerOptions,
  )
import qualified System.Random.Stateful as Random
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (pooledMapConcurrentlyN_)
import UnliftIO.Exception (bracket, try)

numRequests :: Int
numRequests = 100000

maxParallelRequests :: Int
maxParallelRequests = 100

main :: IO ()
main = withTracer $ \tracerProvider -> do
  let tracer = makeTracer tracerProvider "stress-test" tracerOptions

  rng <- Random.newIOGenM (Random.mkStdGen 42)
  _ <- forkIO $ Warp.run 4567 (app rng)

  settings <- addTracerToManagerSettings HTTP.defaultManagerSettings
  manager <- HTTP.newManager settings

  -- Wait for the server to start
  threadDelay 100000

  inSpan tracer "pooledMapConcurrentlyN" defaultSpanArguments $
    tracedPooledMapConcurrentlyN_ maxParallelRequests (void . client manager) [1 .. numRequests]

  -- Let GC do its thing
  threadDelay (1 * 1000 * 1000)

app :: Random.IOGenM Random.StdGen -> Wai.Application
app rng _ respond = do
  delay <- Random.uniformRM (20000, 70000) rng
  isOk <- Random.uniformM rng
  threadDelay delay
  let status = if isOk then status200 else status404
  respond $ Wai.responseLBS status [] "Hello world!"

req :: HTTP.Request
req = HTTP.parseRequest_ "http://localhost:4567"

client :: HTTP.Manager -> Int -> IO (Either HTTP.HttpException (HTTP.Response Lazy.ByteString))
client manager _ =
  try $ HTTP.httpLbs req manager

withTracer :: (TracerProvider -> IO a) -> IO a
withTracer =
  bracket initializeGlobalTracerProvider shutdownTracerProvider

tracedPooledMapConcurrentlyN_ :: (MonadIO m, MonadUnliftIO m, Traversable t) => Int -> (a -> m ()) -> t a -> m ()
tracedPooledMapConcurrentlyN_ numThreads action xs = do
  ctxt <- getContext
  let wrappedAction x = attachContext ctxt >> action x
  pooledMapConcurrentlyN_ numThreads wrappedAction xs
