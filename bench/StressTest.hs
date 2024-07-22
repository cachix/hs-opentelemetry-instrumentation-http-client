{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Concurrent (forkIO, threadDelay)
import qualified Data.ByteString.Lazy as Lazy
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status200, status404)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Instrumentation.HttpClient (addTracerToManagerSettings)
import OpenTelemetry.Trace (TracerProvider, initializeGlobalTracerProvider, shutdownTracerProvider)
import qualified System.Random.Stateful as Random
import UnliftIO.Async (pooledMapConcurrentlyN_)
import UnliftIO.Exception (bracket, try)

numRequests :: Int
numRequests = 100000

numThreads :: Int
numThreads = 100

main :: IO ()
main = withTracer $ \_ -> do
  rng <- Random.newIOGenM (Random.mkStdGen 42)
  _ <- forkIO $ Warp.run 4567 (app rng)

  settings <- addTracerToManagerSettings HTTP.defaultManagerSettings
  manager <- HTTP.newManager settings

  -- Wait for the server to start
  threadDelay 100000

  pooledMapConcurrentlyN_ numThreads (client manager) [1 .. numRequests]

  -- Let GC do its thing
  threadDelay (1 * 1000 * 1000)

withTracer :: (TracerProvider -> IO a) -> IO a
withTracer =
  bracket initializeGlobalTracerProvider shutdownTracerProvider

app :: Random.IOGenM Random.StdGen -> Wai.Application
app rng _ respond = do
  delay <- Random.uniformRM (20000, 70000) rng
  isOk <- Random.uniformM rng
  threadDelay delay
  let status = if isOk then status200 else status404
  respond $ Wai.responseLBS status [] "Hello world!"

req :: HTTP.Request
req = HTTP.parseRequest_ "http://localhost:4567"

client :: HTTP.Manager -> Int -> IO ()
client manager _ = do
  _response :: Either HTTP.HttpException (HTTP.Response Lazy.ByteString) <- try $ HTTP.httpLbs req manager
  return ()
