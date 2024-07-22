{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (bracket)
import Control.Monad (when)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (status200)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import OpenTelemetry.Instrumentation.HttpClient (addTracerToManagerSettings)
import OpenTelemetry.Trace (TracerProvider, initializeGlobalTracerProvider, shutdownTracerProvider)
import qualified System.Random.Stateful as Random
import UnliftIO.Async (pooledMapConcurrentlyN_)

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

withTracer :: (TracerProvider -> IO a) -> IO a
withTracer =
  bracket initializeGlobalTracerProvider shutdownTracerProvider

app :: Random.IOGenM Random.StdGen -> Wai.Application
app rng _ respond = do
  delay <- Random.uniformRM (20000, 70000) rng
  threadDelay delay
  respond $ Wai.responseLBS status200 [] "Hello world!"

req :: HTTP.Request
req = HTTP.parseRequest_ "http://localhost:4567"

client :: HTTP.Manager -> Int -> IO ()
client manager _ = do
  response <- HTTP.httpLbs req manager
  when (HTTP.responseStatus response /= status200) $ error "Request failed"
