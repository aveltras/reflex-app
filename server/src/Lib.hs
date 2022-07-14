{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Concurrent (Chan, forkIO, newChan, threadDelay, writeChan)
import Control.Lens (view)
import Data.Binary.Builder (fromByteString)
import Data.ByteString.Builder
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Hasql.Connection
import Hasql.Notifications
import Network.HTTP.Types
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Reflex.Dom hiding (run)

client :: IO ()
client = mainWidget $ do
  onBuild <- getPostBuild
  display =<< count =<< button "ClickMe"
  onResponse <- performRequestAsync $ req <$ onBuild
  asText <- holdDyn "No results." $ onResponse <&> fromJust . view xhrResponse_responseText
  dynText asText
  where
    req = XhrRequest "GET" "http://localhost:8080" def

server :: IO ()
server = do
  dbOrError <- acquire "postgres://postgres/reflex-app?host=/var/run/postgresql/"
  dbEventChannel <- newChan @ServerEvent
  case dbOrError of
    Right db -> do
      let channelToListen = toPgIdentifier "events"
      listen db channelToListen
      forkIO $ waitForNotifications (\channel payload -> writeChan dbEventChannel $ ServerEvent Nothing Nothing [fromByteString payload]) db
      run 8080 $ simpleCors $ app dbEventChannel
    _ -> error "Could not open database connection"

app :: Chan ServerEvent -> Application
app chan req respond = do
  case pathInfo req of
    [] -> do
      putStrLn "I've done some IO here"
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Hello, Web!"
    ["sse"] -> eventSourceAppChan chan req respond
    _ -> error $ show $ pathInfo req

eventIO :: IO ServerEvent
eventIO = do
  threadDelay 1000000
  time <- getPOSIXTime
  return $
    ServerEvent
      (Just $ string8 "io")
      Nothing
      [string8 . show $ time]
