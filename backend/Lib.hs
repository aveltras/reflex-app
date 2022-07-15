{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Concurrent (Chan, forkIO, newChan, threadDelay, writeChan)
import Control.Lens (view)
import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Binary.Builder (fromByteString)
import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHCJS.DOM qualified as DOM
import GHCJS.DOM.EventM qualified as DOM
import GHCJS.DOM.EventSource qualified as DOM
import GHCJS.DOM.MessageEvent qualified as DOM
import GHCJS.DOM.Types qualified as DOM
import GHCJS.DOM.Window (alert)
import Hasql.Connection
import Hasql.Notifications
import Language.Javascript.JSaddle (JSM, textToJSString, valToJSON, valToText)
import Language.Javascript.JSaddle.Monad (liftJSM)
import Language.Javascript.JSaddle.Object (function)
import Language.Javascript.JSaddle.Run (syncPoint)
import Language.Javascript.JSaddle.WebSockets
import Network.HTTP.Types
import Network.Wai
import Network.Wai.EventSource
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Network.WebSockets (defaultConnectionOptions)
import Reflex.Dom.Core

client :: JSM ()
client = runHydrationWidgetWithHeadAndBody (pure ()) $ \appendHead appendBody -> do
  void $ appendHead headWidget
  void $ appendBody bodyWidget
  blank

headWidget :: (DomBuilder t m, Prerender t m) => m ()
headWidget = do
  el "title" $ text "Reflex App"
  prerender_ (elAttr "script" ("src" =: "http://localhost:8080/jsaddle.js") blank) blank
  elAttr "link" ("rel" =: "shortcut icon" <> "href" =: "data:image/x-icon;," <> "type" =: "image/x-icon") blank

bodyWidget :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, Prerender t m, TriggerEvent t m) => m ()
bodyWidget = do
  prerender_ (el "div" $ text "loading...") $ do
    onBuild <- getPostBuild
    onResponse <- performRequestAsync $ req <$ onBuild
    asText <- holdDyn "No results." $ onResponse <&> fromJust . view xhrResponse_responseText
    el "div" $ dynText asText
  display =<< count =<< button "ClickMe"
  -- v <- holdDyn 0 $ 1 <$ onClick
  -- el "div" $ dynText "tac"
  (onMessage, sendMessage) <- newTriggerEvent
  display =<< holdDyn (mempty :: Aeson.Object) (traceEvent "onMessage" $ onMessage)
  prerender_ (text "sse here") $
    liftJSM $ do
      w <- DOM.currentWindowUnchecked
      -- alert w ("prerender" :: String)
      eventSource <- DOM.newEventSource (DOM.toJSString ("http://localhost:8080/sse" :: String)) DOM.noEventSourceInit
      onMessage <- function $ \_ _ (e : _) -> do
        let e' = DOM.MessageEvent e
        msg <- DOM.getData e'
        -- msg' <- DOM.unMessageEvent <$> DOM.fromJSValUnchecked msg
        -- jsonString <- valToJSON msg'
        -- alert w jsonString
        val <- DOM.fromJSValUnchecked msg
        liftIO $ sendMessage $ fromMaybe mempty $ jsonDecode val
      DOM.addListener eventSource DOM.message (DOM.SaferEventListener onMessage) True
      blank
  blank
  where
    req = XhrRequest "GET" "http://localhost:8080/api" def

server :: IO ()
server = do
  dbOrError <- acquire "postgres://postgres/reflex-app?host=/var/run/postgresql/"
  dbEventChannel <- newChan @ServerEvent
  case dbOrError of
    Right db -> do
      let channelToListen = toPgIdentifier "events"
      listen db channelToListen
      forkIO $ waitForNotifications (\channel payload -> writeChan dbEventChannel $ ServerEvent Nothing Nothing [fromByteString payload]) db
      builtApp <- jsaddleOr defaultConnectionOptions (client >> syncPoint) $ app dbEventChannel
      run 8080 $ simpleCors builtApp
    _ -> error "Could not open database connection"

app :: Chan ServerEvent -> Application
app chan req respond = do
  case (requestMethod req, pathInfo req) of
    ("GET", []) -> do
      (_, bs) <- renderStatic . runHydratableT $
        el "html" $ do
          el "head" $ do
            headWidget
            elAttr "script" ("src" =: "http://localhost:8080/jsaddle.js") blank
          el "body" $ do
            bodyWidget

      respond $
        responseLBS
          status200
          [("Content-Type", "text/html")]
          (LBS.fromStrict $ "<!doctype html>" <> bs)
    ("GET", ["api"]) -> do
      putStrLn "I've done some IO here"
      respond $
        responseLBS
          status200
          [("Content-Type", "text/plain")]
          "Hello, Web!"
    ("GET", ["sse"]) -> eventSourceAppChan chan req respond
    ("GET", ["jsaddle.js"]) -> respond $ responseLBS status200 [("Content-Type", "application/javascript")] $ jsaddleJs' Nothing False
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
