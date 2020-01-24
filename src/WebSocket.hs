{-# LANGUAGE OverloadedStrings #-}

module WebSocket where

import           Control.Concurrent (forkIO, threadDelay)
import           Control.Monad (forever)
import           Data.Aeson (decode, encode)
import           Data.ByteString.Lazy (empty)
import           Data.Maybe (fromJust)
import           Event
import           Payload
import           Wuss (runSecureClient)
import qualified Network.WebSockets as WS

sendHeartbeat :: WS.ClientApp()
sendHeartbeat conn = do
  WS.sendTextData conn (encode heartbeatPayload)
  threadDelay 41250000
  sendHeartbeat conn

sendIdentify :: WS.ClientApp()
sendIdentify conn = do
  token <- filter (/= '\n') <$> readFile "./token.txt"
  WS.sendTextData conn (encode $ identifyPayload token)

app :: WS.ClientApp ()
app conn = do
  _ <- forever $ do
    msg <- WS.receiveData conn
    let payload = fromJust $ decode msg :: Payload
    case op payload of
      0  -> handleEvent (fromJust $ t payload) msg
      10 -> forkIO (sendHeartbeat conn) >> forkIO (sendIdentify conn) >> return ()
      _  -> return ()

  WS.sendClose conn empty

client :: IO ()
client = runSecureClient "gateway.discord.gg" 443 "/" app
