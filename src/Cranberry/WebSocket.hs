module Cranberry.WebSocket where

import Cranberry.Event
import Cranberry.Payload

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.Aeson (decode, encode)
import Data.ByteString.Lazy (empty)
import Data.IORef
import Data.Maybe (fromJust, fromMaybe)
import Network.WebSockets (ClientApp)
import Wuss (runSecureClient)
import qualified Network.WebSockets as WS

type Token = String

heartbeat :: IORef Int -> ClientApp ()
heartbeat ref conn = forever $ do
  seq <- readIORef ref
  WS.sendTextData conn . encode $ heartbeatPayload seq
  threadDelay 41250000

identify :: Token -> ClientApp ()
identify token conn = do
  WS.sendTextData conn . encode $ identifyPayload token

app :: ClientApp ()
app conn = do
  token <- filter (/= '\n') <$> readFile "./token.txt"
  ref <- newIORef 0

  _ <- forever $ do
    payload <- fromJust . decode <$> WS.receiveData conn
    modifyIORef' ref . const . fromMaybe 0 $ s payload

    case op payload of
      0  -> let
              eventType = fromJust $ t payload
              eventData = encode .fromJust $ d payload
            in handleEvent token eventType eventData
      10 -> forkIO (heartbeat ref conn)
         >> forkIO (identify token conn)
         >> return ()
      _  -> return ()

  WS.sendClose conn empty

client :: IO ()
client = runSecureClient "gateway.discord.gg" 443 "/" app
