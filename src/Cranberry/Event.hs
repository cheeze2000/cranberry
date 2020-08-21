module Cranberry.Event where

import Cranberry.Payload
import Cranberry.Request

import Data.Aeson (Value, decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.Maybe (fromJust)

type EventData = ByteString
type EventType = String

onMessageCreate :: Token -> EventData -> IO ()
onMessageCreate token d = do
  let msg = fromJust $ decode d :: MessageCreatePayload
  let c = channelId msg
  if content msg == "ping"
    then createMessage token c "pong!"
    else return ()

handleEvent :: Token -> EventType -> EventData -> IO ()
handleEvent token t d = do
  putStrLn t
  case t of
    "MESSAGE_CREATE" -> onMessageCreate token d
    _                -> return ()
