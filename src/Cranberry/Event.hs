module Cranberry.Event where

import Cranberry.Payload
import Cranberry.Request

import Data.Aeson (Value, decode, encode)
import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)

type Command = String
type EventData = ByteString
type EventType = String

prefix :: String
prefix = "c."

onMessageCreate :: Token -> EventData -> IO ()
onMessageCreate token d = do
  let msg = fromJust $ decode d :: Message
  let c = content msg
  if prefix `isPrefixOf` c
    then handleCommand token msg . drop 2 $ c
    else return ()

handleCommand :: Token -> Message -> Command -> IO ()
handleCommand t m "ping" = createMessage t (channelId m) "pong!"
handleCommand t m _ = return ()

handleEvent :: Token -> EventType -> EventData -> IO ()
handleEvent token t d = do
  putStrLn t
  case t of
    "MESSAGE_CREATE" -> onMessageCreate token d
    _                -> return ()
