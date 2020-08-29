module Cranberry.Event where

import Cranberry.Payload
import Cranberry.Request
import Cranberry.Utils (emoteUrl)

import Control.Monad (when)
import Control.Monad.Reader (runReaderT)
import Data.Aeson (decode)
import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)

type Args = [String]
type Command = String
type EventData = ByteString
type EventType = String

parse :: String -> (Command, [String])
parse xs = (y, ys)
  where (y:ys) = words xs

prefix :: String
prefix = "c."

onMessageCreate :: EventData -> Action
onMessageCreate d = do
  let msg = fromJust $ decode d :: Message
  let c = content msg
  when (prefix `isPrefixOf` c) $
    let
      (x, y) = parse $ drop (length prefix) c
    in
      handleCommand x y msg

handleCommand :: Command -> Args -> Message -> Action
handleCommand "emoji" xs m = if url == []
  then return ()
  else createMessage (channelId m) url
    where
      url = emoteUrl $ head xs
handleCommand "ping" _ m = createMessage (channelId m) "pong!"
handleCommand _ _ _ = return ()

handleEvent :: EventType -> EventData -> Token -> IO ()
handleEvent t d token = do
  let bot = Bot { token = token }
  case t of
    "MESSAGE_CREATE" -> runReaderT (onMessageCreate d) bot
    _                -> return ()
