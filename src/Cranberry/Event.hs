module Cranberry.Event where

import Cranberry.Payload
import Cranberry.Request

import Data.Aeson (Value, decode, encode)
import Data.Char (isDigit)
import Data.ByteString.Lazy (ByteString)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)

type Command = String
type EventData = ByteString
type EventType = String

parse :: String -> (Command, [String])
parse xs = (y, ys)
  where (y:ys) = words xs

prefix :: String
prefix = "c."

onMessageCreate :: Token -> EventData -> IO ()
onMessageCreate token d = do
  let msg = fromJust $ decode d :: Message
  let c = content msg
  if prefix `isPrefixOf` c
    then handleCommand token msg . parse . drop 2 $ c
    else return ()

handleCommand :: Token -> Message -> (Command, [String]) -> IO ()
handleCommand t m ("emoji", xs) = createMessage t (channelId m) url
  where url = "https://cdn.discordapp.com/emojis/"
           ++ takeWhile isDigit (dropWhile (not . isDigit) $ unwords xs)
           ++ ".png?size=1024"
handleCommand t m ("ping", _) = createMessage t (channelId m) "pong!"
handleCommand t m _ = return ()

handleEvent :: Token -> EventType -> EventData -> IO ()
handleEvent token t d = do
  putStrLn t
  case t of
    "MESSAGE_CREATE" -> onMessageCreate token d
    _                -> return ()
