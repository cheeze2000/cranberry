{-# LANGUAGE OverloadedStrings #-}

module Event where

import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe (fromJust)
import           Payload
import           Request
import qualified Data.HashMap.Strict as HM

onMessageCreate :: MessageCreatePayload -> IO ()
onMessageCreate msg = case content msg of
  "ping" -> createMessage (channelId msg) "pong"
  _      -> return ()

handleEvent :: String -> ByteString -> IO ()
handleEvent event msg = do
  let Object xs = fromJust $ decode msg :: Value
  let payloadData = encode . fromJust $ HM.lookup "d" xs

  case event of
    "MESSAGE_CREATE" -> onMessageCreate . fromJust $ decode payloadData
    _ -> putStrLn $ event ++ " event fired!"
