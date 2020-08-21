{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Cranberry.Payload where

import Data.Aeson
import Data.Aeson.TH
import Text.Casing (fromHumps, toQuietSnake)

heartbeatPayload :: Int -> Value
heartbeatPayload seq = object
  [ "op" .= (1 :: Int)
  , "d"  .= (seq :: Int)
  ]

identifyPayload :: String -> Value
identifyPayload token = object
  [ "op" .= (2 :: Int)
  , "d"  .= object
    [ "token" .= token
    , "properties" .= object
      [ "$os"      .= ("Cranberry" :: String)
      , "$browser" .= ("Cranberry" :: String)
      , "$device"  .= ("Cranberry" :: String)
      ]
    ]
  ]

data Payload = Payload
  { op :: Int
  , d  :: Maybe Value
  , s  :: Maybe Int
  , t  :: Maybe String
  }

data Message = Message
  { channelId :: String
  , content   :: String
  }

deriveJSON defaultOptions ''Payload
deriveJSON defaultOptions{fieldLabelModifier = toQuietSnake . fromHumps} ''Message
