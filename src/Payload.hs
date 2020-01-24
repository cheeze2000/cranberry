{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Payload where

import Data.Aeson
import Data.Aeson.TH
import Text.Casing (fromHumps, toQuietSnake)

data Payload = Payload
  { op :: Int
  , t  :: Maybe String
  }

data MessageCreatePayload = MessageCreatePayload
  { channelId :: String
  , content   :: String
  }

heartbeatPayload :: Value
heartbeatPayload = object
  [ "op" .= (1 :: Int)
  , "d"  .= (0 :: Int)
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

deriveJSON defaultOptions ''Payload
deriveJSON defaultOptions{fieldLabelModifier = toQuietSnake . fromHumps} ''MessageCreatePayload
