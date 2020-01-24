{-# LANGUAGE OverloadedStrings #-}

module Request where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString (append)
import Data.ByteString.Internal (packChars)
import Data.Text (pack)
import Network.HTTP.Req

createMessage :: String -> String -> IO ()
createMessage channelId content = runReq defaultHttpConfig $ do
  token <- liftIO $ packChars . filter (/= '\n') <$> readFile "./token.txt"
  _ <- req POST
    (https "discordapp.com" /: "api" /: "channels" /: (pack channelId) /: "messages")
    (ReqBodyJson $ object ["content" .= content])
    jsonResponse
    (header "Authorization" (append "Bot " token) <> header "Content-Type" "application/json") :: Req (JsonResponse Value)
  liftIO $ return ()
