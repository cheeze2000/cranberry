{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Cranberry.Request where

import Control.Exception (catch)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.ByteString (ByteString, append)
import Data.ByteString.Internal (packChars)
import Data.Text (pack)
import Network.HTTP.Req

data Bot = Bot { token :: Token }

type Action = ReaderT Bot IO ()
type ChannelId = String
type Token = String

apiUrl :: Url 'Https
apiUrl = (https "discordapp.com") /: "api"

authHeader :: ByteString -> Option scheme
authHeader t = header "Authorization" (append "Bot " t)
            <> header "Content-Type" "application/json"

createMessage' :: Token -> ChannelId -> String -> IO ()
createMessage' t c s = runReq defaultHttpConfig $ do
  let url = apiUrl /: "channels" /: (pack c) /: "messages"
  _ <- req
    POST
    url
    (ReqBodyJson $ object ["content" .= s])
    jsonResponse
    (authHeader $ packChars t) :: Req (JsonResponse Value)
  liftIO $ return ()

createMessage :: ChannelId -> String -> Action
createMessage c s = do
  t <- asks token
  liftIO $ catch (createMessage' t c s) logError

logError :: HttpException -> IO ()
logError = print
