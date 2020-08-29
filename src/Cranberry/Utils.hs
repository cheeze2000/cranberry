module Cranberry.Utils where

import Data.Char (isDigit)

emoteId :: String -> String
emoteId = takeWhile isDigit . drop 1 . dropWhile (/= ':') . drop 3

emoteUrl :: String -> String
emoteUrl xs =
  let
    e = if xs !! 1 == 'a' then ".gif" else ".png"
    i = emoteId xs
  in
    case i of
      [] -> ""
      i  -> "https://cdn.discordapp.com/emojis/"
         ++ i
         ++ e
         ++ "?size=1024"
