{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Vk.MainVk
import           Tg.MainTg
import           Data.Char                      (toLower)
import           System.Environment             (getArgs)

main :: IO ()
main =  do
  args <- getArgs
  case map parseArg args of
    (Vk:_)          -> mainVk
    (Tg:_)          -> mainTg
    []              -> putStrLn emptyArgErrMsg
    (Unknown arg:_) -> putStrLn $ unKnownArgErrMsg arg

emptyArgErrMsg :: String
emptyArgErrMsg = "Error. Empty input. In program's command line should be an argument.\nArgument is a messenger, where you want start program \nUse <Vk> - for messenger Vkontakte\nUse <Tg> - for messenger Telegram\nPlease try again with valid argument"
unKnownArgErrMsg :: String -> String
unKnownArgErrMsg str = "Error. Unknown argument in program's command line: \"" ++ str ++ "\".\nArgument is a messenger, where you want start program \nUse <Vk> - for messenger Vkontakte\nUse <Tg> - for messenger Telegram\nPlease try again with valid argument"

data Arg = Vk | Tg | Unknown String

parseArg :: String -> Arg 
parseArg arg = case map toLower arg of
  "vk"        -> Vk
  "vkontakte" -> Vk
  "вк"        -> Vk
  "вконтакте" -> Vk
  "tg"        -> Tg
  "telegram"  -> Tg
  "telegramm" -> Tg
  "тг"        -> Tg
  "телеграм"  -> Tg
  "телеграмм" -> Tg
  _           -> Unknown arg

