{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           App
import           Logger
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State



pullConfig :: IO C.Config
pullConfig = do
  conf <- C.load [C.Required "./bot.config"]
  return conf


main :: IO ()
main =  do
  conf <- pullConfig
  startN   <- C.lookupDefault 1   conf "telegram.startN"
  botToken <- C.lookupDefault "1" conf "telegram.botToken"
  let config = Config startN botToken
  let handle = Handle config handleLog (getUpdates' handle) (confirmUpdates' handle) (sendMessage' handle) (sendKeybWithMsg' handle)
  evalStateT (forever $ run (handle {hConf = config} ) ) []


--pullStartN :: IO Int
--pullStartN = do
  --conf <- C.load [C.Required "./bot.config"]
  --let key = T.pack "telegram.startN"
  --startNn <- C.lookupDefault 1 conf key 
  --return startNn