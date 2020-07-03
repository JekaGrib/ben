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
import           Data.Char



pullConfig :: IO C.Config
pullConfig = do
  conf <- C.load [C.Required "./bot.config"]
  return conf


main :: IO ()
main =  do
  conf <- pullConfig
  startN   <- C.lookupDefault 1       conf "telegram.startN"
  botToken <- C.lookupDefault "1"     conf "telegram.botToken"
  prio     <- parseConfPrio conf
  let config = Config startN botToken
  let handleLog = LogHandle (LogConfig prio) (logger handleLog)
  let handle = Handle config handleLog (getUpdates' handle) (confirmUpdates' handle) (sendMessage' handle) (sendKeybWithMsg' handle)
  startApp handle
  evalStateT (forever $ run handle ) []


parseConfPrio :: C.Config -> IO Priority
parseConfPrio conf = do
  str <- C.lookup conf "telegram.logLevel" :: IO (Maybe String)
  case str of
    Nothing -> inputLogLevel
    Just "DEBUG"   -> return DEBUG
    Just "INFO"    -> return INFO
    Just "WARNING" -> return WARNING
    Just "ERROR"   -> return ERROR
    Just _         -> inputLogLevel

inputLogLevel :: IO Priority
inputLogLevel = do
  putStrLn "Can`t parse value \"logLevel\" from configuration file or command line\nPlease, enter logging level (logs of this level and higher will be recorded)\nAvailable levels: DEBUG ; INFO ; WARNING ; ERROR (without quotes)"
  input <- getLine
  case (map toUpper input) of
    "DEBUG"   -> return DEBUG
    "INFO"    -> return INFO
    "WARNING" -> return WARNING
    "ERROR"   -> return ERROR
    _         -> inputLogLevel