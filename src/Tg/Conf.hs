{-# LANGUAGE OverloadedStrings #-}

module Tg.Conf where

import qualified Control.Exception as E
import Data.Char (toUpper)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Time.LocalTime (getZonedTime)
import Logger (Priority (..))
import Oops
  ( handleExGetTime,
    handleExInput,
    handleExParseConf,
    handleExPullConf,
  )
import Types
import Conf (Config(..))


parseConf :: IO Config
parseConf = do
  conf <- pullConfig `E.catch` handleExPullConf
  startN <- parseConfStartN conf `E.catch` handleExParseConf "telegram.startN"
  botToken <-
    parseConfBotToken conf `E.catch` handleExParseConf "telegram.botToken"
  prio <- parseConfPrio conf `E.catch` handleExParseConf "telegram.logLevel"
  helpMsg <-
    parseConfHelpMsg conf `E.catch` handleExParseConf "telegram.help_Info_Msg"
  repeatQuestion <-
    parseConfRepeatQ conf
      `E.catch` handleExParseConf "telegram.repeat_Info_Question"
  return $ Config startN botToken helpMsg repeatQuestion prio

pullConfig :: IO C.Config
pullConfig =
  C.load [C.Required "./bot.config"]
    `E.catch` (\e -> print (e :: C.ConfigError) >> return C.empty)
    `E.catch` (\e -> print (e :: C.KeyError) >> return C.empty)
    `E.catch` (\e -> print (e :: E.IOException) >> return C.empty)

-- parse config values functions:
parseConfStartN :: C.Config -> IO N
parseConfStartN conf = do
  str <-
    (C.lookup conf "telegram.startN" :: IO (Maybe N))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe N))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe N))
  case str of
    Nothing -> inputStartN `E.catch` handleExInput "startN"
    Just 1 -> return 1
    Just 2 -> return 2
    Just 3 -> return 3
    Just 4 -> return 4
    Just 5 -> return 5
    Just _ -> inputStartN `E.catch` handleExInput "startN"

parseConfBotToken :: C.Config -> IO String
parseConfBotToken conf = do
  str <-
    (C.lookup conf "telegram.botToken" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  maybe inputBotToken return str

parseConfPrio :: C.Config -> IO Priority
parseConfPrio conf = do
  str <-
    (C.lookup conf "telegram.logLevel" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case str of
    Nothing -> inputLogLevel `E.catch` handleExInput "logLevel"
    Just "DEBUG" -> return DEBUG
    Just "INFO" -> return INFO
    Just "WARNING" -> return WARNING
    Just "ERROR" -> return ERROR
    Just _ -> inputLogLevel `E.catch` handleExInput "logLevel"

parseConfHelpMsg :: C.Config -> IO String
parseConfHelpMsg conf = do
  str <-
    (C.lookup conf "telegram.help_Info_Msg" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case str of
    Nothing -> inputHelpMsg `E.catch` handleExInput "help_Info_Msg"
    Just n -> return n

parseConfRepeatQ :: C.Config -> IO String
parseConfRepeatQ conf = do
  str <-
    (C.lookup conf "telegram.repeat_Info_Question" :: IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe String))
      `E.catch` ((\_ -> return Nothing) :: E.IOException -> IO (Maybe String))
  case str of
    Nothing -> inputRepeatQ `E.catch` handleExInput "repeat_Info_Question"
    Just n -> return n

-- input functions:
inputStartN :: IO N
inputStartN = do
  putStrLn
    "Can`t parse value \"startN\" from configuration file or command line\nPlease, enter start number of repeats. Number from 1 to 5"
  input <- getLine
  case input of
    "1" -> return 1
    "2" -> return 2
    "3" -> return 3
    "4" -> return 4
    "5" -> return 5
    _ -> inputStartN `E.catch` handleExInput "startN"

inputBotToken :: IO String
inputBotToken = do
  putStrLn
    "Can`t parse value \"botToken\" from configuration file or command line\nPlease, enter bot token"
  getLine

inputLogLevel :: IO Priority
inputLogLevel = do
  putStrLn
    "Can`t parse value \"logLevel\" from configuration file or command line\nPlease, enter logging level (logs of this level and higher will be recorded)\nAvailable levels: DEBUG ; INFO ; WARNING ; ERROR (without quotes)"
  input <- getLine
  case map toUpper input of
    "DEBUG" -> return DEBUG
    "INFO" -> return INFO
    "WARNING" -> return WARNING
    "ERROR" -> return ERROR
    _ -> inputLogLevel

inputHelpMsg :: IO String
inputHelpMsg = do
  putStrLn
    "Can`t parse value \"/help Info Msg\" from configuration file or command line\nPlease, enter \"/help Info Msg\"\nExample: I`m super bot"
  getLine

inputRepeatQ :: IO String
inputRepeatQ = do
  putStrLn
    "Can`t parse value \"/repeat Info Question\" from configuration file or command line\nPlease, enter \"/repeat Info Question\"\nExample: How many times to repeat message in the future?"
  getLine

inputLocalTime :: IO String
inputLocalTime = do
  putStrLn
    "Local time not found\nPlease, enter your local time in any form\nExample: 06.07.2020 16:21"
  getLine

-- getTime function:
getTime :: IO String
getTime =
  (show <$> getZonedTime)
    `E.catch` (\e -> print (e :: E.SomeException) >> inputLocalTime `E.catch` handleExInput "local_time")
    `E.catch` handleExGetTime
