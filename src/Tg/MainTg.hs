{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.MainTg where

import           Tg.App
import           Tg.Logger
import qualified Data.Configurator              as C
import qualified Data.Configurator.Types        as C
import           Control.Monad.State
import           Data.Char
import qualified Control.Exception              as E
import           Data.Time.LocalTime


pullConfig :: IO C.Config
pullConfig = do
  C.load [C.Required "./bot.config"] 
    `E.catch` (\e -> putStrLn (show (e :: C.ConfigError)) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: C.KeyError   )) >> return C.empty)
    `E.catch` (\e -> putStrLn (show (e :: E.IOException  )) >> return C.empty)
    `E.catch` (\e -> E.throw $ DuringPullConfigException  $ show (e :: E.SomeException))


mainTg :: IO ()
mainTg =  do
  time <- getTime                          
  let currLogPath = "./TG.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath  "Create log file\n"
  conf           <- pullConfig             
  startN         <- parseConfStartN   conf 
  botToken       <- parseConfBotToken conf 
  prio           <- parseConfPrio     conf 
  helpMsg        <- parseConfHelpMsg  conf 
  repeatQuestion <- parseConfRepeatQ  conf 
  let config = Config startN botToken helpMsg repeatQuestion
  let handleLog = LogHandle (LogConfig prio) (logger handleLog currLogPath)
  let handle = Handle config handleLog (getUpdates' handle) (getShortUpdates' handle) (confirmUpdates' handle) (sendMsg' handle) (sendKeyb' handle) (copyMsg' handle)
  startApp handle
  evalStateT (forever $ run handle ) []

getTime :: IO String
getTime = (do
  time    <- getZonedTime
  return $ show time)     
    `E.catch` (\e -> ((putStrLn $ show (e :: E.SomeException)) >> inputLocalTime) )
    
      


parseConfStartN :: C.Config -> IO Int
parseConfStartN conf = (do
  str <- ((C.lookup conf "telegram.startN") :: IO (Maybe Int))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe Int) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe Int) ) 
  case str of
    Nothing -> inputStartN
    Just 1  -> return 1
    Just 2  -> return 2
    Just 3  -> return 3
    Just 4  -> return 4
    Just 5  -> return 5
    Just _  -> inputStartN)
      `E.catch` (\e -> E.throw $ DuringParseConfigException $ "startN\n" ++ show (e :: E.SomeException))

parseConfBotToken :: C.Config -> IO String
parseConfBotToken conf = (do
  str <- ((C.lookup conf "telegram.botToken") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputBotToken
    Just n  -> return n)
      `E.catch` (\e -> E.throw $ DuringParseConfigException $ "botToken\n" ++ show (e :: E.SomeException))

parseConfPrio :: C.Config -> IO Priority
parseConfPrio conf = (do
  str <- (C.lookup conf "telegram.logLevel" :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputLogLevel
    Just "DEBUG"   -> return DEBUG
    Just "INFO"    -> return INFO
    Just "WARNING" -> return WARNING
    Just "ERROR"   -> return ERROR
    Just _         -> inputLogLevel)
      `E.catch` (\e -> E.throw $ DuringParseConfigException $ "logLevel\n" ++ show (e :: E.SomeException))

parseConfHelpMsg :: C.Config -> IO String
parseConfHelpMsg conf = (do
  str <- ((C.lookup conf "telegram.help_Info_Msg") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputHelpMsg
    Just n  -> return n)
      `E.catch` (\e -> E.throw $ DuringParseConfigException $ "helpMsg\n" ++ show (e :: E.SomeException))

parseConfRepeatQ :: C.Config -> IO String
parseConfRepeatQ conf = (do
  str <- ((C.lookup conf "telegram.repeat_Info_Question") :: IO (Maybe String))
    `E.catch` ( (\_ -> return Nothing) :: C.KeyError  -> IO (Maybe String) )
    `E.catch` ( (\_ -> return Nothing) :: E.IOException -> IO (Maybe String) ) 
  case str of
    Nothing -> inputRepeatQ
    Just n  -> return n)
      `E.catch` (\e -> E.throw $ DuringParseConfigException $ "repeatQuestion\n" ++ show (e :: E.SomeException))



inputStartN :: IO Int
inputStartN = do
  putStrLn "Can`t parse value \"startN\" from configuration file or command line\nPlease, enter start number of repeats. Number from 1 to 5"
  input <- getLine
  case input of
    "1" -> return 1
    "2" -> return 2
    "3" -> return 3
    "4" -> return 4
    "5" -> return 5
    _   -> inputStartN

inputBotToken :: IO String
inputBotToken = do
  putStrLn "Can`t parse value \"botToken\" from configuration file or command line\nPlease, enter bot token"
  getLine

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

inputHelpMsg :: IO String
inputHelpMsg = do
  putStrLn "Can`t parse value \"/help Info Msg\" from configuration file or command line\nPlease, enter \"/help Info Msg\"\nExample: I`m super bot"
  getLine

inputRepeatQ :: IO String
inputRepeatQ = do
  putStrLn "Can`t parse value \"/repeat Info Question\" from configuration file or command line\nPlease, enter \"/repeat Info Question\"\nExample: How many times to repeat message in the future?"
  getLine

inputLocalTime :: IO String
inputLocalTime = (do
  putStrLn "Local time not found\nPlease, enter your local time in any form\nExample: 06.07.2020 16:21"
  getLine) 
    `E.catch` (\e -> E.throw $ DuringGetTimeException $ show (e :: E.SomeException))