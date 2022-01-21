{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Tg.MainTg where

import           Tg.App                         (startApp, run, Handle(..), getUpdates', getShortUpdates', confirmUpdates', sendMsg', sendKeyb', copyMsg')
import           Tg.Logger                      (LogHandle(..), LogConfig(..), logger)
import           Tg.Conf                        (Config(..), parseConf, getTime)
import           Control.Monad.State            (evalStateT, forever)


mainTg :: IO ()
mainTg =  do
  time <- getTime                          
  let currLogPath = "./TG.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath  "Create log file\n" 
  config <- parseConf
  let prio = cPriority config
  let handleLog = LogHandle (LogConfig prio) (logger handleLog currLogPath)
  let handle = Handle config handleLog (getUpdates' handle) (getShortUpdates' handle) (confirmUpdates' handle) (sendMsg' handle) (sendKeyb' handle) (copyMsg' handle)
  startApp handle
  evalStateT (forever $ run handle ) []


