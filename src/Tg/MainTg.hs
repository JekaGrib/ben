{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Tg.MainTg where

import Control.Monad.State (evalStateT, forever)
import Tg.App
  ( Handle(..)
  , confirmUpdates'
  , copyMsg'
  , getShortUpdates'
  , getUpdates'
  , run
  , sendKeyb'
  , sendMsg'
  , startApp
  )
import Tg.Conf (Config(..), getTime, parseConf)
import Tg.Logger (LogConfig(..), LogHandle(..), logger)

mainTg :: IO ()
mainTg = do
  time <- getTime
  let currLogPath = "./TG.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath "Create log file\n"
  config <- parseConf
  let prio = cPriority config
  let handleLog = LogHandle (LogConfig prio) (logger handleLog currLogPath)
  let handle =
        Handle
          config
          handleLog
          (getUpdates' handle)
          (getShortUpdates' handle)
          (confirmUpdates' handle)
          (sendMsg' handle)
          (sendKeyb' handle)
          (copyMsg' handle)
  startApp handle
  evalStateT (forever $ run handle) []
