{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Tg.MainTg where

import Control.Monad.State (evalStateT, forever)
import Tg.App (startApp,run)
import qualified Tg.App (makeH)
import Tg.Conf (Config(..), getTime, parseConf)
import Tg.Logger (LogConfig(..), LogHandle(..), logger)
import qualified Data.Map as Map(fromList) 


mainTg :: IO ()
mainTg = do
  time <- getTime
  let currLogPath = "./TG.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath "Create log file\n"
  config <- parseConf
  let prio = cPriority config
  let handleLog = LogHandle (LogConfig prio) (logger currLogPath)
  let handle = Tg.App.makeH config handleLog
  startApp handle
  evalStateT (forever $ run handle) $ Map.fromList []
