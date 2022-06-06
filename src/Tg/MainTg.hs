module Tg.MainTg where

import Control.Monad.State (evalStateT, forever)
import qualified Data.Map as Map (empty)
import Tg.App (run, startApp)
import qualified Tg.App (makeH)
import Conf (Config (..),getTime, parseConf)
import Logger (LogConfig (..), LogHandle (..), logger)
import Types (Messenger(TG))

mainTg :: IO ()
mainTg = do
  time <- getTime
  let currLogPath = "./TG.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath "Create log file\n"
  config <- parseConf TG
  let prio = cPriority config
  let handleLog = LogHandle (LogConfig prio) (logger currLogPath)
  let handle = Tg.App.makeH config handleLog
  startApp handle
  evalStateT (forever $ run handle) Map.empty
