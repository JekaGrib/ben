{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.MainVk where

import Control.Monad.State (evalStateT, forever)
import qualified Data.Map as Map (fromList)
import Vk.Api.Response (ServerInfo(ServerInfo))
import Vk.App (run)
import qualified Vk.App (makeH)
import Vk.Conf (Config(..), getTime, parseConf)
import Vk.Logger (LogConfig(..), LogHandle(..), logger)

mainVk :: IO ()
mainVk = do
  time <- getTime
  let currLogPath = "./VK.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath "Create log file\n"
  config <- parseConf
  let handleLog =
        LogHandle (LogConfig (cPriority config)) (logger handleLog currLogPath)
  let handle = Vk.App.makeH config handleLog
  putStrLn "App started"
  evalStateT (forever $ run handle) (ServerInfo "A" "A" "1", Map.fromList [])
  
