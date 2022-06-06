{-# LANGUAGE OverloadedStrings #-}

module Vk.MainVk where

import qualified Data.Map as Map (empty)
import Vk.App (run)
import qualified Vk.App (makeH)
import Conf (getTime)
import Vk.Conf (VkConfig(..), parseVkConf)
import Conf (Config(..))
import Logger (LogConfig (..), LogHandle (..), logger)

mainVk :: IO ()
mainVk = do
  time <- getTime
  let currLogPath = "./VK.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath "Create log file\n"
  config <- parseVkConf
  let handleLog =
        LogHandle (LogConfig (cPriority (cConf config))) (logger currLogPath)
  let handle = Vk.App.makeH config handleLog
  putStrLn "App started"
  run handle Map.empty
