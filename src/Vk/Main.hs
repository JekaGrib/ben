{-# LANGUAGE OverloadedStrings #-}

module Vk.Main where

import Conf (Config (..), getTime)
import qualified Data.Map as Map (empty)
import Logger (LogConfig (..), LogHandle (..), logger)
import Vk.App (run)
import qualified Vk.App (makeH)
import Vk.Conf (VkConfig (..), parseVkConf)

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
