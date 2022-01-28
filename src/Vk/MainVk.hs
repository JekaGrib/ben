{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.MainVk where

import Control.Monad.State (evalStateT, forever)
import qualified Data.Map as Map (fromList)
import Vk.Api.Response (ServerInfo(ServerInfo))
import Vk.App
  ( Handle(..)
  , getDocServer'
  , getLongPollServer'
  , getPhotoServer'
  , getUpdates'
  , goToUrl'
  , loadDocToServ'
  , loadPhotoToServ'
  , run
  , saveDocOnServ'
  , savePhotoOnServ'
  , sendKeyb'
  , sendMsg'
  )
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
  let handle =
        Handle
          config
          handleLog
          (getLongPollServer' handle)
          getUpdates'
          (sendMsg' handle)
          (sendKeyb' handle)
          (getPhotoServer' handle)
          loadPhotoToServ'
          (savePhotoOnServ' handle)
          (getDocServer' handle)
          loadDocToServ'
          (saveDocOnServ' handle)
          goToUrl'
  putStrLn "App started"
  evalStateT (forever $ run handle) (ServerInfo "A" "A" "1", Map.fromList [])
  
