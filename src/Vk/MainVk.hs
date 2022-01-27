{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.MainVk where

import           Vk.App (Handle(..),run,getLongPollServer',getUpdates',sendMsg',sendKeyb',getPhotoServer',loadPhotoToServ',savePhotoOnServ',getDocServer',loadDocToServ',saveDocOnServ',goToUrl')
import           Vk.Logger (LogHandle(..),LogConfig(..),logger)
import           Vk.Api.Response (ServerInfo(ServerInfo))
import           Control.Monad.State (evalStateT,forever)
import Vk.Conf (getTime,parseConf,Config(..))
import qualified Data.Map as Map(fromList) 


mainVk :: IO ()
mainVk = do
  time <- getTime                          
  let currLogPath = "./VK.LogSession: " ++ show time ++ " bot.log"
  writeFile currLogPath  "Create log file\n"
  config <- parseConf
  let handleLog = LogHandle (LogConfig (cPriority config)) (logger handleLog currLogPath)
  let handle = Handle config handleLog (getLongPollServer' handle) getUpdates' (sendMsg' handle) (sendKeyb' handle) (getPhotoServer' handle) loadPhotoToServ' (savePhotoOnServ' handle) (getDocServer' handle) loadDocToServ' (saveDocOnServ' handle) goToUrl'
  putStrLn "App started"
  evalStateT (forever $ run handle) (ServerInfo "A" "A" "1" ,Map.fromList [])
  
