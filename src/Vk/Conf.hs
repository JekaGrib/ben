{-# LANGUAGE OverloadedStrings #-}

module Vk.Conf where

import Conf (Config (..), parseConf, pullConfig)
import Control.Exception as E
import Data.Char (isNumber)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Error
  ( handleExInput,
    handleExParseConf,
    handleExPullConf,
  )
import Types
import Vk.Types

data VkConfig = VkConfig
  { cGroupId :: GroupId,
    cConf :: Config
  }

parseVkConf :: IO VkConfig
parseVkConf = do
  config <- parseConf VK
  conf <- pullConfig `E.catch` handleExPullConf
  groupId <- parseConfGroupId conf `E.catch` handleExParseConf "VK.group_id"
  return $ VkConfig groupId config

-- parse config values functions:

parseConfGroupId :: C.Config -> IO GroupId
parseConfGroupId conf = do
  str <-
    (C.lookup conf "VK.group_id" :: IO (Maybe GroupId))
      `catch` ((\_ -> return Nothing) :: C.KeyError -> IO (Maybe GroupId))
      `catch` ((\_ -> return Nothing) :: IOException -> IO (Maybe GroupId))
  case str of
    Nothing -> inputGroupId `E.catch` handleExInput "group_id"
    Just n -> return n

-- input functions:

inputGroupId :: IO GroupId
inputGroupId = do
  putStrLn
    "Can`t parse value \"group id\" from configuration file or command line\nPlease, enter NUMBER of group id\nExample: 123456789"
  str <- getLine
  if all isNumber str
    then return (read str)
    else inputGroupId `E.catch` handleExInput "group_id"
