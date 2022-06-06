{-# LANGUAGE OverloadedStrings #-}

module Conf where


import Logger (Priority (..))
import Types

data Config = Config
  { cStartN :: N,
    cBotToken :: String,
    cHelpMsg :: String,
    cRepeatQ :: String,
    cPriority :: Priority
  }

