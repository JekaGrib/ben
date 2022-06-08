{-# LANGUAGE OverloadedStrings #-}

module Spec.Conf where

import Conf (Config (..))
import Logger (Priority (..))

config1 :: Config
config1 =
  Config
    { cStartN = 2,
      cBotToken = "ABC123",
      cHelpMsg = "Hello",
      cRepeatQ = "Why?",
      cPriority = DEBUG
    }
