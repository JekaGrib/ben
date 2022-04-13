{-# LANGUAGE OverloadedStrings #-}

module Spec.Tg.Conf where

import Tg.Conf (Config (..))
import Tg.Logger (Priority (..))

config1 :: Config
config1 =
  Config
    { cStartN = 2,
      cBotToken = "ABC123",
      cHelpMsg = "Hello",
      cRepeatQ = "Why?",
      cPriority = DEBUG
    }
