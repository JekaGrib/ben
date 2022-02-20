{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

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
