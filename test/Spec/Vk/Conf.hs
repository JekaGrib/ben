{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Vk.Conf where

import Vk.Conf (Config (..))
import Vk.Logger (Priority (..))

config1 :: Config
config1 =
  Config
    { cStartN = 2,
      cBotToken = "ABC123",
      cHelpMsg = "Hello",
      cRepeatQ = "Why?",
      cGroupId = 321,
      cPriority = DEBUG
    }
