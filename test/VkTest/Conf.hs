{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module VkTest.Conf where


import Vk.Conf (Config(..))
import Vk.Logger (Priority(..))

config1 :: Config
config1 =
  Config
    { cStartN = 2
    , cBotToken = "ABC123"
    , cHelpMsg = "Hello"
    , cRepeatQ = "Why?"
    , cGroupId = 321
    , cPriority = DEBUG
    }











