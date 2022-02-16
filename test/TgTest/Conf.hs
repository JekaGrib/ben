{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module TgTest.Conf where


import Tg.Conf (Config(..))
import Tg.Logger (Priority(..))


config1 :: Config
config1 =
  Config
    { cStartN = 2
    , cBotToken = "ABC123"
    , cHelpMsg = "Hello"
    , cRepeatQ = "Why?"
    , cPriority = DEBUG
    }










