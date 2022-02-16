{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}


module VkTest.PrepareAttachment.Log where

import Vk.Logger 
import VkTest.PrepareAttachment.Types
import           Control.Monad.State  (StateT(..))          


handLogDebug :: LogHandle (StateT [MockAction] IO)
handLogDebug = LogHandle (LogConfig DEBUG) logTest

handLogWarn :: LogHandle (StateT [MockAction] IO)
handLogWarn = LogHandle (LogConfig WARNING) logTest

logTest :: Priority -> String -> StateT [MockAction] IO ()
logTest prio _ = StateT $ \acts -> 
  return (() , LOG prio : acts)






