{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}


module VkTest.Log where

import Vk.Logger 
import VkTest.Types
import           Control.Monad.State  (StateT(..))          

handLogDebug :: LogHandle (StateT [MockAction] IO)
handLogDebug = LogHandle (LogConfig DEBUG) logTest

handLogMsgDebug :: LogHandle (StateT [MockAction] IO)
handLogMsgDebug = LogHandle (LogConfig DEBUG) logMsgTest

handLogWarn :: LogHandle (StateT [MockAction] IO)
handLogWarn = LogHandle (LogConfig WARNING) logTest

logTest :: Priority -> String -> StateT [MockAction] IO ()
logTest prio _ = StateT $ \acts -> 
  return (() , LOG prio : acts)   

logMsgTest :: Priority -> String -> StateT [MockAction] IO ()
logMsgTest prio msg = StateT $ \acts -> 
  return (() , LOGMSG prio msg : acts)





