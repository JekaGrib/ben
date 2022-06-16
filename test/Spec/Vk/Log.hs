module Spec.Vk.Log where

import Control.Monad.State (StateT (..))
import Logger
import Spec.Types
import Vk.Types

handLogDebug :: LogHandle (StateT [MockAction VkAttachMSG] IO)
handLogDebug = LogHandle (LogConfig DEBUG) logTest

handLogMsgDebug :: LogHandle (StateT [MockAction VkAttachMSG] IO)
handLogMsgDebug = LogHandle (LogConfig DEBUG) logMsgTest

handLogWarn :: LogHandle (StateT [MockAction VkAttachMSG] IO)
handLogWarn = LogHandle (LogConfig WARNING) logTest

handLogMsgInfo :: LogHandle (StateT [MockAction VkAttachMSG] IO)
handLogMsgInfo = LogHandle (LogConfig INFO) logMsgTest

logTest :: Priority -> String -> StateT [MockAction VkAttachMSG] IO ()
logTest prio _ = StateT $ \acts ->
  return ((), LOG prio : acts)

logMsgTest :: Priority -> String -> StateT [MockAction VkAttachMSG] IO ()
logMsgTest prio msg = StateT $ \acts ->
  return ((), LOGMSG prio msg : acts)
