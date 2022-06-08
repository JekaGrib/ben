module Spec.Log where

import Control.Monad.State (StateT (..),modify)
import Spec.Types
import Logger

handLogDebug :: LogHandle (StateT [MockAction a] IO)
handLogDebug = LogHandle (LogConfig DEBUG) logTest

handLogMsgDebug :: LogHandle (StateT [MockAction a] IO)
handLogMsgDebug = LogHandle (LogConfig DEBUG) logMsgTest

handLogWarn :: LogHandle (StateT [MockAction a] IO)
handLogWarn = LogHandle (LogConfig WARNING) logTest

logTest :: Priority -> String -> StateT [MockAction a] IO ()
logTest prio _ = 
  modify ((LOG prio) :) 

logMsgTest :: Priority -> String -> StateT [MockAction a] IO ()
logMsgTest prio msg = 
  modify ((LOGMSG prio msg) :)
