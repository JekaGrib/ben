{-# LANGUAGE RecordWildCards #-}

module Logger where

import Control.Monad (when)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Prelude hiding (log)

data LogHandle m = LogHandle
  { hLogConf :: LogConfig,
    log :: Priority -> String -> m ()
  }

newtype LogConfig = LogConfig
  { cLogLevel :: Priority
  }

liftLogHandle :: (Monad m, MonadTrans t) => LogHandle m -> LogHandle (t m)
liftLogHandle LogHandle {..} = LogHandle hLogConf log''
  where
    log'' prio str = lift $ log prio str

data Priority
  = DEBUG
  | INFO
  | WARNING
  | ERROR
  deriving (Ord, Eq, Show, Read)

logger :: String -> Priority -> String -> IO ()
logger logPath currP str = do
  putStrLn (show currP ++ ": " ++ str)
  appendFile logPath (show currP ++ ": " ++ str ++ "\n")

checkPrioAndLog :: (Applicative m) => LogHandle m -> Priority -> String -> m ()
checkPrioAndLog h prio = when (prio >= configP) . log h prio
  where
    configP = cLogLevel (hLogConf h)

logDebug, logInfo, logWarning, logError :: (Applicative m) => LogHandle m -> String -> m ()
logDebug h = checkPrioAndLog h DEBUG
logInfo h = checkPrioAndLog h INFO
logWarning h = checkPrioAndLog h WARNING
logError h = checkPrioAndLog h ERROR
