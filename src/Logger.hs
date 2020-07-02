module Logger where

import           Prelude          hiding (log)
import           System.IO        (appendFile)


logPath = "./bot.log"

data LogHandle m = LogHandle 
   { hLogConf :: LogConfig,
     log      :: Priority -> String -> m ()}

data LogConfig = LogConfig
  { cLogLevel :: Priority }

data Priority = DEBUG | INFO | WARNING | ERROR 
                                          deriving (Ord,Eq,Show)

logger :: LogHandle IO -> Priority -> String -> IO ()
logger h currP str  
    | currP >= configP = appendFile logPath (show currP ++ ": " ++ str)
    | otherwise        = return ()
      where configP = cLogLevel (hLogConf h)

logDebug, logInfo, logWarning, logError :: LogHandle m -> String -> m ()
logDebug   h = log h DEBUG
logInfo    h = log h INFO
logWarning h = log h WARNING
logError   h = log h ERROR

--handleLog = LogHandle (logger handleLog) (cLogLevel (hConf handleLog))


