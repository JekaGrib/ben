module Logger where

import Prelude   hiding (log)
import System.IO        (appendFile)

logPath1 = "/home/evgenya/abc.log"

data LogHandle m = LogHandle 
   { log :: String -> m ()}

logger :: LogHandle IO -> String -> IO ()
logger h str = appendFile logPath1 str

handleLog = LogHandle (logger handleLog)


