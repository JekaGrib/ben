{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Vk.Oops where

import           Control.Monad.Catch (Exception,MonadCatch,SomeException,throwM)
import Vk.Types
import Vk.Logger (LogHandle(..), logError)
import qualified Control.Exception as E
import qualified Data.Configurator.Types              as C



data VKBotException 
  = GetLongPollServerException String
  | CheckGetServerResponseException String 
  | GetUpdatesException String
  | CheckGetUpdatesResponseException String
  | SendMsgException MSG ToUserId String
  | CheckSendMsgResponseException MSG ToUserId String
  | SendKeybException ToUserId String
  | CheckSendKeybResponseException ToUserId String
  | PrependAttachmetToSendException PrependAttachmetException
  | ConfigException ConfigException
    deriving (Eq,Show)

instance Exception VKBotException 

data PrependAttachmetException
  = GetUploadServerException String
  | CheckGetUploadServerResponseException String 
  | LoadToServException String
  | CheckLoadToServResponseException String
  | SaveOnServException String
  | CheckSaveOnServResponseException String
  | GoToUrlException String
    deriving (Eq,Show)

instance Exception PrependAttachmetException 

data ConfigException
  = PullConfigException String
  | ParseConfigException String
  | GetTimeException String
  | InputException String
    deriving (Eq,Show)

instance Exception ConfigException 

throwAndLogEx :: (Monad m, MonadCatch m) => LogHandle m -> VKBotException -> m a
throwAndLogEx logH ex = do
  let info = show ex
  logError logH info
  throwM ex

throwAndLogPrepAttEx :: (Monad m, MonadCatch m) => LogHandle m -> PrependAttachmetException -> m a
throwAndLogPrepAttEx logH ex = throwAndLogEx logH (PrependAttachmetToSendException ex)

--throwAndLogConfEx :: (Monad m, MonadCatch m) => LogHandle m -> PrependAttachmetException -> m a
--throwAndLogConfEx logH ex = throwAndLogEx logH (ConfigException ex)

handleExGetLongPollServ ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExGetLongPollServ logH e = do
  let ex = GetLongPollServerException $ show e
  throwAndLogEx logH ex

handleExGetUpd ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExGetUpd logH e = do
  let ex = GetUpdatesException $ show e
  throwAndLogEx logH ex

handleExSendMsg ::
     (Monad m, MonadCatch m) => LogHandle m -> UserId -> MSG -> SomeException -> m Response
handleExSendMsg logH usId msg e = do
  let ex = SendMsgException msg (ToUserId usId) $ show e 
  throwAndLogEx logH ex

handleExSendKeyb ::
     (Monad m, MonadCatch m) => LogHandle m -> UserId -> SomeException -> m Response
handleExSendKeyb logH usId e = do
  let ex = SendKeybException (ToUserId usId) $ show e 
  throwAndLogEx logH ex

handleExGetUploadServ ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExGetUploadServ logH e = do
  let ex = GetUploadServerException $ show e
  throwAndLogPrepAttEx logH ex

handleExLoadToServ ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExLoadToServ logH e = do
  let ex = LoadToServException $ show e 
  throwAndLogPrepAttEx logH ex

handleExSaveOnServ ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExSaveOnServ logH e = do
  let ex = SaveOnServException $ show e 
  throwAndLogPrepAttEx logH ex

handleExGoToUrl ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m ResponseS
handleExGoToUrl logH e = do
  let ex = GoToUrlException $ show e 
  throwAndLogPrepAttEx logH ex

-- handles to catch exceptions in IO configuration functions:
handleExPullConf :: E.SomeException -> IO C.Config
handleExPullConf e = do
  print e
  E.throw $ PullConfigException $ show e

handleExParseConf :: String -> E.SomeException -> IO a
handleExParseConf str e = do
  print e
  E.throw $ ParseConfigException $ str ++ "\n" ++ show e

handleExGetTime :: E.SomeException -> IO String
handleExGetTime e = do
  print e
  E.throw $ GetTimeException $ show e

handleExInput :: String -> E.SomeException -> IO a
handleExInput str e = do
  print e
  E.throw $ InputException $ str ++ "\n" ++ show e