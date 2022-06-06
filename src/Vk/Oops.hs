module Vk.Oops where

import Control.Monad.Catch (Exception, MonadCatch, SomeException, throwM)
import Logger (LogHandle (..), logError)
import Vk.Types
import Types

data VKBotException
  = GetLongPollServerException String
  | CheckGetServerResponseException String
  | GetUpdatesException String
  | CheckGetUpdatesResponseException String
  | PrependAttachmetToSendException PrependAttachmetException
  deriving (Eq, Show)

instance Exception VKBotException

data PrependAttachmetException
  = GetUploadServerException String
  | CheckGetUploadServerResponseException String
  | LoadToServException String
  | CheckLoadToServResponseException String
  | SaveOnServException String
  | CheckSaveOnServResponseException String
  | GoToUrlException String
  deriving (Eq, Show)

instance Exception PrependAttachmetException

throwAndLogEx :: (MonadCatch m) => LogHandle m -> VKBotException -> m b
throwAndLogEx logH ex = do
  let info = show ex
  logError logH info
  throwM ex

throwAndLogPrepAttEx ::
  (MonadCatch m) => LogHandle m -> PrependAttachmetException -> m a
throwAndLogPrepAttEx logH ex =
  throwAndLogEx logH (PrependAttachmetToSendException ex)

-- handles to catch exceptions in logic functions:
handleExGetLongPollServ ::
  (MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExGetLongPollServ logH e = do
  let ex = GetLongPollServerException $ show e
  throwAndLogEx logH ex

handleExGetUpd ::
  (MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExGetUpd logH e = do
  let ex = GetUpdatesException $ show e
  throwAndLogEx logH ex


handleExGetUploadServ ::
  (MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExGetUploadServ logH e = do
  let ex = GetUploadServerException $ show e
  throwAndLogPrepAttEx logH ex

handleExLoadToServ ::
  (MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExLoadToServ logH e = do
  let ex = LoadToServException $ show e
  throwAndLogPrepAttEx logH ex

handleExSaveOnServ ::
  (MonadCatch m) => LogHandle m -> SomeException -> m Response
handleExSaveOnServ logH e = do
  let ex = SaveOnServException $ show e
  throwAndLogPrepAttEx logH ex

handleExGoToUrl ::
  (MonadCatch m) => LogHandle m -> SomeException -> m ResponseS
handleExGoToUrl logH e = do
  let ex = GoToUrlException $ show e
  throwAndLogPrepAttEx logH ex

