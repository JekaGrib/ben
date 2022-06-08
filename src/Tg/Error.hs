module Tg.Error where

import Control.Monad.Catch (Exception, MonadCatch (..), SomeException, throwM)
import qualified Data.ByteString.Lazy as LBS
import Logger (LogHandle (..), logError)
import Tg.Api.Response (Update (..))

data TGBotException
  = GetUpdatesException String
  | CheckGetUpdatesResponseException String
  | ConfirmUpdatesException String
  | CheckConfirmUpdatesResponseException String
  deriving (Eq, Show)

instance Exception TGBotException

throwAndLogEx :: (MonadCatch m) => LogHandle m -> TGBotException -> m b
throwAndLogEx logH ex = do
  let info = show ex
  logError logH info
  throwM ex

-- handles to catch exceptions in logic functions:
handleExGetUpd ::
  (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m LBS.ByteString
handleExGetUpd logH e = do
  let ex = GetUpdatesException $ show e
  throwAndLogEx logH ex

handleExConfUpd ::
  (Monad m, MonadCatch m) =>
  LogHandle m ->
  [Update] ->
  SomeException ->
  m LBS.ByteString
handleExConfUpd logH upds e = do
  let ex =
        ConfirmUpdatesException $
          show e ++ "\nWhen try to confirm old updates: " ++ show upds
  throwAndLogEx logH ex
