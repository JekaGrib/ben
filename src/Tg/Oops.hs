{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Tg.Oops where

import qualified Control.Exception as E
import Control.Monad.Catch (Exception, MonadCatch (..), SomeException, throwM)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import Tg.Api.Response (Update (..))
import Tg.Logger (LogHandle (..), logError)
import Tg.Types

data TGBotException
  = GetUpdatesException String
  | CheckGetUpdatesResponseException String
  | ConfirmUpdatesException String
  | CheckConfirmUpdatesResponseException String
  | SendMsgException Msg ToUserId String
  | CopyMsgException MsgId ToUserId String
  | CheckSendMsgResponseException Msg ToUserId String
  | CheckCopyMsgResponseException MsgId ToUserId String
  | SendKeybException ToUserId String
  | CheckSendKeybResponseException ToUserId String
  | ConfigException ConfigException
  deriving (Eq, Show)

instance Exception TGBotException

data ConfigException
  = PullConfigException String
  | ParseConfigException String
  | GetTimeException String
  | InputException String
  deriving (Eq, Show)

instance Exception ConfigException

throwAndLogEx :: (Monad m, MonadCatch m) => LogHandle m -> TGBotException -> m a
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

handleExSendMsg ::
  (Monad m, MonadCatch m) =>
  LogHandle m ->
  UserId ->
  T.Text ->
  SomeException ->
  m LBS.ByteString
handleExSendMsg logH usId infoMsg e = do
  let ex = SendMsgException (Msg infoMsg) (ToUserId usId) $ show e
  throwAndLogEx logH ex

handleExCopyMsg ::
  (Monad m, MonadCatch m) =>
  LogHandle m ->
  UserId ->
  MessageId ->
  SomeException ->
  m LBS.ByteString
handleExCopyMsg logH usId msgId e = do
  let ex = CopyMsgException (MsgId msgId) (ToUserId usId) $ show e
  throwAndLogEx logH ex

handleExSendKeyb ::
  (Monad m, MonadCatch m) =>
  LogHandle m ->
  UserId ->
  SomeException ->
  m LBS.ByteString
handleExSendKeyb logH usId e = do
  let ex = SendKeybException (ToUserId usId) $ show e
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
