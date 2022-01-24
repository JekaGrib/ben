{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Tg.Oops where

import qualified Control.Exception as E
import Control.Monad.Catch (Exception, MonadCatch(..), SomeException, throwM)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Configurator.Types as C
import qualified Data.Text as T
import Tg.Logger (LogHandle(..), logError)
import Tg.TypeSynonym


data TGBotException
  = DuringGetUpdatesException String
  | CheckGetUpdatesResponseException String
  | DuringConfirmUpdatesException String
  | CheckConfirmUpdatesResponseException String
  | DuringSendMsgException Msg ToUserId String
  | DuringCopyMsgException MsgId ToUserId String
  | CheckSendMsgResponseException Msg ToUserId String
  | CheckCopyMsgResponseException MsgId ToUserId String
  | DuringSendKeybException ToUserId String
  | CheckSendKeybResponseException ToUserId String
  | DuringGetTimeException String
  | DuringPullConfigException String
  | DuringParseConfigException String
  | DuringInputException String
  deriving (Eq, Show)

instance Exception TGBotException

newtype Msg =
  Msg T.Text
  deriving (Eq, Show)

newtype ToUserId =
  ToUserId UserId
  deriving (Eq, Show)

newtype MsgId =
  MsgId MessageId
  deriving (Eq, Show)

throwAndLogEx :: (Monad m, MonadCatch m) => LogHandle m -> TGBotException -> m a
throwAndLogEx logH ex = do
  let info = show ex
  logError logH info
  throwM ex

-- handles to catch exceptions in logic functions:
handleExGetUpd ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m LBS.ByteString
handleExGetUpd logH e = do
  let ex = DuringGetUpdatesException $ show e
  logError logH $ show ex
  throwM ex

handleExSendMsg ::
     (Monad m, MonadCatch m)
  => LogHandle m
  -> UserId
  -> T.Text
  -> SomeException
  -> m LBS.ByteString
handleExSendMsg logH usId infoMsg e = do
  let ex = DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show e
  logError logH $ show ex
  throwM ex

handleExCopyMsg ::
     (Monad m, MonadCatch m)
  => LogHandle m
  -> UserId
  -> MessageId
  -> SomeException
  -> m LBS.ByteString
handleExCopyMsg logH usId msgId e = do
  let ex = DuringCopyMsgException (MsgId msgId) (ToUserId usId) $ show e
  logError logH $ show ex
  throwM ex

handleExSendKeyb ::
     (Monad m, MonadCatch m)
  => LogHandle m
  -> UserId
  -> SomeException
  -> m LBS.ByteString
handleExSendKeyb logH usId e = do
  let ex = DuringSendKeybException (ToUserId usId) $ show e
  logError logH $ show ex
  throwM ex

handleExConfUpd ::
     (Monad m, MonadCatch m)
  => LogHandle m
  -> LBS.ByteString
  -> SomeException
  -> m LBS.ByteString
handleExConfUpd logH json e = do
  let ex =
        DuringConfirmUpdatesException $
        show e ++ "\nWhen try to confirm old updates: " ++ show json
  logError logH $ show ex
  throwM ex

-- handles to catch exceptions in IO configuration functions:
handleExPullConf :: E.SomeException -> IO C.Config
handleExPullConf e = do
  print e
  E.throw $ DuringPullConfigException $ show e

handleExParseConf :: String -> E.SomeException -> IO a
handleExParseConf str e = do
  print e
  E.throw $ DuringParseConfigException $ str ++ "\n" ++ show e

handleExGetTime :: E.SomeException -> IO String
handleExGetTime e = do
  print e
  E.throw $ DuringGetTimeException $ show e

handleExInput :: String -> E.SomeException -> IO a
handleExInput str e = do
  print e
  E.throw $ DuringInputException $ str ++ "\n" ++ show e
