{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Tg.Oops where

import qualified Data.Text                      as T
import           Control.Monad.Catch            (Exception, MonadCatch(..), SomeException, throwM)
import qualified Data.ByteString.Lazy           as LBS
import           Tg.Logger                      (LogHandle(..), logError)


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
    deriving (Eq,Show)

instance Exception TGBotException 

data Msg           = Msg        T.Text            deriving (Eq,Show)
data ToUserId      = ToUserId   Integer           deriving (Eq,Show)
data MsgId         = MsgId      Integer           deriving (Eq,Show)


handleExGetUpd :: (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m LBS.ByteString
handleExGetUpd logH e = do
  let ex = DuringGetUpdatesException $ show e 
  logError logH $ show ex
  throwM ex

handleExSendMsg :: (Monad m, MonadCatch m) => LogHandle m -> Integer -> T.Text -> SomeException -> m LBS.ByteString
handleExSendMsg logH usId infoMsg e = do
  let ex = DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show e 
  logError logH $ show ex
  throwM ex

handleExCopyMsg :: (Monad m, MonadCatch m) => LogHandle m -> Integer -> Integer -> SomeException -> m LBS.ByteString
handleExCopyMsg logH usId msgId e = do
  let ex = DuringCopyMsgException (MsgId msgId) (ToUserId usId) $ show e
  logError logH $ show ex
  throwM ex

handleExSendKeyb :: (Monad m, MonadCatch m) => LogHandle m -> Integer -> SomeException -> m LBS.ByteString
handleExSendKeyb logH usId e = do
  let ex = DuringSendKeybException (ToUserId usId) $ show e 
  logError logH $ show ex
  throwM ex

handleExConfUpd :: (Monad m, MonadCatch m) => LogHandle m -> LBS.ByteString -> SomeException -> m LBS.ByteString
handleExConfUpd logH json e = do
  let ex = DuringConfirmUpdatesException $ show e ++ "\nWhen try to confirm old updates: " ++ show json 
  logError logH $ show ex
  throwM ex
 



 
