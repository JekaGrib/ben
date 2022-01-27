{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Vk.Oops where

import           Control.Monad.Catch
import Vk.TypeSynonym
import qualified Data.ByteString.Lazy           as LBS
import Vk.Logger (LogHandle(..), logError)


data VKBotException 
  = DuringGetLongPollServerException String
  | CheckGetServerResponseException String 
  | DuringGetUpdatesException String
  | CheckGetUpdatesResponseException String
  | DuringSendMsgException MSG ToUserId String
  | CheckSendMsgResponseException MSG ToUserId String
  | DuringSendKeybException ToUserId String
  | CheckSendKeybResponseException ToUserId String
  | DuringGetTimeException String
  | DuringPullConfigException String
  | DuringParseConfigException String
    deriving (Eq,Show)

instance Exception VKBotException 

data ToUserId      = ToUserId   UserId               deriving (Eq,Show)


throwAndLogEx :: (Monad m, MonadCatch m) => LogHandle m -> VKBotException -> m a
throwAndLogEx logH ex = do
  let info = show ex
  logError logH info
  throwM ex


handleExGetServ ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m LBS.ByteString
handleExGetServ logH e = do
  let ex = DuringGetLongPollServerException $ show e
  throwAndLogEx logH ex

handleExGetUpd ::
     (Monad m, MonadCatch m) => LogHandle m -> SomeException -> m LBS.ByteString
handleExGetUpd logH e = do
  let ex = DuringGetUpdatesException $ show e
  throwAndLogEx logH ex

handleExSendMsg ::
     (Monad m, MonadCatch m) => LogHandle m -> UserId -> MSG -> SomeException -> m LBS.ByteString
handleExSendMsg logH usId msg e = do
  let ex = DuringSendMsgException msg (ToUserId usId) $ show e 
  throwAndLogEx logH ex

handleExSendKeyb ::
     (Monad m, MonadCatch m) => LogHandle m -> UserId -> SomeException -> m LBS.ByteString
handleExSendKeyb logH usId e = do
  let ex = DuringSendKeybException (ToUserId usId) $ show e 
  throwAndLogEx logH ex