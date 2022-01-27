{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Vk.Oops where

import           Control.Monad.Catch (Exception,MonadCatch,SomeException,throwM)
import Vk.TypeSynonym
import qualified Data.ByteString.Lazy           as LBS
import Vk.Logger (LogHandle(..), logError)
import qualified Control.Exception as E
import qualified Data.Configurator.Types              as C



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
  | DuringInputException String
    deriving (Eq,Show)

instance Exception VKBotException 



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