{-# LANGUAGE OverloadedStrings #-}

module Spec.App.Handlers where

import App (Handle (..))
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.State (StateT (..), modify)
import Network.HTTP.Client (HttpException (InvalidUrlException))
import Spec.Conf (config1)
import Spec.Log
import Spec.Types
import Types

sendMsgTest ::
  Response -> UserId -> TextOfMsg -> StateT [MockAction a] IO Response
sendMsgTest json usId msg = do
  modify (SENDMSG usId msg :)
  return json

sendAttachMsgTest ::
  Response -> a -> UserId -> StateT [MockAction a] IO Response
sendAttachMsgTest json att usId = do
  modify (SENDAttachMSG usId att :)
  return json

sendKeybTest ::
  Response -> UserId -> N -> TextOfMsg -> StateT [MockAction a] IO Response
sendKeybTest json usId currN msg = do
  modify (SENDKEYB usId currN msg :)
  return json

isValidResponseTest ::
  Response -> Result
isValidResponseTest "ok" = Success
isValidResponseTest _ = NotSuccess "oops"

throwHttpEx :: (MonadThrow m) => m a
throwHttpEx = throwM $ InvalidUrlException "" ""

sendMsgTestEx ::
  (MonadThrow m) =>
  UserId ->
  TextOfMsg ->
  m Response
sendMsgTestEx _ _ = throwHttpEx

sendAttachMsgTestEx ::
  (MonadThrow m) =>
  a ->
  UserId ->
  m Response
sendAttachMsgTestEx _ _ = throwHttpEx

sendKeybTestEx ::
  (MonadThrow m) =>
  UserId ->
  N ->
  TextOfMsg ->
  m Response
sendKeybTestEx _ _ _ = throwHttpEx

handle1 :: Handle (StateT [MockAction a] IO) a
handle1 =
  Handle
    { hConf = config1,
      hLog = handLogWarn,
      sendTxtMsg = sendMsgTest "ok",
      sendKeyb = sendKeybTest "ok",
      sendAttachMsg = sendAttachMsgTest "ok",
      isValidResponse = isValidResponseTest
    }

handle0,
  handle2,
  handle3,
  handle4,
  handle5,
  handle6,
  handle7 ::
    Handle (StateT [MockAction a] IO) a
handle0 = handle1 {hLog = handLogMsgDebug}
handle2 = handle0 {sendTxtMsg = sendMsgTest "oops"}
handle3 = handle0 {sendKeyb = sendKeybTest "oops"}
handle4 = handle0 {sendAttachMsg = sendAttachMsgTest "oops"}
handle5 = handle0 {sendTxtMsg = sendMsgTestEx}
handle6 = handle0 {sendKeyb = sendKeybTestEx}
handle7 = handle0 {sendAttachMsg = sendAttachMsgTestEx}

