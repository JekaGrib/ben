{-# LANGUAGE OverloadedStrings #-}

module Spec.App.Handlers where

import Control.Monad.Catch (throwM,MonadThrow)
import Control.Monad.State (StateT (..),modify)
import Network.HTTP.Client (HttpException (InvalidUrlException))
import Spec.Conf (config1)
import Spec.Log
import Spec.Types
import App (Handle (..))
import Types


sendMsgTest ::
  Response -> UserId -> TextOfMsg -> StateT [MockAction a] IO Response
sendMsgTest json usId msg = do
  modify ((SENDMSG usId msg) :)
  return json

sendAttachMsgTest ::
  Response -> a -> UserId -> StateT [MockAction a] IO Response
sendAttachMsgTest json att usId = do
  modify ((SENDAttachMSG usId att) :)
  return json

sendKeybTest ::
  Response -> UserId -> N -> TextOfMsg -> StateT [MockAction a] IO Response
sendKeybTest json usId currN msg = do
  modify ((SENDKEYB usId currN msg) :)
  return json

isValidResponseTest ::
  Response -> Result
isValidResponseTest "ok" = Success
isValidResponseTest _ = NotSuccess "oops"

throwHttpEx :: (MonadThrow m) => m Response
throwHttpEx = throwM $ InvalidUrlException "" ""

sendMsgTestEx :: (MonadThrow m) =>
  UserId -> TextOfMsg -> m Response
sendMsgTestEx _ _ = throwHttpEx

sendAttachMsgTestEx :: (MonadThrow m) =>
   a -> UserId -> m Response
sendAttachMsgTestEx _ _ = throwHttpEx

sendKeybTestEx :: (MonadThrow m) =>
  UserId -> N -> TextOfMsg -> m Response
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
  handle7 :: Handle (StateT [MockAction a] IO) a
handle0 = handle1 {hLog = handLogMsgDebug}
handle2 = handle0 {sendTxtMsg    = sendMsgTest       "oops"}
handle3 = handle0 {sendKeyb      = sendKeybTest      "oops"}
handle4 = handle0 {sendAttachMsg = sendAttachMsgTest "oops"}
handle5 = handle0 {sendTxtMsg    = sendMsgTestEx}
handle6 = handle0 {sendKeyb      = sendKeybTestEx}
handle7 = handle0 {sendAttachMsg = sendAttachMsgTestEx}

{-

handle0,
  handle2,
  handle3,
  handle4,
  handle5,
  handle6,
  handle7,
  handle8,
  handle9 ::
    Handle (StateT [MockAction] IO)
handle0 = handle1 {hLog = handLogMsgDebug}

handle10,
  handle11,
  handle12,
  handle13,
  handle14,
  handle15,
  handle16,
  handle17,
  handle18,
  handle19 ::
    Handle (StateT [MockAction] IO)
handle13 = handle1 {hLog = handLogWarn}

handle20,
  handle21,
  handle22,
  handle23,
  handle24,
  handle25,
  handle26,
  handle27,
  handle28,
  handle29 ::
    Handle (StateT [MockAction] IO)
handle29 = handle13 {sendMsg = sendMsgTest json21}

handle30,
  handle31,
  handle32,
  handle33,
  handle34,
  handle35,
  handle36,
  handle37,
  handle38,
  handle39 ::
    Handle (StateT [MockAction] IO)
handle30 = handle13 {sendMsg = sendMsgTest json22}
handle31 = handle13 {sendMsg = sendMsgTest json2}
handle33 = handle32 {copyMsg = copyMsgTest json21}
handle34 = handle32 {copyMsg = copyMsgTest json22}
handle35 = handle32 {copyMsg = copyMsgTest json2}

handle40,
  handle41,
  handle42,
  handle43,
  handle44,
  handle45,
  handle46,
  handle47,
  handle48,
  handle49 ::
    Handle (StateT [MockAction] IO)
handle44 = handle43 {sendKeyb = sendKeybTest json21}
handle45 = handle43 {sendKeyb = sendKeybTest json22}
handle46 = handle43 {sendKeyb = sendKeybTest json2}

handle50,
  handle51,
  handle52,
  handle53,
  handle54 ::
    Handle (StateT [MockAction] IO)
handle50 = handle13 {sendMsg = sendMsgTestEx}
handle53 = handle10 {sendKeyb = sendKeybTestEx}
handle54 = handle11 {copyMsg = copyMsgTestEx}
-}