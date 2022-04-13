{-# LANGUAGE OverloadedStrings #-}

module Spec.Tg.App.Handlers where

import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT (..))
import Network.HTTP.Client (HttpException (InvalidUrlException))
import Spec.Tg.App.ResponseExample
import Spec.Tg.Conf (config1)
import Spec.Tg.Log
import Spec.Tg.Types
import Tg.App (Handle (..))
import Tg.Types

getUpdatesTest :: Response -> StateT [MockAction] IO Response
getUpdatesTest json = StateT $ \s -> return (json, GOTUPDATES : s)

confirmUpdatesTest :: Response -> Offset -> StateT [MockAction] IO Response
confirmUpdatesTest json offset =
  StateT $ \s -> return (json, CONFIRMUPDATES offset : s)

sendMsgTest ::
  Response -> UserId -> TextOfMsg -> StateT [MockAction] IO Response
sendMsgTest json usId msg = StateT $ \s -> return (json, SENDMSG usId msg : s)

copyMsgTest ::
  Response -> UserId -> MessageId -> StateT [MockAction] IO Response
copyMsgTest json usId msgId =
  StateT $ \s -> return (json, COPYMSG usId msgId : s)

sendKeybTest ::
  Response -> UserId -> N -> TextOfMsg -> StateT [MockAction] IO Response
sendKeybTest json usId currN msg =
  StateT $ \s -> return (json, SENDKEYB usId currN msg : s)

throwHttpEx :: StateT [MockAction] IO Response
throwHttpEx = throwM $ InvalidUrlException "" ""

getUpdatesTestEx :: StateT [MockAction] IO Response
getUpdatesTestEx = throwHttpEx

confirmUpdatesTestEx :: Offset -> StateT [MockAction] IO Response
confirmUpdatesTestEx _ = throwHttpEx

sendMsgTestEx ::
  UserId -> TextOfMsg -> StateT [MockAction] IO Response
sendMsgTestEx _ _ = throwHttpEx

copyMsgTestEx ::
  UserId -> MessageId -> StateT [MockAction] IO Response
copyMsgTestEx _ _ = throwHttpEx

sendKeybTestEx ::
  UserId -> N -> TextOfMsg -> StateT [MockAction] IO Response
sendKeybTestEx _ _ _ = throwHttpEx

handle1 :: Handle (StateT [MockAction] IO)
handle1 =
  Handle
    { hConf = config1,
      hLog = handLogDebug,
      getUpdates = getUpdatesTest json6,
      getShortUpdates = getUpdatesTest json1,
      confirmUpdates = confirmUpdatesTest json1,
      sendMsg = sendMsgTest json4,
      sendKeyb = sendKeybTest json4,
      copyMsg = copyMsgTest json11
    }

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
handle2 = handle1 {getShortUpdates = getUpdatesTest json2}
handle3 = handle1 {getShortUpdates = getUpdatesTest json3}
handle4 = handle1 {getShortUpdates = getUpdatesTest json4}
handle5 = handle1 {getShortUpdates = getUpdatesTest json5, hLog = handLogMsgDebug}
handle6 = handle1 {getUpdates = getUpdatesTest json2}
handle7 = handle1 {getUpdates = getUpdatesTest json3}
handle8 = handle1 {getUpdates = getUpdatesTest json4}
handle9 = handle1 {getUpdates = getUpdatesTest json7, hLog = handLogMsgDebug}

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
handle10 = handle1 {getUpdates = getUpdatesTest json8}
handle11 = handle1 {getUpdates = getUpdatesTest json9}
handle12 = handle1 {getUpdates = getUpdatesTest json10}
handle13 = handle1 {hLog = handLogWarn}
handle14 = handle13 {getUpdates = getUpdatesTest json12}
handle15 = handle13 {getShortUpdates = getUpdatesTest json12}
handle16 = handle13 {confirmUpdates = confirmUpdatesTest json2}
handle17 = handle13 {confirmUpdates = confirmUpdatesTest json5}
handle18 = handle13 {confirmUpdates = confirmUpdatesTest json3}
handle19 = handle13 {getUpdates = getUpdatesTest json13}

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
handle20 = handle13 {confirmUpdates = confirmUpdatesTest json12}
handle21 = handle13 {getShortUpdates = getUpdatesTest json14}
handle22 = handle13 {getUpdates = getUpdatesTest json5}
handle23 = handle13 {getUpdates = getUpdatesTest json15}
handle24 = handle13 {getUpdates = getUpdatesTest json16}
handle25 = handle13 {getUpdates = getUpdatesTest json17}
handle26 = handle13 {getUpdates = getUpdatesTest json18}
handle27 = handle13 {getUpdates = getUpdatesTest json19}
handle28 = handle13 {getUpdates = getUpdatesTest json20}
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
handle32 = handle13 {getUpdates = getUpdatesTest json9}
handle33 = handle32 {copyMsg = copyMsgTest json21}
handle34 = handle32 {copyMsg = copyMsgTest json22}
handle35 = handle32 {copyMsg = copyMsgTest json2}
handle36 = handle13 {getUpdates = getUpdatesTest json23}
handle37 = handle13 {getUpdates = getUpdatesTest json24}
handle38 = handle13 {getUpdates = getUpdatesTest json25}
handle39 = handle13 {getUpdates = getUpdatesTest json26}

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
handle40 = handle13 {getUpdates = getUpdatesTest json27}
handle41 = handle13 {getUpdates = getUpdatesTest json28}
handle42 = handle13 {getUpdates = getUpdatesTest json29}
handle43 = handle13 {getUpdates = getUpdatesTest json8}
handle44 = handle43 {sendKeyb = sendKeybTest json21}
handle45 = handle43 {sendKeyb = sendKeybTest json22}
handle46 = handle43 {sendKeyb = sendKeybTest json2}
handle47 = handle13 {confirmUpdates = confirmUpdatesTest json21}
handle48 = handle13 {confirmUpdates = confirmUpdatesTest json22}
handle49 = handle13 {confirmUpdates = confirmUpdatesTest json2}

handle50,
  handle51,
  handle52,
  handle53,
  handle54 ::
    Handle (StateT [MockAction] IO)
handle50 = handle13 {sendMsg = sendMsgTestEx}
handle51 = handle13 {getUpdates = getUpdatesTestEx}
handle52 = handle13 {confirmUpdates = confirmUpdatesTestEx}
handle53 = handle10 {sendKeyb = sendKeybTestEx}
handle54 = handle11 {copyMsg = copyMsgTestEx}
