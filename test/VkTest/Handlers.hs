{-{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-} -}

module VkTest.Handlers where
  
import Vk.App (Handle(..))
import Control.Monad.State (StateT(..))
import VkTest.Log
import VkTest.Conf
import VkTest.Types 
import Vk.Types
import Vk.Api.Response (ServerInfo(..))
import VkTest.ResponseExample
import qualified VkTest.PrepareAttachment.Handlers as PrAtt

 
handle1 :: Handle (StateT [MockAction] IO)
handle1 =
  Handle
    { hConf = config1
    , hLog = handLogWarn
    , getLongPollServer = getServerTest json1
    , getUpdates = getUpdatesTest json2
    , sendMsg = sendMsgTest json4
    , sendKeyb = sendKeybTest json4
    , hPrepAttach = PrAtt.handle0
    }

getServerTest :: Response -> StateT [MockAction] IO Response
getServerTest json = StateT $ \s -> return (json, GOTSERVER : s)

getUpdatesTest :: Response -> ServerInfo -> StateT [MockAction] IO Response
getUpdatesTest json sI = StateT $ \s -> return (json, GOTUPDATES sI : s)

sendMsgTest :: Response -> UserId -> MSG -> StateT [MockAction] IO Response
sendMsgTest json usId msg = StateT $ \s -> return (json, SENDMSG usId msg : s)

sendKeybTest ::
     Response -> UserId -> N -> TextOfMsg -> StateT [MockAction] IO Response
sendKeybTest json usId currN msg =
  StateT $ \s -> return (json, SENDKEYB usId currN msg : s)


handle0, handle2, handle3, handle4, handle5, handle6, handle7, handle8,handle9 ::
     Handle (StateT [MockAction] IO)
handle0 = handle1 {hLog = handLogMsgDebug}

handle2 = handle1 {getUpdates = getUpdatesTest json3, hLog = handLogMsgDebug}

handle3 = handle1 {getLongPollServer = getServerTest json5}

handle4 = handle1 {getLongPollServer = getServerTest json6}

handle5 = handle1 {getUpdates = getUpdatesTest json7, hLog = handLogMsgDebug}

handle6 = handle1 {getUpdates = getUpdatesTest json5}

handle7 = handle1 {getUpdates = getUpdatesTest json6}

handle8 = handle1 {getUpdates = getUpdatesTest json8}

handle9 = handle1 {getUpdates = getUpdatesTest json9,hPrepAttach = PrAtt.handle19}

--handle10, handle11, handle12, handle13, handle14, handle15, handle16, handle17,handle9 ::
handle10 = handle1 {hLog = handLogMsgInfo}

handle11 = handle1 {getUpdates = getUpdatesTest json3}

handle12 = handle1 {getUpdates = getUpdatesTest json7}

handle13 = handle1 {getUpdates = getUpdatesTest json10,hPrepAttach = PrAtt.handle20}

handle14 = handle1 {getUpdates = getUpdatesTest json11}

handle15 = handle1 {getUpdates = getUpdatesTest json12}

handle16 = handle1 {getUpdates = getUpdatesTest json13}

handle17 = handle1 {getUpdates = getUpdatesTest json14}

handle18 = handle1 {getUpdates = getUpdatesTest json15}

handle19 = handle1 {getUpdates = getUpdatesTest json16}

handle20 = handle1 {getUpdates = getUpdatesTest json17}

handle21 = handle1 {getUpdates = getUpdatesTest json18}

handle22 = handle1 {getUpdates = getUpdatesTest json19}

handle23 = handle1 {getUpdates = getUpdatesTest json20}





