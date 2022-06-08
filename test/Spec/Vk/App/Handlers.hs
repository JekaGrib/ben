module Spec.Vk.App.Handlers where

import qualified App
import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT (..))
import Network.HTTP.Client (HttpException (InvalidUrlException))
import qualified Spec.App.Handlers as App
import qualified Spec.Conf as C
import Spec.Types
import Spec.Vk.App.ResponseExample
import Spec.Vk.Conf
import Spec.Vk.Log
import qualified Spec.Vk.PrepareAttachment.Handlers as PrAtt
import Spec.Vk.Types
import Types
import Vk.Api.Response (ServerInfo (..))
import Vk.App (Handle (..), isValidResponse')
import Vk.Types

appHandle1 :: App.Handle (StateT [MockAction VkAttachMSG] IO) VkAttachMSG
appHandle1 =
  App.Handle
    { App.hConf = C.config1,
      App.hLog = handLogWarn,
      App.sendTxtMsg = App.sendMsgTest json4,
      App.sendKeyb = App.sendKeybTest json4,
      App.sendAttachMsg = App.sendAttachMsgTest json4,
      App.isValidResponse = isValidResponse'
    }

appHandle0, appHandle2 :: App.Handle (StateT [MockAction VkAttachMSG] IO) VkAttachMSG
appHandle0 = appHandle1 {App.hLog = handLogMsgDebug}
appHandle2 = appHandle1 {App.hLog = handLogMsgInfo}

handle1 :: Handle (StateT [MockAction VkAttachMSG] IO)
handle1 =
  Handle
    { hConf = config1,
      hLog = handLogWarn,
      getLongPollServer = getServerTest json1,
      getUpdates = getUpdatesTest json2,
      hApp = appHandle1,
      hPrepAttach = PrAtt.handle0
    }

getServerTest :: Response -> StateT [MockAction VkAttachMSG] IO Response
getServerTest json = StateT $ \s -> return (json, VkMock GOTSERVER : s)

getUpdatesTest :: Response -> ServerInfo -> StateT [MockAction VkAttachMSG] IO Response
getUpdatesTest json sI = StateT $ \s -> return (json, VkMock (GOTUPDATES sI) : s)

getServerTestEx :: StateT [MockAction VkAttachMSG] IO Response
getServerTestEx = App.throwHttpEx

getUpdatesTestEx :: ServerInfo -> StateT [MockAction VkAttachMSG] IO Response
getUpdatesTestEx _ = App.throwHttpEx

handle0,
  handle2,
  handle3,
  handle4,
  handle5,
  handle6,
  handle7,
  handle8,
  handle9 ::
    Handle (StateT [MockAction VkAttachMSG] IO)
handle0 = handle1 {hLog = handLogMsgDebug, hApp = appHandle0}
handle2 = handle1 {getUpdates = getUpdatesTest json3, hLog = handLogMsgDebug, hApp = appHandle0}
handle3 = handle1 {getLongPollServer = getServerTest json5}
handle4 = handle1 {getLongPollServer = getServerTest json6}
handle5 = handle1 {getUpdates = getUpdatesTest json7, hLog = handLogMsgDebug, hApp = appHandle0}
handle6 = handle1 {getUpdates = getUpdatesTest json5}
handle7 = handle1 {getUpdates = getUpdatesTest json6}
handle8 = handle1 {getUpdates = getUpdatesTest json8}
handle9 = handle1 {getUpdates = getUpdatesTest json9, hPrepAttach = PrAtt.handle19}

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
    Handle (StateT [MockAction VkAttachMSG] IO)
handle10 = handle1 {hLog = handLogMsgInfo, hApp = appHandle2}
handle11 = handle1 {getUpdates = getUpdatesTest json3}
handle12 = handle1 {getUpdates = getUpdatesTest json7}
handle13 = handle1 {getUpdates = getUpdatesTest json10, hPrepAttach = PrAtt.handle20}
handle14 = handle1 {getUpdates = getUpdatesTest json11}
handle15 = handle1 {getUpdates = getUpdatesTest json12}
handle16 = handle1 {getUpdates = getUpdatesTest json13}
handle17 = handle1 {getUpdates = getUpdatesTest json14}
handle18 = handle1 {getUpdates = getUpdatesTest json15}
handle19 = handle1 {getUpdates = getUpdatesTest json16}

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
    Handle (StateT [MockAction VkAttachMSG] IO)
handle20 = handle1 {getUpdates = getUpdatesTest json17}
handle21 = handle1 {getUpdates = getUpdatesTest json18}
handle22 = handle1 {getUpdates = getUpdatesTest json19}
handle23 = handle1 {getUpdates = getUpdatesTest json20}
handle24 = handle1 {getUpdates = getUpdatesTest json21}
handle25 = handle1 {getUpdates = getUpdatesTest json22}
handle26 = handle1 {getUpdates = getUpdatesTest json23}
handle27 = handle1 {getUpdates = getUpdatesTest json24}
handle28 = handle1 {getUpdates = getUpdatesTest json25}
handle29 = handle1 {getUpdates = getUpdatesTest json26}

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
    Handle (StateT [MockAction VkAttachMSG] IO)
handle30 = handle1 {getUpdates = getUpdatesTest json27}
handle31 = handle1 {getUpdates = getUpdatesTest json28}
handle32 = handle1 {getUpdates = getUpdatesTest json29}
handle33 = handle1 {getUpdates = getUpdatesTest json30}
handle34 = handle1 {getUpdates = getUpdatesTest json31}
handle35 = handle1 {getUpdates = getUpdatesTest json32}
handle36 = handle1 {getUpdates = getUpdatesTest json33}
handle37 = handle1 {getUpdates = getUpdatesTest json34}
handle38 = handle1 {getUpdates = getUpdatesTest json35}
handle39 = handle1 {getUpdates = getUpdatesTest json36}

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
    Handle (StateT [MockAction VkAttachMSG] IO)
handle40 = handle1 {getUpdates = getUpdatesTest json37}
handle41 = handle1 {getUpdates = getUpdatesTest json38}
handle42 = handle1 {getUpdates = getUpdatesTest json39}
handle43 = handle1 {getUpdates = getUpdatesTest json40}
handle44 = handle1 {getUpdates = getUpdatesTest json41}
handle45 = handle1 {getLongPollServer = getServerTestEx}
handle46 = handle1 {getUpdates = getUpdatesTestEx}
handle47 = handle1 {getUpdates = getUpdatesTest json3, hApp = appHandle0 {App.sendTxtMsg = App.sendMsgTestEx}}
handle48 = handle1 {getUpdates = getUpdatesTest json40, hApp = appHandle0 {App.sendKeyb = App.sendKeybTestEx}}
handle49 = handle1 {getLongPollServer = getServerTestEx, getUpdates = getUpdatesTest json35}
