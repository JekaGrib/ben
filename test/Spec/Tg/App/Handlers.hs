{-# LANGUAGE OverloadedStrings #-}

module Spec.Tg.App.Handlers where

import Control.Monad.Catch (throwM)
import Control.Monad.State (StateT (..),modify)
import Network.HTTP.Client (HttpException (InvalidUrlException))
import Spec.Tg.App.ResponseExample
import Spec.Conf (config1)
import Spec.Log
import Spec.Tg.Types
import Tg.App (Handle (..),isValidResponse')
import Tg.Types
import Spec.Types
import Types
import qualified Spec.App.Handlers as App
import qualified App 

getUpdatesTest :: Response -> StateT [MockAction MessageId] IO Response
getUpdatesTest json = StateT $ \s -> return (json, TgMock GOTUPDATES : s)

confirmUpdatesTest :: Response -> UpdateId -> StateT [MockAction MessageId] IO Response
confirmUpdatesTest json offset =
  StateT $ \s -> return (json, TgMock (CONFIRMUPDATES offset) : s)

getUpdatesTestEx :: StateT [MockAction MessageId] IO Response
getUpdatesTestEx = App.throwHttpEx

confirmUpdatesTestEx :: UpdateId -> StateT [MockAction MessageId] IO Response
confirmUpdatesTestEx _ = App.throwHttpEx


appHandle1 :: App.Handle (StateT [MockAction MessageId] IO) MessageId
appHandle1 = App.Handle
  { App.hConf = config1,
    App.hLog = handLogDebug,
    App.sendTxtMsg = App.sendMsgTest json4,
    App.sendKeyb = App.sendKeybTest json4,
    App.sendAttachMsg = App.sendAttachMsgTest json11,
    App.isValidResponse = isValidResponse'
  }

appHandle0,appHandle2 :: App.Handle (StateT [MockAction MessageId] IO) MessageId
appHandle0 = appHandle1 {App.hLog = handLogMsgDebug}
appHandle2 = appHandle1 {App.hLog = handLogWarn}

handle1 :: Handle (StateT [MockAction MessageId] IO) 
handle1 =
  Handle
    { hConf = config1,
      hLog = handLogDebug,
      getUpdates = getUpdatesTest json6,
      getShortUpdates = getUpdatesTest json1,
      confirmUpdates = confirmUpdatesTest json1,
      hApp = appHandle1
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
    Handle (StateT [MockAction MessageId] IO)
handle0 = handle1 {hLog = handLogMsgDebug,hApp = appHandle0}
handle2 = handle1 {getShortUpdates = getUpdatesTest json2}
handle3 = handle1 {getShortUpdates = getUpdatesTest json3}
handle4 = handle1 {getShortUpdates = getUpdatesTest json4}
handle5 = handle1 {getShortUpdates = getUpdatesTest json5, hLog = handLogMsgDebug,hApp = appHandle0}
handle6 = handle1 {getUpdates = getUpdatesTest json2}
handle7 = handle1 {getUpdates = getUpdatesTest json3}
handle8 = handle1 {getUpdates = getUpdatesTest json4}
handle9 = handle1 {getUpdates = getUpdatesTest json7, hLog = handLogMsgDebug,hApp = appHandle0}

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
    Handle (StateT [MockAction MessageId] IO)
handle10 = handle1 {getUpdates = getUpdatesTest json8}
handle11 = handle1 {getUpdates = getUpdatesTest json9}
handle12 = handle1 {getUpdates = getUpdatesTest json10}
handle13 = handle1 {hLog = handLogWarn,hApp = appHandle2}
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
    Handle (StateT [MockAction MessageId] IO)
handle20 = handle13 {confirmUpdates = confirmUpdatesTest json12}
handle21 = handle13 {getShortUpdates = getUpdatesTest json14}
handle22 = handle13 {getUpdates = getUpdatesTest json5}
handle23 = handle13 {getUpdates = getUpdatesTest json15}
handle24 = handle13 {getUpdates = getUpdatesTest json16}
handle25 = handle13 {getUpdates = getUpdatesTest json17}
handle26 = handle13 {getUpdates = getUpdatesTest json18}
handle27 = handle13 {getUpdates = getUpdatesTest json19}
handle28 = handle13 {getUpdates = getUpdatesTest json20}
handle29 = handle13 {hApp = appHandle2 {App.sendTxtMsg = App.sendMsgTest json21}}

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
    Handle (StateT [MockAction MessageId] IO)
handle30 = handle13 {hApp = appHandle2 {App.sendTxtMsg = App.sendMsgTest json22}}
handle31 = handle13 {hApp = appHandle2 {App.sendTxtMsg = App.sendMsgTest json2}}
handle32 = handle13 {getUpdates = getUpdatesTest json9}
handle33 = handle32 {hApp = appHandle2 {App.sendAttachMsg = App.sendAttachMsgTest json21}}
handle34 = handle32 {hApp = appHandle2 {App.sendAttachMsg = App.sendAttachMsgTest json22}}
handle35 = handle32 {hApp = appHandle2 {App.sendAttachMsg = App.sendAttachMsgTest json2}}
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
    Handle (StateT [MockAction MessageId] IO)
handle40 = handle13 {getUpdates = getUpdatesTest json27}
handle41 = handle13 {getUpdates = getUpdatesTest json28}
handle42 = handle13 {getUpdates = getUpdatesTest json29}
handle43 = handle13 {getUpdates = getUpdatesTest json8}
handle44 = handle43 {hApp = appHandle2 {App.sendKeyb = App.sendKeybTest json21}}
handle45 = handle43 {hApp = appHandle2 {App.sendKeyb = App.sendKeybTest json22}}
handle46 = handle43 {hApp = appHandle2 {App.sendKeyb = App.sendKeybTest json2}}
handle47 = handle13 {confirmUpdates = confirmUpdatesTest json21}
handle48 = handle13 {confirmUpdates = confirmUpdatesTest json22}
handle49 = handle13 {confirmUpdates = confirmUpdatesTest json2}

handle50,
  handle51,
  handle52,
  handle53,
  handle54 ::
    Handle (StateT [MockAction MessageId] IO)
handle50 = handle13 {hApp = appHandle2 {App.sendTxtMsg = App.sendMsgTestEx}}
handle51 = handle13 {getUpdates = getUpdatesTestEx}
handle52 = handle13 {confirmUpdates = confirmUpdatesTestEx}
handle53 = handle10 {hApp = appHandle2 {App.sendKeyb = App.sendKeybTestEx}}
handle54 = handle11 {hApp = appHandle2 {App.sendAttachMsg = App.sendAttachMsgTestEx}}
