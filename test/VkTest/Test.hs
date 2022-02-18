{-{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}-}
{-# LANGUAGE OverloadedStrings #-} 

module VkTest.Test where

import VkTest.PrepareAttachment (testVkPrAtt)
import Vk.App (run,getServInfoAndCheckResp,runServ,startApp)
import Vk.Types
import qualified Data.Map as Map
import Vk.Oops (VKBotException(..))
import VkTest.ResponseExample
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow)
import VkTest.Handlers
import Control.Monad.State (evalStateT,execStateT)
import VkTest.Types
import Vk.Logger ( Priority(..))
import Vk.Api.Response (ServerInfo(..))


initialDB1 :: MapUserN
initialDB1 = Map.fromList []

--emptyServInf :: ServerInfo
--emptyServInf = ServerInfo "" "" 0


testVk :: IO ()
testVk = do
  testVkPrAtt
  hspec $ do
    describe "getServInfoAndCheckResp" $ do
      it "throw Exception with error answer" $
        evalStateT
          (getServInfoAndCheckResp handle3)
          [] `shouldThrow`
        (== (CheckGetServerResponseException $
             "NEGATIVE RESPONSE:" ++ show json5))
      it "throw CheckGetServerResponseException with unknown answer" $
        evalStateT
          (getServInfoAndCheckResp handle4) 
          [] `shouldThrow`
        (== (CheckGetServerResponseException $
             "UNKNOWN RESPONSE:" ++ show json6))
    describe "(runServ after startApp)" $ do
      it "work with empty update list" $ do
        actions <-
          execStateT
               (startApp handle0 >>= \servInfo -> evalStateT (runServ handle0) (servInfo,initialDB1))
            []
        reverse actions `shouldBe`
          [ LOGMSG
              DEBUG
              "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103"
          , GOTSERVER
          , LOGMSG DEBUG $ "Get response: " ++ show json1 
          , LOGMSG INFO "Work with received server"
          , LOGMSG
              DEBUG
              "Send request to getUpdates: https://lp.vk.com/wh000?act=a_check&key=912481cc91cb3b0e119b9be5c75b383d6887438f&ts=289&wait=20"
          , GOTUPDATES $
            ServerInfo
              "912481cc91cb3b0e119b9be5c75b383d6887438f"
              "https://lp.vk.com/wh000"
              289
          , LOGMSG DEBUG $ "Get response: " ++ show json2 
          , LOGMSG INFO "No new updates"
          ]
      it "work with singleton update list with text msg" $ do
        actions <-
          execStateT
               (startApp handle2 >>= \servInfo -> evalStateT (runServ handle2) (servInfo,initialDB1))
            []
        reverse actions `shouldBe`
          [ LOGMSG
              DEBUG
              "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103"
          , GOTSERVER
          , LOGMSG
              DEBUG
              "Get response: \"{\\\"response\\\":{\\\"key\\\":\\\"912481cc91cb3b0e119b9be5c75b383d6887438f\\\",\\\"server\\\":\\\"https:\\\\/\\\\/lp.vk.com\\\\/wh000\\\",\\\"ts\\\":\\\"289\\\"}}\""
          , LOGMSG INFO "Work with received server"
          , LOGMSG
              DEBUG
              "Send request to getUpdates: https://lp.vk.com/wh000?act=a_check&key=912481cc91cb3b0e119b9be5c75b383d6887438f&ts=289&wait=20"
          , GOTUPDATES $
            ServerInfo
              "912481cc91cb3b0e119b9be5c75b383d6887438f"
              "https://lp.vk.com/wh000"
              289
          , LOGMSG
              DEBUG
              "Get response: \"{\\\"ts\\\":\\\"290\\\",\\\"updates\\\":[{\\\"type\\\":\\\"message_new\\\",\\\"object\\\":{\\\"date\\\":1594911394,\\\"from_id\\\":123,\\\"id\\\":597,\\\"out\\\":0,\\\"peer_id\\\":16063921,\\\"text\\\":\\\"love\\\",\\\"conversation_message_id\\\":562,\\\"fwd_messages\\\":[],\\\"important\\\":false,\\\"random_id\\\":0,\\\"attachments\\\":[],\\\"is_hidden\\\":false},\\\"group_id\\\":194952914,\\\"event_id\\\":\\\"35ec397e45dfe993d365912ea32be41be5e77a0c\\\"}]}\\r\\n\""
          , LOGMSG INFO "There is new updates list"
          , LOGMSG INFO "Analysis update from the list"
          , LOGMSG INFO "Get TextMsg: \"love\" from user 123"
          ] ++
          (concat . replicate 2 $
           [ LOGMSG
               DEBUG
               "Send request to send to user_id:123 msg: TextMsg \"love\""
           , SENDMSG 123 (TextMsg "love")
           , LOGMSG DEBUG "Get response: \"{\\\"response\\\":626}\""
           , LOGMSG INFO "Msg \"love\" was sent to user 123"
           ])
      it "work with singleton update list with sticker msg " $ do
        actions <-
          execStateT
            (startApp handle5 >>= \servInfo -> evalStateT (runServ handle5) (servInfo,initialDB1))
            []
        reverse actions `shouldBe`
          [ LOGMSG
              DEBUG
              "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103"
          , GOTSERVER
          , LOGMSG DEBUG $ "Get response: " ++ show json1 
          , LOGMSG INFO "Work with received server"
          , LOGMSG
              DEBUG
              "Send request to getUpdates: https://lp.vk.com/wh000?act=a_check&key=912481cc91cb3b0e119b9be5c75b383d6887438f&ts=289&wait=20"
          , GOTUPDATES $
            ServerInfo
              "912481cc91cb3b0e119b9be5c75b383d6887438f"
              "https://lp.vk.com/wh000"
              289
          , LOGMSG DEBUG $ "Get response: " ++ show json7 
          , LOGMSG INFO "There is new updates list"
          , LOGMSG INFO "Analysis update from the list"
          , LOGMSG
              INFO
              "Get AttachmentMsg: [StickerAttachment {sticker = StickerInfo {sticker_id = 9014}}] from user 16063921"
          ] ++
          (concat . replicate 2 $
           [ LOGMSG
               DEBUG
               "Send request to send to user_id:16063921 msg: StickerMsg 9014"
           , SENDMSG 16063921 (StickerMsg 9014)
           , LOGMSG DEBUG "Get response: \"{\\\"response\\\":626}\""
           , LOGMSG INFO "Sticker_id 9014 was sent to user 16063921"
           ])
      it "throw CheckGetUpdatesException with error answer" $
        evalStateT
             (startApp handle6 >>= \servInfo -> evalStateT (runServ handle6) (servInfo,initialDB1))
          [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $
             "NEGATIVE RESPONSE:" ++ show json5))
      it "throw CheckGetUpdatesException with unknown answer" $
        evalStateT
             (startApp handle7 >>= \servInfo -> evalStateT (runServ handle7) (servInfo,initialDB1))
          [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $
             "UNKNOWN RESPONSE:" ++ show json6))



{-
import Control.Monad.State (StateT(..), evalStateT, execStateT)
import qualified Data.Map as Map
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow)
import Vk.Api.Response (ServerInfo(..))
import Vk.App (Handle(..), getServer, runServ)
import Vk.Conf (Config(..))
import Vk.Logger (LogConfig(..), LogHandle(..), Priority(..))
import Vk.Oops (VKBotException(..))

data MockAction
  = GOTSERVER
  | GOTUPDATES ServerInfo
  | SENDMSG UserId MSG
  | SENDKEYB UserId N TextOfKeyb
  | LOG Priority
  | LOGMSG Priority String
  deriving (Eq, Show)

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

logTest :: Priority -> String -> StateT [MockAction] IO ()
logTest prio _ = StateT $ \s -> return ((), LOG prio : s)

logTest0 :: Priority -> String -> StateT [MockAction] IO ()
logTest0 prio str = StateT $ \s -> return ((), LOGMSG prio str : s)


handleLog1, handleLog0 :: LogHandle (StateT [MockAction] IO)
handleLog1 = LogHandle (LogConfig DEBUG) logTest

handleLog0 = LogHandle (LogConfig DEBUG) logTest0

handle1 :: Handle (StateT [MockAction] IO)
handle1 =
  Handle
    { hConf = config1
    , hLog = handleLog1
    , getLongPollServer = getServerTest json1
    , getUpdates = getUpdatesTest json2
    , sendMsg = sendMsgTest json4
    , sendKeyb = sendKeybTest json4
    , getPhotoServer = \_ -> return json1
    , loadPhotoToServ = \_ _ _ -> return json1
    , savePhotoOnServ = \_ -> return json1
    , getDocServer = \_ _ -> return json1
    , loadDocToServ = \_ _ _ _ -> return json1
    , saveDocOnServ = \_ _ -> return json1
    , goToUrl = \_ -> return jsonA
    }

handle0, handle2, handle3, handle4, handle5, handle6, handle7 ::
     Handle (StateT [MockAction] IO)
handle0 = handle1 {hLog = handleLog0}

handle2 = handle1 {getUpdates = getUpdatesTest json3, hLog = handleLog0}

handle3 = handle1 {getLongPollServer = getServerTest json5}

handle4 = handle1 {getLongPollServer = getServerTest json6}

handle5 = handle1 {getUpdates = getUpdatesTest json7, hLog = handleLog0}

handle6 = handle1 {getUpdates = getUpdatesTest json5}

handle7 = handle1 {getUpdates = getUpdatesTest json6}

initialDB1 :: MapUserN
initialDB1 = Map.fromList []

testVk :: IO ()
testVk =
  

json1, json2, json3, json4, json5, json6, json7 :: Response
json1 =
  "{\"response\":{\"key\":\"912481cc91cb3b0e119b9be5c75b383d6887438f\",\"server\":\"https:\\/\\/lp.vk.com\\/wh000\",\"ts\":\"289\"}}"

json2 = "{\"ts\":\"289\",\"updates\":[]}\r\n"

json3 =
  "{\"ts\":\"290\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1594911394,\"from_id\":123,\"id\":597,\"out\":0,\"peer_id\":16063921,\"text\":\"love\",\"conversation_message_id\":562,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"group_id\":194952914,\"event_id\":\"35ec397e45dfe993d365912ea32be41be5e77a0c\"}]}\r\n"

json4 = "{\"response\":626}"

json5 =
  "{\"error\":{\"error_code\":5,\"error_msg\":\"User authorization failed: invalid access_token (4).\",\"request_params\":[{\"key\":\"user_id\",\"value\":\"16063921\"},{\"key\":\"random_id\",\"value\":\"0\"},{\"key\":\"v\",\"value\":\"5.103\"},{\"key\":\"method\",\"value\":\"messages.send\"},{\"key\":\"oauth\",\"value\":\"1\"}]}}"

json6 = "lalala"

json7 =
  "{\"ts\":\"304\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1594932378,\"from_id\":16063921,\"id\":629,\"out\":0,\"peer_id\":16063921,\"text\":\"\",\"conversation_message_id\":594,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[{\"type\":\"sticker\",\"sticker\":{\"product_id\":279,\"sticker_id\":9014,\"images\":[{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-64\",\"width\":64,\"height\":64},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-128\",\"width\":128,\"height\":128},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-256\",\"width\":256,\"height\":256},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-352\",\"width\":352,\"height\":352},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-512\",\"width\":512,\"height\":512}],\"images_with_background\":[{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-64b\",\"width\":64,\"height\":64},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-128b\",\"width\":128,\"height\":128},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-256b\",\"width\":256,\"height\":256},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-352b\",\"width\":352,\"height\":352},{\"url\":\"https:\\/\\/vk.com\\/sticker\\/1-9014-512b\",\"width\":512,\"height\":512}]}}],\"is_hidden\":false},\"group_id\":194952914,\"event_id\":\"a3d68972637b90446ac3be5a171d923fa0f10f31\"}]}\r\n"

jsonA :: ResponseS
jsonA =
  "{\"response\":{\"key\":\"912481cc91cb3b0e119b9be5c75b383d6887438f\",\"server\":\"https:\\/\\/lp.vk.com\\/wh000\",\"ts\":\"289\"}}"
  -}