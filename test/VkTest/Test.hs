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
import Vk.Api.Response (ServerInfo(..),LoadPhotoResp(..),LoadDocResp(..))


initialDB1 :: MapUserN
initialDB1 = Map.fromList []

emptyServInf :: ServerInfo
emptyServInf = ServerInfo "" "" 0


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
    describe "runServ" $ do
      it "work with empty update list" $ do 
        actions <-
          execStateT
               (evalStateT (runServ handle10) (emptyServInf,initialDB1))
            []
        reverse actions `shouldBe`
          [ GOTUPDATES emptyServInf
            , LOGMSG INFO "No new updates"
          ]
      it "work with singleton update list with text msg" $ do 
        actions <-
          execStateT
               (evalStateT (runServ handle11) (emptyServInf,initialDB1))
            []
        reverse actions `shouldBe`
          [ GOTUPDATES emptyServInf
            ,SENDMSG 123 (TextMsg "love")
            ,SENDMSG 123 (TextMsg "love")
          ]
      it "work with singleton update list with sticker msg " $ do 
        actions <-
          execStateT
               (evalStateT (runServ handle12) (emptyServInf,initialDB1))
            []
        reverse actions `shouldBe`
          [ GOTUPDATES emptyServInf
            ,SENDMSG 1606 (StickerMsg 9014)
            ,SENDMSG 1606 (StickerMsg 9014)
          ]
      it "work with singleton update list with photo attachment msg " $ do 
        actions <-
          execStateT
               (evalStateT (runServ handle8) (emptyServInf,initialDB1))
            []
        reverse actions `shouldBe`
          [ GOTUPDATES emptyServInf
          , GOTPhotoSERVER 1606
          , GOTOURL "https:photo"
          , LOADPhotoTOSERV "http://toLoadPic" "https:photo" "anyPhoto"
          , SAVEPhotoONSERV $
            LoadPhotoResp 24 "anyHash" "anyPhotoSring"
          , SENDMSG 1606 (AttachmentMsg "" ["photo50_25"] ("",""))
          , SENDMSG 1606 (AttachmentMsg "" ["photo50_25"] ("",""))
          ]
      it "work with singleton update list with doc attachment msg " $ do 
        actions <-
          execStateT
               (evalStateT (runServ handle9) (emptyServInf,initialDB1))
            []
        reverse actions `shouldBe`
          [ GOTUPDATES emptyServInf
          , GOTDocSERVER 1606 "doc"
          , GOTOURL "https:doc"
          , LOADDocTOSERV "http://toLoadDoc" "https:doc" "anyDoc" "sql"
          , SAVEDocONSERV (LoadDocResp "anyFile") "car.sql"
          , SENDMSG 1606 (AttachmentMsg "" ["doc50_25"] ("",""))
          , SENDMSG 1606 (AttachmentMsg "" ["doc50_25"] ("",""))
          ]
      it "work with singleton update list with audio message attachment msg" $ do 
        actions <-
          execStateT
               (evalStateT (runServ handle13) (emptyServInf,initialDB1))
            []
        reverse actions `shouldBe`
          [ GOTUPDATES emptyServInf
          , GOTDocSERVER 1606 "audio_message"
          ,GOTOURL "https:audiomsg.ogg"
          ,LOADDocTOSERV "http://toLoadDoc" "https:audiomsg.ogg" "anyDoc" "ogg"
          ,SAVEDocONSERV (LoadDocResp  "anyFile") "audio_message"
          ,SENDMSG 1606 (AttachmentMsg "" ["doc50_25"] ("",""))
          ,SENDMSG 1606 (AttachmentMsg "" ["doc50_25"] ("",""))
          ]
      it "work with singleton update list with video attachment msg" $ do 
        actions <-
          execStateT
               (evalStateT (runServ handle14) (emptyServInf,initialDB1))
            []
        reverse actions `shouldBe`
          [ GOTUPDATES emptyServInf
            ,SENDMSG 1606 (AttachmentMsg "" ["video-4144_1714"] ("",""))
            ,SENDMSG 1606 (AttachmentMsg "" ["video-4144_1714"] ("",""))
          ]
    describe "(startApp >>= runServ)" $ do
      it "work with empty update list" $ do
        actions <-
          execStateT
               (startApp handle0 initialDB1 >>= evalStateT (runServ handle0))
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
              "Send request to getUpdates: https:anyServer?act=a_check&key=anyKey&ts=289&wait=20"
          , GOTUPDATES $
            ServerInfo
              "anyKey"
              "https:anyServer"
              289
          , LOGMSG DEBUG $ "Get response: " ++ show json2 
          , LOGMSG INFO "No new updates"
          ]
      it "work with singleton update list with text msg" $ do
        actions <-
          execStateT
               (startApp handle2 initialDB1 >>= evalStateT (runServ handle2))
            []
        reverse actions `shouldBe`
          [ LOGMSG
              DEBUG
              "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103"
          , GOTSERVER
          , LOGMSG
              DEBUG
              "Get response: \"{\\\"response\\\":{\\\"key\\\":\\\"anyKey\\\",\\\"server\\\":\\\"https:anyServer\\\",\\\"ts\\\":\\\"289\\\"}}\""
          , LOGMSG INFO "Work with received server"
          , LOGMSG
              DEBUG
              "Send request to getUpdates: https:anyServer?act=a_check&key=anyKey&ts=289&wait=20"
          , GOTUPDATES $
            ServerInfo
              "anyKey"
              "https:anyServer"
              289
          , LOGMSG
              DEBUG
              "Get response: \"{\\\"ts\\\":\\\"290\\\",\\\"updates\\\":[{\\\"type\\\":\\\"message_new\\\",\\\"object\\\":{\\\"date\\\":1594911394,\\\"from_id\\\":123,\\\"id\\\":597,\\\"out\\\":0,\\\"peer_id\\\":1606,\\\"text\\\":\\\"love\\\",\\\"conversation_message_id\\\":562,\\\"fwd_messages\\\":[],\\\"important\\\":false,\\\"random_id\\\":0,\\\"attachments\\\":[],\\\"is_hidden\\\":false},\\\"group_id\\\":19495,\\\"event_id\\\":\\\"123\\\"}]}\\r\\n\""
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
            (startApp handle5 initialDB1 >>= evalStateT (runServ handle5))
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
              "Send request to getUpdates: https:anyServer?act=a_check&key=anyKey&ts=289&wait=20"
          , GOTUPDATES $
            ServerInfo
              "anyKey"
              "https:anyServer"
              289
          , LOGMSG DEBUG $ "Get response: " ++ show json7 
          , LOGMSG INFO "There is new updates list"
          , LOGMSG INFO "Analysis update from the list"
          , LOGMSG
              INFO
              "Get AttachmentMsg: [StickerAttachment {sticker = StickerInfo {sticker_id = 9014}}] from user 1606"
          ] ++
          (concat . replicate 2 $
           [ LOGMSG
               DEBUG
               "Send request to send to user_id:1606 msg: StickerMsg 9014"
           , SENDMSG 1606 (StickerMsg 9014)
           , LOGMSG DEBUG "Get response: \"{\\\"response\\\":626}\""
           , LOGMSG INFO "Sticker_id 9014 was sent to user 1606"
           ])
      it "throw CheckGetUpdatesException with error answer" $
        evalStateT
             (startApp handle6 initialDB1 >>= evalStateT (runServ handle6))
          [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $
             "NEGATIVE RESPONSE:" ++ show json5))
      it "throw CheckGetUpdatesException with unknown answer" $
        evalStateT
             (startApp handle7 initialDB1 >>= evalStateT (runServ handle7))
          [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $
             "UNKNOWN RESPONSE:" ++ show json6))



