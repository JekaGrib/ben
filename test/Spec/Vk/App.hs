{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Vk.App where

import Control.Monad.State (evalStateT, execStateT, runStateT)
import qualified Data.Map as Map
import Spec.Vk.App.Handlers
import Spec.Vk.App.Oops
import Spec.Vk.App.ResponseExample
import Spec.Vk.Types
import Test.Hspec (describe, hspec, it, shouldBe, shouldNotBe, shouldThrow)
import Vk.Api.Response (LoadDocResp (..), LoadPhotoResp (..), ServerInfo (..))
import Vk.App (getServInfoAndCheckResp, run, runServ, startApp)
import Vk.AppT (TryServer (..), firstTry, nextTry, secondTry, thirdTry)
import Vk.Logger (Priority (..))
import Vk.Oops (VKBotException (..))
import Vk.Types

initialDB1, initialDB2, initialDB3 :: MapUserN
initialDB1 = Map.fromList []
initialDB2 = Map.fromList [(1118, Left (OpenRepeat 2)), (1234, Right 3), (123, Left (OpenRepeat 4))]
initialDB3 = Map.fromList [(1118, Left (OpenRepeat 2)), (123, Right 3), (1606, Right 2)]

emptyServInf :: ServerInfo
emptyServInf = ServerInfo "" "" 0

emptyTryServInf :: TryServer
emptyTryServInf = firstTry emptyServInf

testVkApp :: IO ()
testVkApp = do
  hspec $ do
    describe "getServInfoAndCheckResp" $ do
      it "throw Exception with error answer" $
        evalStateT
          (getServInfoAndCheckResp handle3)
          []
          `shouldThrow` ( ==
                            ( CheckGetServerResponseException $
                                "NEGATIVE RESPONSE:" ++ show json5
                            )
                        )
      it "throw CheckGetServerResponseException with unknown answer" $
        evalStateT
          (getServInfoAndCheckResp handle4)
          []
          `shouldThrow` ( ==
                            ( CheckGetServerResponseException $
                                "UNKNOWN RESPONSE:" ++ show json6
                            )
                        )
      it "throw GetLongPollServerException getServer HttpException" $
        evalStateT
          (getServInfoAndCheckResp handle45)
          []
          `shouldThrow` isGetLongPollServerException
    describe "runServ" $ do
      it "work with empty update list" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle10) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOGMSG INFO "No new updates"
                     ]
      it "work with singleton update list with \"love\" text msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle11) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 123 (TextMsg "love"),
                       SENDMSG 123 (TextMsg "love")
                     ]
      it "work with /repeat text msg(send Keyb,put user in OpenRepeat mode)" $ do
        (st, actions) <-
          runStateT
            (execStateT (runServ handle43) (emptyTryServInf, initialDB3))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDKEYB 123 3 " : Current number of repeats your message.\nWhy?"
                     ]
        snd st `shouldBe` Map.insert 123 (Left (OpenRepeat 3)) initialDB3
      it "work with /help text msg(send infoMsg)" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle44) (emptyTryServInf, initialDB3))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 123 (TextMsg "Hello")
                     ]
      it "work with text msg \"love\" if user is in OpenRepeat mode (send warning info msg)" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle11) (emptyTryServInf, initialDB2))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING,
                       SENDMSG 123 (TextMsg "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still 4\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later")
                     ]
      it "work with text msg \"3\" if user is in (OpenRepeat 4) mode (change N to 3, send info msg)" $ do
        (st, actions) <-
          runStateT
            (execStateT (runServ handle41) (emptyTryServInf, initialDB2))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 123 (TextMsg "Number of repeats successfully changed from 4 to 3")
                     ]
        snd st `shouldBe` Map.insert 123 (Right 3) initialDB2
      it "work with text msg \"love\" after text msg \"3\" if user is in (OpenRepeat 4) mode (change N to 3, send info msg,send \"love\" 3 times)" $ do
        (st, actions) <-
          runStateT
            (execStateT (runServ handle42) (emptyTryServInf, initialDB2))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 123 (TextMsg "Number of repeats successfully changed from 4 to 3"),
                       SENDMSG 123 (TextMsg "love"),
                       SENDMSG 123 (TextMsg "love"),
                       SENDMSG 123 (TextMsg "love")
                     ]
        snd st `shouldBe` Map.insert 123 (Right 3) initialDB2
      it "work with singleton update list with sticker msg " $ do
        actions <-
          execStateT
            (evalStateT (runServ handle12) (emptyTryServInf, initialDB3))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (StickerMsg 9014),
                       SENDMSG 1606 (StickerMsg 9014)
                     ]
      it "work with singleton update list with photo attachment msg " $ do
        actions <-
          execStateT
            (evalStateT (runServ handle8) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       GOTPhotoSERVER 1606,
                       GOTOURL "https:photo",
                       LOADPhotoTOSERV "http://toLoadPic" "https:photo" "anyPhoto",
                       SAVEPhotoONSERV $
                         LoadPhotoResp 24 "anyHash" "anyPhotoSring",
                       SENDMSG 1606 (AttachmentMsg "" ["photo50_25"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["photo50_25"] ("", ""))
                     ]
      it "work with singleton update list with doc attachment msg " $ do
        actions <-
          execStateT
            (evalStateT (runServ handle9) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       GOTDocSERVER 1606 "doc",
                       GOTOURL "https:doc",
                       LOADDocTOSERV "http://toLoadDoc" "https:doc" "anyDoc" "sql",
                       SAVEDocONSERV (LoadDocResp "anyFile") "car.sql",
                       SENDMSG 1606 (AttachmentMsg "" ["doc50_25"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["doc50_25"] ("", ""))
                     ]
      it "work with singleton update list with audio message attachment msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle13) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       GOTDocSERVER 1606 "audio_message",
                       GOTOURL "https:audiomsg.ogg",
                       LOADDocTOSERV "http://toLoadDoc" "https:audiomsg.ogg" "anyDoc" "ogg",
                       SAVEDocONSERV (LoadDocResp "anyFile") "audio_message",
                       SENDMSG 1606 (AttachmentMsg "" ["doc50_25"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["doc50_25"] ("", ""))
                     ]
      it "work with singleton update list with video attachment msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle14) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "" ["video-4144_1714"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["video-4144_1714"] ("", ""))
                     ]
      it "work with singleton update list with audio attachment msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle15) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "" ["audio1606_3483"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["audio1606_3483"] ("", ""))
                     ]
      it "work with singleton update list with market attachment msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle16) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "" ["market-1196_3822"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["market-1196_3822"] ("", ""))
                     ]
      it "work with singleton update list with wall attachment msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle17) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "" ["wall-6799_4584"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["wall-6799_4584"] ("", ""))
                     ]
      it "work with singleton update list with poll attachment msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle18) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "" ["poll-6799_3839"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["poll-6799_3839"] ("", ""))
                     ]
      it "work with singleton update list with audio attachment msg with text" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle19) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "hello" ["audio1606_3483"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "hello" ["audio1606_3483"] ("", ""))
                     ]
      it "work with singleton update list with market attachment msg with text" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle20) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "hello" ["market-1196_3822"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "hello" ["market-1196_3822"] ("", ""))
                     ]
      it "work with forward msg (IGNORE)" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle21) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING,
                       SENDMSG 1606 (TextMsg "I`m sorry, I can`t work with forward messages, so I will ignore this message")
                     ]
      it "work with singleton update list with GEO msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle22) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "" [] ("69.409", "32.456")),
                       SENDMSG 1606 (AttachmentMsg "" [] ("69.409", "32.456"))
                     ]
      it "work with singleton update list with GEO msg with text" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle23) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "hello" [] ("69.409", "32.456")),
                       SENDMSG 1606 (AttachmentMsg "hello" [] ("69.409", "32.456"))
                     ]
      it "work with singleton update list with GEO msg with audio attachment" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle24) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "" ["audio1606_3483"] ("69.409", "32.456")),
                       SENDMSG 1606 (AttachmentMsg "" ["audio1606_3483"] ("69.409", "32.456"))
                     ]
      it "work with singleton update list with GEO msg with audio attachment with text" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle25) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 1606 (AttachmentMsg "hello" ["audio1606_3483"] ("69.409", "32.456")),
                       SENDMSG 1606 (AttachmentMsg "hello" ["audio1606_3483"] ("69.409", "32.456"))
                     ]
      it "work with singleton update list with GEO msg with photo attachment with text" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle26) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       GOTPhotoSERVER 1606,
                       GOTOURL "https:photo",
                       LOADPhotoTOSERV "http://toLoadPic" "https:photo" "anyPhoto",
                       SAVEPhotoONSERV $
                         LoadPhotoResp 24 "anyHash" "anyPhotoSring",
                       SENDMSG 1606 (AttachmentMsg "hello" ["photo50_25"] ("69.409", "32.456")),
                       SENDMSG 1606 (AttachmentMsg "hello" ["photo50_25"] ("69.409", "32.456"))
                     ]
      it "work with singleton update list with GEO forward msg (IGNORE)" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle27) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING,
                       SENDMSG 123 (TextMsg "I`m sorry, I can`t work with forward messages, so I will ignore this message")
                     ]
      it "work with update list with several text msgs from different users" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle28) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 123 (TextMsg "love"),
                       SENDMSG 123 (TextMsg "love"),
                       SENDMSG 555 (TextMsg "hello"),
                       SENDMSG 555 (TextMsg "hello")
                     ]
      it "work with update list with several attachment msgs from different users" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle29) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       SENDMSG 123 (AttachmentMsg "" ["audio1606_3483"] ("", "")),
                       SENDMSG 123 (AttachmentMsg "" ["audio1606_3483"] ("", "")),
                       GOTPhotoSERVER 1606,
                       GOTOURL "https:photo",
                       LOADPhotoTOSERV "http://toLoadPic" "https:photo" "anyPhoto",
                       SAVEPhotoONSERV $
                         LoadPhotoResp 24 "anyHash" "anyPhotoSring",
                       SENDMSG 1606 (AttachmentMsg "" ["photo50_25"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["photo50_25"] ("", ""))
                     ]
      it "work with update list with several attachments in one msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle30) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       GOTPhotoSERVER 1606,
                       GOTOURL "https:photo",
                       LOADPhotoTOSERV "http://toLoadPic" "https:photo" "anyPhoto",
                       SAVEPhotoONSERV $
                         LoadPhotoResp 24 "anyHash" "anyPhotoSring",
                       SENDMSG 1606 (AttachmentMsg "" ["audio1606_3483", "photo50_25"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["audio1606_3483", "photo50_25"] ("", ""))
                     ]
      it "work with update list with several attachments with text in one msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle31) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       GOTPhotoSERVER 1606,
                       GOTOURL "https:photo",
                       LOADPhotoTOSERV "http://toLoadPic" "https:photo" "anyPhoto",
                       SAVEPhotoONSERV $
                         LoadPhotoResp 24 "anyHash" "anyPhotoSring",
                       SENDMSG 1606 (AttachmentMsg "hello" ["audio1606_3483", "photo50_25"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "hello" ["audio1606_3483", "photo50_25"] ("", ""))
                     ]
      it "work with update list with several attachments with text in one GEO msg" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle32) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       GOTPhotoSERVER 1606,
                       GOTOURL "https:photo",
                       LOADPhotoTOSERV "http://toLoadPic" "https:photo" "anyPhoto",
                       SAVEPhotoONSERV $
                         LoadPhotoResp 24 "anyHash" "anyPhotoSring",
                       SENDMSG 1606 (AttachmentMsg "hello" ["audio1606_3483", "photo50_25"] ("69.409", "32.456")),
                       SENDMSG 1606 (AttachmentMsg "hello" ["audio1606_3483", "photo50_25"] ("69.409", "32.456"))
                     ]
      it "work with update list with Unknown update (IGNORE only unknown update)" $ do
        actions <-
          execStateT
            (evalStateT (runServ handle33) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING,
                       SENDMSG 1606 (AttachmentMsg "" ["audio1606_3483"] ("", "")),
                       SENDMSG 1606 (AttachmentMsg "" ["audio1606_3483"] ("", ""))
                     ]
      it "work with update list with sticker msg with text (IGNORE) " $ do
        actions <-
          execStateT
            (evalStateT (runServ handle34) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING
                     ]
      it "work with update list with sticker and other attachment in one msg (IGNORE) " $ do
        actions <-
          execStateT
            (evalStateT (runServ handle35) (emptyTryServInf, initialDB1))
            []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING
                     ]
      it "throw CheckGetUpdatesException with unknown getUpdates answer" $
        runStateT (evalStateT (runServ handle36) (emptyTryServInf, initialDB1)) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "throw CheckGetUpdatesException with error getUpdates answer" $
        runStateT (evalStateT (runServ handle6) (emptyTryServInf, initialDB1)) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "got new server info and send nothing if getUpdates answer=fail2 FirstTime" $ do
        (newTryServInfo, _) <- evalStateT (execStateT (runServ handle37) (emptyTryServInf, initialDB1)) []
        newTryServInfo
          `shouldNotBe` emptyTryServInf
        actions <- execStateT (evalStateT (runServ handle37) (emptyTryServInf, initialDB1)) []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING,
                       GOTSERVER
                     ]
      it "got new server info and send nothing if getUpdates answer=fail2 SecondTime" $ do
        (newTryServInfo, _) <- evalStateT (execStateT (runServ handle37) (secondTry emptyServInf, initialDB1)) []
        newTryServInfo
          `shouldNotBe` emptyTryServInf
        actions <- execStateT (evalStateT (runServ handle37) (secondTry emptyServInf, initialDB1)) []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING,
                       GOTSERVER
                     ]
      it "change FirstTry to SecondTry if getUpdates answer=fail2 FirstTime" $ do
        (newTryServInfo, _) <- evalStateT (execStateT (runServ handle37) (emptyTryServInf, initialDB1)) []
        tryNum newTryServInfo `shouldBe` 2
      it "throw exception if getUpdates answer=fail2 ThirdTime" $
        evalStateT (execStateT (runServ handle37) (thirdTry emptyServInf, initialDB1)) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "got new server info and send nothing if getUpdates answer=fail3 FirstTime" $ do
        (newTryServInfo, _) <- evalStateT (execStateT (runServ handle38) (emptyTryServInf, initialDB1)) []
        newTryServInfo
          `shouldNotBe` emptyTryServInf
        actions <- execStateT (evalStateT (runServ handle38) (emptyTryServInf, initialDB1)) []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING,
                       GOTSERVER
                     ]
      it "got new server info and send nothing if getUpdates answer=fail3 SecondTime" $ do
        (newTryServInfo, _) <- evalStateT (execStateT (runServ handle38) (secondTry emptyServInf, initialDB1)) []
        newTryServInfo
          `shouldNotBe` emptyTryServInf
        actions <- execStateT (evalStateT (runServ handle38) (secondTry emptyServInf, initialDB1)) []
        reverse actions
          `shouldBe` [ GOTUPDATES emptyServInf,
                       LOG WARNING,
                       GOTSERVER
                     ]
      it "change FirstTry to SecondTry if getUpdates answer=fail3 FirstTime" $ do
        (newTryServInfo, _) <- evalStateT (execStateT (runServ handle38) (emptyTryServInf, initialDB1)) []
        tryNum newTryServInfo `shouldBe` 2
      it "throw exception if getUpdates answer=fail3 ThirdTime" $
        evalStateT (execStateT (runServ handle38) (thirdTry emptyServInf, initialDB1)) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "change ts in serverInfo and FirstTry to SecondTry if getUpdates answer=FailTs(ts=25) FirstTime" $ do
        (newTryServInfo, _) <- evalStateT (execStateT (runServ handle39) (emptyTryServInf, initialDB1)) []
        newTryServInfo `shouldBe` nextTry (emptyTryServInf {servInf = emptyServInf {tsSI = 25}})
      it "change ts in serverInfo and FirstTry to SecondTry if getUpdates answer=FailTs(ts=25,fail=1) FirstTime" $ do
        (newTryServInfo, _) <- evalStateT (execStateT (runServ handle40) (emptyTryServInf, initialDB1)) []
        newTryServInfo `shouldBe` nextTry (emptyTryServInf {servInf = emptyServInf {tsSI = 25}})
      it "throw exception if getUpdates answer=failTs(ts=25) ThirdTime" $
        evalStateT (execStateT (runServ handle39) (thirdTry emptyServInf, initialDB1)) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "throw GetUpdatesException if getUpdates HttpException" $
        runStateT (runStateT (runServ handle46) (emptyTryServInf, initialDB1)) []
          `shouldThrow` isGetUpdatesException
      it "throw SendMsgException if sendMsg HttpException" $
        runStateT (runStateT (runServ handle47) (emptyTryServInf, initialDB1)) []
          `shouldThrow` isSendMsgException
      it "throw SendKeybException if sendKeyb HttpException" $
        runStateT (runStateT (runServ handle48) (emptyTryServInf, initialDB1)) []
          `shouldThrow` isSendKeybException
      it "throw GetLongPollServerException if getServer HttpException; getUpdates answer=Fail" $
        runStateT (runStateT (runServ handle49) (emptyTryServInf, initialDB1)) []
          `shouldThrow` isGetLongPollServerException
    describe "(startApp >>= runServ)" $ do
      it "work with empty update list" $ do
        actions <-
          execStateT
            (startApp handle0 initialDB1 >>= evalStateT (runServ handle0))
            []
        reverse actions
          `shouldBe` [ LOGMSG
                         DEBUG
                         "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103",
                       GOTSERVER,
                       LOGMSG DEBUG $ "Get response: " ++ show json1,
                       LOGMSG INFO "Work with received server",
                       LOGMSG
                         DEBUG
                         "Send request to getUpdates: https:anyServer?act=a_check&key=anyKey&ts=289&wait=20",
                       GOTUPDATES $
                         ServerInfo
                           "anyKey"
                           "https:anyServer"
                           289,
                       LOGMSG DEBUG $ "Get response: " ++ show json2,
                       LOGMSG INFO "No new updates"
                     ]
      it "work with singleton update list with text msg" $ do
        actions <-
          execStateT
            (startApp handle2 initialDB1 >>= evalStateT (runServ handle2))
            []
        reverse actions
          `shouldBe` [ LOGMSG
                         DEBUG
                         "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103",
                       GOTSERVER,
                       LOGMSG
                         DEBUG
                         "Get response: \"{\\\"response\\\":{\\\"key\\\":\\\"anyKey\\\",\\\"server\\\":\\\"https:anyServer\\\",\\\"ts\\\":\\\"289\\\"}}\"",
                       LOGMSG INFO "Work with received server",
                       LOGMSG
                         DEBUG
                         "Send request to getUpdates: https:anyServer?act=a_check&key=anyKey&ts=289&wait=20",
                       GOTUPDATES $
                         ServerInfo
                           "anyKey"
                           "https:anyServer"
                           289,
                       LOGMSG
                         DEBUG
                         "Get response: \"{\\\"ts\\\":\\\"290\\\",\\\"updates\\\":[{\\\"type\\\":\\\"message_new\\\",\\\"object\\\":{\\\"date\\\":1594911394,\\\"from_id\\\":123,\\\"id\\\":597,\\\"out\\\":0,\\\"peer_id\\\":1606,\\\"text\\\":\\\"love\\\",\\\"conversation_message_id\\\":562,\\\"fwd_messages\\\":[],\\\"important\\\":false,\\\"random_id\\\":0,\\\"attachments\\\":[],\\\"is_hidden\\\":false},\\\"group_id\\\":19495,\\\"event_id\\\":\\\"123\\\"}]}\\r\\n\"",
                       LOGMSG INFO "There is new updates list",
                       LOGMSG INFO "Analysis update from the list",
                       LOGMSG INFO "Get TextMsg: \"love\" from user 123"
                     ]
          ++ ( concat . replicate 2 $
                 [ LOGMSG
                     DEBUG
                     "Send request to send to user_id:123 msg: TextMsg \"love\"",
                   SENDMSG 123 (TextMsg "love"),
                   LOGMSG DEBUG "Get response: \"{\\\"response\\\":626}\"",
                   LOGMSG INFO "Msg \"love\" was sent to user 123"
                 ]
             )
      it "work with singleton update list with sticker msg " $ do
        actions <-
          execStateT
            (startApp handle5 initialDB1 >>= evalStateT (runServ handle5))
            []
        reverse actions
          `shouldBe` [ LOGMSG
                         DEBUG
                         "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=321&access_token=ABC123&v=5.103",
                       GOTSERVER,
                       LOGMSG DEBUG $ "Get response: " ++ show json1,
                       LOGMSG INFO "Work with received server",
                       LOGMSG
                         DEBUG
                         "Send request to getUpdates: https:anyServer?act=a_check&key=anyKey&ts=289&wait=20",
                       GOTUPDATES $
                         ServerInfo
                           "anyKey"
                           "https:anyServer"
                           289,
                       LOGMSG DEBUG $ "Get response: " ++ show json7,
                       LOGMSG INFO "There is new updates list",
                       LOGMSG INFO "Analysis update from the list",
                       LOGMSG
                         INFO
                         "Get AttachmentMsg: [StickerAttachment {sticker = StickerInfo {sticker_id = 9014}}] from user 1606"
                     ]
          ++ ( concat . replicate 2 $
                 [ LOGMSG
                     DEBUG
                     "Send request to send to user_id:1606 msg: StickerMsg 9014",
                   SENDMSG 1606 (StickerMsg 9014),
                   LOGMSG DEBUG "Get response: \"{\\\"response\\\":626}\"",
                   LOGMSG INFO "Sticker_id 9014 was sent to user 1606"
                 ]
             )
      it "throw CheckGetUpdatesException with error getUpdates answer" $
        evalStateT
          (startApp handle6 initialDB1 >>= evalStateT (runServ handle6))
          []
          `shouldThrow` ( ==
                            ( CheckGetUpdatesResponseException $
                                "NEGATIVE RESPONSE:" ++ show json5
                            )
                        )
      it "throw CheckGetUpdatesException with unknown getUpdates answer" $
        evalStateT
          (startApp handle7 initialDB1 >>= evalStateT (runServ handle7))
          []
          `shouldThrow` ( ==
                            ( CheckGetUpdatesResponseException $
                                "UNKNOWN RESPONSE:" ++ show json6
                            )
                        )
    describe "run" $ do
      it "throw CheckGetUpdatesResponseException if getUpdates forever answer=fail2 (more then 2 times) " $
        evalStateT (run handle37 initialDB1) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "throw CheckGetUpdatesResponseException if getUpdates forever answer=fail3 (more then 2 times) " $
        evalStateT (run handle38 initialDB1) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "throw CheckGetUpdatesResponseException if getUpdates forever answer=FailTs(ts=25) (more then 2 times) " $
        evalStateT (run handle39 initialDB1) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "throw CheckGetUpdatesResponseException if getUpdates forever answer=FailTs(ts=25,fail=1) (more then 2 times) " $
        evalStateT (run handle40 initialDB1) []
          `shouldThrow` isCheckGetUpdatesResponseException