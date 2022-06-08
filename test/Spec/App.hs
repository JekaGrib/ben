{-# LANGUAGE OverloadedStrings #-}

module Spec.App where

import Control.Monad.State (evalStateT, execStateT)
import qualified Data.Map as Map
--import Spec.Error
import Spec.Types
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow)
import App (chooseActionOfUpd)
import Error (BotException (..))
import Logger (Priority (..))
import Types
import Spec.App.Handlers

initialDB1, initialDB2, initialDB3 :: MapUserN
initialDB1 = Map.empty
initialDB2 = Map.fromList [(1118, Left (OpenRepeat 2)), (1234, Right 3), (2581, Left (OpenRepeat 4))]
initialDB3 = Map.fromList [(1118, Right 2), (1234, Right 3), (2581, Left (OpenRepeat 4))]

testApp :: IO ()
testApp =
  hspec $ do
    describe "chooseActionOfUpd" $ do
      it "work with update with text msg" $ do
        let upd = ValidUpdate 1118 (TextMsg "love") :: ValidUpdate AttachNotMatter
        state <- execStateT (evalStateT (chooseActionOfUpd handle0 upd) initialDB1) []
        reverse state 
          `shouldBe` 
          [LOGMSG INFO "Analysis update from the list"
          ,LOGMSG INFO "Get msg from user 1118"
          ,LOGMSG INFO "Msg has text: \"love\""
          ] ++
            (concat . replicate 2 $ 
              [LOGMSG DEBUG "Send request to send msg TextMsg \"love\" to userId 1118"
              ,SENDMSG 1118 "love"
              ,LOGMSG DEBUG "Get response: \"ok\""
              ,LOGMSG INFO "Msg TextMsg \"love\" was sent to user 1118"
              ])
      it "work with text msg if user is in OpenRepeatMode " $ do
        let upd = ValidUpdate 1118 (TextMsg "love") :: ValidUpdate AttachNotMatter
        state <- execStateT (evalStateT (chooseActionOfUpd handle1 upd) initialDB2) []
        reverse state
          `shouldBe` [ LOG WARNING,
                       SENDMSG 1118 "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still 2\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later"
                     ]
      it "work with /help msg" $ do
        let upd = ValidUpdate 1118 (TextMsg "/help") :: ValidUpdate AttachNotMatter
        dbState <- evalStateT (execStateT (chooseActionOfUpd handle0 upd) initialDB1) []
        dbState `shouldBe` initialDB1
        state <- execStateT (evalStateT (chooseActionOfUpd handle0 upd) initialDB1) []
        reverse state 
          `shouldBe` 
          [LOGMSG INFO "Analysis update from the list"
          ,LOGMSG INFO "Get msg from user 1118"
          ,LOGMSG INFO "Msg has text: \"/help\""
          ,LOGMSG DEBUG "Send request to send msg TextMsg \"Hello\" to userId 1118"
          ,SENDMSG 1118 "Hello"
          ,LOGMSG DEBUG "Get response: \"ok\""
          ,LOGMSG INFO "Msg TextMsg \"Hello\" was sent to user 1118"
          ]
      it "work with /repeat msg" $ do
        let upd = ValidUpdate 1118 (TextMsg "/repeat") :: ValidUpdate AttachNotMatter
        dbState <- evalStateT (execStateT (chooseActionOfUpd handle1 upd) initialDB1) []
        dbState `shouldBe` Map.fromList [(1118, Left (OpenRepeat 2))]
        state <- execStateT (evalStateT (chooseActionOfUpd handle1 upd) initialDB1) []
        reverse state
          `shouldBe` [SENDKEYB 1118 2 " : Current number of repeats your message.\nWhy?"]
      it "work with attachment msg" $ do
        let upd = ValidUpdate 1118 (AttachMsg AttachNotMatter) :: ValidUpdate AttachNotMatter
        state <- execStateT (evalStateT (chooseActionOfUpd handle1 upd) initialDB1) []
        reverse state
          `shouldBe` [SENDAttachMSG 1118 AttachNotMatter,SENDAttachMSG 1118 AttachNotMatter]
      it "work with msg 4 if user is in OpenRepeatMode " $ do
        let upd = ValidUpdate 1118 (TextMsg "4") :: ValidUpdate AttachNotMatter
        dbState <- evalStateT (execStateT (chooseActionOfUpd handle1 upd) initialDB2) []
        dbState `shouldBe` Map.fromList [(1118,Right 4),(1234,Right 3),(2581,Left (OpenRepeat 4))]
        state <- execStateT (evalStateT (chooseActionOfUpd handle1 upd) initialDB2) []
        reverse state
          `shouldBe` [SENDMSG 1118 "Number of repeats successfully changed from 2 to 4"]
      it "ignore invalid update " $ do
        let upd = InvalidUpdate :: ValidUpdate AttachNotMatter
        state <- execStateT (evalStateT (chooseActionOfUpd handle1 upd) initialDB1) []
        reverse state
          `shouldBe` [LOG WARNING]
      it "ignore invalid update (plus info) " $ do
        let upd = InvalidUpdatePlusInfo "oops" :: ValidUpdate AttachNotMatter
        state <- execStateT (evalStateT (chooseActionOfUpd handle1 upd) initialDB1) []
        reverse state
          `shouldBe` [LOG WARNING]
{-}      it "ignore unknown update " $ do
        state <- execStateT (evalStateT (run handle24) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 235801, LOG WARNING]
      it "ignore unknown empty update " $ do
        state <- execStateT (evalStateT (run handle25) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 235801, LOG WARNING]
      it "ignore unknown update in not single update list" $ do
        state <- execStateT (evalStateT (run handle26) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 235802, LOG WARNING, SENDMSG 1118 "love", SENDMSG 1118 "love"]
      it "work with update with without extra data" $ do
        state <- execStateT (evalStateT (run handle27) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 235802, SENDMSG 1118 "love", SENDMSG 1118 "love"]
      it "work with update with wrong extra data" $ do
        state <- execStateT (evalStateT (run handle28) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 235802, SENDMSG 1118 "love", SENDMSG 1118 "love"]
      it "work with several users " $ do
        state <- execStateT (evalStateT (run handle22) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 235804, LOG WARNING, COPYMSG 1118 2110, COPYMSG 1118 2110, SENDMSG 12677 "Toni", SENDMSG 12677 "Toni", COPYMSG 12677 2112, COPYMSG 12677 2112]
      it "work with photo update " $ do
        state <- execStateT (evalStateT (run handle36) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 17107, COPYMSG 11189 2625, COPYMSG 11189 2625]
      it "work with doc update " $ do
        state <- execStateT (evalStateT (run handle37) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 17107, COPYMSG 11189 2643, COPYMSG 11189 2643]
      it "work with audio message update " $ do
        state <- execStateT (evalStateT (run handle38) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 17107, COPYMSG 11189 2646, COPYMSG 11189 2646]
      it "work with gif update " $ do
        state <- execStateT (evalStateT (run handle39) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 17107, COPYMSG 11189 2649, COPYMSG 11189 2649]
      it "work with video update " $ do
        state <- execStateT (evalStateT (run handle40) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 17107, COPYMSG 11189 2655, COPYMSG 11189 2655]
      it "work with audio update " $ do
        state <- execStateT (evalStateT (run handle41) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 17107, COPYMSG 11189 2661, COPYMSG 11189 2661]
      it "work with forward message update " $ do
        state <- execStateT (evalStateT (run handle42) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 17107, COPYMSG 11189 2664, COPYMSG 11189 2664]
      it "throw CheckGetUpdatesResponseException on negative getUpdates response" $
        evalStateT (evalStateT (run handle6) initialDB1) []
          `shouldThrow` ( ==
                            ( CheckGetUpdatesResponseException $
                                "NEGATIVE RESPONSE:" ++ show json2
                            )
                        )
      it "throw CheckGetUpdatesResponseException on unknown getUpdates response" $
        evalStateT (evalStateT (run handle7) initialDB1) []
          `shouldThrow` ( ==
                            ( CheckGetUpdatesResponseException $
                                "UNKNOWN RESPONSE:" ++ show json3
                            )
                        )
      it "throw CheckGetUpdatesResponseException on other negative getUpdates response" $
        evalStateT (evalStateT (run handle14) initialDB1) []
          `shouldThrow` isCheckGetUpdatesResponseException
      it "work with other ok confirmUpdates response" $ do
        dbState <- evalStateT (execStateT (run handle9) initialDB1) []
        dbState `shouldBe` initialDB1
        state <- execStateT (evalStateT (run handle17) initialDB1) []
        reverse state
          `shouldBe` [GOTUPDATES, CONFIRMUPDATES 235802, SENDMSG 1118 "love", SENDMSG 1118 "love"]
      it "throw CheckConfirmUpdatesResponseException on negative confirmUpdates response" $
        evalStateT (evalStateT (run handle16) initialDB1) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it "throw CheckConfirmUpdatesResponseException on other negative confirmUpdates response" $
        evalStateT (evalStateT (run handle20) initialDB1) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it "throw CheckConfirmUpdatesResponseException on unknown confirmUpdates response" $
        evalStateT (evalStateT (run handle18) initialDB1) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it "throw ConfirmUpdatesException on confirmUpdates where getUpdates response with negative updateId" $
        evalStateT (evalStateT (run handle19) initialDB1) []
          `shouldThrow` isConfirmUpdatesException
      it "throw CheckSendMsgResponseException on unknown sendMsg response" $
        evalStateT (evalStateT (run handle29) initialDB2) []
          `shouldThrow` isCheckSendMsgResponseException
      it "throw CheckSendMsgResponseException on negative sendMsg response" $
        evalStateT (evalStateT (run handle30) initialDB2) []
          `shouldThrow` isCheckSendMsgResponseException
      it "throw CheckSendMsgResponseException on other negative sendMsg response" $
        evalStateT (evalStateT (run handle31) initialDB2) []
          `shouldThrow` isCheckSendMsgResponseException
      it "throw CheckCopyMsgResponseException on unknown copyMsg response" $
        evalStateT (evalStateT (run handle33) initialDB2) []
          `shouldThrow` isCheckCopyMsgResponseException
      it "throw CheckCopyMsgResponseException on negative copyMsg response" $
        evalStateT (evalStateT (run handle34) initialDB2) []
          `shouldThrow` isCheckCopyMsgResponseException
      it "throw CheckCopyMsgResponseException on other negative copyMsg response" $
        evalStateT (evalStateT (run handle35) initialDB2) []
          `shouldThrow` isCheckCopyMsgResponseException
      it "throw CheckSendKeybResponseException on unknown sendKeyboard response" $
        evalStateT (evalStateT (run handle44) initialDB3) []
          `shouldThrow` isCheckSendKeybResponseException
      it "throw CheckSendKeybResponseException on negative sendKeyboard response" $
        evalStateT (evalStateT (run handle45) initialDB3) []
          `shouldThrow` isCheckSendKeybResponseException
      it "throw CheckSendKeybResponseException on other negative sendKeyboard response" $
        evalStateT (evalStateT (run handle46) initialDB3) []
          `shouldThrow` isCheckSendKeybResponseException
      it "throw CheckConfirmUpdatesResponseException on unknown confirmUpdates response" $
        evalStateT (evalStateT (run handle47) initialDB3) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it "throw CheckConfirmUpdatesResponseException on negative confirmUpdates response" $
        evalStateT (evalStateT (run handle48) initialDB3) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it "throw CheckConfirmUpdatesResponseException on other negative confirmUpdates response" $
        evalStateT (evalStateT (run handle49) initialDB3) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it "throw SendMsgException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle50) initialDB3) []
          `shouldThrow` isSendMsgException
      it "throw GetUpdatesException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle51) initialDB3) []
          `shouldThrow` isGetUpdatesException
      it "throw ConfirmUpdatesException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle52) initialDB3) []
          `shouldThrow` isConfirmUpdatesException
      it "throw SendKeybException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle53) initialDB3) []
          `shouldThrow` isSendKeybException
      it "throw CopyMsgException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle54) initialDB3) []
          `shouldThrow` isCopyMsgException
-}