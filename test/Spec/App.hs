{-# LANGUAGE OverloadedStrings #-}

module Spec.App where

import Control.Monad.State (evalStateT, execStateT)
import qualified Data.Map as Map
import Spec.Error
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
      it "throw CheckSendMsgResponseException on unknown sendMsg response" $ do
        let upd = ValidUpdate 1118 (TextMsg "4") :: ValidUpdate AttachNotMatter
        evalStateT (evalStateT (chooseActionOfUpd handle2 upd) initialDB2) []
          `shouldThrow` (isCheckSendMsgResponseException :: BotException AttachNotMatter -> Bool)
      it "throw CheckSendMsgResponseException on unknown sendAttachMsg response" $ do
        let upd = ValidUpdate 1118 (AttachMsg AttachNotMatter) :: ValidUpdate AttachNotMatter
        evalStateT (evalStateT (chooseActionOfUpd handle2 upd) initialDB2) []
          `shouldThrow` (isCheckSendMsgResponseException :: BotException AttachNotMatter -> Bool)
      it "throw CheckSendKeybResponseException on unknown sendKeyboard response" $ do
        let upd = ValidUpdate 1118 (TextMsg "/repeat") :: ValidUpdate AttachNotMatter
        evalStateT (evalStateT (chooseActionOfUpd handle3 upd) initialDB1) []
          `shouldThrow` (isCheckSendKeybResponseException :: BotException AttachNotMatter -> Bool)
      
{-}      it "throw CheckConfirmUpdatesResponseException on unknown confirmUpdates response" $
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