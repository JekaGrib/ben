module Spec.Tg.App where

import Control.Monad.State (evalStateT, execStateT)
import qualified Data.Map as Map
import Error (BotException (..))
import Logger (Priority (..))
import Spec.Error
import Spec.Tg.App.Handlers
import Spec.Tg.App.ResponseExample
import Spec.Tg.Error
import Spec.Tg.Types
import Spec.Types
import Test.Hspec (Selector, describe, hspec, it, shouldBe, shouldThrow)
import Tg.App (run, startApp)
import Tg.Error (TGBotException (..))
import Tg.Types
import Types

initialDB1, initialDB2, initialDB3 :: MapUserN
initialDB1 = Map.empty
initialDB2 =
  Map.fromList
    [ (1118, Left (OpenRepeat 2)),
      (1234, Right 3),
      (2581, Left (OpenRepeat 4))
    ]
initialDB3 = Map.fromList [(1118, Right 2), (1234, Right 3), (2581, Left (OpenRepeat 4))]

testTGApp :: IO ()
testTGApp =
  hspec $ do
    describe "startApp" $ do
      it
        "return [LOG INFO, LOG DEBUG, TgMock GOTUPDATES , LOG INFO] when given empty update list"
        $ do
          state <- execStateT (startApp handle0) []
          reverse state
            `shouldBe` [ LOGMSG INFO "App started",
                         LOGMSG
                           DEBUG
                           "Send request to getUpdates:\
                           \ https://api.telegram.org/botABC123/getUpdates",
                         TgMock GOTUPDATES,
                         LOGMSG DEBUG $ "Get response: " ++ show json1,
                         LOGMSG INFO "No new updates"
                       ]
      it
        "return [LOG DEBUG, TgMock GOTUPDATES, TgMock $ CONFIRMUPDATES, LOG DEBUG] \
        \when given unempty update list"
        $ do
          state <- execStateT (startApp handle5) []
          reverse state
            `shouldBe` [ LOGMSG INFO "App started",
                         LOGMSG
                           DEBUG
                           "Send request to getUpdates:\
                           \ https://api.telegram.org/botABC123/getUpdates",
                         TgMock GOTUPDATES,
                         LOGMSG DEBUG $ "Get response: " ++ show json5,
                         LOGMSG INFO "There is new updates list",
                         LOGMSG
                           DEBUG
                           "Send request to confirmOldUpdates with offset:235804\
                           \ https://api.telegram.org/botABC123/getUpdates",
                         TgMock $ CONFIRMUPDATES 235804,
                         LOGMSG DEBUG $ "Get response: " ++ show json1,
                         LOGMSG INFO "Received updates confirmed"
                       ]
      it "throw CheckGetUpdatesResponseException on negative getUpdates response" $
        evalStateT (startApp handle2) []
          `shouldThrow` ( ==
                            ( CheckGetUpdatesResponseException $
                                "NEGATIVE RESPONSE:" ++ show json2
                            )
                        )
      it "throw CheckGetUpdatesResponseException on unknown getUpdates response" $
        evalStateT (startApp handle3) []
          `shouldThrow` ( ==
                            ( CheckGetUpdatesResponseException $
                                "UNKNOWN RESPONSE:" ++ show json3
                            )
                        )
      it "throw CheckGetUpdatesResponseException on unknown result getUpdates response" $
        evalStateT (startApp handle4) []
          `shouldThrow` ( ==
                            ( CheckGetUpdatesResponseException $
                                "UNKNOWN RESULT IN RESPONSE:" ++ show json4
                            )
                        )
      it "throw CheckGetUpdatesResponseException on other unknown result getUpdates response" $
        evalStateT (startApp handle21) []
          `shouldThrow` ( ==
                            ( CheckGetUpdatesResponseException $
                                "UNKNOWN RESULT IN RESPONSE:" ++ show json14
                            )
                        )
    describe "run" $ do
      it "work with singleton update list with text msg" $ do
        state <- execStateT (evalStateT (run handle0) initialDB1) []
        reverse state
          `shouldBe` [ LOGMSG
                         DEBUG
                         "Send request to getUpdates:\
                         \ https://api.telegram.org/botABC123/getUpdates",
                       TgMock GOTUPDATES,
                       LOGMSG DEBUG $ "Get response: " ++ show json6,
                       LOGMSG INFO "There is new updates list",
                       LOGMSG
                         DEBUG
                         "Send request to confirmOldUpdates with offset:235802\
                         \ https://api.telegram.org/botABC123/getUpdates",
                       TgMock $ CONFIRMUPDATES 235802,
                       LOGMSG DEBUG $ "Get response: " ++ show json1,
                       LOGMSG INFO "Received updates confirmed",
                       LOGMSG INFO "Analysis update from the list",
                       LOGMSG INFO "Get msg from user 1118",
                       LOGMSG INFO "Msg has text: \"love\""
                     ]
          ++ ( concat . replicate 2 $
                 [ LOGMSG
                     DEBUG
                     "Send request to send msg TextMsg \"love\" to userId 1118",
                   SENDMSG 1118 "love",
                   LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"",
                   LOGMSG INFO "Msg TextMsg \"love\" was sent to user 1118"
                 ]
             )
      it "work with text msg if user is in OpenRepeatMode " $ do
        state <- execStateT (evalStateT (run handle13) initialDB2) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 235802,
                       LOG WARNING,
                       SENDMSG
                         1118
                         "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed,\
                         \ it is still 2\nTo change it you may sent me command \"/repeat\"\
                         \ and then choose number from 1 to 5 on keyboard\
                         \\nPlease, try again later"
                     ]
      it "work with singleton update list with /help msg" $ do
        dbState <- evalStateT (execStateT (run handle9) initialDB1) []
        dbState `shouldBe` initialDB1
        state <- execStateT (evalStateT (run handle9) initialDB1) []
        reverse state
          `shouldBe` [ LOGMSG
                         DEBUG
                         "Send request to getUpdates:\
                         \ https://api.telegram.org/botABC123/getUpdates",
                       TgMock GOTUPDATES,
                       LOGMSG DEBUG $ "Get response: " ++ show json7,
                       LOGMSG INFO "There is new updates list",
                       LOGMSG
                         DEBUG
                         "Send request to confirmOldUpdates with offset:235802\
                         \ https://api.telegram.org/botABC123/getUpdates",
                       TgMock $ CONFIRMUPDATES 235802,
                       LOGMSG
                         DEBUG
                         "Get response: \"{\\\"ok\\\":true,\\\"result\\\":[]}\"",
                       LOGMSG INFO "Received updates confirmed",
                       LOGMSG INFO "Analysis update from the list",
                       LOGMSG INFO "Get msg from user 1118",
                       LOGMSG INFO "Msg has text: \"/help\"",
                       LOGMSG
                         DEBUG
                         "Send request to send msg TextMsg \"Hello\" to userId 1118",
                       SENDMSG 1118 "Hello",
                       LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"",
                       LOGMSG INFO "Msg TextMsg \"Hello\" was sent to user 1118"
                     ]
      it "work with singleton update list with /repeat msg" $ do
        dbState <- evalStateT (execStateT (run handle10) initialDB1) []
        dbState `shouldBe` Map.fromList [(1118, Left (OpenRepeat 2))]
        state <- execStateT (evalStateT (run handle10) initialDB1) []
        reverse state
          `shouldBe` [ LOG DEBUG,
                       TgMock GOTUPDATES,
                       LOG DEBUG,
                       LOG INFO,
                       LOG DEBUG,
                       TgMock $ CONFIRMUPDATES 235801,
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG DEBUG,
                       SENDKEYB
                         1118
                         2
                         " : Current number of repeats your message.\nWhy?",
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO
                     ]
      it "work with sticker update" $ do
        state <- execStateT (evalStateT (run handle11) initialDB1) []
        reverse state
          `shouldBe` [ LOG DEBUG,
                       TgMock GOTUPDATES,
                       LOG DEBUG,
                       LOG INFO,
                       LOG DEBUG,
                       TgMock $ CONFIRMUPDATES 235808,
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG DEBUG,
                       SENDAttachMSG 12677 2140,
                       LOG DEBUG,
                       LOG INFO,
                       LOG DEBUG,
                       SENDAttachMSG 12677 2140,
                       LOG DEBUG,
                       LOG INFO
                     ]
      it "work with msg:4 after /repeat" $ do
        dbState <-
          evalStateT (execStateT (run handle10 >> run handle12) initialDB1) []
        dbState `shouldBe` Map.fromList [(1118, Right 4)]
        state <-
          execStateT (evalStateT (run handle10 >> run handle12) initialDB1) []
        reverse state
          `shouldBe` [ LOG DEBUG,
                       TgMock GOTUPDATES,
                       LOG DEBUG,
                       LOG INFO,
                       LOG DEBUG,
                       TgMock $ CONFIRMUPDATES 235801,
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG DEBUG,
                       SENDKEYB
                         1118
                         2
                         " : Current number of repeats your message.\nWhy?",
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO,
                       LOG DEBUG,
                       TgMock GOTUPDATES,
                       LOG DEBUG,
                       LOG INFO,
                       LOG DEBUG,
                       TgMock $ CONFIRMUPDATES 235802,
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG DEBUG,
                       SENDMSG
                         1118
                         "Number of repeats successfully changed from 2 to 4",
                       LOG DEBUG,
                       LOG INFO
                     ]
      it "warning with msg:love after /repeat" $ do
        dbState <-
          evalStateT (execStateT (run handle10 >> run handle1) initialDB1) []
        dbState `shouldBe` Map.fromList [(1118, Right 2)]
        state <-
          execStateT (evalStateT (run handle10 >> run handle1) initialDB1) []
        reverse state
          `shouldBe` [ LOG DEBUG,
                       TgMock GOTUPDATES,
                       LOG DEBUG,
                       LOG INFO,
                       LOG DEBUG,
                       TgMock $ CONFIRMUPDATES 235801,
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG DEBUG,
                       SENDKEYB
                         1118
                         2
                         " : Current number of repeats your message.\nWhy?",
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO,
                       LOG DEBUG,
                       TgMock GOTUPDATES,
                       LOG DEBUG,
                       LOG INFO,
                       LOG DEBUG,
                       TgMock $ CONFIRMUPDATES 235802,
                       LOG DEBUG,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG INFO,
                       LOG WARNING,
                       LOG DEBUG,
                       SENDMSG
                         1118
                         "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed,\
                         \ it is still 2\nTo change it you may sent me command \"/repeat\"\
                         \ and then choose number from 1 to 5 on keyboard\
                         \\nPlease, try again later",
                       LOG DEBUG,
                       LOG INFO
                     ]
      it "ignore unknown update (edited_message) " $ do
        state <- execStateT (evalStateT (run handle23) initialDB1) []
        reverse state
          `shouldBe` [TgMock GOTUPDATES, TgMock $ CONFIRMUPDATES 235801, LOG WARNING]
      it "ignore unknown update " $ do
        state <- execStateT (evalStateT (run handle24) initialDB1) []
        reverse state
          `shouldBe` [TgMock GOTUPDATES, TgMock $ CONFIRMUPDATES 235801, LOG WARNING]
      it "ignore unknown empty update " $ do
        state <- execStateT (evalStateT (run handle25) initialDB1) []
        reverse state
          `shouldBe` [TgMock GOTUPDATES, TgMock $ CONFIRMUPDATES 235801, LOG WARNING]
      it "ignore unknown update in not single update list" $ do
        state <- execStateT (evalStateT (run handle26) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 235802,
                       LOG WARNING,
                       SENDMSG 1118 "love",
                       SENDMSG 1118 "love"
                     ]
      it "work with update with without extra data" $ do
        state <- execStateT (evalStateT (run handle27) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 235802,
                       SENDMSG 1118 "love",
                       SENDMSG 1118 "love"
                     ]
      it "work with update with wrong extra data" $ do
        state <- execStateT (evalStateT (run handle28) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 235802,
                       SENDMSG 1118 "love",
                       SENDMSG 1118 "love"
                     ]
      it "work with several users " $ do
        state <- execStateT (evalStateT (run handle22) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 235804,
                       LOG WARNING,
                       SENDAttachMSG 1118 2110,
                       SENDAttachMSG 1118 2110,
                       SENDMSG 12677 "Toni",
                       SENDMSG 12677 "Toni",
                       SENDAttachMSG 12677 2112,
                       SENDAttachMSG 12677 2112
                     ]
      it "work with photo update " $ do
        state <- execStateT (evalStateT (run handle36) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 17107,
                       SENDAttachMSG 11189 2625,
                       SENDAttachMSG 11189 2625
                     ]
      it "work with doc update " $ do
        state <- execStateT (evalStateT (run handle37) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 17107,
                       SENDAttachMSG 11189 2643,
                       SENDAttachMSG 11189 2643
                     ]
      it "work with audio message update " $ do
        state <- execStateT (evalStateT (run handle38) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 17107,
                       SENDAttachMSG 11189 2646,
                       SENDAttachMSG 11189 2646
                     ]
      it "work with gif update " $ do
        state <- execStateT (evalStateT (run handle39) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 17107,
                       SENDAttachMSG 11189 2649,
                       SENDAttachMSG 11189 2649
                     ]
      it "work with video update " $ do
        state <- execStateT (evalStateT (run handle40) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 17107,
                       SENDAttachMSG 11189 2655,
                       SENDAttachMSG 11189 2655
                     ]
      it "work with audio update " $ do
        state <- execStateT (evalStateT (run handle41) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 17107,
                       SENDAttachMSG 11189 2661,
                       SENDAttachMSG 11189 2661
                     ]
      it "work with forward message update " $ do
        state <- execStateT (evalStateT (run handle42) initialDB1) []
        reverse state
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 17107,
                       SENDAttachMSG 11189 2664,
                       SENDAttachMSG 11189 2664
                     ]
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
          `shouldBe` [ TgMock GOTUPDATES,
                       TgMock $ CONFIRMUPDATES 235802,
                       SENDMSG 1118 "love",
                       SENDMSG 1118 "love"
                     ]
      it "throw CheckConfirmUpdatesResponseException on negative confirmUpdates response" $
        evalStateT (evalStateT (run handle16) initialDB1) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it "throw CheckConfirmUpdatesResponseException on other negative confirmUpdates response" $
        evalStateT (evalStateT (run handle20) initialDB1) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it "throw CheckConfirmUpdatesResponseException on unknown confirmUpdates response" $
        evalStateT (evalStateT (run handle18) initialDB1) []
          `shouldThrow` isCheckConfirmUpdatesResponseException
      it
        "throw ConfirmUpdatesException on confirmUpdates \
        \where getUpdates response with negative updateId"
        $ evalStateT (evalStateT (run handle19) initialDB1) []
          `shouldThrow` isConfirmUpdatesException
      it "throw CheckSendMsgResponseException on unknown sendMsg response" $
        evalStateT (evalStateT (run handle29) initialDB2) []
          `shouldThrow` (isCheckSendMsgResponseException :: Selector (BotException MessageId))
      it "throw CheckSendMsgResponseException on negative sendMsg response" $
        evalStateT (evalStateT (run handle30) initialDB2) []
          `shouldThrow` (isCheckSendMsgResponseException :: Selector (BotException MessageId))
      it "throw CheckSendMsgResponseException on other negative sendMsg response" $
        evalStateT (evalStateT (run handle31) initialDB2) []
          `shouldThrow` (isCheckSendMsgResponseException :: Selector (BotException MessageId))
      it "throw CheckSendMsgResponseException on unknown sendAttachMsg response" $
        evalStateT (evalStateT (run handle33) initialDB2) []
          `shouldThrow` (isCheckSendMsgResponseException :: Selector (BotException MessageId))
      it "throw CheckSendMsgResponseException on negative sendAttachMsg response" $
        evalStateT (evalStateT (run handle34) initialDB2) []
          `shouldThrow` (isCheckSendMsgResponseException :: Selector (BotException MessageId))
      it "throw CheckSendMsgResponseException on other negative sendAttachMsg response" $
        evalStateT (evalStateT (run handle35) initialDB2) []
          `shouldThrow` (isCheckSendMsgResponseException :: Selector (BotException MessageId))
      it "throw CheckSendKeybResponseException on unknown sendKeyboard response" $
        evalStateT (evalStateT (run handle44) initialDB3) []
          `shouldThrow` (isCheckSendKeybResponseException :: Selector (BotException AttachNotMatter))
      it "throw CheckSendKeybResponseException on negative sendKeyboard response" $
        evalStateT (evalStateT (run handle45) initialDB3) []
          `shouldThrow` (isCheckSendKeybResponseException :: Selector (BotException AttachNotMatter))
      it "throw CheckSendKeybResponseException on other negative sendKeyboard response" $
        evalStateT (evalStateT (run handle46) initialDB3) []
          `shouldThrow` (isCheckSendKeybResponseException :: Selector (BotException AttachNotMatter))
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
          `shouldThrow` (isSendMsgException :: Selector (BotException MessageId))
      it "throw GetUpdatesException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle51) initialDB3) []
          `shouldThrow` isGetUpdatesException
      it "throw ConfirmUpdatesException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle52) initialDB3) []
          `shouldThrow` isConfirmUpdatesException
      it "throw SendKeybException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle53) initialDB3) []
          `shouldThrow` (isSendKeybException :: Selector (BotException AttachNotMatter))
      it "throw SendMsgException on sendMsg ConnectionTimeout" $
        evalStateT (evalStateT (run handle54) initialDB3) []
          `shouldThrow` (isSendMsgException :: Selector (BotException MessageId))
