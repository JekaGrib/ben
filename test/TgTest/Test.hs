{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module TgTest.Test where

import Control.Monad.State (evalStateT, execStateT)
import qualified Data.Map as Map
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow)
import Tg.App (run, startApp)
import Tg.Logger (Priority(..))
import Tg.Oops (TGBotException(..))
import Tg.Types
import TgTest.Types
import TgTest.Oops
import TgTest.Handlers


initialDB1,initialDB2 :: MapUserN
initialDB1 = Map.fromList []
initialDB2 = Map.fromList [(1118, Left (OpenRepeat 2)),(1234, Right 3),(2581, Left (OpenRepeat 4))]


testTG :: IO ()
testTG =
  hspec $ do
    describe "startApp" $ do
      it
        "return [LOG INFO, LOG DEBUG, GOTUPDATES , LOG INFO] when given empty update list" $ do
        state <- execStateT (startApp handle0) []
        reverse state `shouldBe`
          [ LOGMSG INFO "App started\n"
          , LOGMSG
              DEBUG
              "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n"
          , GOTUPDATES
          , LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n"
          , LOGMSG INFO "No new updates\n"
          ]
      it
        "return [LOG DEBUG, GOTUPDATES, CONFIRMUPDATES, LOG DEBUG] when given unempty update list" $ do
        state <- execStateT (startApp handle5) []
        reverse state `shouldBe`
          [ LOGMSG INFO "App started\n"
          , LOGMSG
              DEBUG
              "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n"
          , GOTUPDATES
          , LOGMSG DEBUG $ "Get response: " ++ show json5 ++ "\n"
          , LOGMSG INFO "There is new updates list\n"
          , LOGMSG
              DEBUG
              "Send request to confirmOldUpdates with offset:235804 https://api.telegram.org/botABC123/getUpdates\n"
          , CONFIRMUPDATES 235804
          , LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n"
          , LOGMSG INFO "Received updates confirmed\n"
          ]
      it "throw Exception on negative getUpdates response" $
        evalStateT (startApp handle2) [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $
             "NEGATIVE RESPONSE:\n" ++ show json2))
      it "throw Exception on unknown getUpdates response" $
        evalStateT (startApp handle3) [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $
             "UNKNOWN RESPONSE:\n" ++ show json3))
      it "throw Exception on unknown result getUpdates response"  $
        evalStateT (startApp handle4) [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $ "UNKNOWN RESULT IN RESPONSE:\n" ++ show json4 ))
      it "throw Exception on other unknown result getUpdates response"  $
        evalStateT (startApp handle21) [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $ "UNKNOWN RESULT IN RESPONSE:\n" ++ show json14 ))
    describe "run" $ do
      it "work with singleton update list with text msg" $ do
        state <- execStateT (evalStateT (run handle0) initialDB1) []
        reverse state `shouldBe`
          [ LOGMSG
              DEBUG
              "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n"
          , GOTUPDATES
          , LOGMSG DEBUG $ "Get response: " ++ show json6 ++ "\n"
          , LOGMSG INFO "There is new updates list\n"
          , LOGMSG
              DEBUG
              "Send request to confirmOldUpdates with offset:235802 https://api.telegram.org/botABC123/getUpdates\n"
          , CONFIRMUPDATES 235802
          , LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n"
          , LOGMSG INFO "Received updates confirmed\n"
          , LOGMSG INFO "Analysis update from the list\n"
          , LOGMSG INFO "Get msg_id: 2112 from user 1118\n"
          , LOGMSG INFO "Msg_id:2112 is text: \"love\"\n"
          ] ++
          (concat . replicate 2 $
           [ LOGMSG
               DEBUG
               "Send request to send msg \"love\" to userId 1118: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118, text = \"love\"}\n"
           , SENDMSG 1118 "love"
           , LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n"
           , LOGMSG INFO "Msg \"love\" was sent to user 1118\n"
           ])
      it "work with text msg if user is in OpenRepeatMode " $ do
        state <- execStateT (evalStateT (run handle13) initialDB2) []
        reverse state `shouldBe`
          [GOTUPDATES,CONFIRMUPDATES 235802,LOG WARNING,
          SENDMSG 1118 "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still 2\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"]
      it "work with singleton update list with /help msg" $ do
        dbState <- evalStateT (execStateT (run handle9) initialDB1) []
        dbState `shouldBe` initialDB1
        state <- execStateT (evalStateT (run handle9) initialDB1) []
        reverse state `shouldBe`
          [ LOGMSG
              DEBUG
              "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n"
          , GOTUPDATES
          , LOGMSG DEBUG $ "Get response: " ++ show json7 ++ "\n"
          , LOGMSG INFO "There is new updates list\n"
          , LOGMSG
              DEBUG
              "Send request to confirmOldUpdates with offset:235802 https://api.telegram.org/botABC123/getUpdates\n"
          , CONFIRMUPDATES 235802
          , LOGMSG
              DEBUG
              "Get response: \"{\\\"ok\\\":true,\\\"result\\\":[]}\"\n"
          , LOGMSG INFO "Received updates confirmed\n"
          , LOGMSG INFO "Analysis update from the list\n"
          , LOGMSG INFO "Get msg_id: 2113 from user 1118\n"
          , LOGMSG INFO "Msg_id:2113 is text: \"/help\"\n"
          , LOGMSG
              DEBUG
              "Send request to send msg \"Hello\" to userId 1118: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118, text = \"Hello\"}\n"
          , SENDMSG 1118 "Hello"
          , LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n"
          , LOGMSG INFO "Msg \"Hello\" was sent to user 1118\n"
          ]
      it "work with singleton update list with /repeat msg" $ do
        dbState <- evalStateT (execStateT (run handle10) initialDB1) []
        dbState `shouldBe` Map.fromList [(1118, Left (OpenRepeat 2))]
        state <- execStateT (evalStateT (run handle10) initialDB1) []
        reverse state `shouldBe`
          [ LOG DEBUG
          , GOTUPDATES
          , LOG DEBUG
          , LOG INFO
          , LOG DEBUG
          , CONFIRMUPDATES 235801
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG DEBUG
          , SENDKEYB
              1118
              2
              " : Current number of repeats your message.\nWhy?"
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          ]
      it "work with sticker update" $ do
        state <- execStateT (evalStateT (run handle11) initialDB1) []
        reverse state `shouldBe`
          [ LOG DEBUG
          , GOTUPDATES
          , LOG DEBUG
          , LOG INFO
          , LOG DEBUG
          , CONFIRMUPDATES 235808
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG DEBUG
          , COPYMSG 12677 2140
          , LOG DEBUG
          , LOG INFO
          , LOG DEBUG
          , COPYMSG 12677 2140
          , LOG DEBUG
          , LOG INFO
          ]
      it "work with msg:4 after /repeat" $ do
        dbState <-
          evalStateT (execStateT (run handle10 >> run handle12) initialDB1) []
        dbState `shouldBe` Map.fromList [(1118, Right 4)]
        state <-
          execStateT (evalStateT (run handle10 >> run handle12) initialDB1) []
        reverse state `shouldBe`
          [ LOG DEBUG
          , GOTUPDATES
          , LOG DEBUG
          , LOG INFO
          , LOG DEBUG
          , CONFIRMUPDATES 235801
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG DEBUG
          , SENDKEYB
              1118
              2
              " : Current number of repeats your message.\nWhy?"
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          , LOG DEBUG
          , GOTUPDATES
          , LOG DEBUG
          , LOG INFO
          , LOG DEBUG
          , CONFIRMUPDATES 235802
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG DEBUG
          , SENDMSG
              1118
              "Number of repeats successfully changed from 2 to 4\n"
          , LOG DEBUG
          , LOG INFO
          ]
      it "warning with msg:love after /repeat" $ do
        dbState <-
          evalStateT (execStateT (run handle10 >> run handle1) initialDB1) []
        dbState `shouldBe` Map.fromList [(1118, Right 2)]
        state <-
          execStateT (evalStateT (run handle10 >> run handle1) initialDB1) []
        reverse state `shouldBe`
          [ LOG DEBUG
          , GOTUPDATES
          , LOG DEBUG
          , LOG INFO
          , LOG DEBUG
          , CONFIRMUPDATES 235801
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG DEBUG
          , SENDKEYB
              1118
              2
              " : Current number of repeats your message.\nWhy?"
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          , LOG DEBUG
          , GOTUPDATES
          , LOG DEBUG
          , LOG INFO
          , LOG DEBUG
          , CONFIRMUPDATES 235802
          , LOG DEBUG
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG INFO
          , LOG WARNING
          , LOG DEBUG
          , SENDMSG
              1118
              "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still 2\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
          , LOG DEBUG
          , LOG INFO
          ]
      it "ignore unknown update (edited_message) " $ do
        state <- execStateT (evalStateT (run handle23) initialDB1) []
        reverse state `shouldBe`
          [GOTUPDATES,CONFIRMUPDATES 235801,LOG WARNING]
      it "ignore unknown update " $ do
        state <- execStateT (evalStateT (run handle24) initialDB1) []
        reverse state `shouldBe`
          [GOTUPDATES,CONFIRMUPDATES 235801,LOG WARNING]
      it "ignore unknown empty update " $ do
        state <- execStateT (evalStateT (run handle25) initialDB1) []
        reverse state `shouldBe`
          [GOTUPDATES,CONFIRMUPDATES 235801,LOG WARNING]
      it "ignore unknown update in not single update list" $ do
        state <- execStateT (evalStateT (run handle26) initialDB1) []
        reverse state `shouldBe`
          [GOTUPDATES,CONFIRMUPDATES 235802,LOG WARNING,SENDMSG 1118 "love",SENDMSG 1118 "love"]
      it "work with several users " $ do
        state <- execStateT (evalStateT (run handle22) initialDB1) []
        reverse state `shouldBe`
          [GOTUPDATES,CONFIRMUPDATES 235804,LOG WARNING,COPYMSG 1118 2110,COPYMSG 1118 2110,SENDMSG 12677 "Toni",SENDMSG 12677 "Toni",COPYMSG 12677 2112,COPYMSG 12677 2112]
      it "throw Exception on negative getUpdates response" $
        evalStateT (evalStateT (run handle6) initialDB1) [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $
             "NEGATIVE RESPONSE:\n" ++ show json2))
      it "throw Exception on unknown getUpdates response" $
        evalStateT (evalStateT (run handle7) initialDB1) [] `shouldThrow`
        (== (CheckGetUpdatesResponseException $
             "UNKNOWN RESPONSE:\n" ++ show json3))
      it "throw Exception on other negative getUpdates response" $
        evalStateT (evalStateT (run handle14) initialDB1) [] `shouldThrow`
        isCheckGetUpdatesResponseException
      it "work with other ok confirmUpdates response" $ do
        dbState <- evalStateT (execStateT (run handle9) initialDB1) []
        dbState `shouldBe` initialDB1
        state <- execStateT (evalStateT (run handle17) initialDB1) []
        reverse state `shouldBe`
          [GOTUPDATES,CONFIRMUPDATES 235802,SENDMSG 1118 "love",SENDMSG 1118 "love"]
      it "throw Exception on negative confirmUpdates response" $
        evalStateT (evalStateT (run handle16) initialDB1) [] `shouldThrow`
        isCheckConfirmUpdatesResponseException
      it "throw Exception on other negative confirmUpdates response" $
        evalStateT (evalStateT (run handle20) initialDB1) [] `shouldThrow`
        isCheckConfirmUpdatesResponseException
      it "throw Exception on unknown confirmUpdates response" $
        evalStateT (evalStateT (run handle18) initialDB1) [] `shouldThrow`
        isCheckConfirmUpdatesResponseException
      it "throw Exception on confirmUpdates where getUpdates response with negative updateId" $
        evalStateT (evalStateT (run handle19) initialDB1) [] `shouldThrow`
        isConfirmUpdatesException
      
            


