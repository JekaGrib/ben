--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module TestTg where

import           Test.Hspec (it,shouldThrow,shouldBe,describe,hspec)
import           Tg.App (Handle(..),run,startApp)
import           Tg.Logger (LogHandle(..),Priority(..),LogConfig(..))
import           Tg.Oops (TGBotException(..))
import           Tg.Conf (Config(..))
import           Tg.Types
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State (StateT(..),evalStateT,execStateT)
import qualified Data.Map as Map

data MockAction = GOTUPDATES | SENDMSG UserId TextOfMsg | COPYMSG UserId MessageId | CONFIRMUPDATES Offset | SENDKEYB UserId N TextOfKeyb | LOG Priority | LOGMSG Priority String
                                       deriving (Eq,Show)


getUpdatesTest:: Response -> StateT [MockAction] IO Response
getUpdatesTest json = StateT $ \s -> return ( json , GOTUPDATES : s)

confirmUpdatesTest :: Response -> Offset -> StateT [MockAction] IO Response
confirmUpdatesTest json offset = StateT $ \s -> 
    return ( json , CONFIRMUPDATES offset : s)

sendMsgTest :: Response -> UserId -> TextOfMsg -> StateT [MockAction] IO Response
sendMsgTest json usId msg = StateT $ \s -> 
    return ( json , SENDMSG usId msg : s)

copyMsgTest :: Response -> UserId -> MessageId -> StateT [MockAction] IO Response
copyMsgTest json usId msgId = StateT $ \s -> 
    return ( json , COPYMSG usId msgId : s)

sendKeybTest :: Response -> UserId -> N -> TextOfMsg -> StateT [MockAction] IO Response
sendKeybTest json usId currN msg = StateT $ \s -> 
    return ( json , SENDKEYB usId currN msg : s)

logTest :: Priority -> String -> StateT [MockAction] IO ()
logTest prio _ = StateT $ \s -> 
    return (() , LOG prio : s)

logTest0 :: Priority -> String -> StateT [MockAction] IO ()
logTest0 prio str = StateT $ \s -> 
    return (() , LOGMSG prio str : s)

config1 = Config { cStartN = 2 , cBotToken = "ABC123" , cHelpMsg = "Hello" , cRepeatQ = "Why?", cPriority = DEBUG}
handleLog1 = LogHandle (LogConfig DEBUG) logTest
handleLog0 = LogHandle (LogConfig DEBUG) logTest0
handle1 = Handle { hConf = config1,
                   hLog = handleLog1,
                   getUpdates = getUpdatesTest json6,
                   getShortUpdates = getUpdatesTest json1,
                   confirmUpdates = confirmUpdatesTest json1,
                   sendMsg = sendMsgTest json4,
                   sendKeyb = sendKeybTest json4,
                   copyMsg = copyMsgTest json11}

handle0  = handle1 { hLog = handleLog0 }
handle2  = handle1 { getShortUpdates = getUpdatesTest json2}
handle3  = handle1 { getShortUpdates = getUpdatesTest json3}
handle4  = handle1 { getShortUpdates = getUpdatesTest json4}
handle5  = handle1 { getShortUpdates = getUpdatesTest json5,hLog = handleLog0}
handle6  = handle1 { getUpdates = getUpdatesTest json2}
handle7  = handle1 { getUpdates = getUpdatesTest json3}
handle8  = handle1 { getUpdates = getUpdatesTest json4}
handle9  = handle1 { getUpdates = getUpdatesTest json7,hLog = handleLog0}
handle10 = handle1 { getUpdates = getUpdatesTest json8}
handle11 = handle1 { getUpdates = getUpdatesTest json9}
handle12 = handle1 { getUpdates = getUpdatesTest json10}

initialDB1 = Map.fromList []

testTG :: IO ()
testTG = hspec $ do
  describe "startApp" $ do
    it "return [LOG INFO, LOG DEBUG, GOTUPDATES , LOG INFO] when given empty update list" $ do
      state <- execStateT (startApp handle0) []
      reverse state `shouldBe` 
        [LOGMSG INFO "App started\n",
        LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n",
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "No new updates\n"]

    it "return [LOG DEBUG, GOTUPDATES, CONFIRMUPDATES, LOG DEBUG] when given unempty update list" $ do
      state <- execStateT (startApp handle5) []
      reverse state `shouldBe` 
        [LOGMSG INFO "App started\n",
        LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json5 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800274 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800274,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "Received updates confirmed\n"]
    
    it "throw Exception on negative getUpdates response" $ 
      evalStateT (startApp handle2) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show json2))

    it "throw Exception on unknown getUpdates response" $ 
      evalStateT (startApp handle3) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show json3))
    
    it "throw Exception on short getUpdates response" $ 
      evalStateT (startApp handle4) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "Too short response:\n" ++ show json4))

  describe "run" $ do
    it "work with singleton update list with text msg" $ do
      state <- execStateT (evalStateT (run handle0 ) initialDB1 ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json6 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800277 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800277,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "Received updates confirmed\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO "Get msg_id: 2112 from user 1118947329\n",
        LOGMSG INFO "Msg_id:2112 is text: \"love\"\n"] ++
        (concat . replicate 2 $ 
          [LOGMSG DEBUG "Send request to send msg \"love\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118947329, text = \"love\"}\n",
          SENDMSG 1118947329 "love",
          LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n",
          LOGMSG INFO "Msg \"love\" was sent to user 1118947329\n"])
    
    it "work with singleton update list with /help msg" $ do
      dbState <- evalStateT (execStateT (run handle9 ) initialDB1 ) []
      dbState `shouldBe` initialDB1
      state <- execStateT (evalStateT (run handle9 ) initialDB1 ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json7 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800277 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800277,
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true,\\\"result\\\":[]}\"\n",
        LOGMSG INFO "Received updates confirmed\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO "Get msg_id: 2113 from user 1118947329\n",
        LOGMSG INFO "Msg_id:2113 is text: \"/help\"\n",
        LOGMSG DEBUG "Send request to send msg \"Hello\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118947329, text = \"Hello\"}\n",
        SENDMSG 1118947329 "Hello",
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n",
        LOGMSG INFO "Msg \"Hello\" was sent to user 1118947329\n"]
    
    it "work with singleton update list with /repeat msg" $ do
      dbState <- evalStateT (execStateT (run handle10 ) initialDB1 ) []
      dbState `shouldBe` Map.fromList [(1118947329,Left (OpenRepeat 2))]
      state <- execStateT (evalStateT (run handle10 ) initialDB1 ) []
      reverse state `shouldBe`
        [LOG DEBUG , 
        GOTUPDATES,
        LOG DEBUG ,
        LOG INFO ,
        LOG DEBUG ,
        CONFIRMUPDATES 235800276,
        LOG DEBUG ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG DEBUG ,
        SENDKEYB 1118947329 2 " : Current number of repeats your message.\nWhy?",
        LOG DEBUG ,
        LOG INFO ,
        LOG INFO ]
      
      
    
    it "work with sticker update" $ do
      state <- execStateT (evalStateT (run handle11 ) initialDB1 ) []
      reverse state `shouldBe`
        [LOG DEBUG , 
        GOTUPDATES,
        LOG DEBUG ,
        LOG INFO ,
        LOG DEBUG ,
        CONFIRMUPDATES 235800287,
        LOG DEBUG ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG DEBUG ,
        COPYMSG 1267750993 2140,
        LOG DEBUG ,
        LOG INFO ,
        LOG DEBUG ,
        COPYMSG 1267750993 2140,
        LOG DEBUG ,
        LOG INFO ]

    it "work with msg:4 after /repeat" $ do
      dbState <- evalStateT (execStateT (run handle10 >> run handle12) initialDB1 ) []
      dbState `shouldBe` Map.fromList [(1118947329,Right 4)]
      state <- execStateT (evalStateT (run handle10 >> run handle12) initialDB1 ) []
      reverse state `shouldBe`
        [LOG DEBUG , 
        GOTUPDATES,
        LOG DEBUG ,
        LOG INFO ,
        LOG DEBUG ,
        CONFIRMUPDATES 235800276,
        LOG DEBUG,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG DEBUG ,
        SENDKEYB 1118947329 2 " : Current number of repeats your message.\nWhy?",
        LOG DEBUG ,
        LOG INFO ,
        LOG INFO ,
        LOG DEBUG , 
        GOTUPDATES,
        LOG DEBUG ,
        LOG INFO ,
        LOG DEBUG ,
        CONFIRMUPDATES 235800277,
        LOG DEBUG ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG DEBUG ,
        SENDMSG 1118947329 "Number of repeats successfully changed from 2 to 4\n",
        LOG DEBUG ,
        LOG INFO 
        ]
    
    it "warning with msg:love after /repeat" $ do
      dbState <- evalStateT (execStateT (run handle10 >> run handle1) initialDB1 ) []
      dbState `shouldBe` Map.fromList [(1118947329,Right 2)]
      state <- execStateT (evalStateT (run handle10 >> run handle1) initialDB1 ) []
      reverse state `shouldBe`
        [LOG DEBUG , 
        GOTUPDATES,
        LOG DEBUG ,
        LOG INFO ,
        LOG DEBUG ,
        CONFIRMUPDATES 235800276,
        LOG DEBUG ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG DEBUG ,
        SENDKEYB 1118947329 2 " : Current number of repeats your message.\nWhy?",
        LOG DEBUG ,
        LOG INFO ,
        LOG INFO ,
        LOG DEBUG , 
        GOTUPDATES,
        LOG DEBUG ,
        LOG INFO ,
        LOG DEBUG ,
        CONFIRMUPDATES 235800277,
        LOG DEBUG ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG INFO ,
        LOG WARNING ,
        LOG DEBUG ,
        SENDMSG 1118947329 "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still 2\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n",
        LOG DEBUG ,
        LOG INFO]

    it "throw Exception on negative getUpdates response" $ 
      evalStateT (evalStateT (run handle6) initialDB1 ) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show json2))
  
    it "throw Exception on unknown getUpdates response" $ 
      evalStateT (evalStateT (run handle7) initialDB1 ) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show json3))
    
    it "throw Exception on short getUpdates response" $ 
      evalStateT (evalStateT (run handle8) initialDB1 ) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "Too short response:\n" ++ show json4))
      

json1  :: Response
json1  = "{\"ok\":true,\"result\":[]}"
json2  = "{\"ok\":false,\"result\":235}"
json3  = "lalala"
json4  = "{\"ok\":true}"
json5  = "{\"ok\":true,\"result\":[{\"update_id\":235800270,\n\"edited_message\":{\"message_id\":2109,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157544,\"edit_date\":1594157550,\"text\":\"sev\"}},{\"update_id\":235800271,\n\"message\":{\"message_id\":2110,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157558,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\udc4b\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgg-XwTp9qVkXckjuJdFWs8YfRcnlKIAAgUAA8A2TxP5al-agmtNdZc4uA8ABAEAB20AA7V6AAIaBA\",\"file_unique_id\":\"AQADlzi4DwAEtXoAAg\",\"file_size\":3848,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIPl8E6falZF3JI7iXRVrPGH0XJ5SiAAIFAAPANk8T-WpfmoJrTXUaBA\",\"file_unique_id\":\"AgADBQADwDZPEw\",\"file_size\":7285}}},{\"update_id\":235800272,\n\"message\":{\"message_id\":2111,\"from\":{\"id\":1267750993,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":1267750993,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157567,\"text\":\"Toni\"}},{\"update_id\":235800273,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":1267750993,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":1267750993,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157574,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\ude18\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAghAXwTqBtXCprOmMNPhHaxRKoqSqVoAAgIAA8A2TxMI9W5F-oSnWRQsuA8ABAEAB20AAyZfAAIaBA\",\"file_unique_id\":\"AQADFCy4DwAEJl8AAg\",\"file_size\":4498,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIQF8E6gbVwqazpjDT4R2sUSqKkqlaAAICAAPANk8TCPVuRfqEp1kaBA\",\"file_unique_id\":\"AgADAgADwDZPEw\",\"file_size\":15955}}}]}"
json6  = "{\"ok\":true,\"result\":[{\"update_id\":235800276,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"love\"}}]}"
json7  = "{\"ok\":true,\"result\":[{\"update_id\":235800276,\n\"message\":{\"message_id\":2113,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"/help\"}}]}"
json8  = "{\"ok\":true,\"result\":[{\"update_id\":235800275,\n\"message\":{\"message_id\":2114,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"/repeat\"}}]}"
json9  = "{\"ok\":true,\"result\":[{\"update_id\":235800286,\n\"message\":{\"message_id\":2140,\"from\":{\"id\":1267750993,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":1267750993,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594219382,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\udc4b\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAghcXwXbdpokxFdrCT29uzt0nEq3KzwAAgUAA8A2TxP5al-agmtNdZc4uA8ABAEAB20AA7V6AAIaBA\",\"file_unique_id\":\"AQADlzi4DwAEtXoAAg\",\"file_size\":3848,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIXF8F23aaJMRXawk9vbs7dJxKtys8AAIFAAPANk8T-WpfmoJrTXUaBA\",\"file_unique_id\":\"AgADBQADwDZPEw\",\"file_size\":7285}}}]}"
json10 = "{\"ok\":true,\"result\":[{\"update_id\":235800276,\n\"message\":{\"message_id\":2117,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"4\"}}]}"
json11 = "{\"ok\":true,\"result\":{\"message_id\":2141}}"
