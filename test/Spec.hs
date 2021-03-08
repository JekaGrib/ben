{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           App
import           Logger
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State

data MockAction = GOTUPDATES | SENDMSG Int T.Text | COPYMSG Int Int | CONFIRMUPDATES Int | SENDKEYB Int Int T.Text | LOGMSG Priority String
                                       deriving (Eq,Show)


getUpdatesTest:: LBS.ByteString -> StateT [MockAction] IO LBS.ByteString
getUpdatesTest json = StateT $ \s -> return ( json , GOTUPDATES : s)

confirmUpdatesTest :: LBS.ByteString -> Int -> StateT [MockAction] IO LBS.ByteString
confirmUpdatesTest json offset = StateT $ \s -> 
    return ( json , (CONFIRMUPDATES offset) : s)

sendMsgTest :: LBS.ByteString -> Int -> T.Text -> StateT [MockAction] IO LBS.ByteString
sendMsgTest json usId msg = StateT $ \s -> 
    return ( json , (SENDMSG usId msg) : s)

copyMsgTest :: LBS.ByteString -> Int -> Int -> StateT [MockAction] IO LBS.ByteString
copyMsgTest json usId msgId = StateT $ \s -> 
    return ( json , (COPYMSG usId msgId) : s)

sendKeybTest :: LBS.ByteString -> Int -> Int -> T.Text-> StateT [MockAction] IO LBS.ByteString
sendKeybTest json usId currN msg = StateT $ \s -> 
    return ( json , (SENDKEYB usId currN msg) : s)

logTest :: Priority -> String -> StateT [MockAction] IO ()
logTest prio text = StateT $ \s -> 
    return (() , LOGMSG prio text : s)


config1 = Config { cStartN = 2 , cBotToken = "ABC123" , cHelpMsg = "Hello" , cRepeatQ = "Why?"}
handleLog1 = LogHandle (LogConfig DEBUG) logTest
handle1 = Handle { hConf = config1,
                   hLog = handleLog1,
                   getUpdates = getUpdatesTest json6,
                   getShortUpdates = getUpdatesTest json1,
                   confirmUpdates = confirmUpdatesTest json1,
                   sendMsg = sendMsgTest json4,
                   sendKeyb = sendKeybTest json4,
                   copyMsg = copyMsgTest json11}

handle2  = handle1 { getShortUpdates = getUpdatesTest json2}
handle3  = handle1 { getShortUpdates = getUpdatesTest json3}
handle4  = handle1 { getShortUpdates = getUpdatesTest json4}
handle5  = handle1 { getShortUpdates = getUpdatesTest json5}
handle6  = handle1 { getUpdates = getUpdatesTest json2}
handle7  = handle1 { getUpdates = getUpdatesTest json3}
handle8  = handle1 { getUpdates = getUpdatesTest json4}
handle9  = handle1 { getUpdates = getUpdatesTest json7}
handle10 = handle1 { getUpdates = getUpdatesTest json8}
handle11 = handle1 { getUpdates = getUpdatesTest json9}
handle12 = handle1 { getUpdates = getUpdatesTest json10}

initialDB1 = []

main :: IO ()
main = hspec $ do
  describe "startApp" $ do
    it "return [LOGMSG INFO, LOGMSG DEBUG, GOTUPDATES , LOGMSG INFO] when given empty update list" $ do
      state <- execStateT (startApp handle1) []
      reverse state `shouldBe` 
        [LOGMSG INFO "App started\n",
        LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n",
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "No new updates\n"]

    it "return [LOGMSG DEBUG, GOTUPDATES, CONFIRMUPDATES, LOGMSG DEBUG] when given unempty update list" $ do
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
    
    it "throw Exception on negative getUpdates response" $ do
      evalStateT (startApp handle2) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show json2))

    it "throw Exception on unknown getUpdates response" $ do
      evalStateT (startApp handle3) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show json3))
    
    it "throw Exception on short getUpdates response" $ do
      evalStateT (startApp handle4) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "Too short response:\n" ++ show json4))

  describe "run" $ do
    it "work with singleton update list with text msg" $ do
      state <- execStateT (evalStateT (run handle1 ) initialDB1 ) []
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
          [LOGMSG DEBUG $ "Send request to send msg \"love\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118947329, text = \"love\"}\n",
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
        LOGMSG DEBUG $ "Send request to send msg \"Hello\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118947329, text = \"Hello\"}\n",
        SENDMSG 1118947329 "Hello",
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n",
        LOGMSG INFO "Msg \"Hello\" was sent to user 1118947329\n"]
    
    it "work with singleton update list with /repeat msg" $ do
      dbState <- evalStateT (execStateT (run handle10 ) initialDB1 ) []
      dbState `shouldBe` [(1118947329,Left (OpenRepeat 2))]
      state <- execStateT (evalStateT (run handle10 ) initialDB1 ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json8 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800276 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800276,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "Received updates confirmed\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO "Get msg_id: 2114 from user 1118947329\n",
        LOGMSG INFO "Msg_id:2114 is text: \"/repeat\"\n",
        LOGMSG DEBUG "Send request to send keyboard with message: 2\" : Current number of repeats your message.\\nWhy?\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage\n",
        SENDKEYB 1118947329 2 " : Current number of repeats your message.\nWhy?",
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n",
        LOGMSG INFO "Keyboard with message: 2\" : Current number of repeats your message.\\nWhy?\" was sent to user 1118947329\n",
        LOGMSG INFO "Put user 1118947329 to OpenRepeat mode\n"]
      
      
    
    it "work with sticker update" $ do
      state <- execStateT (evalStateT (run handle11 ) initialDB1 ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json9 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800287 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800287,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "Received updates confirmed\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO "Get msg_id: 2140 from user 1267750993\n",
        LOGMSG INFO "Msg_id:2140 is attachment\n",
        LOGMSG DEBUG "Send request to send attachment msg_id: 2140 to userId 1267750993: https://api.telegram.org/botABC123/copyMessage   JSON body : {chat_id = 1267750993,from_chat_id = 1267750993, message_id = 2140}\n",
        COPYMSG 1267750993 2140,
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true,\\\"result\\\":{\\\"message_id\\\":2141}}\"\n",
        LOGMSG INFO "Attachment msg_id: 2140 was sent to user 1267750993\n",
        LOGMSG DEBUG "Send request to send attachment msg_id: 2140 to userId 1267750993: https://api.telegram.org/botABC123/copyMessage   JSON body : {chat_id = 1267750993,from_chat_id = 1267750993, message_id = 2140}\n",
        COPYMSG 1267750993 2140,
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true,\\\"result\\\":{\\\"message_id\\\":2141}}\"\n",
        LOGMSG INFO "Attachment msg_id: 2140 was sent to user 1267750993\n"]

    it "work with msg:4 after /repeat" $ do
      dbState <- evalStateT (execStateT (run handle10 >> run handle12) initialDB1 ) []
      dbState `shouldBe` [(1118947329,Right 4)]
      state <- execStateT (evalStateT (run handle10 >> run handle12) initialDB1 ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json8 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800276 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800276,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "Received updates confirmed\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO "Get msg_id: 2114 from user 1118947329\n",
        LOGMSG INFO "Msg_id:2114 is text: \"/repeat\"\n",
        LOGMSG DEBUG "Send request to send keyboard with message: 2\" : Current number of repeats your message.\\nWhy?\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage\n",
        SENDKEYB 1118947329 2 " : Current number of repeats your message.\nWhy?",
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n",
        LOGMSG INFO "Keyboard with message: 2\" : Current number of repeats your message.\\nWhy?\" was sent to user 1118947329\n",
        LOGMSG INFO "Put user 1118947329 to OpenRepeat mode\n",
        LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json10 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800277 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800277,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "Received updates confirmed\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO "Get msg_id: 2117 from user 1118947329\n",
        LOGMSG INFO "User 1118947329 is in OpenRepeat mode\n",
        LOGMSG INFO "Change number of repeats to 4 for user 1118947329\n",
        LOGMSG DEBUG $ "Send request to send msg \"Number of repeats successfully changed from 2 to 4\\n\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118947329, text = \"Number of repeats successfully changed from 2 to 4\\n\"}\n",
        SENDMSG 1118947329 $ "Number of repeats successfully changed from 2 to 4\n",
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n",
        LOGMSG INFO "Msg \"Number of repeats successfully changed from 2 to 4\\n\" was sent to user 1118947329\n"
        ]
    
    it "warning with msg:love after /repeat" $ do
      dbState <- evalStateT (execStateT (run handle10 >> run handle1) initialDB1 ) []
      dbState `shouldBe` [(1118947329,Right 2)]
      state <- execStateT (evalStateT (run handle10 >> run handle1) initialDB1 ) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json8 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800276 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800276,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "Received updates confirmed\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO "Get msg_id: 2114 from user 1118947329\n",
        LOGMSG INFO "Msg_id:2114 is text: \"/repeat\"\n",
        LOGMSG DEBUG "Send request to send keyboard with message: 2\" : Current number of repeats your message.\\nWhy?\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage\n",
        SENDKEYB 1118947329 2 " : Current number of repeats your message.\nWhy?",
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n",
        LOGMSG INFO "Keyboard with message: 2\" : Current number of repeats your message.\\nWhy?\" was sent to user 1118947329\n",
        LOGMSG INFO "Put user 1118947329 to OpenRepeat mode\n",
        LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json6 ++ "\n",
        LOGMSG INFO "There is new updates list\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates with offset:235800277 https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800277,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG INFO "Received updates confirmed\n",
        LOGMSG INFO "Analysis update from the list\n",
        LOGMSG INFO "Get msg_id: 2112 from user 1118947329\n",
        LOGMSG INFO "User 1118947329 is in OpenRepeat mode\n",
        LOGMSG WARNING $ "User 1118947329 press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: 2\n",
        LOGMSG DEBUG $ "Send request to send msg \"UNKNOWN NUMBER\\nI,m ssory, number of repeats has not changed, it is still 2\\nTo change it you may sent me command \\\"/repeat\\\" and then choose number from 1 to 5 on keyboard\\nPlease, try again later\\n\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118947329, text = \"UNKNOWN NUMBER\\nI,m ssory, number of repeats has not changed, it is still 2\\nTo change it you may sent me command \\\"/repeat\\\" and then choose number from 1 to 5 on keyboard\\nPlease, try again later\\n\"}\n",
        SENDMSG 1118947329 $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still 2\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n",
        LOGMSG DEBUG "Get response: \"{\\\"ok\\\":true}\"\n",
        LOGMSG INFO "Msg \"UNKNOWN NUMBER\\nI,m ssory, number of repeats has not changed, it is still 2\\nTo change it you may sent me command \\\"/repeat\\\" and then choose number from 1 to 5 on keyboard\\nPlease, try again later\\n\" was sent to user 1118947329\n"]

    it "throw Exception on negative getUpdates response" $ do
      evalStateT (evalStateT (run handle6) initialDB1 ) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show json2))
  
    it "throw Exception on unknown getUpdates response" $ do
      evalStateT (evalStateT (run handle7) initialDB1 ) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show json3))
    
    it "throw Exception on short getUpdates response" $ do
      evalStateT (evalStateT (run handle8) initialDB1 ) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "Too short response:\n" ++ show json4))
      

json1  :: LBS.ByteString
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
