{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           App
import           Logger
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State

data MockAction = GOTUPDATES | SENDMSG Int T.Text | CONFIRMUPDATES Int | SENDKEYB Int Int T.Text | LOGMSG Priority String
                                       deriving (Eq,Show)


getUpdatesTest:: LBS.ByteString -> StateT [MockAction] IO LBS.ByteString
getUpdatesTest json = StateT $ \s -> return ( json , GOTUPDATES : s)

confirmUpdatesTest :: LBS.ByteString -> Int -> StateT [MockAction] IO LBS.ByteString
confirmUpdatesTest json offset = StateT $ \s -> 
    return ( json , (CONFIRMUPDATES offset) : s)

sendMsgTest :: LBS.ByteString -> Int -> T.Text -> StateT [MockAction] IO LBS.ByteString
sendMsgTest json usId msg = StateT $ \s -> 
    return ( json , (SENDMSG usId msg) : s)

sendKeybTest :: LBS.ByteString -> Int -> Int -> T.Text-> StateT [MockAction] IO LBS.ByteString
sendKeybTest json usId currN msg = StateT $ \s -> 
    return ( json4 , (SENDKEYB usId currN msg) : s)

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
                   sendKeyb = sendKeybTest json4}

handle2 = handle1 { getShortUpdates = getUpdatesTest json2}
handle3 = handle1 { getShortUpdates = getUpdatesTest json3}
handle4 = handle1 { getShortUpdates = getUpdatesTest json4}
handle5 = handle1 { getShortUpdates = getUpdatesTest json5}
handle6 = handle1 { getUpdates = getUpdatesTest json2}
handle7 = handle1 { getUpdates = getUpdatesTest json3}
handle8 = handle1 { getUpdates = getUpdatesTest json4}

main :: IO ()
main = hspec $ do
  describe "startApp" $ do
    it "return [LOGMSG DEBUG, GOTUPDATES] when given empty update list" $ do
      state <- execStateT (startApp handle1) []
      reverse state `shouldBe` [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", GOTUPDATES]

    it "return [LOGMSG DEBUG, GOTUPDATES, CONFIRMUPDATES, LOGMSG DEBUG] when given unempty update list" $ do
      state <- execStateT (startApp handle5) []
      reverse state `shouldBe` 
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        CONFIRMUPDATES 235800274,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n"]
    
    it "throw Exception on negative getUpdates response" $ do
      evalStateT (startApp handle2) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "Error at StartApp. NEGATIVE RESPONSE:\n" ++ show json2))

    it "throw Exception on unknown getUpdates response" $ do
      evalStateT (startApp handle3) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "Error at StartApp. UNKNOWN RESPONSE:\n" ++ show json3))
    
    it "throw Exception on short getUpdates response" $ do
      evalStateT (startApp handle4) [] `shouldThrow` ( == (CheckGetUpdatesResponseException $ "Error at StartApp. Too short response:\n" ++ show json4))

  describe "run" $ do
    it "work with singleton update list with text msg" $ do
      state <- execStateT (evalStateT (run handle1 ) []) []
      reverse state `shouldBe`
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        LOGMSG DEBUG $ "Get response: " ++ show json6 ++ "\n",
        LOGMSG DEBUG "Send request to confirmOldUpdates: https://api.telegram.org/botABC123/getUpdates\n",
        CONFIRMUPDATES 235800277,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n",
        LOGMSG DEBUG $ "Update info: got msg \"love\" from user 1118947329\n"] ++
        (concat . replicate 2 $ 
          [LOGMSG DEBUG $ "Send request to send msg \"love\" to userId 1118947329: https://api.telegram.org/botABC123/sendMessage   JSON body : {chat_id = 1118947329, text = \"love\"}\n",
          SENDMSG 1118947329 "love"])
    
    it "throw Exception on negative getUpdates response" $ do
      evalStateT (evalStateT (run handle6) []) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show json2))
  
    it "throw Exception on unknown getUpdates response" $ do
      evalStateT (evalStateT (run handle7) []) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show json3))
    
    it "throw Exception on short getUpdates response" $ do
      evalStateT (evalStateT (run handle8) []) [] 
        `shouldThrow` ( == (CheckGetUpdatesResponseException $ "Too short response:\n" ++ show json4))
    
      


json1 :: LBS.ByteString
json1 = "{\"ok\":true,\"result\":[]}"
json2 = "{\"ok\":false,\"result\":235}"
json3 = "lalala"
json4 = "{\"ok\":true}"
json5 = "{\"ok\":true,\"result\":[{\"update_id\":235800270,\n\"edited_message\":{\"message_id\":2109,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157544,\"edit_date\":1594157550,\"text\":\"sev\"}},{\"update_id\":235800271,\n\"message\":{\"message_id\":2110,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157558,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\udc4b\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgg-XwTp9qVkXckjuJdFWs8YfRcnlKIAAgUAA8A2TxP5al-agmtNdZc4uA8ABAEAB20AA7V6AAIaBA\",\"file_unique_id\":\"AQADlzi4DwAEtXoAAg\",\"file_size\":3848,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIPl8E6falZF3JI7iXRVrPGH0XJ5SiAAIFAAPANk8T-WpfmoJrTXUaBA\",\"file_unique_id\":\"AgADBQADwDZPEw\",\"file_size\":7285}}},{\"update_id\":235800272,\n\"message\":{\"message_id\":2111,\"from\":{\"id\":1267750993,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":1267750993,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157567,\"text\":\"Toni\"}},{\"update_id\":235800273,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":1267750993,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":1267750993,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157574,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\ude18\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAghAXwTqBtXCprOmMNPhHaxRKoqSqVoAAgIAA8A2TxMI9W5F-oSnWRQsuA8ABAEAB20AAyZfAAIaBA\",\"file_unique_id\":\"AQADFCy4DwAEJl8AAg\",\"file_size\":4498,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIQF8E6gbVwqazpjDT4R2sUSqKkqlaAAICAAPANk8TCPVuRfqEp1kaBA\",\"file_unique_id\":\"AgADAgADwDZPEw\",\"file_size\":15955}}}]}"
json6 = "{\"ok\":true,\"result\":[{\"update_id\":235800276,\n\"message\":{\"message_id\":2117,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"love\"}}]}"




