{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec
import           App
import           Logger
import qualified Data.Text                      as T
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State

data MockAction = GOTUPDATES | SENDMESSAGE Int T.Text | CONFIRMUPDATES Int | SENDKEYBWITHMSG Int T.Text | LOGMSG Priority String
                                       deriving (Eq,Show)


getUpdatesTest1 :: StateT [MockAction] IO LBS.ByteString
getUpdatesTest1 = StateT $ \s -> return ("{\"name\":\"Joe\",\"age\":12}" , GOTUPDATES : s)

returnUpdates:: LBS.ByteString -> StateT [MockAction] IO LBS.ByteString
returnUpdates json = StateT $ \s -> return ( json , GOTUPDATES : s)

getShortUpdatesTest1 :: StateT [MockAction] IO LBS.ByteString
getShortUpdatesTest1 = StateT $ \s -> return ( json1 , GOTUPDATES : s)

getShortUpdatesTest2 :: StateT [MockAction] IO LBS.ByteString
getShortUpdatesTest2 = StateT $ \s -> return ( json2 , GOTUPDATES : s)

returnConfirmedUpdates :: LBS.ByteString -> Int -> StateT [MockAction] IO LBS.ByteString
returnConfirmedUpdates json offset = StateT $ \s -> 
    return ( json , (CONFIRMUPDATES offset) : s)

confirmUpdatesTest1 :: Int -> StateT [MockAction] IO LBS.ByteString
confirmUpdatesTest1 offset = StateT $ \s -> 
    return ("{\"ok\":true,\"result\":[]}" , (CONFIRMUPDATES offset) : s)

sendMessageTest1 :: Int -> T.Text -> StateT [MockAction] IO LBS.ByteString
sendMessageTest1 usId msg = StateT $ \s -> 
    return ("{\"name\":\"Joe\",\"age\":12}" , (SENDMESSAGE usId msg) : s)

sendKeybWithMsgTest1 :: Int -> Int -> T.Text-> StateT [MockAction] IO LBS.ByteString
sendKeybWithMsgTest1 usId currN msg = StateT $ \s -> 
    return ("{\"name\":\"Joe\",\"age\":12}" , (SENDKEYBWITHMSG usId msg) : s)

logTest1 :: Priority -> String -> StateT [MockAction] IO ()
logTest1 prio text = StateT $ \s -> 
    return (() , LOGMSG prio text : s)

configTest1 = Config { cStartN = 2 , cBotToken = "ABC123" , cHelpMsg = "Lala" , cRepeatQ = "Why?"}
handleLogTest1 = LogHandle (LogConfig DEBUG) logTest1
handleTest1 = Handle configTest1 handleLogTest1 getUpdatesTest1 getShortUpdatesTest1 confirmUpdatesTest1 sendMessageTest1 sendKeybWithMsgTest1

main :: IO ()
main = hspec $ do
  describe "startApp" $ do
    it "returns a [LOGMSG DEBUG, GOTUPDATES] when given empty update list" $ do
      state <- execStateT (startApp handleTest1) []
      reverse state `shouldBe` [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", GOTUPDATES]

    it "confirm updates when given unempty update list" $ do
      state <- execStateT (startApp handleTest1 {getShortUpdates = returnUpdates json5 , confirmUpdates = returnConfirmedUpdates json1 }) []
      reverse state `shouldBe` 
        [LOGMSG DEBUG "Send request to getUpdates: https://api.telegram.org/botABC123/getUpdates\n", 
        GOTUPDATES,
        CONFIRMUPDATES 235800274,
        LOGMSG DEBUG $ "Get response: " ++ show json1 ++ "\n"]
    
    it "throw Exception on negative response" $ do
      evalStateT (startApp handleTest1 { getShortUpdates = returnUpdates json2}) [] `shouldThrow` ( == (StartAppCheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show json2))

    it "throw Exception on unknown response" $ do
      evalStateT (startApp handleTest1 { getShortUpdates = returnUpdates json3}) [] `shouldThrow` ( == (StartAppCheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show json3))
    
    it "throw Exception on short ok response" $ do
      evalStateT (startApp handleTest1 { getShortUpdates = returnUpdates json4}) [] `shouldThrow` ( == (StartAppCheckGetUpdatesResponseException $ "Too short response:\n" ++ show json4))
      



json1 :: LBS.ByteString
json1 = "{\"ok\":true,\"result\":[]}"
json2 = "{\"ok\":false,\"result\":235}"
json3 = "lalala"
json4 = "{\"ok\":true}"
json5 = "{\"ok\":true,\"result\":[{\"update_id\":235800270,\n\"edited_message\":{\"message_id\":2109,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157544,\"edit_date\":1594157550,\"text\":\"sev\"}},{\"update_id\":235800271,\n\"message\":{\"message_id\":2110,\"from\":{\"id\":1118947329,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118947329,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157558,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\udc4b\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgg-XwTp9qVkXckjuJdFWs8YfRcnlKIAAgUAA8A2TxP5al-agmtNdZc4uA8ABAEAB20AA7V6AAIaBA\",\"file_unique_id\":\"AQADlzi4DwAEtXoAAg\",\"file_size\":3848,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIPl8E6falZF3JI7iXRVrPGH0XJ5SiAAIFAAPANk8T-WpfmoJrTXUaBA\",\"file_unique_id\":\"AgADBQADwDZPEw\",\"file_size\":7285}}},{\"update_id\":235800272,\n\"message\":{\"message_id\":2111,\"from\":{\"id\":1267750993,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":1267750993,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157567,\"text\":\"Toni\"}},{\"update_id\":235800273,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":1267750993,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":1267750993,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157574,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\ude18\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAghAXwTqBtXCprOmMNPhHaxRKoqSqVoAAgIAA8A2TxMI9W5F-oSnWRQsuA8ABAEAB20AAyZfAAIaBA\",\"file_unique_id\":\"AQADFCy4DwAEJl8AAg\",\"file_size\":4498,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIQF8E6gbVwqazpjDT4R2sUSqKkqlaAAICAAPANk8TCPVuRfqEp1kaBA\",\"file_unique_id\":\"AgADAgADwDZPEw\",\"file_size\":15955}}}]}"