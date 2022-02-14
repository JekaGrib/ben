--{-# OPTIONS_GHC -Werror #-}
--{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module TgTest.Handlers where

import Control.Monad.State (StateT(..), evalStateT, execStateT)
import qualified Data.Map as Map
import Test.Hspec (describe, hspec, it, shouldBe, shouldThrow)
import Tg.App (Handle(..), run, startApp)
import Tg.Conf (Config(..))
import Tg.Logger (Priority(..))
import Tg.Oops (TGBotException(..))
import Tg.Types
import TgTest.Types
import TgTest.Log
import TgTest.Oops

getUpdatesTest :: Response -> StateT [MockAction] IO Response
getUpdatesTest json = StateT $ \s -> return (json, GOTUPDATES : s)

confirmUpdatesTest :: Response -> Offset -> StateT [MockAction] IO Response
confirmUpdatesTest json offset =
  StateT $ \s -> return (json, CONFIRMUPDATES offset : s)

sendMsgTest ::
     Response -> UserId -> TextOfMsg -> StateT [MockAction] IO Response
sendMsgTest json usId msg = StateT $ \s -> return (json, SENDMSG usId msg : s)

copyMsgTest ::
     Response -> UserId -> MessageId -> StateT [MockAction] IO Response
copyMsgTest json usId msgId =
  StateT $ \s -> return (json, COPYMSG usId msgId : s)

sendKeybTest ::
     Response -> UserId -> N -> TextOfMsg -> StateT [MockAction] IO Response
sendKeybTest json usId currN msg =
  StateT $ \s -> return (json, SENDKEYB usId currN msg : s)

logTest :: Priority -> String -> StateT [MockAction] IO ()
logTest prio _ = StateT $ \s -> return ((), LOG prio : s)

logTest0 :: Priority -> String -> StateT [MockAction] IO ()
logTest0 prio str = StateT $ \s -> return ((), LOGMSG prio str : s)

config1 :: Config
config1 =
  Config
    { cStartN = 2
    , cBotToken = "ABC123"
    , cHelpMsg = "Hello"
    , cRepeatQ = "Why?"
    , cPriority = DEBUG
    }

handle1 :: Handle (StateT [MockAction] IO)
handle1 =
  Handle
    { hConf = config1
    , hLog = handLogDebug
    , getUpdates = getUpdatesTest json6
    , getShortUpdates = getUpdatesTest json1
    , confirmUpdates = confirmUpdatesTest json1
    , sendMsg = sendMsgTest json4
    , sendKeyb = sendKeybTest json4
    , copyMsg = copyMsgTest json11
    }

handle0, handle2, handle3, handle4, handle5, handle6 ::
     Handle (StateT [MockAction] IO)
handle0 = handle1 {hLog = handLogMsgDebug}

handle2 = handle1 {getShortUpdates = getUpdatesTest json2}

handle3 = handle1 {getShortUpdates = getUpdatesTest json3}

handle4 = handle1 {getShortUpdates = getUpdatesTest json4}

handle5 = handle1 {getShortUpdates = getUpdatesTest json5, hLog = handLogMsgDebug}

handle6 = handle1 {getUpdates = getUpdatesTest json2}

handle7, handle8, handle9, handle10, handle11, handle12 ::
     Handle (StateT [MockAction] IO)
handle7 = handle1 {getUpdates = getUpdatesTest json3}

handle8 = handle1 {getUpdates = getUpdatesTest json4}

handle9 = handle1 {getUpdates = getUpdatesTest json7, hLog = handLogMsgDebug}

handle10 = handle1 {getUpdates = getUpdatesTest json8}

handle11 = handle1 {getUpdates = getUpdatesTest json9}

handle12 = handle1 {getUpdates = getUpdatesTest json10}

handle13 = handle1 {hLog = handLogWarn}

handle14 =  handle13 {getUpdates = getUpdatesTest json12}

handle15 =  handle13 {getShortUpdates = getUpdatesTest json12}

handle16 =  handle13 {confirmUpdates = confirmUpdatesTest json2}

handle17 =  handle13 {confirmUpdates = confirmUpdatesTest json5}

handle18 =  handle13 {confirmUpdates = confirmUpdatesTest json3}

handle19 =  handle13 {getUpdates = getUpdatesTest json13}

handle20 =  handle13 {confirmUpdates = confirmUpdatesTest json12}

handle21 = handle13 {getShortUpdates = getUpdatesTest json14}

handle22 = handle13 {getUpdates = getUpdatesTest json5}

handle23 = handle13 {getUpdates = getUpdatesTest json15}

handle24 = handle13 {getUpdates = getUpdatesTest json16}

handle25 = handle13 {getUpdates = getUpdatesTest json17}

handle26 = handle13 {getUpdates = getUpdatesTest json18}


--handle21 =  handle13 {confirmUpdates = sendMsgTest json2}

      

json1, json2, json3, json4, json5, json6, json7, json8, json9, json10, json11 ::
     Response
json1 = "{\"ok\":true,\"result\":[]}"

json2 = "{\"ok\":false,\"result\":235}"

json3 = "lalala"

json4 = "{\"ok\":true}"

json5 =
  "{\"ok\":true,\"result\":[{\"update_id\":235800,\n\"edited_message\":{\"message_id\":2109,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157544,\"edit_date\":1594157550,\"text\":\"sev\"}},{\"update_id\":235801,\n\"message\":{\"message_id\":2110,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157558,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\udc4b\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgg-XwTp9qVkXckjuJdFWs8YfRcnlKIAAgUAA8A2TxP5al-agmtNdZc4uA8ABAEAB20AA7V6AAIaBA\",\"file_unique_id\":\"AQADlzi4DwAEtXoAAg\",\"file_size\":3848,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIPl8E6falZF3JI7iXRVrPGH0XJ5SiAAIFAAPANk8T-WpfmoJrTXUaBA\",\"file_unique_id\":\"AgADBQADwDZPEw\",\"file_size\":7285}}},{\"update_id\":235802,\n\"message\":{\"message_id\":2111,\"from\":{\"id\":12677,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":12677,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157567,\"text\":\"Toni\"}},{\"update_id\":235803,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":12677,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":12677,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157574,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\ude18\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAghAXwTqBtXCprOmMNPhHaxRKoqSqVoAAgIAA8A2TxMI9W5F-oSnWRQsuA8ABAEAB20AAyZfAAIaBA\",\"file_unique_id\":\"AQADFCy4DwAEJl8AAg\",\"file_size\":4498,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIQF8E6gbVwqazpjDT4R2sUSqKkqlaAAICAAPANk8TCPVuRfqEp1kaBA\",\"file_unique_id\":\"AgADAgADwDZPEw\",\"file_size\":15955}}}]}"

json6 =
  "{\"ok\":true,\"result\":[{\"update_id\":235801,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"love\"}}]}"

json7 =
  "{\"ok\":true,\"result\":[{\"update_id\":235801,\n\"message\":{\"message_id\":2113,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"/help\"}}]}"

json8 =
  "{\"ok\":true,\"result\":[{\"update_id\":235800,\n\"message\":{\"message_id\":2114,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"/repeat\"}}]}"

json9 =
  "{\"ok\":true,\"result\":[{\"update_id\":235807,\n\"message\":{\"message_id\":2140,\"from\":{\"id\":12677,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":12677,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594219382,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\udc4b\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAghcXwXbdpokxFdrCT29uzt0nEq3KzwAAgUAA8A2TxP5al-agmtNdZc4uA8ABAEAB20AA7V6AAIaBA\",\"file_unique_id\":\"AQADlzi4DwAEtXoAAg\",\"file_size\":3848,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIXF8F23aaJMRXawk9vbs7dJxKtys8AAIFAAPANk8T-WpfmoJrTXUaBA\",\"file_unique_id\":\"AgADBQADwDZPEw\",\"file_size\":7285}}}]}"

json10 =
  "{\"ok\":true,\"result\":[{\"update_id\":235801,\n\"message\":{\"message_id\":2117,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"4\"}}]}"

json11 = "{\"ok\":true,\"result\":{\"message_id\":2141}}"

json12 =
  "{\"ok\":false,\"result\":[{\"update_id\":235800,\n\"edited_message\":{\"message_id\":2109,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157544,\"edit_date\":1594157550,\"text\":\"sev\"}},{\"update_id\":235801,\n\"message\":{\"message_id\":2110,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157558,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\udc4b\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgg-XwTp9qVkXckjuJdFWs8YfRcnlKIAAgUAA8A2TxP5al-agmtNdZc4uA8ABAEAB20AA7V6AAIaBA\",\"file_unique_id\":\"AQADlzi4DwAEtXoAAg\",\"file_size\":3848,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIPl8E6falZF3JI7iXRVrPGH0XJ5SiAAIFAAPANk8T-WpfmoJrTXUaBA\",\"file_unique_id\":\"AgADBQADwDZPEw\",\"file_size\":7285}}},{\"update_id\":235802,\n\"message\":{\"message_id\":2111,\"from\":{\"id\":12677,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":12677,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157567,\"text\":\"Toni\"}},{\"update_id\":235803,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":12677,\"is_bot\":false,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"language_code\":\"ru\"},\"chat\":{\"id\":12677,\"first_name\":\"Vitalik\",\"last_name\":\"Gribov\",\"type\":\"private\"},\"date\":1594157574,\"sticker\":{\"width\":512,\"height\":512,\"emoji\":\"\\ud83d\\ude18\",\"set_name\":\"HotCherry\",\"is_animated\":true,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAghAXwTqBtXCprOmMNPhHaxRKoqSqVoAAgIAA8A2TxMI9W5F-oSnWRQsuA8ABAEAB20AAyZfAAIaBA\",\"file_unique_id\":\"AQADFCy4DwAEJl8AAg\",\"file_size\":4498,\"width\":128,\"height\":128},\"file_id\":\"CAACAgIAAxkBAAIIQF8E6gbVwqazpjDT4R2sUSqKkqlaAAICAAPANk8TCPVuRfqEp1kaBA\",\"file_unique_id\":\"AgADAgADwDZPEw\",\"file_size\":15955}}}]}"

json13 =
  "{\"ok\":true,\"result\":[{\"update_id\":-235802,\n\"message\":{\"message_id\":2114,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"/repeat\"}}]}"

json14 = "{\"ok\":true,\"result\":235}"

json15 =
  "{\"ok\":true,\"result\":[{\"update_id\":235800,\n\"edited_message\":{\"message_id\":2109,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594157544,\"edit_date\":1594157550,\"text\":\"sev\"}}]}"

json16 =
  "{\"ok\":true,\"result\":[{\"update_id\":235800,\n\"unknown_update\":1594157544}]}"

json17 =
  "{\"ok\":true,\"result\":[{\"update_id\":235800}]}"

json18 =
  "{\"ok\":true,\"result\":[{\"update_id\":235800},{\"update_id\":235801,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":1118,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":1118,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1594202617,\"text\":\"love\"}}]}"