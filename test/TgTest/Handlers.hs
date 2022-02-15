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
import Control.Monad.Catch (throwM,SomeException(..))
import Network.HTTP.Client (HttpException( InvalidUrlException ))

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

throwHttpEx :: StateT [MockAction] IO Response
throwHttpEx = throwM $ InvalidUrlException "" ""

getUpdatesTestEx :: StateT [MockAction] IO Response
getUpdatesTestEx = throwHttpEx

confirmUpdatesTestEx :: Offset -> StateT [MockAction] IO Response
confirmUpdatesTestEx _ = throwHttpEx

sendMsgTestEx ::
     UserId -> TextOfMsg -> StateT [MockAction] IO Response
sendMsgTestEx _ _ = throwHttpEx

copyMsgTestEx ::
     UserId -> MessageId -> StateT [MockAction] IO Response
copyMsgTestEx _ _ = throwHttpEx

sendKeybTestEx ::
     UserId -> N -> TextOfMsg -> StateT [MockAction] IO Response
sendKeybTestEx _ _ _ = throwHttpEx


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

handle27 = handle13 {getUpdates = getUpdatesTest json19}

handle28 = handle13 {getUpdates = getUpdatesTest json20}

handle29 = handle13 {sendMsg = sendMsgTest json21}

handle30 = handle13 {sendMsg = sendMsgTest json22}

handle31 = handle13 {sendMsg = sendMsgTest json2}

handle32 = handle13 {getUpdates = getUpdatesTest json9}

handle33 = handle32 {copyMsg = copyMsgTest json21}

handle34 = handle32 {copyMsg = copyMsgTest json22}

handle35 = handle32 {copyMsg = copyMsgTest json2}

handle36 = handle13 {getUpdates = getUpdatesTest json23}

handle37 = handle13 {getUpdates = getUpdatesTest json24}

handle38 = handle13 {getUpdates = getUpdatesTest json25}

handle39 = handle13 {getUpdates = getUpdatesTest json26}

handle40 = handle13 {getUpdates = getUpdatesTest json27}

handle41 = handle13 {getUpdates = getUpdatesTest json28}

handle42 = handle13 {getUpdates = getUpdatesTest json29}

handle43 = handle13 {getUpdates = getUpdatesTest json8}

handle44 = handle43 {sendKeyb = sendKeybTest json21}

handle45 = handle43 {sendKeyb = sendKeybTest json22}

handle46 = handle43 {sendKeyb = sendKeybTest json2}

handle47 = handle13 {confirmUpdates = confirmUpdatesTest json21}

handle48 = handle13 {confirmUpdates = confirmUpdatesTest json22}

handle49 = handle13 {confirmUpdates = confirmUpdatesTest json2}

handle50 = handle13 {sendMsg = sendMsgTestEx}

handle51 = handle13 {getUpdates = getUpdatesTestEx}

handle52 = handle13 {confirmUpdates = confirmUpdatesTestEx}

handle53 = handle10 {sendKeyb = sendKeybTestEx}

handle54 = handle11 {copyMsg = copyMsgTestEx}



json1, json2, json3, json4, json5, json6, json7, json8, json9, json10, json11 ::
     Response
json1 = "{\"ok\":true,\"result\":[]}"

json2 = "{\"ok\":false,\"result\":235}"

json3 = "blablabla"

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

json19 =
  "{\"ok\":true,\"result\":[{\"update_id\":235801,\n\"message\":{\"message_id\":2112,\"from\":{\"id\":1118},\"text\":\"love\"}}]}"

json20 =
  "{\"ok\":true,\"blabla\":true,\"result\":[{\"chat\":0,\"update_id\":235801,\n\"message\":{\"message_id\":2112,\"lala\":2112,\"from\":{\"id\":1118},\"text\":\"love\"}}]}"

json21 = "{\"blabla\":27,\"type\":\"blabla\"}"

json22 = "{\"ok\":false}"

json23 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2625,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644879314,\"photo\":[{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADcwADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUh4\",\"file_size\":1788,\"width\":90,\"height\":90},{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADbQADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUhy\",\"file_size\":21339,\"width\":320,\"height\":320},{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADeAADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUh9\",\"file_size\":55515,\"width\":640,\"height\":640}],\"caption\":\"ddddd\"}}]}"

json24 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2643,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880938,\"document\":{\"file_name\":\"abc.log\",\"mime_type\":\"text/x-log\",\"file_id\":\"BQACAgIAAxkBAAIKU2IK5CoejiITl-pT1gAB8oIugJiBFwACsRUAAq5qWUiBeVmKn48ILiME\",\"file_unique_id\":\"AgADsRUAAq5qWUg\",\"file_size\":721}}}]}"

json25 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2646,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880948,\"voice\":{\"duration\":1,\"mime_type\":\"audio/ogg\",\"file_id\":\"AwACAgIAAxkBAAIKVmIK5DTxTQp9JjCcUYkLn4SpxQMxAAKyFQACrmpZSPDUnm4jTe90IwQ\",\"file_unique_id\":\"AgADshUAAq5qWUg\",\"file_size\":5333}}}]}"

json26 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2649,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880958,\"animation\":{\"file_name\":\"mp4.mp4\",\"mime_type\":\"video/mp4\",\"duration\":2,\"width\":258,\"height\":210,\"file_id\":\"CgACAgQAAxkBAAIKWWIK5D5j9KeR5MIAAbZsYct2ZcviBQACeAIAAs_9jVIBjFpdHYSIxyME\",\"file_unique_id\":\"AgADeAIAAs_9jVI\",\"file_size\":92912},\"document\":{\"file_name\":\"mp4.mp4\",\"mime_type\":\"video/mp4\",\"file_id\":\"CgACAgQAAxkBAAIKWWIK5D5j9KeR5MIAAbZsYct2ZcviBQACeAIAAs_9jVIBjFpdHYSIxyME\",\"file_unique_id\":\"AgADeAIAAs_9jVI\",\"file_size\":92912}}}]}"

json27 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2655,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644881082,\"video\":{\"duration\":41,\"width\":1920,\"height\":1080,\"file_name\":\"Forest - 49981.mp4\",\"mime_type\":\"video/mp4\",\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgpfYgrkujEiYH3jBgFmHxbZMgJLtQcAArMVAAKuallI9dcQ8Daeid0BAAdtAAMjBA\",\"file_unique_id\":\"AQADsxUAAq5qWUhy\",\"file_size\":19252,\"width\":320,\"height\":180},\"file_id\":\"BAACAgIAAxkBAAIKX2IK5LoxImB94wYBZh8W2TICS7UHAAKzFQACrmpZSPXXEPA2nondIwQ\",\"file_unique_id\":\"AgADsxUAAq5qWUg\",\"file_size\":29753626}}}]}"

json28 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2661,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644881987,\"audio\":{\"duration\":153,\"file_name\":\"music1.mp3\",\"mime_type\":\"audio/mpeg\",\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgplYgroQ2G1BjtuTaDek7JaggiD5RcAArYVAAKuallIeQ7HbD2wXdUBAAdtAAMjBA\",\"file_unique_id\":\"AQADthUAAq5qWUhy\",\"file_size\":5092,\"width\":320,\"height\":320},\"file_id\":\"CQACAgIAAxkBAAIKZWIK6ENhtQY7bk2g3pOyWoIIg-UXAAK2FQACrmpZSHkOx2w9sF3VIwQ\",\"file_unique_id\":\"AgADthUAAq5qWUg\",\"file_size\":6363491}}}]}"

json29 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2664,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644882028,\"forward_from\":{\"id\":41908,\"is_bot\":false,\"first_name\":\"frik\",\"last_name\":\"as\",\"username\":\"ozib1\"},\"forward_date\":1588076433,\"sticker\":{\"width\":512,\"height\":470,\"emoji\":\"\\ud83d\\udc4d\",\"set_name\":\"Otbitye\",\"is_animated\":false,\"is_video\":false,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgpoYgrobOMHCp2ylpoDy6JCZ7EkOIYAAsIAA_DDwA46uuYXa63sDwEAB20AAyME\",\"file_unique_id\":\"AQADwgAD8MPADnI\",\"file_size\":8080,\"width\":128,\"height\":117},\"file_id\":\"CAACAgIAAxkBAAIKaGIK6GzjBwqdspaaA8uiQmexJDiGAALCAAPww8AOOrrmF2ut7A8jBA\",\"file_unique_id\":\"AgADwgAD8MPADg\",\"file_size\":45706}}}]}"




