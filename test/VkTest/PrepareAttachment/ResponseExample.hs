{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module VkTest.PrepareAttachment.ResponseExample where

import Vk.Types (Response)
import Data.String (fromString)

picUploadUrl,docUploadUrl :: String
picUploadUrl = "http://toLoadPic"
docUploadUrl = "http://toLoadDoc"

docId,ownerId :: Int
docId = 25
ownerId = 50

json1, json2, json3, json4, json5, json6, json7, json8, json9, json10 :: Response
json1 = "{\"blabla\":\"blabla\"}"

json2 = fromString $
  "{\"response\":{\"album_id\":-64,\"upload_url\":\"" ++ picUploadUrl ++ "\",\"user_id\":0,\"group_id\":194952914}}"

json3 = "{\"server\":24,\"photo\":\"anyPhotoSring\",\"hash\":\"anyHash\"}"

json4 = fromString $
  "{\"response\":[{\"album_id\":-64,\"date\":1644999562,\"id\":" ++ show docId ++ ",\"owner_id\":" ++ show ownerId ++ ",\"access_key\":\"35546f916f1affb66f\",\"sizes\":[{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"o\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"p\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"q\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"r\",\"width\":17}],\"text\":\"\"}]}"

json5 = fromString $
  "{\"response\":{\"upload_url\":\"" ++ docUploadUrl ++ "\"}}"

json6 = 
  "{\"file\":\"anyFile\"}"

json7 = fromString $
  "{\"response\":{\"type\":\"doc\",\"doc\":{\"id\":" ++ show docId ++ ",\"owner_id\":" ++ show ownerId ++ ",\"title\":\"car.sql\",\"size\":88583,\"ext\":\"sql\",\"date\":1644999586,\"type\":8,\"url\":\"https:\\/\\/vk.com\\/doc16063921_626276949?hash=977b8afbe994b8c09d&dl=FUYTSNBZGUZDSMJU:1644999586:4ca2a1750bf9f6403d&api=1&no_preview=1\"}}}"

json8 = fromString $
  "{\"response\":{\"type\":\"audio_message\",\"audio_message\":{\"duration\":2,\"id\":" ++ show docId ++ ",\"link_mp3\":\"https:\\/\\/psv4.userapi.com\\/c613501\\/\\/u16063921\\/audiomsg\\/d1\\/822afd1f8c.mp3\",\"link_ogg\":\"https:\\/\\/psv4.userapi.com\\/c613501\\/\\/u16063921\\/audiomsg\\/d1\\/822afd1f8c.ogg\",\"owner_id\":" ++ show ownerId ++ ",\"access_key\":\"64f3c82e314bbe46fd\",\"waveform\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,1,1,1,1,1,1,0,0,0,0]}}}"

json9 = fromString $
  "{\"response\":{\"upload_url\":\"" ++ docUploadUrl ++ "\"},\"blabla\":\"blabla\"}"
  
json10 = fromString $
  "{\"response\":{\"upload_url\":\"" ++ picUploadUrl ++ "\"}}"

json11, json12, json13, json14, json15, json16, json17, json18, json19, json20 :: Response
json11 = fromString $
  "{\"response\":[{\"id\":" ++ show docId ++ ",\"owner_id\":" ++ show ownerId ++ "}]}"

json12 = fromString $
  "{\"response\":{\"blabla\":\"blabla\",\"album_id\":\"blabla\",\"upload_url\":\"" ++ picUploadUrl ++ "\",\"user_id\":-25}}"

json13 =
  "{\"server\":24,\"photo\":\"anyPhotoSring\",\"hash\":\"anyHash\",\"blabla\":\"blabla\"}"

json14 = fromString $
  "{\"response\":[{\"blabla\":\"blabla\",\"album_id\":\"blabla\",\"id\":" ++ show docId ++ ",\"owner_id\":" ++ show ownerId ++ ",\"access_key\":25}]}"
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

json21, json22, json23, json24, json25, json26, json27, json28, json29, json30 :: Response
json21 = "{\"blabla\":27,\"type\":\"blabla\"}"

json22 = "{\"ok\":false}"

json23 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2625,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644879314,\"photo\":[{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADcwADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUh4\",\"file_size\":1788,\"width\":90,\"height\":90},{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADbQADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUhy\",\"file_size\":21339,\"width\":320,\"height\":320},{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADeAADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUh9\",\"file_size\":55515,\"width\":640,\"height\":640}],\"caption\":\"ddddd\"}}]}"

json24 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2643,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880938,\"document\":{\"file_name\":\"abc.log\",\"mime_type\":\"text/x-log\",\"file_id\":\"BQACAgIAAxkBAAIKU2IK5CoejiITl-pT1gAB8oIugJiBFwACsRUAAq5qWUiBeVmKn48ILiME\",\"file_unique_id\":\"AgADsRUAAq5qWUg\",\"file_size\":721}}}]}"

json25 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2646,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880948,\"voice\":{\"duration\":1,\"mime_type\":\"audio/ogg\",\"file_id\":\"AwACAgIAAxkBAAIKVmIK5DTxTQp9JjCcUYkLn4SpxQMxAAKyFQACrmpZSPDUnm4jTe90IwQ\",\"file_unique_id\":\"AgADshUAAq5qWUg\",\"file_size\":5333}}}]}"

json26 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2649,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880958,\"animation\":{\"file_name\":\"mp4.mp4\",\"mime_type\":\"video/mp4\",\"duration\":2,\"width\":258,\"height\":210,\"file_id\":\"CgACAgQAAxkBAAIKWWIK5D5j9KeR5MIAAbZsYct2ZcviBQACeAIAAs_9jVIBjFpdHYSIxyME\",\"file_unique_id\":\"AgADeAIAAs_9jVI\",\"file_size\":92912},\"document\":{\"file_name\":\"mp4.mp4\",\"mime_type\":\"video/mp4\",\"file_id\":\"CgACAgQAAxkBAAIKWWIK5D5j9KeR5MIAAbZsYct2ZcviBQACeAIAAs_9jVIBjFpdHYSIxyME\",\"file_unique_id\":\"AgADeAIAAs_9jVI\",\"file_size\":92912}}}]}"

json27 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2655,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644881082,\"video\":{\"duration\":41,\"width\":1920,\"height\":1080,\"file_name\":\"Forest - 49981.mp4\",\"mime_type\":\"video/mp4\",\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgpfYgrkujEiYH3jBgFmHxbZMgJLtQcAArMVAAKuallI9dcQ8Daeid0BAAdtAAMjBA\",\"file_unique_id\":\"AQADsxUAAq5qWUhy\",\"file_size\":19252,\"width\":320,\"height\":180},\"file_id\":\"BAACAgIAAxkBAAIKX2IK5LoxImB94wYBZh8W2TICS7UHAAKzFQACrmpZSPXXEPA2nondIwQ\",\"file_unique_id\":\"AgADsxUAAq5qWUg\",\"file_size\":29753626}}}]}"

json28 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2661,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644881987,\"audio\":{\"duration\":153,\"file_name\":\"music1.mp3\",\"mime_type\":\"audio/mpeg\",\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgplYgroQ2G1BjtuTaDek7JaggiD5RcAArYVAAKuallIeQ7HbD2wXdUBAAdtAAMjBA\",\"file_unique_id\":\"AQADthUAAq5qWUhy\",\"file_size\":5092,\"width\":320,\"height\":320},\"file_id\":\"CQACAgIAAxkBAAIKZWIK6ENhtQY7bk2g3pOyWoIIg-UXAAK2FQACrmpZSHkOx2w9sF3VIwQ\",\"file_unique_id\":\"AgADthUAAq5qWUg\",\"file_size\":6363491}}}]}"

json29 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2664,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644882028,\"forward_from\":{\"id\":41908,\"is_bot\":false,\"first_name\":\"frik\",\"last_name\":\"as\",\"username\":\"ozib1\"},\"forward_date\":1588076433,\"sticker\":{\"width\":512,\"height\":470,\"emoji\":\"\\ud83d\\udc4d\",\"set_name\":\"Otbitye\",\"is_animated\":false,\"is_video\":false,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgpoYgrobOMHCp2ylpoDy6JCZ7EkOIYAAsIAA_DDwA46uuYXa63sDwEAB20AAyME\",\"file_unique_id\":\"AQADwgAD8MPADnI\",\"file_size\":8080,\"width\":128,\"height\":117},\"file_id\":\"CAACAgIAAxkBAAIKaGIK6GzjBwqdspaaA8uiQmexJDiGAALCAAPww8AOOrrmF2ut7A8jBA\",\"file_unique_id\":\"AgADwgAD8MPADg\",\"file_size\":45706}}}]}"

json30 = "{\"ok\":true}"
