{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module VkTest.ResponseExample where

import Vk.Types (Response)

json1, json2, json3, json4, json5, json6, json7, json8, json9, json10 :: Response
json1 =
  "{\"response\":{\"key\":\"anyKey\",\"server\":\"https:anyServer\",\"ts\":\"289\"}}"

json2 = "{\"ts\":\"289\",\"updates\":[]}\r\n"

json3 =
  "{\"ts\":\"290\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1594911394,\"from_id\":123,\"id\":597,\"out\":0,\"peer_id\":1606,\"text\":\"love\",\"conversation_message_id\":562,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"group_id\":19495,\"event_id\":\"123\"}]}\r\n"

json4 = "{\"response\":626}"

json5 =
  "{\"error\":{\"error_code\":5,\"error_msg\":\"User authorization failed: invalid access_token (4).\",\"request_params\":[{\"key\":\"user_id\",\"value\":\"1606\"},{\"key\":\"random_id\",\"value\":\"0\"},{\"key\":\"v\",\"value\":\"5.103\"},{\"key\":\"method\",\"value\":\"messages.send\"},{\"key\":\"oauth\",\"value\":\"1\"}]}}"

json6 = "lalala"

json7 =
  "{\"ts\":\"304\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1594932378,\"from_id\":1606,\"id\":629,\"out\":0,\"peer_id\":1606,\"text\":\"\",\"conversation_message_id\":594,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[{\"type\":\"sticker\",\"sticker\":{\"product_id\":279,\"sticker_id\":9014,\"images\":[{\"url\":\"https:sticker\",\"width\":64,\"height\":64},{\"url\":\"https:sticker\",\"width\":128,\"height\":128},{\"url\":\"https:sticker\",\"width\":256,\"height\":256},{\"url\":\"https:sticker352\",\"width\":352,\"height\":352},{\"url\":\"https:sticker\",\"width\":512,\"height\":512}],\"images_with_background\":[{\"url\":\"https:sticker\",\"width\":64,\"height\":64},{\"url\":\"https:sticker\",\"width\":128,\"height\":128},{\"url\":\"https:sticker256b\",\"width\":256,\"height\":256},{\"url\":\"https:sticker352b\",\"width\":352,\"height\":352},{\"url\":\"https:sticker512b\",\"width\":512,\"height\":512}]}}],\"is_hidden\":false},\"group_id\":19495,\"event_id\":\"123\"}]}"

json8 =
  "{\"ts\":\"635\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645105994,\"from_id\":1606,\"id\":1435,\"out\":0,\"attachments\":[{\"type\":\"photo\",\"photo\":{\"album_id\":282858707,\"date\":1644582624,\"id\":457240244,\"owner_id\":1606,\"access_key\":\"321\",\"sizes\":[{\"height\":17,\"url\":\"https:photo\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"o\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"p\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"q\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"r\",\"width\":17}],\"text\":\"\"}}],\"conversation_message_id\":1399,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":19495,\"event_id\":\"123\"}]}"

json9 =
  "{\"ts\":\"632\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645034914,\"from_id\":1606,\"id\":1426,\"out\":0,\"attachments\":[{\"type\":\"doc\",\"doc\":{\"id\":626273448,\"owner_id\":1606,\"title\":\"car.sql\",\"size\":88583,\"ext\":\"sql\",\"date\":1644996178,\"type\":8,\"url\":\"https:doc\",\"access_key\":\"321\"}}],\"conversation_message_id\":1390,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":19495,\"event_id\":\"123\"}]}"

json10 =
  "{\"ts\":\"633\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645035069,\"from_id\":1606,\"id\":1429,\"out\":0,\"attachments\":[{\"type\":\"audio_message\",\"audio_message\":{\"duration\":2,\"id\":626333795,\"link_mp3\":\"https:audiomsg\",\"link_ogg\":\"https:audiomsg.ogg\",\"owner_id\":1606,\"access_key\":\"321\",\"waveform\":[13,3,30,22,28,24,23,13,26,9,28,22,25,30]}}],\"conversation_message_id\":1393,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":19495,\"event_id\":\"123\"}]}"

json11, json12, json13, json14, json15, json16, json17, json18, json19, json20 :: Response
json11 = "{\"ts\":\"640\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211899,\"from_id\":1606,\"id\":1448,\"out\":0,\"attachments\":[{\"type\":\"video\",\"video\":{\"access_key\":\"321\",\"can_add\":1,\"comments\":19,\"date\":1434455009,\"description\":\"\",\"duration\":5,\"photo_130\":\"https:photo130\",\"photo_320\":\"https:photo320\",\"photo_800\":\"https:photo800\",\"width\":852,\"height\":480,\"id\":1714,\"owner_id\":-4144,\"title\":\"veryFunny\",\"track_code\":\"video_v\",\"views\":165445}}],\"conversation_message_id\":1412,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"
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
  "{\"ok\":true,\"blabla\":true,\"result\":[{\"chat\":0,\"update_id\":235801,\n\"message\":{\"message_id\":2112,\"lala\":2112,\"date\":\"lala\",\"from\":{\"id\":1118},\"text\":\"love\"}}]}"

json21, json22, json23, json24, json25, json26, json27, json28, json29 :: Response
json21 = "{\"blabla\":27,\"type\":\"blabla\"}"

json22 = "{\"ok\":false}"

json23 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2625,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644879314,\"photo\":[{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADcwADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUh4\",\"file_size\":1788,\"width\":90,\"height\":90},{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADbQADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUhy\",\"file_size\":21339,\"width\":320,\"height\":320},{\"file_id\":\"AgACAgIAAxkBAAIKQWIK3dIknDv4-TGPdN8SJWZBzTgbAAItuDEbrmpZSKD5O09PAfJsAQADAgADeAADIwQ\",\"file_unique_id\":\"AQADLbgxG65qWUh9\",\"file_size\":55515,\"width\":640,\"height\":640}],\"caption\":\"ddddd\"}}]}"

json24 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2643,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880938,\"document\":{\"file_name\":\"abc.log\",\"mime_type\":\"text/x-log\",\"file_id\":\"BQACAgIAAxkBAAIKU2IK5CoejiITl-pT1gAB8oIugJiBFwACsRUAAq5qWUiBeVmKn48ILiME\",\"file_unique_id\":\"AgADsRUAAq5qWUg\",\"file_size\":721}}}]}"

json25 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2646,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880948,\"voice\":{\"duration\":1,\"mime_type\":\"audio/ogg\",\"file_id\":\"AwACAgIAAxkBAAIKVmIK5DTxTQp9JjCcUYkLn4SpxQMxAAKyFQACrmpZSPDUnm4jTe90IwQ\",\"file_unique_id\":\"AgADshUAAq5qWUg\",\"file_size\":5333}}}]}"

json26 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2649,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644880958,\"animation\":{\"file_name\":\"mp4.mp4\",\"mime_type\":\"video/mp4\",\"duration\":2,\"width\":258,\"height\":210,\"file_id\":\"CgACAgQAAxkBAAIKWWIK5D5j9KeR5MIAAbZsYct2ZcviBQACeAIAAs_9jVIBjFpdHYSIxyME\",\"file_unique_id\":\"AgADeAIAAs_9jVI\",\"file_size\":92912},\"document\":{\"file_name\":\"mp4.mp4\",\"mime_type\":\"video/mp4\",\"file_id\":\"CgACAgQAAxkBAAIKWWIK5D5j9KeR5MIAAbZsYct2ZcviBQACeAIAAs_9jVIBjFpdHYSIxyME\",\"file_unique_id\":\"AgADeAIAAs_9jVI\",\"file_size\":92912}}}]}"

json27 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2655,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644881082,\"video\":{\"duration\":41,\"width\":1920,\"height\":1080,\"file_name\":\"Forest - 49981.mp4\",\"mime_type\":\"video/mp4\",\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgpfYgrkujEiYH3jBgFmHxbZMgJLtQcAArMVAAKuallI9dcQ8Daeid0BAAdtAAMjBA\",\"file_unique_id\":\"AQADsxUAAq5qWUhy\",\"file_size\":19252,\"width\":320,\"height\":180},\"file_id\":\"BAACAgIAAxkBAAIKX2IK5LoxImB94wYBZh8W2TICS7UHAAKzFQACrmpZSPXXEPA2nondIwQ\",\"file_unique_id\":\"AgADsxUAAq5qWUg\",\"file_size\":29753626}}}]}"

json28 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2661,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644881987,\"audio\":{\"duration\":153,\"file_name\":\"music1.mp3\",\"mime_type\":\"audio/mpeg\",\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgplYgroQ2G1BjtuTaDek7JaggiD5RcAArYVAAKuallIeQ7HbD2wXdUBAAdtAAMjBA\",\"file_unique_id\":\"AQADthUAAq5qWUhy\",\"file_size\":5092,\"width\":320,\"height\":320},\"file_id\":\"CQACAgIAAxkBAAIKZWIK6ENhtQY7bk2g3pOyWoIIg-UXAAK2FQACrmpZSHkOx2w9sF3VIwQ\",\"file_unique_id\":\"AgADthUAAq5qWUg\",\"file_size\":6363491}}}]}"

json29 = "{\"ok\":true,\"result\":[{\"update_id\":17106,\n\"message\":{\"message_id\":2664,\"from\":{\"id\":11189,\"is_bot\":false,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"language_code\":\"ru\"},\"chat\":{\"id\":11189,\"first_name\":\"Jeka\",\"last_name\":\"Grib\",\"type\":\"private\"},\"date\":1644882028,\"forward_from\":{\"id\":41908,\"is_bot\":false,\"first_name\":\"frik\",\"last_name\":\"as\",\"username\":\"ozib1\"},\"forward_date\":1588076433,\"sticker\":{\"width\":512,\"height\":470,\"emoji\":\"\\ud83d\\udc4d\",\"set_name\":\"Otbitye\",\"is_animated\":false,\"is_video\":false,\"thumb\":{\"file_id\":\"AAMCAgADGQEAAgpoYgrobOMHCp2ylpoDy6JCZ7EkOIYAAsIAA_DDwA46uuYXa63sDwEAB20AAyME\",\"file_unique_id\":\"AQADwgAD8MPADnI\",\"file_size\":8080,\"width\":128,\"height\":117},\"file_id\":\"CAACAgIAAxkBAAIKaGIK6GzjBwqdspaaA8uiQmexJDiGAALCAAPww8AOOrrmF2ut7A8jBA\",\"file_unique_id\":\"AgADwgAD8MPADg\",\"file_size\":45706}}}]}"