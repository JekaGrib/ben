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
  "{\"ts\":\"635\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645105994,\"from_id\":1606,\"id\":1435,\"out\":0,\"attachments\":[{\"type\":\"photo\",\"photo\":{\"album_id\":282858707,\"date\":1644582624,\"id\":457240244,\"owner_id\":1606,\"access_key\":\"321\",\"sizes\":[{\"height\":17,\"url\":\"https:photo\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"o\",\"width\":17}],\"text\":\"\"}}],\"conversation_message_id\":1399,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":19495,\"event_id\":\"123\"}]}"

json9 =
  "{\"ts\":\"632\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645034914,\"from_id\":1606,\"id\":1426,\"out\":0,\"attachments\":[{\"type\":\"doc\",\"doc\":{\"id\":626273448,\"owner_id\":1606,\"title\":\"car.sql\",\"size\":88583,\"ext\":\"sql\",\"date\":1644996178,\"type\":8,\"url\":\"https:doc\",\"access_key\":\"321\"}}],\"conversation_message_id\":1390,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":19495,\"event_id\":\"123\"}]}"

json10 =
  "{\"ts\":\"633\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645035069,\"from_id\":1606,\"id\":1429,\"out\":0,\"attachments\":[{\"type\":\"audio_message\",\"audio_message\":{\"duration\":2,\"id\":626333795,\"link_mp3\":\"https:audiomsg\",\"link_ogg\":\"https:audiomsg.ogg\",\"owner_id\":1606,\"access_key\":\"321\",\"waveform\":[13,3,30,22,28,24,23,13,26,9,28,22,25,30]}}],\"conversation_message_id\":1393,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":19495,\"event_id\":\"123\"}]}"

json11, json12, json13, json14, json15, json16, json17, json18, json19, json20 :: Response
json11 = 
  "{\"ts\":\"640\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211899,\"from_id\":1606,\"id\":1448,\"out\":0,\"attachments\":[{\"type\":\"video\",\"video\":{\"access_key\":\"321\",\"can_add\":1,\"comments\":19,\"date\":1434455009,\"description\":\"\",\"duration\":5,\"photo_130\":\"https:photo130\",\"photo_320\":\"https:photo320\",\"photo_800\":\"https:photo800\",\"width\":852,\"height\":480,\"id\":1714,\"owner_id\":-4144,\"title\":\"veryFunny\",\"track_code\":\"video_v\",\"views\":165445}}],\"conversation_message_id\":1412,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json12 =
  "{\"ts\":\"641\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211910,\"from_id\":1606,\"id\":1451,\"out\":0,\"attachments\":[{\"type\":\"audio\",\"audio\":{\"artist\":\"Deftones\",\"id\":3483,\"owner_id\":1606,\"title\":\"Gauze\",\"duration\":281,\"url\":\"https:audio\",\"date\":1425013158,\"lyrics_id\":45597212,\"genre_id\":18,\"is_hq\":true,\"short_videos_allowed\":false,\"stories_allowed\":false,\"stories_cover_allowed\":false}}],\"conversation_message_id\":1415,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json13 =
  "{\"ts\":\"642\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211954,\"from_id\":1606,\"id\":1454,\"out\":0,\"attachments\":[{\"type\":\"market\",\"market\":{\"availability\":0,\"category\":{\"id\":1103,\"name\":\"mama\",\"section\":{\"id\":11,\"name\":\"mama\"}},\"description\":\"veryFunny\",\"id\":3822,\"owner_id\":-1196,\"price\":{\"amount\":\"35000\",\"currency\":{\"id\":643,\"name\":\"RUB\",\"title\":\"ggg\"},\"text\":\"mama\"},\"title\":\"mama\",\"date\":1608254420,\"thumb_photo\":\"https:impg\"}}],\"conversation_message_id\":1418,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json14 = 
  "{\"ts\":\"644\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645212090,\"from_id\":1606,\"id\":1460,\"out\":0,\"attachments\":[{\"type\":\"wall\",\"wall\":{\"id\":4584,\"from_id\":-6799,\"to_id\":-6799,\"date\":1595507808,\"post_type\":\"post\",\"text\":\"mama\",\"marked_as_ads\":0,\"attachments\":[{\"type\":\"photo\",\"photo\":{\"album_id\":-7,\"date\":1595507808,\"id\":4577,\"owner_id\":-6799,\"access_key\":\"321\",\"post_id\":4584420,\"sizes\":[{\"height\":73,\"url\":\"https:impg\",\"type\":\"m\",\"width\":130},{\"height\":87,\"url\":\"https:impg\",\"type\":\"o\",\"width\":130},{\"height\":133,\"url\":\"https:impg\",\"type\":\"p\",\"width\":200}],\"text\":\"\",\"user_id\":100}},{\"type\":\"poll\",\"poll\":{\"multiple\":false,\"end_date\":0,\"closed\":false,\"is_board\":false,\"can_edit\":false,\"can_vote\":true,\"can_report\":true,\"can_share\":true,\"created\":1595507854,\"id\":3839,\"owner_id\":-6799,\"question\":\"what?\",\"votes\":1017,\"disable_unvote\":false,\"anonymous\":true,\"answer_ids\":[],\"embed_hash\":\"hash\",\"answers\":[{\"id\":12787,\"rate\":50.150000,\"text\":\"yes\",\"votes\":510},{\"id\":12787,\"rate\":49.850000,\"text\":\"no\",\"votes\":507}],\"author_id\":-6799}}],\"comments\":{\"count\":25},\"likes\":{\"can_like\":0,\"count\":13,\"user_likes\":0},\"reposts\":{\"count\":1},\"views\":{\"count\":7932},\"access_key\":\"321\",\"from\":{\"id\":67991642,\"name\":\"mama\",\"screen_name\":\"lentaru\",\"is_closed\":0,\"type\":\"page\",\"photo_50\":\"https:photo\"}}}],\"conversation_message_id\":1424,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json15 =
  "{\"ts\":\"645\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645212106,\"from_id\":1606,\"id\":1463,\"out\":0,\"attachments\":[{\"type\":\"poll\",\"poll\":{\"multiple\":false,\"end_date\":0,\"closed\":false,\"is_board\":false,\"can_edit\":false,\"can_vote\":true,\"can_report\":true,\"can_share\":true,\"created\":1595507854,\"id\":3839,\"owner_id\":-6799,\"question\":\"what?\",\"votes\":1017,\"disable_unvote\":false,\"anonymous\":true,\"answer_ids\":[],\"embed_hash\":\"hash\",\"answers\":[{\"id\":1278751160,\"rate\":50.150000,\"text\":\"oh\",\"votes\":510},{\"id\":1278751161,\"rate\":49.850000,\"text\":\"ah\",\"votes\":507}],\"author_id\":-6799}}],\"conversation_message_id\":1427,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json16 =
    "{\"ts\":\"641\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211910,\"from_id\":1606,\"id\":1451,\"out\":0,\"attachments\":[{\"type\":\"audio\",\"audio\":{\"artist\":\"Deftones\",\"id\":3483,\"owner_id\":1606,\"title\":\"Gauze\",\"duration\":281,\"url\":\"https:audio\",\"date\":1425013158,\"lyrics_id\":45597212,\"genre_id\":18,\"is_hq\":true,\"short_videos_allowed\":false,\"stories_allowed\":false,\"stories_cover_allowed\":false}}],\"conversation_message_id\":1415,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"hello\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json17 =
    "{\"ts\":\"642\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211954,\"from_id\":1606,\"id\":1454,\"out\":0,\"attachments\":[{\"type\":\"market\",\"market\":{\"availability\":0,\"category\":{\"id\":1103,\"name\":\"mama\",\"section\":{\"id\":11,\"name\":\"mama\"}},\"description\":\"veryFunny\",\"id\":3822,\"owner_id\":-1196,\"price\":{\"amount\":\"35000\",\"currency\":{\"id\":643,\"name\":\"RUB\",\"title\":\"ggg\"},\"text\":\"mama\"},\"title\":\"mama\",\"date\":1608254420,\"thumb_photo\":\"https:impg\"}}],\"conversation_message_id\":1418,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"hello\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json18 =
  "{\"ts\":\"647\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645216939,\"from_id\":1606,\"id\":1469,\"out\":0,\"attachments\":[],\"conversation_message_id\":1433,\"fwd_messages\":[{\"date\":1636008793,\"from_id\":2799,\"text\":\"92115\",\"attachments\":[]}],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}\r\n"

json19 =
  "{\"ts\":\"649\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645217384,\"from_id\":1606,\"id\":1474,\"out\":0,\"attachments\":[],\"conversation_message_id\":1438,\"fwd_messages\":[],\"geo\":{\"coordinates\":{\"latitude\":69.409,\"longitude\":32.456},\"place\":{\"city\":\"anyCity\",\"country\":\"anyCountry\",\"title\":\"anyTitle\"},\"type\":\"point\"},\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json20 =
  "{\"ts\":\"649\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645217384,\"from_id\":1606,\"id\":1474,\"out\":0,\"attachments\":[],\"conversation_message_id\":1438,\"fwd_messages\":[],\"geo\":{\"coordinates\":{\"latitude\":69.409,\"longitude\":32.456},\"place\":{\"city\":\"anyCity\",\"country\":\"anyCountry\",\"title\":\"anyTitle\"},\"type\":\"point\"},\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"hello\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json21, json22, json23, json24, json25, json26, json27, json28, json29 :: Response
json21 = 
  "{\"ts\":\"649\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645217384,\"from_id\":1606,\"id\":1474,\"out\":0,\"attachments\":[{\"type\":\"audio\",\"audio\":{\"artist\":\"Deftones\",\"id\":3483,\"owner_id\":1606,\"title\":\"Gauze\",\"duration\":281,\"url\":\"https:audio\",\"date\":1425013158,\"lyrics_id\":45597212,\"genre_id\":18,\"is_hq\":true,\"short_videos_allowed\":false,\"stories_allowed\":false,\"stories_cover_allowed\":false}}],\"conversation_message_id\":1438,\"fwd_messages\":[],\"geo\":{\"coordinates\":{\"latitude\":69.409,\"longitude\":32.456},\"place\":{\"city\":\"anyCity\",\"country\":\"anyCountry\",\"title\":\"anyTitle\"},\"type\":\"point\"},\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json22 = 
  "{\"ts\":\"649\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645217384,\"from_id\":1606,\"id\":1474,\"out\":0,\"attachments\":[{\"type\":\"audio\",\"audio\":{\"artist\":\"Deftones\",\"id\":3483,\"owner_id\":1606,\"title\":\"Gauze\",\"duration\":281,\"url\":\"https:audio\",\"date\":1425013158,\"lyrics_id\":45597212,\"genre_id\":18,\"is_hq\":true,\"short_videos_allowed\":false,\"stories_allowed\":false,\"stories_cover_allowed\":false}}],\"conversation_message_id\":1438,\"fwd_messages\":[],\"geo\":{\"coordinates\":{\"latitude\":69.409,\"longitude\":32.456},\"place\":{\"city\":\"anyCity\",\"country\":\"anyCountry\",\"title\":\"anyTitle\"},\"type\":\"point\"},\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"hello\"},\"group_id\":1949,\"event_id\":\"123\"}]}"


json23 = 
    "{\"ts\":\"635\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645105994,\"from_id\":1606,\"id\":1435,\"out\":0,\"attachments\":[{\"type\":\"photo\",\"photo\":{\"album_id\":282858707,\"date\":1644582624,\"id\":457240244,\"owner_id\":1606,\"access_key\":\"321\",\"sizes\":[{\"height\":17,\"url\":\"https:photo\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"o\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"p\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"q\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"r\",\"width\":17}],\"text\":\"\"}}],\"conversation_message_id\":1399,\"fwd_messages\":[],\"geo\":{\"coordinates\":{\"latitude\":69.409,\"longitude\":32.456},\"place\":{\"city\":\"anyCity\",\"country\":\"anyCountry\",\"title\":\"anyTitle\"},\"type\":\"point\"},\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"hello\"},\"group_id\":19495,\"event_id\":\"123\"}]}"

json24 = 
  "{\"ts\":\"290\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1594911394,\"from_id\":123,\"id\":597,\"out\":0,\"peer_id\":1606,\"text\":\"\",\"conversation_message_id\":562,\"fwd_messages\":[{\"date\":1636008793,\"from_id\":2799,\"text\":\"92115\",\"geo\":{\"coordinates\":{\"latitude\":69.409,\"longitude\":32.456},\"place\":{\"city\":\"anyCity\",\"country\":\"anyCountry\",\"title\":\"anyTitle\"},\"type\":\"point\"},\"attachments\":[]}],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"group_id\":19495,\"event_id\":\"123\"}]}\r\n"

json25 = 
  "{\"ts\":\"290\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1594911394,\"from_id\":123,\"id\":597,\"out\":0,\"peer_id\":1606,\"text\":\"love\",\"conversation_message_id\":562,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"group_id\":19495,\"event_id\":\"123\"},{\"type\":\"message_new\",\"object\":{\"date\":1594911394,\"from_id\":555,\"id\":597,\"out\":0,\"peer_id\":1606,\"text\":\"hello\",\"conversation_message_id\":562,\"fwd_messages\":[],\"important\":false,\"random_id\":0,\"attachments\":[],\"is_hidden\":false},\"group_id\":19495,\"event_id\":\"123\"}]}"

json26 = 
  "{\"ts\":\"641\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211910,\"from_id\":123,\"id\":1451,\"out\":0,\"attachments\":[{\"type\":\"audio\",\"audio\":{\"artist\":\"Deftones\",\"id\":3483,\"owner_id\":1606,\"title\":\"Gauze\",\"duration\":281,\"url\":\"https:audio\",\"date\":1425013158,\"lyrics_id\":45597212,\"genre_id\":18,\"is_hq\":true,\"short_videos_allowed\":false,\"stories_allowed\":false,\"stories_cover_allowed\":false}}],\"conversation_message_id\":1415,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"},{\"type\":\"message_new\",\"object\":{\"date\":1645105994,\"from_id\":1606,\"id\":1435,\"out\":0,\"attachments\":[{\"type\":\"photo\",\"photo\":{\"album_id\":282858707,\"date\":1644582624,\"id\":457240244,\"owner_id\":1606,\"access_key\":\"321\",\"sizes\":[{\"height\":17,\"url\":\"https:photo\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"o\",\"width\":17}],\"text\":\"\"}}],\"conversation_message_id\":1399,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":19495,\"event_id\":\"123\"}]}"

json27 = 
  "{\"ts\":\"641\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211910,\"from_id\":1606,\"id\":1451,\"out\":0,\"attachments\":[{\"type\":\"audio\",\"audio\":{\"artist\":\"Deftones\",\"id\":3483,\"owner_id\":1606,\"title\":\"Gauze\",\"duration\":281,\"url\":\"https:audio\",\"date\":1425013158,\"lyrics_id\":45597212,\"genre_id\":18,\"is_hq\":true,\"short_videos_allowed\":false,\"stories_allowed\":false,\"stories_cover_allowed\":false}},{\"type\":\"photo\",\"photo\":{\"album_id\":282858707,\"date\":1644582624,\"id\":457240244,\"owner_id\":1606,\"access_key\":\"321\",\"sizes\":[{\"height\":17,\"url\":\"https:photo\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"o\",\"width\":17}],\"text\":\"\"}}],\"conversation_message_id\":1415,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json28 = 
  "{\"ts\":\"641\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211910,\"from_id\":1606,\"id\":1451,\"out\":0,\"attachments\":[{\"type\":\"audio\",\"audio\":{\"artist\":\"Deftones\",\"id\":3483,\"owner_id\":1606,\"title\":\"Gauze\",\"duration\":281,\"url\":\"https:audio\",\"date\":1425013158,\"lyrics_id\":45597212,\"genre_id\":18,\"is_hq\":true,\"short_videos_allowed\":false,\"stories_allowed\":false,\"stories_cover_allowed\":false}},{\"type\":\"photo\",\"photo\":{\"album_id\":282858707,\"date\":1644582624,\"id\":457240244,\"owner_id\":1606,\"access_key\":\"321\",\"sizes\":[{\"height\":17,\"url\":\"https:photo\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"o\",\"width\":17}],\"text\":\"\"}}],\"conversation_message_id\":1415,\"fwd_messages\":[],\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"hello\"},\"group_id\":1949,\"event_id\":\"123\"}]}"

json29 = 
  "{\"ts\":\"641\",\"updates\":[{\"type\":\"message_new\",\"object\":{\"date\":1645211910,\"from_id\":1606,\"id\":1451,\"out\":0,\"attachments\":[{\"type\":\"audio\",\"audio\":{\"artist\":\"Deftones\",\"id\":3483,\"owner_id\":1606,\"title\":\"Gauze\",\"duration\":281,\"url\":\"https:audio\",\"date\":1425013158,\"lyrics_id\":45597212,\"genre_id\":18,\"is_hq\":true,\"short_videos_allowed\":false,\"stories_allowed\":false,\"stories_cover_allowed\":false}},{\"type\":\"photo\",\"photo\":{\"album_id\":282858707,\"date\":1644582624,\"id\":457240244,\"owner_id\":1606,\"access_key\":\"321\",\"sizes\":[{\"height\":17,\"url\":\"https:photo\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:photo\",\"type\":\"o\",\"width\":17}],\"text\":\"\"}}],\"conversation_message_id\":1415,\"fwd_messages\":[],\"geo\":{\"coordinates\":{\"latitude\":69.409,\"longitude\":32.456},\"place\":{\"city\":\"anyCity\",\"country\":\"anyCountry\",\"title\":\"anyTitle\"},\"type\":\"point\"},\"important\":false,\"is_hidden\":false,\"peer_id\":1606,\"random_id\":0,\"text\":\"hello\"},\"group_id\":1949,\"event_id\":\"123\"}]}"
