{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

module Spec.Vk.PrepareAttachment.ResponseExample where

import Data.String (fromString)
import Vk.Types (Response)

picUploadUrl, docUploadUrl :: String
picUploadUrl = "http://toLoadPic"
docUploadUrl = "http://toLoadDoc"

docId, ownerId :: Int
docId = 25
ownerId = 50

json1, json2, json3, json4, json5, json6, json7, json8, json9, json10 :: Response
json1 = "{\"blabla\":\"blabla\"}"
json2 =
  fromString $
    "{\"response\":{\"album_id\":-64,\"upload_url\":\"" ++ picUploadUrl ++ "\",\"user_id\":0,\"group_id\":194952914}}"
json3 = "{\"server\":24,\"photo\":\"anyPhotoSring\",\"hash\":\"anyHash\"}"
json4 =
  fromString $
    "{\"response\":[{\"album_id\":-64,\"date\":1644999562,\"id\":" ++ show docId ++ ",\"owner_id\":" ++ show ownerId ++ ",\"access_key\":\"35546f916f1affb66f\",\"sizes\":[{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"s\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"m\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"x\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"o\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"p\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"q\",\"width\":17},{\"height\":17,\"url\":\"https:\\/\\/sun9-22.userapi.com\\/impg\\/ER3jufI7WnQFpNJQhXYS11YV5iMEoiSwoKMztA\\/i5nVVocRb3Q.jpg?size=17x17&quality=96&sign=b6d70b2d3c4dbf5cad94d0c403fabe28&c_uniq_tag=N-LR7V66BK7KhSBdGo-k1mmUhCiUDWQaetjwFPdsIFw&type=album\",\"type\":\"r\",\"width\":17}],\"text\":\"\"}]}"
json5 =
  fromString $
    "{\"response\":{\"upload_url\":\"" ++ docUploadUrl ++ "\"}}"
json6 =
  "{\"file\":\"anyFile\"}"
json7 =
  fromString $
    "{\"response\":{\"type\":\"doc\",\"doc\":{\"id\":" ++ show docId ++ ",\"owner_id\":" ++ show ownerId ++ ",\"title\":\"car.sql\",\"size\":88583,\"ext\":\"sql\",\"date\":1644999586,\"type\":8,\"url\":\"https:\\/\\/vk.com\\/doc16063921_626276949?hash=977b8afbe994b8c09d&dl=FUYTSNBZGUZDSMJU:1644999586:4ca2a1750bf9f6403d&api=1&no_preview=1\"}}}"
json8 =
  fromString $
    "{\"response\":{\"type\":\"audio_message\",\"audio_message\":{\"duration\":2,\"id\":" ++ show docId ++ ",\"link_mp3\":\"https:\\/\\/psv4.userapi.com\\/c613501\\/\\/u16063921\\/audiomsg\\/d1\\/822afd1f8c.mp3\",\"link_ogg\":\"https:\\/\\/psv4.userapi.com\\/c613501\\/\\/u16063921\\/audiomsg\\/d1\\/822afd1f8c.ogg\",\"owner_id\":" ++ show ownerId ++ ",\"access_key\":\"64f3c82e314bbe46fd\",\"waveform\":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,2,1,1,1,1,1,1,0,0,0,0]}}}"
json9 =
  fromString $
    "{\"response\":{\"upload_url\":\"" ++ docUploadUrl ++ "\"},\"blabla\":\"blabla\"}"
json10 =
  fromString $
    "{\"response\":{\"upload_url\":\"" ++ picUploadUrl ++ "\"}}"

json11, json12, json13, json14 :: Response
json11 =
  fromString $
    "{\"response\":[{\"id\":" ++ show docId ++ ",\"owner_id\":" ++ show ownerId ++ "}]}"
json12 =
  fromString $
    "{\"response\":{\"blabla\":\"blabla\",\"album_id\":\"blabla\",\"upload_url\":\"" ++ picUploadUrl ++ "\",\"user_id\":-25}}"
json13 =
  "{\"server\":24,\"photo\":\"anyPhotoSring\",\"hash\":\"anyHash\",\"blabla\":\"blabla\"}"
json14 =
  fromString $
    "{\"response\":[{\"blabla\":\"blabla\",\"album_id\":\"blabla\",\"id\":" ++ show docId ++ ",\"owner_id\":" ++ show ownerId ++ ",\"access_key\":25}]}"
