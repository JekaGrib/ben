{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import           Api.Request
import           Api.Response
import           Logger
import qualified Data.Text                      as T
import           Network.HTTP.Simple            ( parseRequest, setRequestBody, getResponseBody, httpLBS )
import           Network.HTTP.Client.Conduit               
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State
import           Data.List
import           Prelude                        hiding (log)
import           Data.Aeson
import           Data.Maybe                     (fromJust)


data Handle m = Handle 
  { hConf            :: Config,
    hLog             :: LogHandle m,
    getUpdates       :: m LBS.ByteString,
    confirmUpdates   :: LBS.ByteString -> m LBS.ByteString,
    sendMessage      :: Int -> T.Text -> m LBS.ByteString,
    sendKeybWithMsg  :: Int -> Int -> T.Text -> m LBS.ByteString
    }

data Config = Config 
  { cStartN :: Int,
    cBotToken :: String
    }

data OpenRepeat = OpenRepeat Int
                        deriving (Eq,Show)


run :: Monad m => Handle m -> StateT [(Int , Either OpenRepeat Int)] m ()
run h = do
  lift $ logDebug (hLog h) ("Send request to getUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
  json <- lift (getUpdates h)
  lift $ logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  newJSON <- lift $ checkUpdates h json
  let msg = extractTextMsg $ newJSON
  let usId = extractUserId $ newJSON
  lift $ logDebug (hLog h) ("Update info: got message " ++ show msg ++ " from user " ++ show usId ++ "\n")
  db <- get
  case lookup usId db of 
    Just (Left (OpenRepeat oldN)) -> do
      lift $ logDebug (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode\n")
      case checkButton msg of
        True -> do
          let newN = read . T.unpack $ msg
          lift $ logDebug (hLog h) ("Change number of repeats to " ++ show newN ++ " for user " ++ show usId ++ "\n")
          modify (dom usId (Right newN))
          let infoMsg = T.pack $ "Number of repeats successfully changed from " ++ show oldN ++ " to " ++ show newN ++ "\n"
          lift $ logDebug (hLog h) ("Send request to send message " ++ show infoMsg ++ " to userId " ++ show usId ++ "  : " ++ "https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
          lift $ (sendMessage h) usId infoMsg
          return ()
        False -> do
          lift $ logDebug (hLog h) ("User " ++ show usId ++ " press unknown button, close OpenRepeat mode, leave old number of repeats: " ++ show oldN ++ "\n")
          modify (dom usId (Right oldN))
          let infoMsg = T.pack $ "UNKNOWN NUMBER\nSsory, number of repeats didn`t change, it is still " ++ show oldN ++ "\nTo change it you may send me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
          lift $ logDebug (hLog h) ("Send request to send message " ++ show infoMsg ++ " to userId " ++ show usId ++ "  : " ++ "https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
          lift $ (sendMessage h) usId infoMsg
          return ()
    _   -> do
      let currN = case lookup usId db of { Just (Right n) -> n ; Nothing -> cStartN (hConf h) }
      case msg of 
        "/help" -> do
          let helpMsg = "I`m super bot"
          lift $ logDebug (hLog h) ("Send request to send message " ++ show helpMsg ++ " to userId " ++ show usId ++ "  : " ++ "https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show helpMsg ++ "}\n" )
          lift $ (sendMessage h) usId helpMsg
          return ()
        "/repeat" -> do
          lift $ logDebug (hLog h) "SendKeyBoard\n"
          lift $ (sendKeybWithMsg h) usId currN " : Current number of repeats your message.\nHow many times to repeat message in the future?"
          lift $ logDebug (hLog h) ("Put user " ++ show usId ++ " to OpenRepeat mode\n")
          modify (dom usId ( Left $ OpenRepeat currN ) )
        _ -> do
          lift $ mapM (logDebug (hLog h)) . replicate currN $ ("Send request to send message " ++ show msg ++ " to userId " ++ show usId ++ "  : " ++ "https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show msg ++ "}\n" )
          lift $ mapM ((sendMessage h) usId) . replicate currN $ msg
          return () 


checkUpdates :: Monad m => Handle m -> LBS.ByteString -> m LBS.ByteString
checkUpdates h json = do
  case decode json of 
    Nothing -> undefined
    Just (Answer True []) -> do
      logDebug (hLog h) ("Send request to getUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
      newJson <- getUpdates h
      logDebug (hLog h) ("Get response: " ++ show newJson ++ "\n")
      checkUpdates h newJson
    Just _                -> do
      logDebug (hLog h) ("Send request to confirmOldUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
      emptyJson <- (confirmUpdates h) json
      logDebug (hLog h) ("Get response: " ++ show emptyJson ++ "\n")
      return json
 




getUpdates' :: Handle IO -> IO LBS.ByteString
getUpdates' h = do
  let toon =  JSONBodyTimeOut {timeout = 20}
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS . encode $ toon), requestHeaders =
                    [ ("Content-Type", "application/json; charset=utf-8")
                    ]}
  res <- httpLBS req
  return (getResponseBody res)
          
confirmUpdates' :: Handle IO -> LBS.ByteString -> IO LBS.ByteString
confirmUpdates' h json = do
  let tup =  extractNextUpdate $ json
  let toon =  JSONBodyOffset {offset = tup }
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS . encode $ toon), requestHeaders =
                    [ ("Content-Type", "application/json; charset=utf-8")
                    ]}
  res <- httpLBS req
  return (getResponseBody res)

sendMessage' :: Handle IO -> Int -> T.Text -> IO LBS.ByteString
sendMessage' h usId msg = do
  let message = encode (SendMsgJSONBody {chat_id = usId, text = msg})
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS $ message), requestHeaders =
                     [ ("Content-Type", "application/json; charset=utf-8")
                     ]}
  res <- httpLBS req
  return (getResponseBody res)

sendKeybWithMsg' :: Handle IO -> Int -> Int -> T.Text-> IO LBS.ByteString
sendKeybWithMsg' h usId n msg = do 
  let loon =  KeybJSONBody {chat_idKeyb  = usId, 
                            textKeyb = T.concat [(T.pack . show $ n), msg],
                            reply_markup = KeyBoard { keyboard = [[KeyButton {textBtn = "1"}],
                                                                  [KeyButton {textBtn = "2"}], 
                                                                  [KeyButton {textBtn = "3"}], 
                                                                  [KeyButton {textBtn = "4"}], 
                                                                  [KeyButton {textBtn = "5"}]
                                                                 ],
                                                      one_time_keyboard = True
                                                    }
                           }
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage")
  let req = initReq { method = "POST",
                      requestBody = (RequestBodyLBS . encode $ loon), 
                      requestHeaders = [ ("Content-Type", "application/json; charset=utf-8")]
                    }
  res <- httpLBS req
  return (getResponseBody res)



extractNextUpdate :: LBS.ByteString -> Int
extractNextUpdate = succ . update_id . last . result . fromJust . decode

extractNewN :: LBS.ByteString -> Int
extractNewN = read . T.unpack . textMsg . message . last . result . fromJust . decode

extractTextMsg :: LBS.ByteString -> T.Text
extractTextMsg = textMsg . message . last . result . fromJust . decode

extractUserId :: LBS.ByteString -> Int
extractUserId = idUser . fromUser . message . last . result . fromJust . decode

dom :: Int -> Either OpenRepeat Int -> [(Int , Either OpenRepeat Int)] -> [(Int,Either OpenRepeat Int)]
dom usId eitherN bd = 
    case lookup usId bd of
        Just eitherX -> (:) (usId,eitherN) . delete (usId, eitherX) $ bd
        Nothing -> (:) (usId,eitherN) $ bd

checkButton :: T.Text -> Bool
checkButton text =
    case text of { "1" -> True ; "2" -> True ; "3" -> True ; "4" -> True ; "5" -> True ; _ -> False }



 
