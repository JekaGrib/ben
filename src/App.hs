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
import           Control.Monad.Catch

data TGBotException 
  = GetUpdatesException String
  | CheckGetUpdatesResponseException String
  | ConfirmUpdatesException String
  | CheckConfirmUpdatesResponseException String
  | SendMsgException String
  | CheckSendMessageResponseException String
  | StartAppGetUpdatesException String
  | StartAppCheckGetUpdatesResponseException String
  | StartAppConfirmUpdatesException String
  | ExtractException String
    deriving (Eq,Show)

instance Exception TGBotException 

data Handle m = Handle 
  { hConf            :: Config,
    hLog             :: LogHandle m,
    getUpdates       :: m LBS.ByteString,
    getShortUpdates  :: m LBS.ByteString,
    confirmUpdates   :: Int -> m LBS.ByteString,
    sendMessage      :: Int -> T.Text -> m LBS.ByteString,
    sendKeybWithMsg  :: Int -> Int -> T.Text -> m LBS.ByteString
    }

data Config = Config 
  { cStartN   :: Int,
    cBotToken :: String,
    cHelpMsg  :: String,
    cRepeatQ  :: String
    }

data OpenRepeat = OpenRepeat Int
                        deriving (Eq,Show)


startApp :: (Monad m, MonadCatch m) => Handle m -> m ()
startApp h = do
  logDebug (hLog h) ("Send request to getUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
  json <- getShortUpdates h `catch` (\e -> throwM $ StartAppGetUpdatesException $ show (e :: SomeException))
  case decode json of
      Nothing -> throwM $ StartAppCheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:" ++ show json
      Just (OkAnswer {ok = False}) -> throwM $ StartAppCheckGetUpdatesResponseException $ "Unsuccessful getUpdates request. Api response:" ++ show json
      Just (Answer True []) -> return ()
      Just _ -> do
        nextUpdate <- return (extractNextUpdate json) `catch` (\e -> throwM $ ExtractException $ show (e :: SomeException) 
                                                                                                 ++ "\nStartAppError.Can`t extract next update number from response: " 
                                                                                                 ++ show json ++ " to set offset to confirm updates")
        emptyJson <- confirmUpdates h nextUpdate `catch` (\e -> throwM $ StartAppConfirmUpdatesException $ show (e :: SomeException) ++ "\nWhen try to confirm old updates: " ++ show json ++ " with offset: " ++ show nextUpdate)
        logDebug (hLog h) ("Get response: " ++ show emptyJson ++ "\n")
        checkConfirmUpdatesResponse h nextUpdate json emptyJson
        return ()

run :: (Monad m, MonadCatch m)=> Handle m -> StateT [(Int , Either OpenRepeat Int)] m ()
run h = do
  lift $ logDebug (hLog h) ("Send request to getUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
  json <- lift (getUpdates h)
  lift $ logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  newJSON <- lift $ checkUpdates h json
  let upds = extractUpdates $ newJSON
  mapM (chooseAction h) upds
  return ()

chooseAction :: (Monad m, MonadCatch m)=> Handle m -> Update -> StateT [(Int , Either OpenRepeat Int)] m ()
chooseAction h upd = do
  case upd of
    (UnknownUpdate _) -> do return ()
    (Update _ _) -> do
      let msg = extractTextMsg $ upd
      let usId = extractUserId $ upd
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
              sendMsgResponse <- lift $ (sendMessage h) usId infoMsg
              lift $ checkSendMessageResponse h usId infoMsg sendMsgResponse
              return ()
            False -> do
              lift $ logDebug (hLog h) ("User " ++ show usId ++ " press unknown button, close OpenRepeat mode, leave old number of repeats: " ++ show oldN ++ "\n")
              modify (dom usId (Right oldN))
              let infoMsg = T.pack $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++ show oldN ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
              lift $ logDebug (hLog h) ("Send request to send message " ++ show infoMsg ++ " to userId " ++ show usId ++ "  : " ++ "https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
              sendMsgResponse <- lift $ (sendMessage h) usId infoMsg
              lift $ checkSendMessageResponse h usId infoMsg sendMsgResponse
              return ()
        _   -> do
          let currN = case lookup usId db of { Just (Right n) -> n ; Nothing -> cStartN (hConf h) }
          case msg of 
                "/help" -> do
                  let infoMsg = T.pack $ cHelpMsg (hConf h)
                  lift $ logDebug (hLog h) ("Send request to send message " ++ show infoMsg ++ " to userId " ++ show usId ++ "  : " ++ "https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
                  sendMsgResponse <- lift $ (sendMessage h) usId infoMsg
                  lift $ checkSendMessageResponse h usId infoMsg sendMsgResponse
                  return ()
                "/repeat" -> do
                  lift $ logDebug (hLog h) "SendKeyBoard\n"
                  lift $ (sendKeybWithMsg h) usId currN $ T.pack $ " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
                  lift $ logDebug (hLog h) ("Put user " ++ show usId ++ " to OpenRepeat mode\n")
                  modify (dom usId ( Left $ OpenRepeat currN ) )
                _ -> do
                  lift $ replicateM currN $ logDebug (hLog h) ("Send request to send message " ++ show msg ++ " to userId " ++ show usId ++ "  : " ++ "https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show msg ++ "}\n" )
                  sendMsgResponse <- lift $ replicateM currN $ sendMessage h usId msg
                  lift $ mapM (checkSendMessageResponse h usId msg) sendMsgResponse
                  return () 


checkUpdates :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m LBS.ByteString
checkUpdates h json = do
  case decode json of
      Nothing -> throwM $ CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:" ++ show json
      Just (OkAnswer {ok = False}) -> throwM $ CheckGetUpdatesResponseException $ "Unsuccessful getUpdates request. Api response:" ++ show json     
      Just (Answer True []) -> do
        logDebug (hLog h) ("Send request to getUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
        newJson <- getUpdates h
        logDebug (hLog h) ("Get response: " ++ show newJson ++ "\n")
        checkUpdates h newJson
      Just _                -> do
        logDebug (hLog h) ("Send request to confirmOldUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
        let nextUpdate =  extractNextUpdate $ json
        emptyJson <- confirmUpdates h nextUpdate
        logDebug (hLog h) ("Get response: " ++ show emptyJson ++ "\n")
        checkConfirmUpdatesResponse h nextUpdate json emptyJson
        return json

checkConfirmUpdatesResponse :: (Monad m, MonadCatch m) => Handle m -> Int -> LBS.ByteString -> LBS.ByteString -> m LBS.ByteString
checkConfirmUpdatesResponse h offset confirmedJson responseJson = do
  case decode responseJson of
      Nothing -> throwM $ CheckConfirmUpdatesResponseException $ "UNKNOWN RESPONSE:" ++ show responseJson ++ "\nUpdates: " ++ show confirmedJson ++ " PROBABLY NOT CONFIRM with offset: " ++ show offset 
      Just (OkAnswer {ok = False}) -> throwM $ CheckConfirmUpdatesResponseException $ "Updates: " ++ show confirmedJson ++ " NOT CONFIRM with offset: " ++ show offset ++ ". Api response:" ++ show responseJson

checkSendMessageResponse :: (Monad m, MonadCatch m) => Handle m -> Int -> T.Text -> LBS.ByteString -> m LBS.ByteString
checkSendMessageResponse h usId msg json = do
  case decode json of
      Nothing -> throwM $ CheckSendMessageResponseException $ "UNKNOWN RESPONSE:" ++ show json ++ "\nMESSAGE: \"" ++ show msg ++ "\" PROBABLY NOT SENT to user: " ++ show usId  
      Just (OkAnswer {ok = False}) -> throwM $ CheckSendMessageResponseException $ "MESSAGE: \"" ++ show msg ++ "\" NOT SENT to user: " ++ show usId ++ ". Api response:" ++ show json




getShortUpdates' :: Handle IO -> IO LBS.ByteString
getShortUpdates' h = do
  req <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  res <- httpLBS req
  return (getResponseBody res)

getUpdates' :: Handle IO -> IO LBS.ByteString
getUpdates' h = do
  let toon =  JSONBodyTimeOut {timeout = 20}
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS . encode $ toon), requestHeaders =
                    [ ("Content-Type", "application/json; charset=utf-8")
                    ]}
  res <- httpLBS req
  return (getResponseBody res)
          
confirmUpdates' :: Handle IO -> Int -> IO LBS.ByteString
confirmUpdates' h nextUpdate = do
  let bodyOffset =  JSONBodyOffset {offset = nextUpdate }
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS . encode $ bodyOffset), requestHeaders =
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

extractUpdates :: LBS.ByteString -> [Update]
extractUpdates = result . fromJust . decode

extractNewN :: LBS.ByteString -> Int
extractNewN = read . T.unpack . textMsg . message . last . result . fromJust . decode

extractTextMsg :: Update -> T.Text
extractTextMsg = textMsg . message 

extractUserId :: Update -> Int
extractUserId = idUser . fromUser . message 

dom :: Int -> Either OpenRepeat Int -> [(Int , Either OpenRepeat Int)] -> [(Int,Either OpenRepeat Int)]
dom usId eitherN bd = 
    case lookup usId bd of
        Just eitherX -> (:) (usId,eitherN) . delete (usId, eitherX) $ bd
        Nothing -> (:) (usId,eitherN) $ bd

checkButton :: T.Text -> Bool
checkButton text =
    case text of { "1" -> True ; "2" -> True ; "3" -> True ; "4" -> True ; "5" -> True ; _ -> False }



 
