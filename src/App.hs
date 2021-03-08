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
import qualified Control.Exception              as E

data Msg           = Msg        T.Text            deriving (Eq,Show)
data ToUserId      = ToUserId   Int               deriving (Eq,Show)
data MsgId         = MsgId      Int               deriving (Eq,Show)


data TGBotException 
  = DuringGetUpdatesException String
  | CheckGetUpdatesResponseException String
  | DuringConfirmUpdatesException String
  | CheckConfirmUpdatesResponseException String
  | DuringSendMsgException Msg ToUserId String
  | DuringCopyMsgException MsgId ToUserId String
  | CheckSendMsgResponseException Msg ToUserId String
  | CheckCopyMsgResponseException MsgId ToUserId String
  | DuringSendKeybException ToUserId String
  | CheckSendKeybResponseException ToUserId String
  | DuringGetTimeException String
  | DuringPullConfigException String
  | DuringParseConfigException String
    deriving (Eq,Show)

instance Exception TGBotException 

data Handle m = Handle 
  { hConf            :: Config,
    hLog             :: LogHandle m,
    getUpdates       :: m LBS.ByteString,
    getShortUpdates  :: m LBS.ByteString,
    confirmUpdates   :: Int -> m LBS.ByteString,
    sendMsg          :: Int -> T.Text -> m LBS.ByteString,
    sendKeyb         :: Int -> Int -> T.Text -> m LBS.ByteString,
    copyMsg          :: Int -> Int -> m LBS.ByteString
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
  logInfo (hLog h) ("App started\n")
  logDebug (hLog h) ("Send request to getUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
  json <- getShortUpdates h `catch` (\e -> do
                                logError (hLog h) $ show e ++ " GetUpdates fail at startApp\n"
                                throwM $ DuringGetUpdatesException $ "Error at StartApp. " ++ show (e :: SomeException))
  logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  checkAndConfirmUpdates h json

run :: (Monad m, MonadCatch m)=> Handle m -> StateT [(Int , Either OpenRepeat Int)] m ()
run h = do
  lift $ logDebug (hLog h) ("Send request to getUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
  json <- lift $ (getUpdates h) `catch` (\e -> do
                                  logError (hLog h) $ show e ++ " GetUpdates fail\n"
                                  throwM $ DuringGetUpdatesException $ show (e :: SomeException))
  lift $ logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  lift $ checkAndConfirmUpdates h json
  let upds = extractUpdates $ json
  mapM_ (chooseActionOfUpd h) upds

chooseActionOfUpd :: (Monad m, MonadCatch m)=> Handle m -> Update -> StateT [(Int , Either OpenRepeat Int)] m ()
chooseActionOfUpd h upd = do
  lift $ logInfo (hLog h) ("Analysis update from the list\n")
  case upd of
    UnknownUpdate _ -> do 
      lift $ logWarning (hLog h) ("There is UNKNOWN UPDATE. Bot will ignore it\n")
      return ()
    Update upId msg -> do
      let msgId = message_id msg
      let usId = extractUserId $ upd
      lift $ logInfo (hLog h) ("Get msg_id: " ++ show msgId ++ " from user " ++ show usId ++ "\n")
      db <- get
      case lookup usId db of 
        Just (Left (OpenRepeat oldN)) -> do
          lift $ logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode\n")
          case checkButton msg of
            Just newN -> do
              lift $ logInfo (hLog h) ("Change number of repeats to " ++ show newN ++ " for user " ++ show usId ++ "\n")
              modify (changeDB usId (Right newN))
              let infoMsg = T.pack $ "Number of repeats successfully changed from " ++ show oldN ++ " to " ++ show newN ++ "\n"
              lift $ logDebug (hLog h) ("Send request to send msg " ++ show infoMsg ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
              response <- lift $ (sendMsg h) usId infoMsg `catch` (\e -> do
                                    logError (hLog h) $ show e ++ " SendMessage fail\n"    
                                    throwM $ DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show (e :: SomeException))
              lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
              lift $ checkSendMsgResponse h usId infoMsg response 
            Nothing -> do
              lift $ logWarning (hLog h) ("User " ++ show usId ++ " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++ show oldN ++ "\n")
              modify (changeDB usId (Right oldN))
              let infoMsg = T.pack $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++ show oldN ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
              lift $ logDebug (hLog h) ("Send request to send msg " ++ show infoMsg ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
              response <- lift $ (sendMsg h) usId infoMsg `catch` (\e -> do
                                     logError (hLog h) $ show e ++ " SendMessage fail\n"
                                     throwM $ DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show (e :: SomeException))
              lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
              lift $ checkSendMsgResponse h usId infoMsg response 
        _   -> do
          let currN = case lookup usId db of { Just (Right n) -> n ; Nothing -> cStartN (hConf h) }
          case pullTextMsg msg of 
            Just txt -> do
              lift $ logInfo (hLog h) ("Msg_id:" ++ show msgId ++ " is text: " ++ show txt ++ "\n")
              chooseActionOfTxt h currN usId txt
            Nothing  -> do
              lift $ logInfo (hLog h) ("Msg_id:" ++ show msgId ++ " is attachment\n")
              let logMsg = "Send request to send attachment msg_id: " ++ show msgId ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/copyMessage   JSON body : {chat_id = " ++ show usId ++ ",from_chat_id = " ++ show usId ++ ", message_id = " ++ show msgId ++ "}\n"
              lift $ replicateM_ currN $ do
                logDebug (hLog h) logMsg
                response <- copyMsg h usId msgId `catch` (\e -> do
                                      logError (hLog h) $ show e ++ " copyMessage fail\n"
                                      throwM $ DuringCopyMsgException (MsgId msgId) (ToUserId usId) $ show (e :: SomeException))
                logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
                checkCopyMsgResponse h usId msgId response
                    
chooseActionOfTxt :: (Monad m, MonadCatch m) => Handle m -> Int -> Int -> T.Text -> StateT [(Int , Either OpenRepeat Int)] m ()
chooseActionOfTxt h currN usId txt = case filter ((/=) ' ') . T.unpack $ txt of
  "/help" -> do
    let infoMsg = T.pack $ cHelpMsg (hConf h)
    lift $ logDebug (hLog h) ("Send request to send msg " ++ show infoMsg ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
    response <- lift $ (sendMsg h) usId infoMsg `catch` (\e -> do
                            logError (hLog h) $ show e ++ " SendMessage fail\n"            
                            throwM $ DuringSendMsgException (Msg infoMsg) (ToUserId usId) $ show (e :: SomeException))
    lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
    lift $ checkSendMsgResponse h usId infoMsg response
  "/repeat" -> do
    let infoMsg = T.pack $ " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
    lift $ logDebug (hLog h) $ "Send request to send keyboard with message: " ++ show currN ++ show infoMsg ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage\n" 
    keybResponse <- lift $ (sendKeyb h) usId currN infoMsg `catch` (\e -> do
                            logError (hLog h) $ show e ++ " SendKeyb fail\n" 
                            throwM $ DuringSendKeybException (ToUserId usId) $ show (e :: SomeException))
    lift $ logDebug (hLog h) ("Get response: " ++ show keybResponse ++ "\n")
    lift $ checkSendKeybResponse h usId currN infoMsg keybResponse
    lift $ logInfo (hLog h) ("Put user " ++ show usId ++ " to OpenRepeat mode\n")
    modify (changeDB usId ( Left $ OpenRepeat currN ) )
  _ -> do
    let logMsg = "Send request to send msg " ++ show txt ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show txt ++ "}\n"
    lift $ replicateM_ currN $ do
      logDebug (hLog h) logMsg
      response <- sendMsg h usId txt `catch` (\e -> do
                            logError (hLog h) $ show e ++ " SendMessage fail\n"
                            throwM $ DuringSendMsgException (Msg txt) (ToUserId usId) $ show (e :: SomeException))
      logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
      checkSendMsgResponse h usId txt response


checkAndConfirmUpdates :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m ()
checkAndConfirmUpdates h json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
      Just (OkAnswer {ok = False}) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n"  ++ show json
      Just (Answer False _) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n"  ++ show json      
      Just (OkAnswer True)         -> do
        logError (hLog h) $ "Too short response to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "Too short response:\n" ++ show json
      Just (Answer True [])        -> do
        logInfo (hLog h) ("No new updates\n")
      Just _                       -> do
        logInfo (hLog h) ("There is new updates list\n" )
        let nextUpdate =  extractNextUpdate $ json
        logDebug (hLog h) ("Send request to confirmOldUpdates with offset:" ++ show nextUpdate ++ " https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
        emptyJson <- confirmUpdates h nextUpdate `catch` (\e -> do
                                           logError (hLog h) $ show e ++ " ConfirmUpdates fail"
                                           throwM $ DuringConfirmUpdatesException $ show (e :: SomeException) ++ "\nWhen try to confirm old updates: " ++ show json )
        logDebug (hLog h) ("Get response: " ++ show emptyJson ++ "\n")
        checkConfirmUpdatesResponse h nextUpdate json emptyJson
        

checkConfirmUpdatesResponse :: (Monad m, MonadCatch m) => Handle m -> Int -> LBS.ByteString -> LBS.ByteString -> m ()
checkConfirmUpdatesResponse h offset confirmedJson responseJson = do
  case decode responseJson of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to confirmUpdates:\n" ++ show responseJson 
        throwM $ CheckConfirmUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show responseJson ++ "\nUpdates: \n" ++ show confirmedJson ++ "\nPROBABLY NOT CONFIRM with offset: " ++ show offset 
      Just (OkAnswer {ok = False}) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to confirmUpdates:\n" ++ show responseJson
        throwM $ CheckConfirmUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show responseJson ++ "\nUpdates: \n" ++ show confirmedJson ++ "\nNOT CONFIRM with offset: " ++ show offset
      Just (Answer False _) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to confirmUpdates:\n" ++ show responseJson
        throwM $ CheckConfirmUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show responseJson ++ "\nUpdates: \n" ++ show confirmedJson ++ "\nNOT CONFIRM with offset: " ++ show offset  
      Just _                       -> do
        logInfo (hLog h) ("Received updates confirmed\n" )

checkSendMsgResponse :: (Monad m, MonadCatch m) => Handle m -> Int -> T.Text -> LBS.ByteString -> m ()
checkSendMsgResponse h usId msg json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to sendMessage:\n" ++ show json
        throwM $ CheckSendMsgResponseException (Msg msg) (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"  
      Just (OkAnswer {ok = False}) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to sendMessage:\n" ++ show json
        throwM $ CheckSendMsgResponseException (Msg msg) (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
      Just (Answer False _) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to sendMessage:\n" ++ show json
        throwM $ CheckSendMsgResponseException (Msg msg) (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
      Just _                       -> do
        logInfo (hLog h) ("Msg " ++ show msg ++ " was sent to user " ++ show usId ++ "\n")

checkCopyMsgResponse :: (Monad m, MonadCatch m) => Handle m -> Int -> Int -> LBS.ByteString -> m ()
checkCopyMsgResponse h usId msgId json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to copyMessage:\n" ++ show json
        throwM $ CheckCopyMsgResponseException (MsgId msgId) (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"  
      Just (OkAnswer {ok = False}) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to copyMessage:\n" ++ show json
        throwM $ CheckCopyMsgResponseException (MsgId msgId) (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
      Just (Answer False _) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to copyMessage:\n" ++ show json
        throwM $ CheckCopyMsgResponseException (MsgId msgId) (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
      Just _                       -> do
        logInfo (hLog h) ("Attachment msg_id: " ++ show msgId ++ " was sent to user " ++ show usId ++ "\n")

checkSendKeybResponse :: (Monad m, MonadCatch m) => Handle m -> Int -> Int -> T.Text -> LBS.ByteString -> m ()
checkSendKeybResponse h usId n msg json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to sendKeyboard:\n" ++ show json
        throwM $ CheckSendKeybResponseException (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nKEYBOARD PROBABLY NOT SENT"  
      Just (OkAnswer {ok = False}) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to sendKeyboard:\n" ++ show json
        throwM $ CheckSendKeybResponseException (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nKEYBOARD NOT SENT"
      Just (Answer False _) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to sendKeyboard:\n" ++ show json
        throwM $ CheckSendKeybResponseException (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nKEYBOARD NOT SENT"
      Just _                       -> do
        logInfo (hLog h) ("Keyboard with message: " ++ show n ++ show msg ++ " was sent to user " ++ show usId ++ "\n")




getShortUpdates' :: Handle IO -> IO LBS.ByteString
getShortUpdates' h = do
  req <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  res <- httpLBS req
  return (getResponseBody res)

getUpdates' :: Handle IO -> IO LBS.ByteString
getUpdates' h = do
  let toon =  JSONBodyTimeOut {timeout = 25}
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

sendMsg' :: Handle IO -> Int -> T.Text -> IO LBS.ByteString
sendMsg' h usId msg = do
  let msgBody = encode (SendMsgJSONBody {chat_id = usId, text = msg})
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS $ msgBody), requestHeaders =
                     [ ("Content-Type", "application/json; charset=utf-8")
                     ]}
  res <- httpLBS req
  return (getResponseBody res)

copyMsg' :: Handle IO -> Int -> Int -> IO LBS.ByteString
copyMsg' h usId msgId = do
  let msgBody = encode (CopyMsgJSONBody {chat_idCM = usId, from_chat_idCM = usId, msg_idCM = msgId})
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/copyMessage")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS $ msgBody), requestHeaders =
                     [ ("Content-Type", "application/json; charset=utf-8")
                     ]}
  res <- httpLBS req
  return (getResponseBody res)

sendKeyb' :: Handle IO -> Int -> Int -> T.Text-> IO LBS.ByteString
sendKeyb' h usId n msg = do 
  let keybBody =  KeybJSONBody {chat_idKeyb  = usId, 
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
                      requestBody = (RequestBodyLBS . encode $ keybBody), 
                      requestHeaders = [ ("Content-Type", "application/json; charset=utf-8")]
                    }
  res <- httpLBS req
  return (getResponseBody res)



extractNextUpdate :: LBS.ByteString -> Int
extractNextUpdate = succ . update_id . last . result . fromJust . decode

extractUpdates :: LBS.ByteString -> [Update]
extractUpdates = result . fromJust . decode

extractTextMsg :: Update -> T.Text
extractTextMsg = textMsg . message 

extractUserId :: Update -> Int
extractUserId = idUser . fromUser . message 

changeDB :: Int -> Either OpenRepeat Int -> [(Int , Either OpenRepeat Int)] -> [(Int,Either OpenRepeat Int)]
changeDB usId eitherN bd = 
    case lookup usId bd of
        Just eitherX -> (:) (usId,eitherN) . delete (usId, eitherX) $ bd
        Nothing -> (:) (usId,eitherN) $ bd


checkButton :: Message -> Maybe Int
checkButton msg =
  case pullTextMsg msg of
    Just txt -> checkTextButton txt
    Nothing  -> Nothing

checkTextButton :: T.Text -> Maybe Int
checkTextButton text =
    case text of 
      { "1" -> Just 1 ; "2" -> Just 2 ; "3" -> Just 3 ; "4" -> Just 4 ; "5" -> Just 5 ; _ -> Nothing }

{-isTextMsg :: Message -> Bool
isTextMsg (TxtMessage _ _ _ _ _) = True
isTextMsg _ = False-}

pullTextMsg :: Message -> Maybe T.Text
pullTextMsg (TxtMessage _ _ _ _ txt) = Just txt
pullTextMsg _ = Nothing


 
