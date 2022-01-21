{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.App where

import           Tg.Api.Request                 (JSONBodyTimeOut(..), JSONBodyOffset(..), SendMsgJSONBody(..), CopyMsgJSONBody(..), KeybJSONBody(..), KeyBoard(..), KeyButton(..))
import           Tg.Api.Response                (Answer(..), Update(..), Message(..), From(..))
import           Tg.Logger                      (LogHandle(..), logDebug, logInfo, logWarning, logError)
import           Tg.Oops
import qualified Data.Text                      as T
import           Network.HTTP.Client            (parseRequest, responseBody, httpLbs, method, requestBody, requestHeaders, RequestBody(RequestBodyLBS) )
import           Network.HTTP.Client.TLS        (newTlsManager)
import qualified Data.ByteString.Lazy           as LBS
import           Control.Monad.State            (StateT, lift, modify, replicateM_, get)
import           Data.List                      (delete)
import           Data.Aeson                     (decode, encode)
import           Data.Maybe                     (fromJust)
import           Control.Monad.Catch            (MonadCatch(..), throwM)


data Handle m = Handle 
  { hConf            :: Config,
    hLog             :: LogHandle m,
    getUpdates       :: m LBS.ByteString,
    getShortUpdates  :: m LBS.ByteString,
    confirmUpdates   :: Integer -> m LBS.ByteString,
    sendMsg          :: Integer -> T.Text -> m LBS.ByteString,
    sendKeyb         :: Integer -> Int -> T.Text -> m LBS.ByteString,
    copyMsg          :: Integer -> Integer -> m LBS.ByteString
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
  json <- getShortUpdates h `catch` handleExGetUpd (hLog h)
  logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  checkAndConfirmUpdates h json

run :: (Monad m, MonadCatch m)=> Handle m -> StateT [(Integer , Either OpenRepeat Int)] m ()
run h = do
  lift $ logDebug (hLog h) ("Send request to getUpdates: https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates\n" )
  json <- lift $ (getUpdates h) `catch` handleExGetUpd (hLog h)
  lift $ logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  lift $ checkAndConfirmUpdates h json
  let upds = extractUpdates $ json
  mapM_ (chooseActionOfUpd h) upds

chooseActionOfUpd :: (Monad m, MonadCatch m) => Handle m -> Update -> StateT [(Integer , Either OpenRepeat Int)] m ()
chooseActionOfUpd h upd = do
  lift $ logInfo (hLog h) ("Analysis update from the list\n")
  case upd of
    UnknownUpdate _ -> do 
      lift $ logWarning (hLog h) ("There is UNKNOWN UPDATE. Bot will ignore it\n")
      return ()
    Update _ msg -> do
      let msgId = message_id msg
      let usId = extractUserId $ upd
      lift $ logInfo (hLog h) ("Get msg_id: " ++ show msgId ++ " from user " ++ show usId ++ "\n")
      chooseActionOfDbState h msg msgId usId
      
chooseActionOfDbState :: (Monad m, MonadCatch m) => Handle m -> Message -> Integer -> Integer -> StateT [(Integer , Either OpenRepeat Int)] m ()
chooseActionOfDbState h msg msgId usId = do
  db <- get
  let nState = lookup usId db
  case nState of 
    Just (Left (OpenRepeat oldN)) -> do
      lift $ logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode\n")
      chooseActionOfButton h msg usId oldN  
    Just (Right n) -> do
      let currN = n 
      chooseActionOfTryPullTxt h msg msgId usId currN
    Nothing -> do
      let currN = cStartN (hConf h)
      chooseActionOfTryPullTxt h msg msgId usId currN

chooseActionOfTryPullTxt :: (Monad m, MonadCatch m) => Handle m -> Message -> Integer -> Integer -> Int -> StateT [(Integer , Either OpenRepeat Int)] m ()
chooseActionOfTryPullTxt h msg msgId usId currN = case pullTextMsg msg of 
  Just txt -> do
    lift $ logInfo (hLog h) ("Msg_id:" ++ show msgId ++ " is text: " ++ show txt ++ "\n")
    chooseActionOfTxt h currN usId txt
  Nothing  -> do
    lift $ logInfo (hLog h) ("Msg_id:" ++ show msgId ++ " is attachment\n")
    let logMsg = "Send request to send attachment msg_id: " ++ show msgId ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/copyMessage   JSON body : {chat_id = " ++ show usId ++ ",from_chat_id = " ++ show usId ++ ", message_id = " ++ show msgId ++ "}\n"
    lift $ replicateM_ currN $ do
      logDebug (hLog h) logMsg
      response <- copyMsg h usId msgId `catch` handleExCopyMsg (hLog h) usId msgId
      logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
      checkCopyMsgResponse h usId msgId response

chooseActionOfButton :: (Monad m, MonadCatch m) => Handle m -> Message -> Integer -> Int -> StateT [(Integer , Either OpenRepeat Int)] m ()
chooseActionOfButton h msg usId oldN = case checkButton msg of
  Just newN -> do
    lift $ logInfo (hLog h) ("Change number of repeats to " ++ show newN ++ " for user " ++ show usId ++ "\n")
    modify (changeDB usId (Right newN))
    let infoMsg = T.pack $ "Number of repeats successfully changed from " ++ show oldN ++ " to " ++ show newN ++ "\n"
    lift $ logDebug (hLog h) ("Send request to send msg " ++ show infoMsg ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
    response <- lift $ (sendMsg h) usId infoMsg `catch` handleExSendMsg (hLog h) usId infoMsg
    lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
    lift $ checkSendMsgResponse h usId infoMsg response 
  Nothing -> do
    lift $ logWarning (hLog h) ("User " ++ show usId ++ " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++ show oldN ++ "\n")
    modify (changeDB usId (Right oldN))
    let infoMsg = T.pack $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++ show oldN ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
    lift $ logDebug (hLog h) ("Send request to send msg " ++ show infoMsg ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
    response <- lift $ (sendMsg h) usId infoMsg `catch` handleExSendMsg (hLog h) usId infoMsg
    lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
    lift $ checkSendMsgResponse h usId infoMsg response 

chooseActionOfTxt :: (Monad m, MonadCatch m) => Handle m -> Int -> Integer -> T.Text -> StateT [(Integer , Either OpenRepeat Int)] m ()
chooseActionOfTxt h currN usId txt = case filter ((/=) ' ') . T.unpack $ txt of
  "/help" -> do
    let infoMsg = T.pack $ cHelpMsg (hConf h)
    lift $ logDebug (hLog h) ("Send request to send msg " ++ show infoMsg ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show infoMsg ++ "}\n" )
    response <- lift $ (sendMsg h) usId infoMsg `catch` handleExSendMsg (hLog h) usId infoMsg
    lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
    lift $ checkSendMsgResponse h usId infoMsg response
  "/repeat" -> do
    let infoMsg = T.pack $ " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
    lift $ logDebug (hLog h) $ "Send request to send keyboard with message: " ++ show currN ++ show infoMsg ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage\n" 
    keybResponse <- lift $ (sendKeyb h) usId currN infoMsg `catch` handleExSendKeyb (hLog h) usId
    lift $ logDebug (hLog h) ("Get response: " ++ show keybResponse ++ "\n")
    lift $ checkSendKeybResponse h usId currN infoMsg keybResponse
    lift $ logInfo (hLog h) ("Put user " ++ show usId ++ " to OpenRepeat mode\n")
    modify (changeDB usId ( Left $ OpenRepeat currN ) )
  _ -> do
    let logMsg = "Send request to send msg " ++ show txt ++ " to userId " ++ show usId ++ ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage   JSON body : {chat_id = " ++ show usId ++ ", text = " ++ show txt ++ "}\n"
    lift $ replicateM_ currN $ do
      logDebug (hLog h) logMsg
      response <- sendMsg h usId txt `catch` handleExSendMsg (hLog h) usId txt
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
        emptyJson <- confirmUpdates h nextUpdate `catch` handleExConfUpd (hLog h) json
        logDebug (hLog h) ("Get response: " ++ show emptyJson ++ "\n")
        checkConfirmUpdatesResponse h nextUpdate json emptyJson
        

checkConfirmUpdatesResponse :: (Monad m, MonadCatch m) => Handle m -> Integer -> LBS.ByteString -> LBS.ByteString -> m ()
checkConfirmUpdatesResponse h offsetArg confirmedJson responseJson = do
  case decode responseJson of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to confirmUpdates:\n" ++ show responseJson 
        throwM $ CheckConfirmUpdatesResponseException $ "UNKNOWN RESPONSE:\n" ++ show responseJson ++ "\nUpdates: \n" ++ show confirmedJson ++ "\nPROBABLY NOT CONFIRM with offset: " ++ show offsetArg 
      Just (OkAnswer {ok = False}) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to confirmUpdates:\n" ++ show responseJson
        throwM $ CheckConfirmUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show responseJson ++ "\nUpdates: \n" ++ show confirmedJson ++ "\nNOT CONFIRM with offset: " ++ show offsetArg
      Just (Answer False _) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to confirmUpdates:\n" ++ show responseJson
        throwM $ CheckConfirmUpdatesResponseException $ "NEGATIVE RESPONSE:\n" ++ show responseJson ++ "\nUpdates: \n" ++ show confirmedJson ++ "\nNOT CONFIRM with offset: " ++ show offsetArg  
      Just _                       -> do
        logInfo (hLog h) ("Received updates confirmed\n" )

checkSendMsgResponse :: (Monad m, MonadCatch m) => Handle m -> Integer -> T.Text -> LBS.ByteString -> m ()
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

checkCopyMsgResponse :: (Monad m, MonadCatch m) => Handle m -> Integer -> Integer -> LBS.ByteString -> m ()
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

checkSendKeybResponse :: (Monad m, MonadCatch m) => Handle m -> Integer -> Int -> T.Text -> LBS.ByteString -> m ()
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
  manager <- newTlsManager 
  req <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  res <- httpLbs req manager
  return (responseBody res)

getUpdates' :: Handle IO -> IO LBS.ByteString
getUpdates' h = do
  let toon =  JSONBodyTimeOut {timeout = 25}
  manager <- newTlsManager 
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS . encode $ toon), requestHeaders =
                    [ ("Content-Type", "application/json; charset=utf-8")
                    ]}
  res <- httpLbs req manager
  return (responseBody res)
          
confirmUpdates' :: Handle IO -> Integer -> IO LBS.ByteString
confirmUpdates' h nextUpdate = do
  let bodyOffset =  JSONBodyOffset {offset = nextUpdate }
  manager <- newTlsManager 
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS . encode $ bodyOffset), requestHeaders =
                    [ ("Content-Type", "application/json; charset=utf-8")
                    ]}
  res <- httpLbs req manager
  return (responseBody res)

sendMsg' :: Handle IO -> Integer -> T.Text -> IO LBS.ByteString
sendMsg' h usId msg = do
  let msgBody = encode (SendMsgJSONBody {chat_id = usId, text = msg})
  manager <- newTlsManager 
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS $ msgBody), requestHeaders =
                     [ ("Content-Type", "application/json; charset=utf-8")
                     ]}
  res <- httpLbs req manager
  return (responseBody res)

copyMsg' :: Handle IO -> Integer -> Integer -> IO LBS.ByteString
copyMsg' h usId msgId = do
  let msgBody = encode (CopyMsgJSONBody {chat_idCM = usId, from_chat_idCM = usId, msg_idCM = msgId})
  manager <- newTlsManager 
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/copyMessage")
  let req = initReq { method = "POST", requestBody = (RequestBodyLBS $ msgBody), requestHeaders =
                     [ ("Content-Type", "application/json; charset=utf-8")
                     ]}
  res <- httpLbs req manager
  return (responseBody res)

sendKeyb' :: Handle IO -> Integer -> Int -> T.Text-> IO LBS.ByteString
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
  manager <- newTlsManager 
  initReq <- parseRequest ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage")
  let req = initReq { method = "POST",
                      requestBody = (RequestBodyLBS . encode $ keybBody), 
                      requestHeaders = [ ("Content-Type", "application/json; charset=utf-8")]
                    }
  res <- httpLbs req manager
  return (responseBody res)



extractNextUpdate :: LBS.ByteString -> Integer
extractNextUpdate = succ . update_id . last . result . fromJust . decode

extractUpdates :: LBS.ByteString -> [Update]
extractUpdates = result . fromJust . decode

extractTextMsg :: Update -> T.Text
extractTextMsg = textMsg . message 

extractUserId :: Update -> Integer
extractUserId = idUser . fromUser . message 

changeDB :: Integer -> Either OpenRepeat Int -> [(Integer , Either OpenRepeat Int)] -> [(Integer, Either OpenRepeat Int)]
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
checkTextButton txt =
    case txt of 
      { "1" -> Just 1 ; "2" -> Just 2 ; "3" -> Just 3 ; "4" -> Just 4 ; "5" -> Just 5 ; _ -> Nothing }

pullTextMsg :: Message -> Maybe T.Text
pullTextMsg (TxtMessage _ _ _ _ txt) = Just txt
pullTextMsg _ = Nothing


 
