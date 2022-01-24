{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Vk.App where

import           Vk.Logger
import           Vk.Api.Response
import           Vk.Api.Request
import           Network.HTTP.Client            (urlEncodedBody, parseRequest, responseBody, httpLbs, RequestBody(..) )
import           Network.HTTP.Client.TLS        (newTlsManager)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString                as BS
import           Data.Aeson (decode,encode)
import qualified Data.Text                      as T
import           Data.Maybe                     ( fromJust )
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Control.Monad.Catch
import           Network.HTTP.Client.MultipartFormData
import           Data.String                    ( fromString )
import Vk.TypeSynonym
import Vk.Oops
import Control.Applicative (liftA3)

data Handle m = Handle
  { hConf             :: Config,
    hLog              :: LogHandle m,
    getLongPollServer :: m LBS.ByteString,
    getUpdates        :: T.Text -> T.Text -> T.Text -> m LBS.ByteString,
    sendMsg           :: UserId -> T.Text -> [Integer] -> [String] -> String -> (String,String) -> m LBS.ByteString,
    sendKeyb          :: UserId -> N -> T.Text -> m LBS.ByteString,
    getPhotoServer    :: UserId -> m LBS.ByteString,
    loadPhotoToServ   :: T.Text -> T.Text -> BS.ByteString -> m LBS.ByteString,
    savePhotoOnServ   :: LoadPhotoResp -> m LBS.ByteString,
    getDocServer      :: UserId -> String -> m LBS.ByteString,
    loadDocToServ     :: T.Text -> T.Text -> BS.ByteString -> String -> m LBS.ByteString,
    saveDocOnServ     :: LoadDocResp -> String -> m LBS.ByteString,
    goToUrl           :: T.Text -> m BS.ByteString
    }

data Config = Config 
  { cStartN   :: N,
    cBotToken :: String,
    cHelpMsg  :: String,
    cRepeatQ  :: String,
    cGroupId  :: GroupId
    }


run :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndUsersNs m ()
run h = do
  getServer h
  forever $ runServ h

getServer :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndUsersNs m ()
getServer h = do
  lift $ logDebug (hLog h) $ "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=" ++ show (cGroupId (hConf h)) ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n"
  jsonServ <- lift $ getLongPollServer h `catch` (\e -> do
                                logError (hLog h) $ show e ++ " GetLongPollServer fail\n"
                                throwM $ DuringGetLongPollServerException $ show (e :: SomeException))
  lift $ logDebug (hLog h) ("Get response: " ++ show jsonServ ++ "\n")
  lift $ checkGetServResponse h jsonServ
  let servInf = extractServerInfo jsonServ
  modify $ changeServerInfo servInf
  
getUpdAndLog :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndUsersNs m LBS.ByteString     
getUpdAndLog h = do
  ServerInfo key server ts <- gets fst
  lift $ logDebug (hLog h) $ "Send request to getUpdates: " ++ T.unpack server ++ "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=20\n"
  json <- lift $ getUpdates h key server ts `catch` (\e -> do
                                logError (hLog h) $ show e ++ " GetUpdates fail\n"
                                throwM $ DuringGetUpdatesException $ show (e :: SomeException))
  lift $ logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  return json

runServ :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndUsersNs m ()   
runServ h = do
  json <- getUpdAndLog h
  upds <- checkUpdates h json
  mapM_ (chooseActionOfUpd h) upds

chooseActionOfUpd :: (Monad m, MonadCatch m) => Handle m -> Update -> StateT ServerAndUsersNs m ()
chooseActionOfUpd h upd = do
  lift $ logInfo (hLog h) ("Analysis update from the list\n")
  case upd of   
    Update "message_new" obj -> do
      lift $ logInfo (hLog h) (logStrForGetObj obj)
      chooseActionOfNState h obj
    UnknownUpdate _ -> do
      lift $ logWarning (hLog h) ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT. " ++ show upd ++ "\n")
    _ -> do
      lift $ logWarning (hLog h) ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT. " ++ show upd ++ "\n")

chooseActionOfNState :: (Monad m, MonadCatch m) => Handle m -> AboutObj -> StateT ServerAndUsersNs m ()
chooseActionOfNState h obj@(AboutObj usId _ _ _ _ _ _) = do
  usersNs <- gets snd
  let nState = lookup usId usersNs
  case nState of
    Just (Left (OpenRepeat oldN)) -> do
      lift $ logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode\n")
      chooseActionOfButton h obj oldN
    Just (Right n) -> do
      let currN = n
      chooseActionOfObject h obj currN
    Nothing -> do
      let currN = cStartN (hConf h)
      chooseActionOfObject h obj currN

chooseActionOfButton :: (Monad m, MonadCatch m) => Handle m -> AboutObj -> N -> StateT ServerAndUsersNs m ()
chooseActionOfButton h obj@(AboutObj usId _ _ _ _ _ _) oldN =
  case checkButton obj of
    Just newN -> do
      lift $ logInfo (hLog h) ("Change number of repeats to " ++ show newN ++ " for user " ++ show usId ++ "\n")
      modify $ func $ changeUsersNs usId $ Right newN
      let infoMsg = T.pack $ "Number of repeats successfully changed from " ++ show oldN ++ " to " ++ show newN ++ "\n"
      lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
      response <- lift $ sendTxtMsg h usId infoMsg `catch` (\e -> do
                            logError (hLog h) $ show e ++ " SendMessage fail\n"    
                            throwM $ DuringSendMsgException (TextMsg infoMsg) (ToUserId usId) $ show (e :: SomeException))
      lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
      lift $ checkSendMsgResponse h usId (TextMsg infoMsg) response
    Nothing -> do
      lift $ logWarning (hLog h) ("User " ++ show usId ++ " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++ show oldN ++ "\n")
      modify $ func $ changeUsersNs usId $ Right oldN
      let infoMsg = T.pack $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++ show oldN ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
      lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
      response <- lift $ sendTxtMsg h usId infoMsg `catch` (\e -> do
                            logError (hLog h) $ show e ++ " SendMessage fail\n"    
                            throwM $ DuringSendMsgException (TextMsg infoMsg) (ToUserId usId) $ show (e :: SomeException))
      lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
      lift $ checkSendMsgResponse h usId (TextMsg infoMsg) response

chooseActionOfObject :: (Monad m, MonadCatch m) => Handle m -> AboutObj -> N -> StateT ServerAndUsersNs m ()
chooseActionOfObject h obj currN =
  case obj of
    AboutObj usId _ _ txt [] [] Nothing -> do
      chooseActionOfTxt h currN usId txt
    AboutObj usId _ _ "" [] [StickerAttachment "sticker" (StickerInfo idSt)] Nothing -> do
      lift $ replicateM_ currN $ do
        logDebug (hLog h) ("Send request to send StickerMsg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&sticker_id=" ++ show idSt ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
        response <- sendStickMsg h usId idSt `catch` (\e -> do
                            logError (hLog h) $ show e ++ " SendMessage fail\n"    
                            throwM $ DuringSendMsgException (StickerMsg idSt) (ToUserId usId) $ show (e :: SomeException))
        checkSendMsgResponse h usId (StickerMsg idSt) response
    AboutObj _ _ _ _ [] _ _ -> do
      chooseActionOfAttachs h currN obj
    AboutObj usId _ _ _ _ _ _ -> do
      lift $ logWarning (hLog h) ("There is forward message. BOT WILL IGNORE IT. " ++ show obj ++ "\n")
      let infoMsg = T.pack $ "I`m sorry, I can`t work with forward messages, so I will ignore this message\n"
      lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
      response <- lift $ sendTxtMsg h usId infoMsg `catch` (\e -> do
                            logError (hLog h) $ show e ++ " SendMessage fail\n"    
                            throwM $ DuringSendMsgException (TextMsg infoMsg) (ToUserId usId) $ show (e :: SomeException))
      lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
      lift $ checkSendMsgResponse h usId (TextMsg infoMsg) response

chooseActionOfTxt :: (Monad m, MonadCatch m) => Handle m -> N -> UserId -> T.Text -> StateT ServerAndUsersNs m ()
chooseActionOfTxt h currN usId txt = case filter ((/=) ' ') . T.unpack $ txt of
  "/help" -> do
    let infoMsg = T.pack $ cHelpMsg (hConf h) 
    lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
    response <- lift $ sendTxtMsg h usId infoMsg `catch` (\e -> do
                          logError (hLog h) $ show e ++ " SendMessage fail\n"    
                          throwM $ DuringSendMsgException (TextMsg infoMsg) (ToUserId usId) $ show (e :: SomeException))
    lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
    lift $ checkSendMsgResponse h usId (TextMsg infoMsg) response
  "/repeat" -> do
    let infoMsg = T.pack $ " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
    lift $ logDebug (hLog h) $ "Send request to send keyboard: https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ show currN ++ T.unpack infoMsg ++ "&keyboard=default_keyboard&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
    response <- lift $ sendKeyb h usId currN infoMsg `catch` (\e -> do
                                logError (hLog h) $ show e ++ " SendKeyb fail\n" 
                                throwM $ DuringSendKeybException (ToUserId usId) $ show (e :: SomeException))
    lift $ logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
    lift $ checkSendKeybResponse h usId currN infoMsg response
    modify $ func $ changeUsersNs usId $ Left $ OpenRepeat currN 
  _ -> do 
    lift $ replicateM_ currN $ do
      logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack txt ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
      response <- sendTxtMsg h usId txt `catch` (\e -> do
                          logError (hLog h) $ show e ++ " SendMessage fail\n"    
                          throwM $ DuringSendMsgException (TextMsg txt) (ToUserId usId) $ show (e :: SomeException))
      logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
      checkSendMsgResponse h usId (TextMsg txt) response

chooseActionOfAttachs :: (Monad m, MonadCatch m) => Handle m -> N -> AboutObj -> StateT ServerAndUsersNs m () 
chooseActionOfAttachs h currN (AboutObj usId _ _ txt _ attachs maybeGeo) = do
  eitherAttachStrings <- lift $ mapM (getAttachmentString h usId) attachs
  case sequence eitherAttachStrings of
    Right attachStrings -> do
      lift $ replicateM_ currN $ do
        logDebug (hLog h) ("Send request to send AttachmentMsg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack txt ++ "&attachment=" ++ intercalate "," attachStrings ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103 . MaybeGeo = " ++ show maybeGeo ++ " \n" )
        response <- sendAttachMaybeGeoMsg h usId txt attachStrings maybeGeo `catch` (\e -> do
                        logError (hLog h) $ show e ++ " SendMessage fail\n"    
                        throwM $ DuringSendMsgException (AttachmentMsg txt attachStrings maybeGeo) (ToUserId usId) $ show (e :: SomeException))
        checkSendMsgResponse h usId (AttachmentMsg txt attachStrings maybeGeo) response
    Left str ->
      lift $ logWarning (hLog h) ("There is UNKNOWN ATTACMENT in updateList. BOT WILL IGNORE IT. " ++ show attachs ++ ". " ++ str ++ "\n")

getAttachmentString :: (Monad m, MonadCatch m) => Handle m -> UserId -> Attachment -> m (Either String String)
getAttachmentString _ _ (PhotoAttachment "photo" (Photo [])) = do
  return $ Left $ "Unknown photo attachment, empty sizes\n"
getAttachmentString h usId (PhotoAttachment "photo" (Photo sizes)) = do
  let picUrl = url . head . reverse . sortOn height $ sizes
  serRespJson <- getPhotoServer h usId
  serUrl <- checkGetUploadServResponse h serRespJson
  bsPic <- goToUrl h picUrl
  loadPhotoJson <- loadPhotoToServ h serUrl picUrl bsPic
  loadPhotoResp <- checkLoadPhotoResponse h loadPhotoJson
  savePhotoJson <- savePhotoOnServ h loadPhotoResp
  (DocInfo idDoc owner_id) <- checkSavePhotoResponse h savePhotoJson
  return $ Right $ "photo" ++ show owner_id ++ "_" ++ show idDoc 
getAttachmentString _ _ (PhotoAttachment attType _) = do
  return $ Left $ "WrongType for photoMessage attachment: " ++ show attType ++ "\n"
getAttachmentString h usId (DocAttachment "doc" (Doc docUrl ext title)) = do
  serRespJson <- getDocServer h usId "doc"
  serUrl <- checkGetUploadServResponse h serRespJson
  bsDoc <- goToUrl h docUrl
  loadDocJson <- loadDocToServ h serUrl docUrl bsDoc ext
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <- saveDocOnServ h loadDocResp title
  (DocInfo idDoc owner_id) <- checkSaveDocResponse h saveDocJson
  return $ Right $ "doc" ++ show owner_id ++ "_" ++ show idDoc 
getAttachmentString _ _ (DocAttachment attType _) = do
  return $ Left $ "WrongType for docMessage attachment: " ++ show attType ++ "\n" 
getAttachmentString h usId (AudioMesAttachment "audio_message" (Audio docUrl)) = do
  serRespJson <- getDocServer h usId "audio_message"
  serUrl <- checkGetUploadServResponse h serRespJson
  bsDoc <- goToUrl h docUrl
  loadDocJson <- loadDocToServ h serUrl docUrl bsDoc "ogg"
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <- saveDocOnServ h loadDocResp "audio_message"
  (DocInfo idDoc owner_id) <- checkSaveDocAuMesResponse h saveDocJson
  return $ Right $ "doc" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (AudioMesAttachment attType _) = do
  return $ Left $ "WrongType for audioMessage attachment: " ++ show attType ++ "\n"
getAttachmentString _ _ (VideoAttachment "video" (DocInfo idDoc owner_id)) = do
  return $ Right $ "video" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (VideoAttachment attType _) = do
  return $ Left $ "WrongType for videoMessage attachment: " ++ show attType ++ "\n"
getAttachmentString _ _ (AudioAttachment "audio" (DocInfo idDoc owner_id)) = do
  return $ Right $ "audio" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (AudioAttachment attType _) = do
  return $ Left $ "WrongType for audio attachment: " ++ show attType ++ "\n"  
getAttachmentString _ _ (MarketAttachment "market" (DocInfo idDoc owner_id)) = do
  return $ Right $ "market" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (MarketAttachment attType _) = do
  return $ Left $ "WrongType for marketMessage attachment: " ++ show attType ++ "\n"
getAttachmentString _ _ (WallAttachment "wall" (WallInfo idWall owner_id)) = do
  return $ Right $ "wall" ++ show owner_id ++ "_" ++ show idWall
getAttachmentString _ _ (WallAttachment attType _) = do
  return $ Left $ "WrongType for marketMessage attachment: " ++ show attType ++ "\n"
getAttachmentString _ _ (PollAttachment "poll" (DocInfo idDoc owner_id)) = do
  return $ Right $ "poll" ++ show owner_id ++ "_" ++ show idDoc  
getAttachmentString _ _ (PollAttachment attType _) = do
  return $ Left $ "WrongType for marketMessage attachment: " ++ show attType ++ "\n"
getAttachmentString _ usId (StickerAttachment "sticker" (StickerInfo _)) = 
  return $ Left $ "Wrong sticker attachment from user: " ++ show usId  ++ ". Sticker not alone in msg.\n"
getAttachmentString _ _ (StickerAttachment attType _) = do
  return $ Left $ "WrongType for stickerMessage attachment: " ++ show attType ++ "\n"
getAttachmentString _ usId (UnknownAttachment obj) = return $ Left $ "Unknown attachment from user: " ++ show usId  ++ ". " ++ show obj ++ "\n"
  


sendTxtMsg :: (Monad m, MonadCatch m) => Handle m -> UserId -> T.Text -> m LBS.ByteString
sendTxtMsg h usId txt = sendMsg h usId txt [] [] "" ("","")

sendAttachMaybeGeoMsg :: (Monad m, MonadCatch m) => Handle m -> UserId -> T.Text -> [String] -> Maybe Geo -> m LBS.ByteString
sendAttachMaybeGeoMsg h usId txt attachStrs maybeGeo = case maybeGeo of 
  Nothing -> sendMsg h usId txt [] attachStrs "" ("","")
  Just (Geo "point" (Coordinates lat long)) -> 
    sendMsg h usId txt [] attachStrs "" (show lat,show long)
  _ -> do
    logError (hLog h) $ "UNKNOWN GEO type\n" ++ show maybeGeo
    throwM $ DuringGetUpdatesException $ "UNKNOWN GEO type\n" ++ show maybeGeo

sendStickMsg :: (Monad m, MonadCatch m) => Handle m -> UserId -> StickerId -> m LBS.ByteString
sendStickMsg  h usId stickerId  = sendMsg h usId "" [] [] (show stickerId) ("","")


checkGetServResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m ()
checkGetServResponse h json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to getLongPollServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
      Just (ErrorAnswerServ { errorEAS =  _ } ) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to getLongPollServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
      Just _ -> do
        logInfo (hLog h) $ "Work with received server\n"

checkUpdates :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> StateT ServerAndUsersNs m [Update]
checkUpdates h json = do
  case decode json of
      Nothing                      -> do
        lift $ logError (hLog h) $ "UNKNOWN RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
      Just (ErrorAnswer { errorEA = _ } ) -> do
        lift $ logError (hLog h) $ "NEGATIVE RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
      Just (FailAnswer 2 ) -> do
        lift $ logWarning (hLog h) ("FAIL. Long poll server key expired, need to request new key\n")
        getServer h
        json2 <- getUpdAndLog h
        checkUpdates h json2
      Just (FailAnswer 3 ) -> do
        lift $ logWarning (hLog h) ("FAIL. Long poll server information is lost, need to request new key and ts\n")
        getServer h
        json2 <- getUpdAndLog h
        checkUpdates h json2
      Just (FailTSAnswer {failFTSA = 1 , tsFTSA = ts } ) -> do
        lift $ logWarning (hLog h) ("FAIL. Ts in request is wrong, need to use received ts\n")
        modify $ changeTs (T.pack . show $ ts)
        json2 <- getUpdAndLog h
        checkUpdates h json2
      Just (FailTSAnswer {failFTSA = _ , tsFTSA = ts } ) -> do
        lift $ logWarning (hLog h) ("FAIL. Ts in request is wrong, need to use received ts\n")
        modify $ changeTs (T.pack . show $ ts)
        json2 <- getUpdAndLog h
        checkUpdates h json2
      Just (FailAnswer _ ) -> do
        lift $ logError (hLog h) $ "NEGATIVE RESPONSE to getUpdates:\n" ++ show json
        throwM $ CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
      Just (AnswerOk { updates = [] }) -> do
        lift $ logInfo (hLog h) ("No new updates\n")
        return []
      Just (AnswerOk ts upds) -> do
        modify $ changeTs ts
        lift $ logInfo (hLog h) ("There is new updates list\n")
        return upds

checkSendMsgResponse :: (Monad m, MonadCatch m) => Handle m -> UserId -> MSG -> LBS.ByteString -> m ()
checkSendMsgResponse h usId msg json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to sendMessage:\n" ++ show json
        throwM $ CheckSendMsgResponseException msg (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"  
      Just (ErrorAnswerMsg { errorEAM = _ } ) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to sendMessage:\n" ++ show json
        throwM $ CheckSendMsgResponseException msg (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
      Just _                       -> case msg of
        TextMsg txt -> logInfo (hLog h) ("Msg " ++ show txt  ++ " was sent to user " ++ show usId ++ "\n")
        StickerMsg idSt -> logInfo (hLog h) ("Sticker_id " ++ show idSt  ++ " was sent to user " ++ show usId ++ "\n")
        AttachmentMsg txt attachStrings Nothing -> logInfo (hLog h) ("AttachmentMsg was sent to user " ++ show usId ++ ". Text: " ++ show txt ++ "; attachments: " ++ show attachStrings ++ "\n")
        AttachmentMsg txt [] (Just geo) -> do
          logInfo (hLog h) ("GeoMsg was sent to user " ++ show usId ++ ". Text: " ++ show txt ++ "; geo: " ++ show geo ++ "\n")
        AttachmentMsg txt attachStrings (Just geo) -> do
          logInfo (hLog h) ("AttachmentAndGeoMsg was sent to user " ++ show usId ++ ". Text: " ++ show txt ++ "; attachments: " ++ show attachStrings ++ "; geo: " ++ show geo ++ "\n")

checkSendKeybResponse :: (Monad m, MonadCatch m) => Handle m -> UserId -> N -> T.Text -> LBS.ByteString -> m ()
checkSendKeybResponse h usId n msg json = do
  case decode json of
      Nothing                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to sendKeyboard:\n" ++ show json
        throwM $ CheckSendKeybResponseException (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nKEYBOARD PROBABLY NOT SENT"  
      Just (ErrorAnswerMsg { errorEAM = _ } ) -> do
        logError (hLog h) $ "NEGATIVE RESPONSE to sendKeyboard:\n" ++ show json
        throwM $ CheckSendKeybResponseException (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nKEYBOARD NOT SENT"
      Just _                       -> do
        logInfo (hLog h) ("Keyboard with message: " ++ show n ++ show msg ++ " was sent to user " ++ show usId ++ "\n")

checkGetUploadServResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m T.Text
checkGetUploadServResponse h json = do
  case decode json of
      Just (UploadServerResponse (UploadUrl serUrl)) -> return serUrl
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to getPhotoServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

checkLoadDocResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m LoadDocResp
checkLoadDocResponse h json = do
  case decode json of
      Just loadDocResp@(LoadDocResp _) -> return loadDocResp
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to loadDocToServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

checkSaveDocResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m DocInfo
checkSaveDocResponse h json = do
  case decode json of
      Just (SaveDocResp (ResponseSDR "doc" docInf )) -> return docInf
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to saveDocOnServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

checkSaveDocAuMesResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m DocInfo
checkSaveDocAuMesResponse h json = do
  case decode json of
      Just (SaveDocAuMesResp (ResponseSDAMR "audio_message" docInf )) -> return docInf
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to saveDocAudioMesOnServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

checkLoadPhotoResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m LoadPhotoResp
checkLoadPhotoResponse h json = do
  case decode json of
      Just loadPhotoResp@(LoadPhotoResp _ _ _) -> return loadPhotoResp
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to loadPhotoToServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json

checkSavePhotoResponse :: (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m DocInfo
checkSavePhotoResponse h json = do
  case decode json of
      Just (SavePhotoResp [DocInfo id' ownerId ]) -> return (DocInfo id' ownerId )
      _                      -> do
        logError (hLog h) $ "UNKNOWN RESPONSE to savePhotoOnServer:\n" ++ show json
        throwM $ CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json




getLongPollServer' :: Handle IO -> IO LBS.ByteString
getLongPollServer' h = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/groups.getLongPollServer?group_id=" ++ show (cGroupId (hConf h)) ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

getUpdates' :: T.Text -> T.Text -> T.Text -> IO LBS.ByteString
getUpdates' key server ts = do
  manager <- newTlsManager  
  req <- parseRequest $ T.unpack server ++ "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=20"
  res <- httpLbs req manager
  return (responseBody res)

sendMsg' :: Handle IO -> UserId -> T.Text -> [Integer] -> [String] -> String -> (String,String) -> IO LBS.ByteString
sendMsg' h usId txt fwds attachStrings stickerId (lat,long)  = do
  manager <- newTlsManager
  let param1  = "user_id=" ++ show usId
  let param2  = "random_id=0"
  let param3  = "message=" ++ T.unpack txt
  let param4  = "forward_messages=" ++ (intercalate "," . fmap show $ fwds) 
  let param5  = "attachment=" ++ intercalate "," attachStrings
  let param6  = "sticker_id=" ++ stickerId
  let param7  = "lat=" ++ lat
  let param8  = "long=" ++ long
  let param9  = "access_token=" ++ cBotToken (hConf h)
  let param10 = "v=5.103"
  let params  = intercalate "&" (param1:param2:param3:param4:param5:param6:param7:param8:param9:param10:[])
  req <- parseRequest $ "https://api.vk.com/method/messages.send?" ++ params
  res <- httpLbs req manager
  return (responseBody res)

sendKeyb' :: Handle IO -> UserId -> N -> T.Text -> IO LBS.ByteString
sendKeyb' h usId n msg = do
  manager <- newTlsManager
  initReq <- parseRequest $ "https://api.vk.com/method/messages.send"
  let param1 = ("user_id"     , fromString . show $ usId) 
  let param2 = ("random_id"   ,"0")
  let param3 = ("message"     , fromString (show  n ++ T.unpack msg))
  let param4 = ("keyboard"    , LBS.toStrict . encode $ kB)
  let param5 = ("access_token", fromString (cBotToken $ (hConf h))) 
  let param6 = ("v"           ,"5.103")
  let params = param1 : param2 : param3 : param4 : param5 : param6 : [] 
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)

getDocServer' :: Handle IO -> UserId -> String -> IO LBS.ByteString
getDocServer' h usId type' = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/docs.getMessagesUploadServer?type=" ++ type' ++ "&peer_id=" ++ show usId ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

loadDocToServ' :: T.Text -> T.Text -> BS.ByteString -> String -> IO LBS.ByteString
loadDocToServ' serUrl docUrl bs ext = do
  manager <- newTlsManager
  initReq <- parseRequest $ T.unpack serUrl
  req     <- (formDataBody [partFileRequestBody "file" (T.unpack docUrl ++ " file." ++ ext) $ RequestBodyBS bs]
                initReq)
  res <- httpLbs req manager
  return (responseBody res)

saveDocOnServ' :: Handle IO -> LoadDocResp -> String -> IO LBS.ByteString
saveDocOnServ' h (LoadDocResp file) title = do
  manager <- newTlsManager
  initReq <- parseRequest $ "https://api.vk.com/method/docs.save"
  let param1 = ("file"        , fromString file)
  let param2 = ("title"       , fromString title)
  let param3 = ("access_token", fromString (cBotToken $ (hConf h))) 
  let param4 = ("v"           ,"5.103")
  let params = param1 : param2 : param3 : param4 : []   
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)

getPhotoServer' :: Handle IO -> UserId -> IO LBS.ByteString
getPhotoServer' h usId = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/photos.getMessagesUploadServer?peer_id=" ++ show usId ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

loadPhotoToServ' :: T.Text -> T.Text -> BS.ByteString -> IO LBS.ByteString
loadPhotoToServ' serUrl picUrl bs = do
  manager <- newTlsManager
  initReq <- parseRequest $ T.unpack serUrl
  req     <- (formDataBody [partFileRequestBody "photo" (T.unpack picUrl) $ RequestBodyBS bs]
                initReq)
  res <- httpLbs req manager
  return (responseBody res)

savePhotoOnServ' :: Handle IO -> LoadPhotoResp -> IO LBS.ByteString
savePhotoOnServ' h (LoadPhotoResp server hash photo) = do
  manager <- newTlsManager
  initReq <- parseRequest $ "https://api.vk.com/method/photos.saveMessagesPhoto"
  let param1 = ("server"      , fromString . show $ server) 
  let param2 = ("hash"        , fromString hash)
  let param3 = ("photo"       , fromString photo)
  let param4 = ("access_token", fromString (cBotToken $ (hConf h))) 
  let param5 = ("v"           ,"5.103")
  let params = param1 : param2 : param3 : param4 : param5 : [] 
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)

goToUrl' :: T.Text -> IO BS.ByteString
goToUrl' urlTxt = do
  manager <- newTlsManager
  req <- parseRequest $ T.unpack urlTxt
  res  <- httpLbs req manager
  let bs = LBS.toStrict . responseBody $ res
  return bs

extractUpdates :: LBS.ByteString -> [Update]
extractUpdates = updates . fromJust . decode 

extractTs :: LBS.ByteString -> T.Text
extractTs = tsSI . responseGPSJB . fromJust . decode

extractKey :: LBS.ByteString -> T.Text
extractKey = keySI . responseGPSJB . fromJust . decode

extractServ :: LBS.ByteString -> T.Text
extractServ = serverSI . responseGPSJB . fromJust . decode

extractServerInfo :: LBS.ByteString -> ServerInfo
extractServerInfo = liftA3 ServerInfo extractKey extractServ extractTs

extractTextMsg :: Update -> T.Text
extractTextMsg = text . objectUpd 

extractUserId :: Update -> UserId
extractUserId = from_id . objectUpd

changeServerInfo :: ServerInfo -> ServerAndUsersNs -> ServerAndUsersNs
changeServerInfo newInfo (_,usersNs) = (newInfo,usersNs)

changeTs :: T.Text -> ServerAndUsersNs -> ServerAndUsersNs
changeTs newTs (serverInfo,usersNs) = (serverInfo {tsSI = newTs}, usersNs)

func :: (b -> b) -> (a,b) -> (a,b)
func f (a,b) = (a, f b)

changeUsersNs :: UserId -> NState -> UsersNs -> UsersNs
changeUsersNs usId nState usersNs = 
    case lookup usId usersNs of
        Just oldNState -> (:) (usId,nState) . delete (usId, oldNState) $ usersNs
        Nothing -> (:) (usId,nState) $ usersNs


checkButton :: AboutObj -> Maybe N
checkButton obj =
  case obj of
    AboutObj _ _ _ txt [] [] Nothing -> checkTextButton txt
    _  -> Nothing

checkTextButton :: T.Text -> Maybe N
checkTextButton txt =
    case txt of 
      { "1" -> Just 1 ; "2" -> Just 2 ; "3" -> Just 3 ; "4" -> Just 4 ; "5" -> Just 5 ; _ -> Nothing }
  
logStrForGetObj :: AboutObj -> String
logStrForGetObj (AboutObj usId _ _ txt [] [] Nothing) = "Get TextMsg: " ++ show txt ++ " from user " ++ show usId ++ "\n"
logStrForGetObj (AboutObj usId _ _ txt fwds [] Nothing) = "Get ForwardMsg: " ++ show fwds ++ addInfoAboutTxt txt ++ " from user " ++ show usId ++ "\n"
logStrForGetObj (AboutObj usId _ _ txt [] [] (Just geo)) = "Get GeoMsg: " ++ show geo ++ addInfoAboutTxt txt ++ " from user " ++ show usId ++ "\n"
logStrForGetObj (AboutObj usId _ _ txt [] attachs Nothing) = "Get AttachmentMsg: " ++ show attachs ++ addInfoAboutTxt txt ++ " from user " ++ show usId ++ "\n"
logStrForGetObj (AboutObj usId _ _ txt fwds attachs Nothing) = "Get AttachmentMsg: " ++ show attachs ++ addInfoAboutTxt txt ++ ", with ForwardParts: " ++ show fwds ++ "  from user " ++ show usId ++ "\n"
logStrForGetObj (AboutObj usId _ _ txt [] attachs (Just geo)) = "Get AttachmentMsg: " ++ show attachs ++ addInfoAboutTxt txt ++ ", with Geo: " ++ show geo ++ "  from user " ++ show usId ++ "\n"
logStrForGetObj (AboutObj usId _ _ txt fwds [] (Just geo)) = "Get ForwardMsg: " ++ show fwds ++ addInfoAboutTxt txt ++ ", with Geo: " ++ show geo ++ "  from user " ++ show usId ++ "\n"
logStrForGetObj (AboutObj usId _ _ txt fwds attachs (Just geo)) = "Get AttachmentMsg: " ++ show attachs ++ addInfoAboutTxt txt ++ ", with Geo: " ++ show geo ++ ", with ForwardParts: " ++ show fwds ++ "  from user " ++ show usId ++ "\n"

addInfoAboutTxt :: T.Text -> String
addInfoAboutTxt "" = ""
addInfoAboutTxt txt = " with text: " ++ show txt  