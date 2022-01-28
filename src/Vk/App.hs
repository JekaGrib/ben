{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Vk.App where

import qualified Data.Map as Map(lookup,insert) 
import           Vk.Logger (LogHandle(..), logDebug, logInfo, logWarning)
import           Vk.Api.Response 
import           Vk.Api.Request (keyBoard)
import           Network.HTTP.Client            (urlEncodedBody, parseRequest, responseBody, httpLbs, RequestBody(..) )
import           Network.HTTP.Client.TLS        (newTlsManager)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString                as BS
import           Data.Aeson (decode,encode)
import qualified Data.Text                      as T
import           Data.Maybe                     ( fromJust )
import           Control.Monad.State            (StateT,lift,modify,replicateM_,forever,gets)
import           Data.List                      (sortOn,intercalate)
import           Control.Monad.Catch (MonadCatch(catch))
import           Network.HTTP.Client.MultipartFormData (formDataBody,partFileRequestBody)
import           Data.String                    ( fromString )
import Vk.Types
import Vk.Oops (VKBotException(..)
  , PrependAttachmetException(..)
  , handleExGetLongPollServ
  , handleExGetUpd
  , handleExSendMsg
  , handleExSendKeyb
  , handleExGetUploadServ
  , handleExLoadToServ
  , handleExSaveOnServ
  , handleExGoToUrl
  , throwAndLogEx
  , throwAndLogPrepAttEx
  )
import Control.Applicative (liftA3)
import Vk.Conf (Config(..))

data Handle m = Handle
  { hConf             :: Config,
    hLog              :: LogHandle m,
    getLongPollServer :: m Response,
    getUpdates        :: ServerInfo -> m Response,
    sendMsg           :: UserId -> MSG -> m Response,
    sendKeyb          :: UserId -> N -> TextOfKeyb -> m Response,
    getPhotoServer    :: UserId -> m Response,
    loadPhotoToServ   :: ServerUrl -> PicUrl -> ResponseS -> m Response,
    savePhotoOnServ   :: LoadPhotoResp -> m Response,
    getDocServer      :: UserId -> TypeInGetServerReq -> m Response,
    loadDocToServ     :: ServerUrl -> DocUrl -> ResponseS -> Extention -> m Response,
    saveDocOnServ     :: LoadDocResp -> Title -> m Response,
    goToUrl           :: Url -> m ResponseS
    }


run :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m ()
run h = do
  getServer h
  forever $ runServ h

getServer :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m ()
getServer h = do
  lift $ logDebug (hLog h) $ "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=" ++ show (cGroupId (hConf h)) ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n"
  jsonServ <- lift $ getLongPollServer h `catch` handleExGetLongPollServ (hLog h)
  lift $ logDebug (hLog h) ("Get response: " ++ show jsonServ ++ "\n")
  lift $ checkGetServResponse h jsonServ
  let servInf = extractServerInfo jsonServ
  modify $ changeServerInfo servInf
  
getUpdAndLog :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m Response     
getUpdAndLog h = do
  sI@(ServerInfo key server ts) <- gets fst
  lift $ logDebug (hLog h) $ "Send request to getUpdates: " ++ T.unpack server ++ "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=20\n"
  json <- lift $ getUpdates h sI `catch` handleExGetUpd (hLog h)
  lift $ logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  return json

runServ :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m ()   
runServ h = do
  json <- getUpdAndLog h
  upds <- checkUpdates h json
  mapM_ (chooseActionOfUpd h) upds

chooseActionOfUpd :: (Monad m, MonadCatch m) => Handle m -> Update -> StateT ServerAndMapUserN m ()
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

chooseActionOfNState :: (Monad m, MonadCatch m) => Handle m -> AboutObj -> StateT ServerAndMapUserN m ()
chooseActionOfNState h obj@(AboutObj usId _ _ _ _ _ _) = do
  mapUN <- gets snd
  let nState = Map.lookup usId mapUN
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

chooseActionOfButton :: (Monad m, MonadCatch m) => Handle m -> AboutObj -> N -> StateT ServerAndMapUserN m ()
chooseActionOfButton h obj@(AboutObj usId _ _ _ _ _ _) oldN =
  case checkButton obj of
    Just newN -> do
      lift $ logInfo (hLog h) ("Change number of repeats to " ++ show newN ++ " for user " ++ show usId ++ "\n")
      modify $ changeSecond $ changeMapUserN usId $ Right newN
      let infoMsg = T.pack $ "Number of repeats successfully changed from " ++ show oldN ++ " to " ++ show newN ++ "\n"
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg
    Nothing -> do
      lift $ logWarning (hLog h) ("User " ++ show usId ++ " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++ show oldN ++ "\n")
      modify $ changeSecond $ changeMapUserN usId $ Right oldN
      let infoMsg = T.pack $ "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++ show oldN ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg

chooseActionOfObject :: (Monad m, MonadCatch m) => Handle m -> AboutObj -> N -> StateT ServerAndMapUserN m ()
chooseActionOfObject h obj currN =
  case obj of
    AboutObj usId _ _ txt [] [] Nothing -> do
      chooseActionOfTxt h currN usId txt
    AboutObj usId _ _ "" [] [SomeAttachment "sticker" _ _ _ _ (Just (StickerInfo idSt)) _ _ _ _] Nothing -> do
      lift $ replicateM_ currN $ do
        let msg = StickerMsg idSt
        sendMsgAndCheckResp h usId msg
    AboutObj _ _ _ _ [] _ _ -> do
      chooseActionOfAttachs h currN obj
    AboutObj usId _ _ _ _ _ _ -> do
      lift $ logWarning (hLog h) ("There is forward message. BOT WILL IGNORE IT. " ++ show obj ++ "\n")
      let infoMsg = T.pack $ "I`m sorry, I can`t work with forward messages, so I will ignore this message\n"
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg

chooseActionOfTxt :: (Monad m, MonadCatch m) => Handle m -> N -> UserId -> TextOfMsg -> StateT ServerAndMapUserN m ()
chooseActionOfTxt h currN usId txt = case filter ((/=) ' ') . T.unpack $ txt of
  "/help" -> do
    let infoMsg = T.pack $ cHelpMsg (hConf h) 
    lift $ logDebug (hLog h) ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++ show usId ++ "&random_id=0&message=" ++ T.unpack infoMsg ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103\n" )
    let msg = TextMsg infoMsg
    lift $ sendMsgAndCheckResp h usId msg
  "/repeat" -> do
    let infoMsg = T.pack $ " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
    lift $ sendKeybAndCheckResp h usId currN infoMsg
    modify $ changeSecond $ changeMapUserN usId $ Left $ OpenRepeat currN 
  _ -> do 
    lift $ replicateM_ currN $ do
      let msg = TextMsg txt
      sendMsgAndCheckResp h usId msg

chooseActionOfAttachs :: (Monad m, MonadCatch m) => Handle m -> N -> AboutObj -> StateT ServerAndMapUserN m () 
chooseActionOfAttachs h currN (AboutObj usId _ _ txt _ attachs maybeGeo) = do
  eitherAttachStrings <- lift $ mapM (getSomeAttachmentString h usId) attachs
  case sequence eitherAttachStrings of
    Right attachStrings -> do
      lift $ replicateM_ currN $ do
        latLong <- pullLatLong h maybeGeo
        let msg = AttachmentMsg txt attachStrings latLong
        sendMsgAndCheckResp h usId msg
    Left str ->
      lift $ logWarning (hLog h) ("There is UNKNOWN ATTACMENT in updateList. BOT WILL IGNORE IT. " ++ show attachs ++ ". " ++ str ++ "\n")

getSomeAttachmentString :: (Monad m, MonadCatch m) => Handle m -> UserId -> SomeAttachment -> m (Either SomethingWrong AttachmentString)
getSomeAttachmentString h usId someAtt = case chooseAttType someAtt of
  Left str -> return . Left $ str
  Right att -> getAttachmentString h usId att

chooseAttType :: SomeAttachment -> Either SomethingWrong Attachment
chooseAttType sAtt = case sAtt of
  SomeAttachment "photo" (Just (Photo [])) _ _ _ _ _ _ _ _ ->  
    Left $ "Unknown photo attachment, empty sizes\n"
  SomeAttachment "photo" (Just photo) _ _ _ _ _ _ _ _ ->
    Right $ PhotoAttachment photo
  SomeAttachment "doc" _ (Just doc) _ _ _ _ _ _ _ ->
    Right $ DocAttachment doc
  SomeAttachment "audio_message" _ _ (Just auMes) _ _ _ _ _ _ ->
    Right $ AudioMesAttachment auMes
  SomeAttachment "video" _ _ _ (Just docInf) _ _ _ _ _ ->
    Right $ VideoAttachment docInf
  SomeAttachment "sticker" _ _ _ _ (Just sticker) _ _ _ _ ->
    Right $ StickerAttachment sticker 
  SomeAttachment "audio" _ _ _ _ _ (Just docInf) _ _ _ ->
    Right $ AudioAttachment docInf
  SomeAttachment "market" _ _ _ _ _ _ (Just docInf) _ _ ->
    Right $ MarketAttachment docInf
  SomeAttachment "wall" _ _ _ _ _ _ _ (Just wall) _ ->
    Right $ WallAttachment wall
  SomeAttachment "poll" _ _ _ _ _ _ _ _ (Just docInf) ->
    Right $ PollAttachment docInf
  _ -> Left $ "Unknown attachment:" ++ show sAtt ++ "\n"


getAttachmentString :: (Monad m, MonadCatch m) => Handle m -> UserId -> Attachment -> m (Either SomethingWrong AttachmentString)
getAttachmentString h usId (PhotoAttachment (Photo sizes)) = do
  let picUrl = url . head . reverse . sortOn height $ sizes
  serRespJson <- getPhotoServer h usId `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson
  bsPic <- goToUrl h picUrl  `catch`  handleExGoToUrl (hLog h)
  loadPhotoJson <- loadPhotoToServ h serUrl picUrl bsPic `catch` handleExLoadToServ (hLog h)
  loadPhotoResp <- checkLoadPhotoResponse h loadPhotoJson
  savePhotoJson <- savePhotoOnServ h loadPhotoResp `catch` handleExSaveOnServ (hLog h)
  (DocInfo idDoc owner_id) <- checkSavePhotoResponse h savePhotoJson
  return $ Right $ "photo" ++ show owner_id ++ "_" ++ show idDoc 
getAttachmentString h usId (DocAttachment (Doc docUrl ext title)) = do
  serRespJson <- getDocServer h usId "doc" `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson
  bsDoc <- goToUrl h docUrl  `catch`  handleExGoToUrl (hLog h)
  loadDocJson <- loadDocToServ h serUrl docUrl bsDoc ext `catch` handleExLoadToServ (hLog h)
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <- saveDocOnServ h loadDocResp title `catch` handleExSaveOnServ (hLog h)
  (DocInfo idDoc owner_id) <- checkSaveDocResponse h saveDocJson
  return $ Right $ "doc" ++ show owner_id ++ "_" ++ show idDoc 
getAttachmentString h usId (AudioMesAttachment (Audio docUrl)) = do
  serRespJson <- getDocServer h usId "audio_message" `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson 
  bsDoc <- goToUrl h docUrl  `catch`  handleExGoToUrl (hLog h)
  loadDocJson <- loadDocToServ h serUrl docUrl bsDoc "ogg" `catch` handleExLoadToServ (hLog h)
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <- saveDocOnServ h loadDocResp "audio_message" `catch` handleExSaveOnServ (hLog h)
  (DocInfo idDoc owner_id) <- checkSaveDocAuMesResponse h saveDocJson
  return $ Right $ "doc" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (VideoAttachment (DocInfo idDoc owner_id)) = do
  return $ Right $ "video" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (AudioAttachment (DocInfo idDoc owner_id)) = do
  return $ Right $ "audio" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (MarketAttachment (DocInfo idDoc owner_id)) = do
  return $ Right $ "market" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (WallAttachment (WallInfo idWall owner_id)) = do
  return $ Right $ "wall" ++ show owner_id ++ "_" ++ show idWall
getAttachmentString _ _ (PollAttachment (DocInfo idDoc owner_id)) = do
  return $ Right $ "poll" ++ show owner_id ++ "_" ++ show idDoc  
getAttachmentString _ usId (StickerAttachment _) = 
  return $ Left $ "Wrong sticker attachment from user: " ++ show usId  ++ ". Sticker not alone in msg.\n"

sendMsgAndCheckResp :: (Monad m, MonadCatch m) => Handle m -> UserId -> MSG -> m ()
sendMsgAndCheckResp h usId msg = do
  logDebug (hLog h) ("Send request to send msg: " ++ show msg ++ "\n" )
  response <- sendMsg h usId msg `catch` handleExSendMsg (hLog h) usId msg 
  logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
  checkSendMsgResponse h usId msg response


checkGetServResponse :: (Monad m, MonadCatch m) => Handle m -> Response -> m ()
checkGetServResponse h json = do
  case decode json of
      Nothing                      -> do
        let ex = CheckGetServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
        throwAndLogEx (hLog h) ex
      Just (ErrorAnswerServ { errorEAS =  _ } ) -> do
        let ex = CheckGetServerResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
        throwAndLogEx (hLog h) ex
      Just _ -> do
        logInfo (hLog h) $ "Work with received server\n"

checkUpdates :: (Monad m, MonadCatch m) => Handle m -> Response -> StateT ServerAndMapUserN m [Update]
checkUpdates h json = do
  case decode json of
      Nothing                      -> do
        let ex = CheckGetUpdatesResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
        lift $ throwAndLogEx (hLog h) ex
      Just (ErrorAnswer { errorEA = _ } ) -> do
        let ex = CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
        lift $ throwAndLogEx (hLog h) ex
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
        let ex = CheckGetUpdatesResponseException $ "NEGATIVE RESPONSE:\n"   ++ show json
        lift $ throwAndLogEx (hLog h) ex
      Just (AnswerOk { updates = [] }) -> do
        lift $ logInfo (hLog h) ("No new updates\n")
        return []
      Just (AnswerOk ts upds) -> do
        modify $ changeTs ts
        lift $ logInfo (hLog h) ("There is new updates list\n")
        return upds

pullLatLong :: (Monad m, MonadCatch m) => Handle m -> Maybe Geo -> m LatLong
pullLatLong h maybeGeo = case maybeGeo of 
  Nothing -> return ("","")
  Just (Geo "point" (Coordinates lat long)) -> return (show lat,show long)
  _ -> do
    let ex = GetUpdatesException $ "UNKNOWN GEO type\n" ++ show maybeGeo
    throwAndLogEx (hLog h) ex


checkSendMsgResponse :: (Monad m, MonadCatch m) => Handle m -> UserId -> MSG -> Response -> m ()
checkSendMsgResponse h usId msg json = do
  case decode json of
      Nothing                      -> do
        let ex = CheckSendMsgResponseException msg (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"  
        throwAndLogEx (hLog h) ex
      Just (ErrorAnswerMsg { errorEAM = _ } ) -> do
        let ex = CheckSendMsgResponseException msg (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
        throwAndLogEx (hLog h) ex
      Just _                       -> case msg of
        TextMsg txt -> logInfo (hLog h) ("Msg " ++ show txt  ++ " was sent to user " ++ show usId ++ "\n")
        StickerMsg idSt -> logInfo (hLog h) ("Sticker_id " ++ show idSt  ++ " was sent to user " ++ show usId ++ "\n")
        AttachmentMsg txt attachStrings ("","") -> logInfo (hLog h) ("AttachmentMsg was sent to user " ++ show usId ++ ". Text: " ++ show txt ++ "; attachments: " ++ show attachStrings ++ "\n")
        AttachmentMsg txt [] latLong -> do
          logInfo (hLog h) ("GeoMsg was sent to user " ++ show usId ++ ". Text: " ++ show txt ++ "; geo: " ++ show latLong ++ "\n")
        AttachmentMsg txt attachStrings latLong -> do
          logInfo (hLog h) ("AttachmentAndGeoMsg was sent to user " ++ show usId ++ ". Text: " ++ show txt ++ "; attachments: " ++ show attachStrings ++ "; geo: " ++ show latLong ++ "\n")

sendKeybAndCheckResp :: (Monad m, MonadCatch m) => Handle m -> UserId -> N -> TextOfKeyb -> m ()
sendKeybAndCheckResp h usId currN txt = do
    logDebug (hLog h) $ "Send request to send keyboard to user: " ++ show usId ++ " with message: " ++ show currN ++ show txt
    response <- sendKeyb h usId currN txt `catch` handleExSendKeyb (hLog h) usId 
    logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
    checkSendKeybResponse h usId currN txt response

checkSendKeybResponse :: (Monad m, MonadCatch m) => Handle m -> UserId -> N -> TextOfKeyb -> Response -> m ()
checkSendKeybResponse h usId n txt json = do
  case decode json of
      Nothing                      -> do
        let ex = CheckSendKeybResponseException (ToUserId usId) $ "UNKNOWN RESPONSE:\n" ++ show json ++ "\nKEYBOARD PROBABLY NOT SENT"  
        throwAndLogEx (hLog h) ex
      Just (ErrorAnswerMsg { errorEAM = _ } ) -> do
        let ex = CheckSendKeybResponseException (ToUserId usId) $ "NEGATIVE RESPONSE:\n" ++ show json ++ "\nKEYBOARD NOT SENT"
        throwAndLogEx (hLog h) ex
      Just _                       -> do
        logInfo (hLog h) ("Keyboard with message: " ++ show n ++ show txt ++ " was sent to user " ++ show usId ++ "\n")

checkGetUploadServResponse :: (Monad m, MonadCatch m) => Handle m -> Response -> m ServerUrl
checkGetUploadServResponse h json = do
  case decode json of
      Just (UploadServerResponse (UploadUrl serUrl)) -> return serUrl
      _                      -> do
        let ex = CheckGetUploadServerResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
        throwAndLogPrepAttEx (hLog h) ex


checkLoadDocResponse :: (Monad m, MonadCatch m) => Handle m -> Response -> m LoadDocResp
checkLoadDocResponse h json = do
  case decode json of
      Just loadDocResp@(LoadDocResp _) -> return loadDocResp
      _                      -> do
        let ex = CheckLoadToServResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
        throwAndLogPrepAttEx (hLog h) ex

checkSaveDocResponse :: (Monad m, MonadCatch m) => Handle m -> Response -> m DocInfo
checkSaveDocResponse h json = do
  case decode json of
      Just (SaveDocResp (ResponseSDR "doc" docInf )) -> return docInf
      _                      -> do
        let ex = CheckSaveOnServResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
        throwAndLogPrepAttEx (hLog h) ex

checkSaveDocAuMesResponse :: (Monad m, MonadCatch m) => Handle m -> Response -> m DocInfo
checkSaveDocAuMesResponse h json = do
  case decode json of
      Just (SaveDocAuMesResp (ResponseSDAMR "audio_message" docInf )) -> return docInf
      _                      -> do
        let ex = CheckSaveOnServResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
        throwAndLogPrepAttEx (hLog h) ex

checkLoadPhotoResponse :: (Monad m, MonadCatch m) => Handle m -> Response -> m LoadPhotoResp
checkLoadPhotoResponse h json = do
  case decode json of
      Just loadPhotoResp@(LoadPhotoResp _ _ _) -> return loadPhotoResp
      _                      -> do
        let ex = CheckLoadToServResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
        throwAndLogPrepAttEx (hLog h) ex


checkSavePhotoResponse :: (Monad m, MonadCatch m) => Handle m -> Response -> m DocInfo
checkSavePhotoResponse h json = do
  case decode json of
      Just (SavePhotoResp [DocInfo id' ownerId ]) -> return (DocInfo id' ownerId )
      _                      -> do
        let ex = CheckSaveOnServResponseException $ "UNKNOWN RESPONSE:\n"   ++ show json
        throwAndLogPrepAttEx (hLog h) ex





getLongPollServer' :: Handle IO -> IO Response
getLongPollServer' h = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/groups.getLongPollServer?group_id=" ++ show (cGroupId (hConf h)) ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

getUpdates' :: ServerInfo -> IO Response
getUpdates' (ServerInfo key server ts) = do
  manager <- newTlsManager  
  req <- parseRequest $ T.unpack server ++ "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=20"
  res <- httpLbs req manager
  return (responseBody res)

sendMsg' :: Handle IO -> UserId -> MSG -> IO Response
sendMsg' h usId msg = do
  manager <- newTlsManager 
  let paramList = chooseParamsForMsg msg
  let param1 = "user_id=" ++ show usId
  let param2 = "random_id=0"
  let param3 = "access_token=" ++ cBotToken (hConf h)
  let param4 = "v=5.103"
  let params  = intercalate "&" (param1:param2:param3:param4:paramList)
  req <- parseRequest $ "https://api.vk.com/method/messages.send?" ++ params
  res <- httpLbs req manager
  return (responseBody res)


sendKeyb' :: Handle IO -> UserId -> N -> TextOfKeyb -> IO Response
sendKeyb' h usId n txt = do
  manager <- newTlsManager
  initReq <- parseRequest $ "https://api.vk.com/method/messages.send"
  let param1 = ("user_id"     , fromString . show $ usId) 
  let param2 = ("random_id"   ,"0")
  let param3 = ("message"     , fromString (show  n ++ T.unpack txt))
  let param4 = ("keyboard"    , LBS.toStrict . encode $ keyBoard)
  let param5 = ("access_token", fromString (cBotToken $ (hConf h))) 
  let param6 = ("v"           ,"5.103")
  let params = param1 : param2 : param3 : param4 : param5 : param6 : [] 
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)

getDocServer' :: Handle IO -> UserId -> TypeInGetServerReq -> IO Response
getDocServer' h usId type' = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/docs.getMessagesUploadServer?type=" ++ type' ++ "&peer_id=" ++ show usId ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

loadDocToServ' :: ServerUrl -> DocUrl -> ResponseS -> Extention -> IO Response
loadDocToServ' serUrl docUrl bs ext = do
  manager <- newTlsManager
  initReq <- parseRequest $ T.unpack serUrl
  req     <- (formDataBody [partFileRequestBody "file" (T.unpack docUrl ++ " file." ++ ext) $ RequestBodyBS bs]
                initReq)
  res <- httpLbs req manager
  return (responseBody res)

saveDocOnServ' :: Handle IO -> LoadDocResp -> Title -> IO Response
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

getPhotoServer' :: Handle IO -> UserId -> IO Response
getPhotoServer' h usId = do
  manager <- newTlsManager
  req <- parseRequest $ "https://api.vk.com/method/photos.getMessagesUploadServer?peer_id=" ++ show usId ++ "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

loadPhotoToServ' :: ServerUrl -> PicUrl -> BS.ByteString -> IO Response
loadPhotoToServ' serUrl picUrl bs = do
  manager <- newTlsManager
  initReq <- parseRequest $ T.unpack serUrl
  req     <- formDataBody [partFileRequestBody "photo" (T.unpack picUrl) $ RequestBodyBS bs]
                initReq
  res <- httpLbs req manager
  return (responseBody res)

savePhotoOnServ' :: Handle IO -> LoadPhotoResp -> IO Response
savePhotoOnServ' h (LoadPhotoResp server hash photo) = do
  manager <- newTlsManager
  initReq <- parseRequest "https://api.vk.com/method/photos.saveMessagesPhoto"
  let param1 = ("server"      , fromString . show $ server) 
  let param2 = ("hash"        , fromString hash)
  let param3 = ("photo"       , fromString photo)
  let param4 = ("access_token", fromString $ cBotToken (hConf h)) 
  let param5 = ("v"           ,"5.103")
  let params = [param1, param2, param3, param4, param5]
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)

goToUrl' :: Url -> IO ResponseS
goToUrl' urlTxt = do
  manager <- newTlsManager
  req <- parseRequest $ T.unpack urlTxt
  res  <- httpLbs req manager
  let bs = LBS.toStrict . responseBody $ res
  return bs

chooseParamsForMsg :: MSG -> [ParameterString]
chooseParamsForMsg (TextMsg txt) =
  let param  = "message=" ++ T.unpack txt
  in [param]

chooseParamsForMsg (StickerMsg idSt) =
  let param  = "sticker_id=" ++ show idSt
  in [param]

chooseParamsForMsg (AttachmentMsg txt attachStrings (latStr,longStr)) =
  let param1  = "message=" ++ T.unpack txt
      param2  = "attachment=" ++ intercalate "," attachStrings
      param3  = "lat=" ++ latStr
      param4  = "long=" ++ longStr
  in [param1, param2, param3, param4]

extractServerInfo :: Response -> ServerInfo
extractServerInfo = liftA3 ServerInfo keySI serverSI tsSI . (responseGPSJB . fromJust . decode)

changeServerInfo :: ServerInfo -> ServerAndMapUserN -> ServerAndMapUserN
changeServerInfo newInfo (_,mapUN) = (newInfo,mapUN)

changeTs :: T.Text -> ServerAndMapUserN -> ServerAndMapUserN
changeTs newTs (serverInfo,mapUN) = (serverInfo {tsSI = newTs}, mapUN)

changeSecond :: (b -> b) -> (a,b) -> (a,b)
changeSecond f (a,b) = (a, f b)

changeMapUserN :: UserId -> NState -> MapUserN -> MapUserN
changeMapUserN = Map.insert 


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

addInfoAboutTxt :: TextOfMsg -> String
addInfoAboutTxt "" = ""
addInfoAboutTxt txt = " with text: " ++ show txt  