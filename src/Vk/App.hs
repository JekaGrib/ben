{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.App where

import Vk.App.PrepareAttachment (getAttachmentString)
import qualified Vk.App.PrepareAttachment (Handle,makeH)
import Control.Monad.Except (runExceptT)
import Control.Applicative (liftA3)
import Control.Monad.Catch (MonadCatch(catch))
import Control.Monad.State (StateT, forever, gets, lift, modify, replicateM_)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import qualified Data.Map as Map (insert, lookup)
import Data.Maybe (fromJust)
import Data.String (fromString)
import qualified Data.Text as T
import Network.HTTP.Client
  ( httpLbs
  , parseRequest
  , responseBody
  , urlEncodedBody
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Vk.Api.Request (keyBoard)
import Vk.Api.Response
import Vk.Conf (Config(..))
import Vk.Logger (LogHandle(..), logDebug, logInfo, logWarning)
import Vk.Oops
  ( PrependAttachmetException(..)
  , VKBotException(..)
  , handleExGetLongPollServ
  , handleExGetUpd
  , handleExSendKeyb
  , handleExSendMsg
  , throwAndLogEx
  , throwAndLogPrepAttEx
  )
import Vk.Types

data Handle m =
  Handle
    { hConf :: Config
    , hLog :: LogHandle m
    , getLongPollServer :: m Response
    , getUpdates :: ServerInfo -> m Response
    , sendMsg :: UserId -> MSG -> m Response
    , sendKeyb :: UserId -> N -> TextOfKeyb -> m Response
    , hPrepAttach :: Vk.App.PrepareAttachment.Handle m
    }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH = Handle 
  conf
  logH
  (getLongPollServer' conf)
  getUpdates'
  (sendMsg' conf)
  (sendKeyb' conf)
  (Vk.App.PrepareAttachment.makeH conf logH)


-- logic functions:
run :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m ()
run h = do
  getServer h
  forever $ runServ h

getServer ::
     (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m ()
getServer h = do
  lift $
    logDebug (hLog h) $
    "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=" ++
    show (cGroupId (hConf h)) ++
    "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  jsonServ <-
    lift $ getLongPollServer h `catch` handleExGetLongPollServ (hLog h)
  lift $ logDebug (hLog h) ("Get response: " ++ show jsonServ)
  lift $ checkGetServResponse h jsonServ
  let servInf = extractServerInfo jsonServ
  modify $ changeServerInfo servInf

runServ :: (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m ()
runServ h = do
  upds <- getUpdAndCheckResp h
  mapM_ (chooseActionOfUpd h) upds

getUpdAndCheckResp ::
     (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m [Update]
getUpdAndCheckResp h = do
  json <- getUpdAndLog h
  checkAndPullUpdates h json

getUpdAndLog ::
     (Monad m, MonadCatch m) => Handle m -> StateT ServerAndMapUserN m Response
getUpdAndLog h = do
  sI@(ServerInfo key server ts) <- gets fst
  lift $
    logDebug (hLog h) $
    "Send request to getUpdates: " ++
    T.unpack server ++
    "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=20"
  json <- lift $ getUpdates h sI `catch` handleExGetUpd (hLog h)
  lift $ logDebug (hLog h) ("Get response: " ++ show json )
  return json

chooseActionOfUpd ::
     (Monad m, MonadCatch m)
  => Handle m
  -> Update
  -> StateT ServerAndMapUserN m ()
chooseActionOfUpd h upd = do
  lift $ logInfo (hLog h) "Analysis update from the list"
  case upd of
    Update "message_new" obj -> do
      lift $ logInfo (hLog h) (logStrForGetObj obj)
      chooseActionOfNState h obj
    UnknownUpdate _ ->
      lift $
      logWarning
        (hLog h)
        ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT. " ++ show upd )
    _ ->
      lift $
      logWarning
        (hLog h)
        ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT. " ++ show upd )

chooseActionOfNState ::
     (Monad m, MonadCatch m)
  => Handle m
  -> AboutObj
  -> StateT ServerAndMapUserN m ()
chooseActionOfNState h obj@(AboutObj usId _ _ _ _ _ _) = do
  mapUN <- gets snd
  let nState = Map.lookup usId mapUN
  case nState of
    Just (Left (OpenRepeat oldN)) -> do
      lift $
        logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode")
      chooseActionOfButton h obj oldN
    Just (Right n) -> do
      let currN = n
      chooseActionOfObject h obj currN
    Nothing -> do
      let currN = cStartN (hConf h)
      chooseActionOfObject h obj currN

chooseActionOfButton ::
     (Monad m, MonadCatch m)
  => Handle m
  -> AboutObj
  -> N
  -> StateT ServerAndMapUserN m ()
chooseActionOfButton h obj@(AboutObj usId _ _ _ _ _ _) oldN =
  case checkButton obj of
    Just newN -> do
      lift $
        logInfo
          (hLog h)
          ("Change number of repeats to " ++
           show newN ++ " for user " ++ show usId)
      modify $ changeSecond $ changeMapUserN usId $ Right newN
      let infoMsg =
            T.pack $
            "Number of repeats successfully changed from " ++
            show oldN ++ " to " ++ show newN
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg
    Nothing -> do
      lift $
        logWarning
          (hLog h)
          ("User " ++
           show usId ++
           " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++
           show oldN )
      modify $ changeSecond $ changeMapUserN usId $ Right oldN
      let infoMsg =
            T.pack $
            "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++
            show oldN ++
            "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later"
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg

chooseActionOfObject ::
     (Monad m, MonadCatch m)
  => Handle m
  -> AboutObj
  -> N
  -> StateT ServerAndMapUserN m ()
chooseActionOfObject h obj currN =
  case obj of
    AboutObj usId _ _ txt [] [] Nothing -> chooseActionOfTxt h currN usId txt
    AboutObj usId _ _ "" [] [StickerAttachment (StickerInfo idSt)] Nothing ->
      lift $
      replicateM_ currN $ do
        let msg = StickerMsg idSt
        sendMsgAndCheckResp h usId msg
    AboutObj _ _ _ _ [] _ _ -> chooseActionOfAttachs h currN obj
    AboutObj usId _ _ _ _ _ _ -> do
      lift $
        logWarning
          (hLog h)
          ("There is forward message. BOT WILL IGNORE IT. " ++ show obj)
      let infoMsg =
              "I`m sorry, I can`t work with forward messages, so I will ignore this message"
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg

chooseActionOfTxt ::
     (Monad m, MonadCatch m)
  => Handle m
  -> N
  -> UserId
  -> TextOfMsg
  -> StateT ServerAndMapUserN m ()
chooseActionOfTxt h currN usId txt =
  case filter (' ' /=) . T.unpack $ txt of
    "/help" -> do
      let infoMsg = T.pack $ cHelpMsg (hConf h)
      lift $
        logDebug
          (hLog h)
          ("Send request to send msg https://api.vk.com/method/messages.send?user_id=" ++
           show usId ++
           "&random_id=0&message=" ++
           T.unpack infoMsg ++
           "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103")
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg
    "/repeat" -> do
      let infoMsg =
            T.pack $
            " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
      lift $ sendKeybAndCheckResp h usId currN infoMsg
      modify $ changeSecond $ changeMapUserN usId $ Left $ OpenRepeat currN
    _ ->
      lift $
      replicateM_ currN $ do
        let msg = TextMsg txt
        sendMsgAndCheckResp h usId msg

chooseActionOfAttachs ::
     (Monad m, MonadCatch m)
  => Handle m
  -> N
  -> AboutObj
  -> StateT ServerAndMapUserN m ()
chooseActionOfAttachs h currN (AboutObj usId _ _ txt _ attachs maybeGeo) = do
  eitherAttachStrings <- lift $ runExceptT $ mapM (getAttachmentString (hPrepAttach h) usId) attachs
  case eitherAttachStrings of
    Right attachStrings ->
      lift $
      replicateM_ currN $ do
        latLong <- checkAndPullLatLong h maybeGeo
        let msg = AttachmentMsg txt attachStrings latLong
        sendMsgAndCheckResp h usId msg
    Left str ->
      lift $
      logWarning
        (hLog h)
        ("There is UNKNOWN ATTACMENT in updateList. BOT WILL IGNORE IT. " ++ str ++ "\n")


sendMsgAndCheckResp ::
     (Monad m, MonadCatch m) => Handle m -> UserId -> MSG -> m ()
sendMsgAndCheckResp h usId msg = do
  logDebug
    (hLog h)
    ("Send request to send to user_id:" ++
     show usId ++ " msg: " ++ show msg )
  response <- sendMsg h usId msg `catch` handleExSendMsg (hLog h) usId msg
  logDebug (hLog h) ("Get response: " ++ show response )
  checkSendMsgResponse h usId msg response

sendKeybAndCheckResp ::
     (Monad m, MonadCatch m) => Handle m -> UserId -> N -> TextOfKeyb -> m ()
sendKeybAndCheckResp h usId currN txt = do
  logDebug (hLog h) $
    "Send request to send keyboard to user: " ++
    show usId ++ " with message: " ++ show currN ++ show txt
  response <- sendKeyb h usId currN txt `catch` handleExSendKeyb (hLog h) usId
  logDebug (hLog h) ("Get response: " ++ show response )
  checkSendKeybResponse h usId currN txt response

checkGetServResponse :: (Monad m, MonadCatch m) => Handle m -> Response -> m ()
checkGetServResponse h json =
  case decode json of
    Nothing -> do
      let ex =
            CheckGetServerResponseException $ "UNKNOWN RESPONSE:" ++ show json
      throwAndLogEx (hLog h) ex
    Just ErrorAnswerServ {} -> do
      let ex =
            CheckGetServerResponseException $
            "NEGATIVE RESPONSE:" ++ show json
      throwAndLogEx (hLog h) ex
    Just _ -> logInfo (hLog h) "Work with received server"

checkAndPullUpdates ::
     (Monad m, MonadCatch m)
  => Handle m
  -> Response
  -> StateT ServerAndMapUserN m [Update]
checkAndPullUpdates h json =
  case decode json of
    Nothing -> do
      let ex =
            CheckGetUpdatesResponseException $
            "UNKNOWN RESPONSE:" ++ show json
      lift $ throwAndLogEx (hLog h) ex
    Just ErrorAnswer {} -> do
      let ex =
            CheckGetUpdatesResponseException $
            "NEGATIVE RESPONSE:" ++ show json
      lift $ throwAndLogEx (hLog h) ex
    Just (FailAnswer 2) -> do
      lift $
        logWarning
          (hLog h)
          "FAIL. Long poll server key expired, need to request new key"
      getServer h
      getUpdAndCheckResp h
    Just (FailAnswer 3) -> do
      lift $
        logWarning
          (hLog h)
          "FAIL. Long poll server information is lost, need to request new key and ts"
      getServer h
      getUpdAndCheckResp h
    Just FailTSAnswer {failFTSA = 1, tsFTSA = ts} -> do
      lift $
        logWarning
          (hLog h)
          "FAIL number 1. Ts in request is wrong, need to use received ts"
      modify $ changeTs (T.pack . show $ ts)
      getUpdAndCheckResp h
    Just FailTSAnswer {tsFTSA = ts} -> do
      lift $
        logWarning
          (hLog h)
          "FAIL. Ts in request is wrong, need to use received ts"
      modify $ changeTs (T.pack . show $ ts)
      getUpdAndCheckResp h
    Just (FailAnswer _) -> do
      let ex =
            CheckGetUpdatesResponseException $
            "NEGATIVE RESPONSE:" ++ show json
      lift $ throwAndLogEx (hLog h) ex
    Just AnswerOk {updates = []} -> do
      lift $ logInfo (hLog h) "No new updates"
      return []
    Just (AnswerOk ts upds) -> do
      modify $ changeTs ts
      lift $ logInfo (hLog h) "There is new updates list"
      return upds

checkAndPullLatLong ::
     (Monad m, MonadCatch m) => Handle m -> Maybe Geo -> m LatLong
checkAndPullLatLong h maybeGeo =
  case maybeGeo of
    Nothing -> return ("", "")
    Just (Geo "point" (Coordinates lat long)) -> return (show lat, show long)
    _ -> do
      let ex = GetUpdatesException $ "UNKNOWN GEO type" ++ show maybeGeo
      throwAndLogEx (hLog h) ex

checkSendMsgResponse ::
     (Monad m, MonadCatch m) => Handle m -> UserId -> MSG -> Response -> m ()
checkSendMsgResponse h usId msg json =
  case decode json of
    Nothing -> do
      let ex =
            CheckSendMsgResponseException msg (ToUserId usId) $
            "UNKNOWN RESPONSE:" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"
      throwAndLogEx (hLog h) ex
    Just ErrorAnswerMsg {} -> do
      let ex =
            CheckSendMsgResponseException msg (ToUserId usId) $
            "NEGATIVE RESPONSE:" ++ show json ++ "\nMESSAGE NOT SENT"
      throwAndLogEx (hLog h) ex
    Just _ ->
      case msg of
        TextMsg txt ->
          logInfo
            (hLog h)
            ("Msg " ++ show txt ++ " was sent to user " ++ show usId )
        StickerMsg idSt ->
          logInfo
            (hLog h)
            ("Sticker_id " ++
             show idSt ++ " was sent to user " ++ show usId )
        AttachmentMsg txt attachStrings ("", "") ->
          logInfo
            (hLog h)
            ("AttachmentMsg was sent to user " ++
             show usId ++
             ". Text: " ++
             show txt ++ "; attachments: " ++ show attachStrings )
        AttachmentMsg txt [] latLong ->
          logInfo
            (hLog h)
            ("GeoMsg was sent to user " ++
             show usId ++
             ". Text: " ++ show txt ++ "; geo: " ++ show latLong )
        AttachmentMsg txt attachStrings latLong ->
          logInfo
            (hLog h)
            ("AttachmentAndGeoMsg was sent to user " ++
             show usId ++
             ". Text: " ++
             show txt ++
             "; attachments: " ++
             show attachStrings ++ "; geo: " ++ show latLong )

checkSendKeybResponse ::
     (Monad m, MonadCatch m)
  => Handle m
  -> UserId
  -> N
  -> TextOfKeyb
  -> Response
  -> m ()
checkSendKeybResponse h usId n txt json =
  case decode json of
    Nothing -> do
      let ex =
            CheckSendKeybResponseException (ToUserId usId) $
            "UNKNOWN RESPONSE:" ++ show json ++ "\nKEYBOARD PROBABLY NOT SENT"
      throwAndLogEx (hLog h) ex
    Just ErrorAnswerMsg {} -> do
      let ex =
            CheckSendKeybResponseException (ToUserId usId) $
            "NEGATIVE RESPONSE:" ++ show json ++ "\nKEYBOARD NOT SENT"
      throwAndLogEx (hLog h) ex
    Just _ ->
      logInfo
        (hLog h)
        ("Keyboard with message: " ++
         show n ++ show txt ++ " was sent to user " ++ show usId)

checkGetUploadServResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m ServerUrl
checkGetUploadServResponse h json =
  case decode json of
    Just (UploadServerResponse (UploadUrl serUrl)) -> return serUrl
    _ -> do
      let ex =
            CheckGetUploadServerResponseException $
            "UNKNOWN RESPONSE:" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkLoadDocResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m LoadDocResp
checkLoadDocResponse h json =
  case decode json of
    Just loadDocResp@(LoadDocResp _) -> return loadDocResp
    _ -> do
      let ex =
            CheckLoadToServResponseException $
            "UNKNOWN RESPONSE:" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkSaveDocResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m DocInfo
checkSaveDocResponse h json =
  case decode json of
    Just (SaveDocResp (ResponseSDR "doc" docInf)) -> return docInf
    _ -> do
      let ex =
            CheckSaveOnServResponseException $
            "UNKNOWN RESPONSE:" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkSaveDocAuMesResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m DocInfo
checkSaveDocAuMesResponse h json =
  case decode json of
    Just (SaveDocAuMesResp (ResponseSDAMR "audio_message" docInf)) ->
      return docInf
    _ -> do
      let ex =
            CheckSaveOnServResponseException $
            "UNKNOWN RESPONSE:" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkLoadPhotoResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m LoadPhotoResp
checkLoadPhotoResponse h json =
  case decode json of
    Just loadPhotoResp -> return loadPhotoResp
    _ -> do
      let ex =
            CheckLoadToServResponseException $
            "UNKNOWN RESPONSE:" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkSavePhotoResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m DocInfo
checkSavePhotoResponse h json =
  case decode json of
    Just (SavePhotoResp [DocInfo id' ownerId]) -> return (DocInfo id' ownerId)
    _ -> do
      let ex =
            CheckSaveOnServResponseException $
            "UNKNOWN RESPONSE:" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

-- IO handle functions:
getLongPollServer' :: Config -> IO Response
getLongPollServer' conf = do
  manager <- newTlsManager
  req <-
    parseRequest $
    "https://api.vk.com/method/groups.getLongPollServer?group_id=" ++
    show (cGroupId conf) ++
    "&access_token=" ++ cBotToken conf ++ "&v=5.103"
  responseBody <$> httpLbs req manager

getUpdates' :: ServerInfo -> IO Response
getUpdates' (ServerInfo key server ts) = do
  manager <- newTlsManager
  req <-
    parseRequest $
    T.unpack server ++
    "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ T.unpack ts ++ "&wait=20"
  responseBody <$> httpLbs req manager

sendMsg' :: Config -> UserId -> MSG -> IO Response
sendMsg' conf usId msg = do
  manager <- newTlsManager
  let paramList = chooseParamsForMsg msg
  let param1 = "user_id=" ++ show usId
  let param2 = "random_id=0"
  let param3 = "access_token=" ++ cBotToken conf
  let param4 = "v=5.103"
  let params = intercalate "&" (param1 : param2 : param3 : param4 : paramList)
  req <- parseRequest $ "https://api.vk.com/method/messages.send?" ++ params
  responseBody <$> httpLbs req manager

sendKeyb' :: Config -> UserId -> N -> TextOfKeyb -> IO Response
sendKeyb' conf usId n txt = do
  manager <- newTlsManager
  initReq <- parseRequest "https://api.vk.com/method/messages.send"
  let param1 = ("user_id", fromString . show $ usId)
  let param2 = ("random_id", "0")
  let param3 = ("message", fromString (show n ++ T.unpack txt))
  let param4 = ("keyboard", LBS.toStrict . encode $ keyBoard)
  let param5 = ("access_token", fromString $ cBotToken conf)
  let param6 = ("v", "5.103")
  let params = [param1, param2, param3, param4, param5, param6]
  let req = urlEncodedBody params initReq
  responseBody <$> httpLbs req manager

-- clear functions:
chooseParamsForMsg :: MSG -> [ParameterString]
chooseParamsForMsg (TextMsg txt) =
  let param = "message=" ++ T.unpack txt
   in [param]
chooseParamsForMsg (StickerMsg idSt) =
  let param = "sticker_id=" ++ show idSt
   in [param]
chooseParamsForMsg (AttachmentMsg txt attachStrings (latStr, longStr)) =
  let param1 = "message=" ++ T.unpack txt
      param2 = "attachment=" ++ intercalate "," attachStrings
      param3 = "lat=" ++ latStr
      param4 = "long=" ++ longStr
   in [param1, param2, param3, param4]

extractServerInfo :: Response -> ServerInfo
extractServerInfo =
  liftA3 ServerInfo keySI serverSI tsSI . (responseGPSJB . fromJust . decode)

changeServerInfo :: ServerInfo -> ServerAndMapUserN -> ServerAndMapUserN
changeServerInfo newInfo (_, mapUN) = (newInfo, mapUN)

changeTs :: T.Text -> ServerAndMapUserN -> ServerAndMapUserN
changeTs newTs (serverInfo, mapUN) = (serverInfo {tsSI = newTs}, mapUN)

changeSecond :: (b -> b) -> (a, b) -> (a, b)
changeSecond f (a, b) = (a, f b)

changeMapUserN :: UserId -> NState -> MapUserN -> MapUserN
changeMapUserN = Map.insert

checkButton :: AboutObj -> Maybe N
checkButton obj =
  case obj of
    AboutObj _ _ _ txt [] [] Nothing -> checkTextButton txt
    _ -> Nothing

checkTextButton :: T.Text -> Maybe N
checkTextButton txt =
  case txt of
    "1" -> Just 1
    "2" -> Just 2
    "3" -> Just 3
    "4" -> Just 4
    "5" -> Just 5
    _ -> Nothing

logStrForGetObj :: AboutObj -> String
logStrForGetObj (AboutObj usId _ _ txt [] [] Nothing) =
  "Get TextMsg: " ++ show txt ++ " from user " ++ show usId 
logStrForGetObj (AboutObj usId _ _ txt fwds [] Nothing) =
  "Get ForwardMsg: " ++
  show fwds ++ addInfoAboutTxt txt ++ " from user " ++ show usId 
logStrForGetObj (AboutObj usId _ _ txt [] [] (Just geo)) =
  "Get GeoMsg: " ++
  show geo ++ addInfoAboutTxt txt ++ " from user " ++ show usId 
logStrForGetObj (AboutObj usId _ _ txt [] attachs Nothing) =
  "Get AttachmentMsg: " ++
  show attachs ++ addInfoAboutTxt txt ++ " from user " ++ show usId 
logStrForGetObj (AboutObj usId _ _ txt fwds attachs Nothing) =
  "Get AttachmentMsg: " ++
  show attachs ++
  addInfoAboutTxt txt ++
  ", with ForwardParts: " ++ show fwds ++ "  from user " ++ show usId 
logStrForGetObj (AboutObj usId _ _ txt [] attachs (Just geo)) =
  "Get AttachmentMsg: " ++
  show attachs ++
  addInfoAboutTxt txt ++
  ", with Geo: " ++ show geo ++ "  from user " ++ show usId 
logStrForGetObj (AboutObj usId _ _ txt fwds [] (Just geo)) =
  "Get ForwardMsg: " ++
  show fwds ++
  addInfoAboutTxt txt ++
  ", with Geo: " ++ show geo ++ "  from user " ++ show usId 
logStrForGetObj (AboutObj usId _ _ txt fwds attachs (Just geo)) =
  "Get AttachmentMsg: " ++
  show attachs ++
  addInfoAboutTxt txt ++
  ", with Geo: " ++
  show geo ++
  ", with ForwardParts: " ++ show fwds ++ "  from user " ++ show usId 

addInfoAboutTxt :: TextOfMsg -> String
addInfoAboutTxt "" = ""
addInfoAboutTxt txt = " with text: " ++ show txt
