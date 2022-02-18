{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


module Vk.App where

import Vk.App.PrepareAttachment (getAttachmentString)
import qualified Vk.App.PrepareAttachment (Handle,makeH)
import Control.Monad.Except (runExceptT)
import Control.Monad.Catch (MonadCatch(catch))
import Control.Monad.State ( lift,replicateM_,forever,evalStateT)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import qualified Data.Map as Map (insert, lookup)
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
  ( VKBotException(..)
  , handleExGetLongPollServ
  , handleExGetUpd
  , handleExSendKeyb
  , handleExSendMsg
  , throwAndLogEx
  )
import Vk.Types
import Vk.AppT (AppT,MonadStateTwo(..))

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
run :: (MonadCatch m) => Handle m -> MapUserN -> m ()
run h initialDB = startApp h initialDB >>= evalStateT (foreverRunServ h)

startApp :: (MonadCatch m) => Handle m -> MapUserN -> m (ServerInfo,MapUserN)
startApp h initialDB = do
  servInfo <- getServInfoAndCheckResp h
  return (servInfo,initialDB)

foreverRunServ :: (MonadCatch m) => Handle m -> AppT m ()
foreverRunServ h = forever (runServ h)

runServ :: (MonadCatch m) => Handle m -> AppT m ()
runServ h = do
  upds <- getUpdAndCheckResp h 
  mapM_ (chooseActionOfUpd h) upds

getServInfoAndCheckResp ::
     (MonadCatch m) => Handle m -> m ServerInfo
getServInfoAndCheckResp h = do
  logDebug (hLog h) $
    "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id=" ++
    show (cGroupId (hConf h)) ++
    "&access_token=" ++ cBotToken (hConf h) ++ "&v=5.103"
  jsonServ <-
    getLongPollServer h `catch` handleExGetLongPollServ (hLog h)
  logDebug (hLog h) ("Get response: " ++ show jsonServ)
  checkGetServResponse h jsonServ

getUpdAndCheckResp ::
     (MonadCatch m) => Handle m -> AppT m [Update]
getUpdAndCheckResp h  = do
  json <- getUpdAndLog h 
  checkAndPullUpdates h json 0

getUpdAndLog ::
     (MonadCatch m) => Handle m -> AppT m Response
getUpdAndLog h = do
  sI@(ServerInfo key server ts) <- get1
  lift $ logDebug (hLog h) $
    "Send request to getUpdates: " ++
    T.unpack server ++
    "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ show ts ++ "&wait=20"
  json <- lift $ getUpdates h sI `catch` handleExGetUpd (hLog h)
  lift $ logDebug (hLog h) ("Get response: " ++ show json )
  return json

chooseActionOfUpd ::
     (MonadCatch m)
  => Handle m
  -> Update
  -> AppT m ()
chooseActionOfUpd h upd = do
  lift $ logInfo (hLog h) "Analysis update from the list"
  case upd of
    Update "message_new" obj -> do
      lift $ logInfo (hLog h) (logStrForGetObj obj)
      chooseActionOfNState h obj
    UnknownUpdate _ ->
      lift $ logWarning
        (hLog h)
        ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT. " ++ show upd )
    _ ->
      lift $ logWarning
        (hLog h)
        ("There is UNKNOWN UPDATE. BOT WILL IGNORE IT. " ++ show upd )

getNState :: UserId -> MapUserN -> Maybe NState
getNState = Map.lookup

chooseActionOfNState ::
     (MonadCatch m)
  => Handle m
  -> AboutObj
  -> AppT m ()
chooseActionOfNState h obj@(AboutObj usId _ _ _ _ _ _) = do
  nState <- (getNState usId <$> get2)
  case nState of
    Just (Left (OpenRepeat oldN)) -> do
      lift $ logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode")
      chooseActionOfButton h obj oldN
    Just (Right n) -> do
      let currN = n
      chooseActionOfObject h obj currN
    Nothing -> do
      let currN = cStartN (hConf h)
      chooseActionOfObject h obj currN

chooseActionOfButton ::
     (MonadCatch m)
  => Handle m
  -> AboutObj
  -> N
  -> AppT m ()
chooseActionOfButton h obj@(AboutObj usId _ _ _ _ _ _) oldN =
  case checkButton obj of
    Just newN -> do
      lift $ logInfo
          (hLog h)
          ("Change number of repeats to " ++
           show newN ++ " for user " ++ show usId)
      modify2 $ changeMapUserN usId $ Right newN
      let infoMsg =
            T.pack $
            "Number of repeats successfully changed from " ++
            show oldN ++ " to " ++ show newN
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg
    Nothing -> do
      lift $ logWarning
          (hLog h)
          ("User " ++
           show usId ++
           " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++
           show oldN )
      modify2 $ changeMapUserN usId $ Right oldN
      let infoMsg =
            T.pack $
            "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++
            show oldN ++
            "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later"
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg

chooseActionOfObject ::
     (MonadCatch m)
  => Handle m
  -> AboutObj
  -> N
  -> AppT m ()
chooseActionOfObject h obj currN =
  case obj of
    AboutObj usId _ _ txt [] [] Nothing -> chooseActionOfTxt h currN usId txt
    AboutObj usId _ _ "" [] [StickerAttachment (StickerInfo idSt)] Nothing ->
      lift $ replicateM_ currN $ do
        let msg = StickerMsg idSt
        sendMsgAndCheckResp h usId msg
    AboutObj _ _ _ _ [] _ _ -> lift $ chooseActionOfAttachs h currN obj
    AboutObj usId _ _ _ _ _ _ -> do
      lift $ logWarning
          (hLog h)
          ("There is forward message. BOT WILL IGNORE IT. " ++ show obj)
      let infoMsg =
              "I`m sorry, I can`t work with forward messages, so I will ignore this message"
      let msg = TextMsg infoMsg
      lift $ sendMsgAndCheckResp h usId msg

chooseActionOfTxt ::
     (MonadCatch m)
  => Handle m
  -> N
  -> UserId
  -> TextOfMsg
  -> AppT m ()
chooseActionOfTxt h currN usId txt =
  case filter (' ' /=) . T.unpack $ txt of
    "/help" -> do
      let infoMsg = T.pack $ cHelpMsg (hConf h)
      lift $ logDebug
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
      modify2 $ changeMapUserN usId $ Left $ OpenRepeat currN
    _ ->
      replicateM_ currN $ do
        let msg = TextMsg txt
        lift $ sendMsgAndCheckResp h usId msg

chooseActionOfAttachs ::
     (MonadCatch m)
  => Handle m
  -> N
  -> AboutObj
  -> m ()
chooseActionOfAttachs h currN (AboutObj usId _ _ txt _ attachs maybeGeo) = do
  eitherAttachStrings <- runExceptT $ mapM (getAttachmentString (hPrepAttach h) usId) attachs
  case eitherAttachStrings of
    Right attachStrings ->
      replicateM_ currN $ do
        latLong <- checkAndPullLatLong h maybeGeo
        let msg = AttachmentMsg txt attachStrings latLong
        sendMsgAndCheckResp h usId msg
    Left str ->
      logWarning
        (hLog h)
        ("There is UNKNOWN ATTACMENT in updateList. BOT WILL IGNORE IT. " ++ str ++ "\n")


sendMsgAndCheckResp ::
     (MonadCatch m) => Handle m -> UserId -> MSG -> m ()
sendMsgAndCheckResp h usId msg = do
  logDebug
    (hLog h)
    ("Send request to send to user_id:" ++
     show usId ++ " msg: " ++ show msg )
  response <- sendMsg h usId msg `catch` handleExSendMsg (hLog h) usId msg
  logDebug (hLog h) ("Get response: " ++ show response )
  checkSendMsgResponse h usId msg response

sendKeybAndCheckResp ::
     (MonadCatch m) => Handle m -> UserId -> N -> TextOfKeyb -> m ()
sendKeybAndCheckResp h usId currN txt = do
  logDebug (hLog h) $
    "Send request to send keyboard to user: " ++
    show usId ++ " with message: " ++ show currN ++ show txt
  response <- sendKeyb h usId currN txt `catch` handleExSendKeyb (hLog h) usId
  logDebug (hLog h) ("Get response: " ++ show response )
  checkSendKeybResponse h usId currN txt response

checkGetServResponse :: (MonadCatch m) => Handle m -> Response -> m ServerInfo
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
    Just (GetPollServerJSONBody servInfo) -> do
      logInfo (hLog h) "Work with received server"
      return servInfo


showServerInfo :: ServerInfo -> String
showServerInfo = show

checkAndPullUpdates ::
     (MonadCatch m)
  => Handle m
  -> Response
  -> Counter
  -> AppT m [Update]
checkAndPullUpdates h json count 
  | count >= 4 = do
      servInfo <- (showServerInfo <$> get1)
      let ex = CheckGetUpdatesResponseException $ "More then three times getUpdates fail:" ++ show json ++ ". Server:" ++ servInfo
      lift $ throwAndLogEx (hLog h) ex
checkAndPullUpdates h json count =
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
      lift $ logWarning
          (hLog h)
          "FAIL. Long poll server key expired, need to request new key"
      newServInfo <- lift $ getServInfoAndCheckResp h
      put1 newServInfo
      newJson <- getUpdAndLog h 
      checkAndPullUpdates h newJson (count+1)
    Just (FailAnswer 3) -> do
      lift $ logWarning
          (hLog h)
          "FAIL. Long poll server information is lost, need to request new key and ts"
      newServInfo <- lift $ getServInfoAndCheckResp h
      put1 newServInfo
      newJson <- getUpdAndLog h 
      checkAndPullUpdates h newJson (count+1)
    Just FailTSAnswer {failFTSA = 1, tsFTSA = ts} -> do
      lift $ logWarning
          (hLog h)
          "FAIL number 1. Ts in request is wrong, need to use received ts"
      modify1 (changeTs ts)
      newJson <- getUpdAndLog h 
      checkAndPullUpdates h  newJson (count+1)
    Just FailTSAnswer {tsFTSA = ts} -> do
      lift $ logWarning
          (hLog h)
          "FAIL. Ts in request is wrong, need to use received ts"
      modify1 (changeTs ts)
      newJson <- getUpdAndLog h 
      checkAndPullUpdates h newJson (count+1)
    Just (FailAnswer _) -> do
      let ex =
            CheckGetUpdatesResponseException $
            "NEGATIVE RESPONSE:" ++ show json
      lift $ throwAndLogEx (hLog h) ex
    Just AnswerOk {updates = []} -> do
      lift $ logInfo (hLog h) "No new updates"
      return []
    Just (AnswerOk ts upds) -> do
      lift $ logInfo (hLog h) "There is new updates list"
      modify1 (changeTs ts)
      return upds

checkAndPullLatLong ::
     (MonadCatch m) => Handle m -> Maybe Geo -> m LatLong
checkAndPullLatLong h maybeGeo =
  case maybeGeo of
    Nothing -> return ("", "")
    Just (Geo "point" (Coordinates lat long)) -> return (show lat, show long)
    _ -> do
      let ex = GetUpdatesException $ "UNKNOWN GEO type" ++ show maybeGeo
      throwAndLogEx (hLog h) ex

checkSendMsgResponse ::
     (MonadCatch m) => Handle m -> UserId -> MSG -> Response -> m ()
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
     (MonadCatch m)
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
    "?act=a_check&key=" ++ T.unpack key ++ "&ts=" ++ show ts ++ "&wait=20"
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


changeMapUserN :: UserId -> NState -> MapUserN -> MapUserN
changeMapUserN = Map.insert

changeTs :: Integer -> ServerInfo -> ServerInfo
changeTs ts servInfo = servInfo {tsSI=ts}

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
