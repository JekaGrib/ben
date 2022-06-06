{-# LANGUAGE OverloadedStrings #-}

module Vk.App where

import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT, forever, lift, modify,get)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.String (fromString)
import qualified Data.Text as T
import Network.HTTP.Client
  ( httpLbs,
    parseRequest,
    responseBody,
    urlEncodedBody,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Vk.Api.Request (keyBoard)
import Vk.Api.Response
import Vk.App.PrepareAttachment (getAttachmentString)
import qualified Vk.App.PrepareAttachment (Handle, makeH)
import Vk.AppT ( AppT,TryServer (..), changeServInfo, changeTs, firstTry, nextTry, resetTry)
import Conf (Config (..))
import Vk.Conf (VkConfig (..))
import Logger (LogHandle (..), logDebug, logInfo, logWarning)
import Vk.Oops
  ( VKBotException (..),
    handleExGetLongPollServ,
    handleExGetUpd,
--    handleExSendKeyb,
--    handleExSendMsg,
    throwAndLogEx,
  )
import Vk.Types
import Types
import qualified App

data Handle m = Handle
  { hConf :: VkConfig,
    hLog :: LogHandle m,
    getLongPollServer :: m Response,
    getUpdates :: ServerInfo -> m Response,
    hApp :: App.Handle m VkAttachMSG,
    hPrepAttach :: Vk.App.PrepareAttachment.Handle m
  }

makeH :: VkConfig -> LogHandle IO -> Handle IO
makeH vkConf logH =
  Handle
    vkConf
    logH
    (getLongPollServer' vkConf)
    getUpdates'
    (makeAppH (cConf vkConf) logH)
    (Vk.App.PrepareAttachment.makeH (cConf vkConf) logH)

makeAppH :: Config -> LogHandle IO -> App.Handle IO VkAttachMSG
makeAppH conf logH =
  App.Handle 
    conf
    logH
    (sendMsg' conf)
    (sendKeyb' conf)
    (sendAttachMsg' conf)
    isValidResponse'

-- logic functions:
run :: (MonadCatch m) => Handle m -> MapUserN -> m ()
run h initialDB = do
  servInfo <- startApp h 
  evalStateT (evalStateT (foreverRunServ h) servInfo) initialDB

startApp :: (MonadCatch m) => Handle m -> m TryServer
startApp h = do
  servInfo <- getServInfoAndCheckResp h
  return (firstTry servInfo)

foreverRunServ :: (MonadCatch m) => Handle m -> AppT m ()
foreverRunServ h = forever (runServ h)

runServ :: (MonadCatch m) => Handle m -> AppT m ()
runServ h = do
  upds <- getUpdAndCheckResp h
  lift $ mapM_ (\upd -> (lift $ isValidUpdate h upd) >>= App.chooseActionOfUpd (hApp h)) upds



getServInfoAndCheckResp ::
  (MonadCatch m) => Handle m -> m ServerInfo
getServInfoAndCheckResp h = do
  logDebug (hLog h) $
    "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id="
      ++ show (cGroupId (hConf h))
      ++ "&access_token="
      ++ cBotToken (cConf (hConf h))
      ++ "&v=5.103"
  jsonServ <-
    getLongPollServer h `catch` handleExGetLongPollServ (hLog h)
  logDebug (hLog h) ("Get response: " ++ show jsonServ)
  checkGetServResponse h jsonServ

getUpdAndCheckResp ::
  (MonadCatch m) => Handle m -> AppT m [Update]
getUpdAndCheckResp h = do
  json <- getUpdAndLog h
  checkAndPullUpdates h json

getUpdAndLog ::
  (MonadCatch m) => Handle m -> AppT m Response
getUpdAndLog h = do
  sI@(ServerInfo key server ts) <- servInf <$> get
  lift $ lift $ logDebug (hLog h) $
    "Send request to getUpdates: "
      ++ T.unpack server
      ++ "?act=a_check&key="
      ++ T.unpack key
      ++ "&ts="
      ++ show ts
      ++ "&wait=20"
  json <- lift $ lift $ getUpdates h sI `catch` handleExGetUpd (hLog h)
  lift $ lift $ logDebug (hLog h) ("Get response: " ++ show json)
  return json


isValidUpdate :: (MonadCatch m) => Handle m -> Update -> m (IsValidUpdate VkAttachMSG)
isValidUpdate _ (UnknownUpdate _) = return InvalidUpdate
isValidUpdate h (Update "message_new" obj) = chooseUpdType h obj
isValidUpdate _ _ = return InvalidUpdate

chooseUpdType :: (MonadCatch m) => Handle m -> AboutObj -> m (IsValidUpdate VkAttachMSG)
chooseUpdType _ (AboutObj usId _ _ txt [] [] Nothing) = 
  return $ ValidUpdate usId $ TextMsg txt
chooseUpdType _ (AboutObj usId _ _ ""  [] [StickerAttachment (StickerInfo idSt)] Nothing) = 
  return $ ValidUpdate usId $ AttachMsg (StickerMsg idSt)
chooseUpdType _ (AboutObj _    _ _ _   [] [StickerAttachment _] _) = 
  return $ InvalidUpdatePlusInfo  "There is unknown sticker message. Sticker can`t have text or geo."
chooseUpdType h (AboutObj usId _ _ txt [] attachs maybeGeo) =  
  prepareAttach h usId txt attachs maybeGeo
chooseUpdType _ _ =
  return $ InvalidUpdatePlusInfo "There is forward message"

prepareAttach ::  (MonadCatch m) => Handle m -> UserId -> TextOfMsg -> [Attachment] -> Maybe Geo -> m (IsValidUpdate VkAttachMSG)
prepareAttach h usId txt attachs maybeGeo = do
  eitherAttachStrings <- runExceptT $ mapM (getAttachmentString (hPrepAttach h) usId) attachs
  case eitherAttachStrings of
    Right attachStrings -> do
      let eitherLatLong = checkAndPullLatLong maybeGeo
      case eitherLatLong of
        Right latLong -> return $ ValidUpdate usId $ AttachMsg $ VkAttachMsg txt attachStrings latLong
        Left str -> 
          return $ InvalidUpdatePlusInfo $ "UNKNOWN ATTACMENT in updateList." ++ str    
    Left str ->
      return $ InvalidUpdatePlusInfo $ "UNKNOWN ATTACMENT in updateList." ++ str

checkAndPullLatLong :: Maybe Geo -> Either SomethingWrong LatLong
checkAndPullLatLong maybeGeo =
  case maybeGeo of
    Nothing -> Right ("", "")
    Just (Geo "point" (Coordinates lat long)) -> Right (show lat, show long)
    _ -> Left $ "UNKNOWN GEO type" ++ show maybeGeo

putNextTryWithNewServer :: (MonadCatch m) => Handle m -> AppT m [Update]
putNextTryWithNewServer h = do
  checkTry h
  modify nextTry
  putNewServerInfo h
  return empty

putNextTryWithNewTS :: (MonadCatch m) => Handle m -> Integer -> AppT m [Update]
putNextTryWithNewTS h ts = do
  checkTry h
  modify (nextTry . changeTs ts)
  return empty

putNewServerInfo :: (MonadCatch m) => Handle m -> AppT m ()
putNewServerInfo h = do
  servInfo <- lift $ lift $ getServInfoAndCheckResp h
  modify (changeServInfo servInfo)

checkTry :: (MonadCatch m) => Handle m -> AppT m ()
checkTry h = do
  TryServer num servInfo <- get
  when (num >= 3) $ do
    let ex = CheckGetUpdatesResponseException $ "More then two times getUpdates fail. ServerInfo:" ++ show servInfo
    lift $ lift $ throwAndLogEx (hLog h) ex


{-
chooseActionOfTxt ::
  (MonadCatch m) =>
  Handle m ->
  N ->
  UserId ->
  TextOfMsg ->
  AppT m ()
chooseActionOfTxt h currN usId txt =
  case filter (' ' /=) . T.unpack $ txt of
    "/help" -> do
      let infoMsg = T.pack $ cHelpMsg (hConf h)
      lift $
        logDebug
          (hLog h)
          ( "Send request to send msg https://api.vk.com/method/messages.send?user_id="
              ++ show usId
              ++ "&random_id=0&message="
              ++ T.unpack infoMsg
              ++ "&access_token="
              ++ cBotToken (hConf h)
              ++ "&v=5.103"
          )
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
  (MonadCatch m) =>
  Handle m ->
  N ->
  AboutObj ->
  m ()
chooseActionOfAttachs h currN (AboutObj usId _ _ txt _ attachs maybeGeo) = do
  eitherAttachStrings <- runExceptT $ mapM (getAttachmentString (hPrepAttach h) usId) attachs
  case eitherAttachStrings of
    Right attachStrings ->
      replicateM_ currN $ do
        latLong <- checkAndPullLatLong h maybeGeo
        let msg = VkAttachMsg txt attachStrings latLong
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
    ( "Send request to send to user_id:"
        ++ show usId
        ++ " msg: "
        ++ show msg
    )
  response <- sendMsg h usId msg `catch` handleExSendMsg (hLog h) usId msg
  logDebug (hLog h) ("Get response: " ++ show response)
  checkSendMsgResponse h usId msg response

sendKeybAndCheckResp ::
  (MonadCatch m) => Handle m -> UserId -> N -> TextOfKeyb -> m ()
sendKeybAndCheckResp h usId currN txt = do
  logDebug (hLog h) $
    "Send request to send keyboard to user: "
      ++ show usId
      ++ " with message: "
      ++ show currN
      ++ show txt
  response <- sendKeyb h usId currN txt `catch` handleExSendKeyb (hLog h) usId
  logDebug (hLog h) ("Get response: " ++ show response)
  checkSendKeybResponse h usId currN txt response
-}
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

checkAndPullUpdates ::
  (MonadCatch m) =>
  Handle m ->
  Response ->
  AppT m [Update]
checkAndPullUpdates h json =
  case decode json of
    Nothing -> do
      let ex =
            CheckGetUpdatesResponseException $
              "UNKNOWN RESPONSE:" ++ show json
      lift $ lift $ throwAndLogEx (hLog h) ex
    Just ErrorAnswer {} -> do
      let ex =
            CheckGetUpdatesResponseException $
              "NEGATIVE RESPONSE:" ++ show json
      lift $ lift $ throwAndLogEx (hLog h) ex
    Just (FailAnswer 2) -> do
      lift $ lift $ 
        logWarning
          (hLog h)
          "FAIL. Long poll server key expired, need to request new key"
      putNextTryWithNewServer h
    Just (FailAnswer 3) -> do
      lift $ lift $ 
        logWarning
          (hLog h)
          "FAIL. Long poll server information is lost, need to request new key and ts"
      putNextTryWithNewServer h
    Just (FailTSAnswer (Just failNum) ts) -> do
      lift $ lift $ 
        logWarning
          (hLog h)
        $ "FAIL number " ++ show failNum ++ ". Ts in request is wrong, need to use received ts"
      putNextTryWithNewTS h ts
    Just (FailTSAnswer _ ts) -> do
      lift $ lift $ 
        logWarning
          (hLog h)
          "FAIL. Ts in request is wrong, need to use received ts"
      putNextTryWithNewTS h ts
    Just (FailAnswer _) -> do
      let ex =
            CheckGetUpdatesResponseException $
              "NEGATIVE RESPONSE:" ++ show json
      lift $ lift $ throwAndLogEx (hLog h) ex
    Just AnswerOk {updates = []} -> do
      lift $ lift $ logInfo (hLog h) "No new updates"
      modify resetTry
      return []
    Just (AnswerOk ts upds) -> do
      lift $ lift $ logInfo (hLog h) "There is new updates list"
      modify (resetTry . changeTs ts)
      return upds



{-
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
            ("Msg " ++ show txt ++ " was sent to user " ++ show usId)
        StickerMsg idSt ->
          logInfo
            (hLog h)
            ( "Sticker_id "
                ++ show idSt
                ++ " was sent to user "
                ++ show usId
            )
        VkAttachMsg txt attachStrings ("", "") ->
          logInfo
            (hLog h)
            ( "VkAttachMsg was sent to user "
                ++ show usId
                ++ ". Text: "
                ++ show txt
                ++ "; attachments: "
                ++ show attachStrings
            )
        VkAttachMsg txt [] latLong ->
          logInfo
            (hLog h)
            ( "GeoMsg was sent to user "
                ++ show usId
                ++ ". Text: "
                ++ show txt
                ++ "; geo: "
                ++ show latLong
            )
        VkAttachMsg txt attachStrings latLong ->
          logInfo
            (hLog h)
            ( "AttachmentAndGeoMsg was sent to user "
                ++ show usId
                ++ ". Text: "
                ++ show txt
                ++ "; attachments: "
                ++ show attachStrings
                ++ "; geo: "
                ++ show latLong
            )

checkSendKeybResponse ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  N ->
  TextOfKeyb ->
  Response ->
  m ()
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
        ( "Keyboard with message: "
            ++ show n
            ++ show txt
            ++ " was sent to user "
            ++ show usId
        )
-}

isValidResponse' ::
  Response ->
  Result
isValidResponse' json =
  case decode json of
    Nothing -> NotSuccess $
              "UNKNOWN RESPONSE:" ++ show json ++ "MESSAGE PROBABLY NOT SENT"
    Just ErrorAnswerMsg {} -> NotSuccess $
              "NEGATIVE RESPONSE:" ++ show json ++ "MESSAGE NOT SENT"
    Just _ -> Success

-- IO handle functions:
getLongPollServer' :: VkConfig -> IO Response
getLongPollServer' conf = do
  manager <- newTlsManager
  req <-
    parseRequest $
      "https://api.vk.com/method/groups.getLongPollServer?group_id="
        ++ show (cGroupId conf)
        ++ "&access_token="
        ++ cBotToken (cConf conf)
        ++ "&v=5.103"
  responseBody <$> httpLbs req manager

getUpdates' :: ServerInfo -> IO Response
getUpdates' (ServerInfo key server ts) = do
  manager <- newTlsManager
  req <-
    parseRequest $
      T.unpack server
        ++ "?act=a_check&key="
        ++ T.unpack key
        ++ "&ts="
        ++ show ts
        ++ "&wait=20"
  responseBody <$> httpLbs req manager

sendMsg' :: Config -> UserId -> TextOfMsg -> IO Response
sendMsg' conf usId txt = do
  manager <- newTlsManager
  let param = "message=" ++ T.unpack txt
  let params = intercalate "&" $ param : (commonParamList conf usId)
  req <- parseRequest $ "https://api.vk.com/method/messages.send?" ++ params
  responseBody <$> httpLbs req manager

sendAttachMsg' :: Config -> VkAttachMSG -> UserId -> IO Response
sendAttachMsg' conf msg usId = do
  manager <- newTlsManager
  let paramList = chooseParamsForMsg msg
  let params = intercalate "&" $ paramList ++ (commonParamList conf usId)
  req <- parseRequest $ "https://api.vk.com/method/messages.send?" ++ params
  responseBody <$> httpLbs req manager

commonParamList :: Config -> UserId -> [String]
commonParamList conf usId =
  let param1 = "user_id=" ++ show usId
      param2 = "random_id=0"
      param3 = "access_token=" ++ cBotToken conf
      param4 = "v=5.103"
  in  [param1,param2,param3,param4]

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
chooseParamsForMsg :: VkAttachMSG -> [ParameterString]
chooseParamsForMsg (StickerMsg idSt) =
  let param = "sticker_id=" ++ show idSt
   in [param]
chooseParamsForMsg (VkAttachMsg txt attachStrings (latStr, longStr)) =
  let param1 = "message=" ++ T.unpack txt
      param2 = "attachment=" ++ intercalate "," attachStrings
      param3 = "lat=" ++ latStr
      param4 = "long=" ++ longStr
   in [param1, param2, param3, param4]

{-changeMapUserN :: UserId -> NState -> MapUserN -> MapUserN
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
-}
logStrForGetObj :: AboutObj -> String
logStrForGetObj (AboutObj usId _ _ txt [] [] Nothing) =
  "Get TextMsg: " ++ show txt ++ " from user " ++ show usId
logStrForGetObj (AboutObj usId _ _ txt fwds [] Nothing) =
  "Get ForwardMsg: "
    ++ show fwds
    ++ addInfoAboutTxt txt
    ++ " from user "
    ++ show usId
logStrForGetObj (AboutObj usId _ _ txt [] [] (Just geo)) =
  "Get GeoMsg: "
    ++ show geo
    ++ addInfoAboutTxt txt
    ++ " from user "
    ++ show usId
logStrForGetObj (AboutObj usId _ _ txt [] attachs Nothing) =
  "Get VkAttachMsg: "
    ++ show attachs
    ++ addInfoAboutTxt txt
    ++ " from user "
    ++ show usId
logStrForGetObj (AboutObj usId _ _ txt fwds attachs Nothing) =
  "Get VkAttachMsg: "
    ++ show attachs
    ++ addInfoAboutTxt txt
    ++ ", with ForwardParts: "
    ++ show fwds
    ++ "  from user "
    ++ show usId
logStrForGetObj (AboutObj usId _ _ txt [] attachs (Just geo)) =
  "Get VkAttachMsg: "
    ++ show attachs
    ++ addInfoAboutTxt txt
    ++ ", with Geo: "
    ++ show geo
    ++ "  from user "
    ++ show usId
logStrForGetObj (AboutObj usId _ _ txt fwds [] (Just geo)) =
  "Get ForwardMsg: "
    ++ show fwds
    ++ addInfoAboutTxt txt
    ++ ", with Geo: "
    ++ show geo
    ++ "  from user "
    ++ show usId
logStrForGetObj (AboutObj usId _ _ txt fwds attachs (Just geo)) =
  "Get VkAttachMsg: "
    ++ show attachs
    ++ addInfoAboutTxt txt
    ++ ", with Geo: "
    ++ show geo
    ++ ", with ForwardParts: "
    ++ show fwds
    ++ "  from user "
    ++ show usId

addInfoAboutTxt :: TextOfMsg -> String
addInfoAboutTxt "" = ""
addInfoAboutTxt txt = " with text: " ++ show txt
