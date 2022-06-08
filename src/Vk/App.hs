{-# LANGUAGE OverloadedStrings #-}

module Vk.App where

import qualified App
import Conf (Config (..))
import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT, forever, get, gets, lift, modify)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (intercalate)
import Data.String (fromString)
import qualified Data.Text as T
import Logger (LogHandle (..), logDebug, logInfo, logWarning)
import Network.HTTP.Client
  ( httpLbs,
    parseRequest,
    responseBody,
    urlEncodedBody,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Types
import Vk.Api.Request (keyBoard)
import Vk.Api.Response
import Vk.App.PrepareAttachment (getAttachmentString)
import qualified Vk.App.PrepareAttachment (Handle, makeH)
import Vk.AppT (AppT, TryServer (..), changeServInfo, changeTs, firstTry, nextTry, resetTry)
import Vk.Conf (VkConfig (..))
import Vk.Error
  ( VKBotException (..),
    handleExGetLongPollServ,
    handleExGetUpd,
    throwAndLogEx,
  )
import Vk.Types

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
  lift $ mapM_ (\upd -> lift (isValidUpdate h upd) >>= App.chooseActionOfUpd (hApp h)) upds

getServInfoAndCheckResp ::
  (MonadCatch m) => Handle m -> m ServerInfo
getServInfoAndCheckResp h = do
  logDebug (hLog h) $
    "Send request to getLongPollServer: https://api.vk.com/method/groups.getLongPollServer?group_id="
      ++ show (cGroupId (hConf h))
      ++ "&access_token="
      ++ cBotToken (cConf (hConf h))
      ++ "&v="
      ++ vkApiVersion
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
  sI@(ServerInfo key server ts) <- gets servInf
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
chooseUpdType _ (AboutObj usId _ _ "" [] [StickerAttachment (StickerInfo idSt)] Nothing) =
  return $ ValidUpdate usId $ AttachMsg (StickerMsg idSt)
chooseUpdType _ (AboutObj _ _ _ _ [] [StickerAttachment _] _) =
  return $ InvalidUpdatePlusInfo "There is unknown sticker message. Sticker can`t have text or geo."
chooseUpdType h (AboutObj usId _ _ txt [] attachs maybeGeo) =
  prepareAttach h usId txt attachs maybeGeo
chooseUpdType _ _ =
  return $ InvalidUpdatePlusInfo "There is forward message"

prepareAttach :: (MonadCatch m) => Handle m -> UserId -> TextOfMsg -> [Attachment] -> Maybe Geo -> m (IsValidUpdate VkAttachMSG)
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
      lift $ lift
        $ logWarning
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
    Just (AnswerOk _ []) -> do
      lift $ lift $ logInfo (hLog h) "No new updates"
      modify resetTry
      return []
    Just (AnswerOk ts upds) -> do
      lift $ lift $ logInfo (hLog h) "There is new updates list"
      modify (resetTry . changeTs ts)
      return upds

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
        ++ "&v="
        ++ vkApiVersion
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
  let paramMsg = "message=" ++ T.unpack txt
  let params = intercalate "&" $ paramMsg : commonParamList conf usId
  req <- parseRequest $ "https://api.vk.com/method/messages.send?" ++ params
  responseBody <$> httpLbs req manager

sendAttachMsg' :: Config -> VkAttachMSG -> UserId -> IO Response
sendAttachMsg' conf msg usId = do
  manager <- newTlsManager
  let paramList = chooseParamsForMsg msg
  let params = intercalate "&" $ paramList ++ commonParamList conf usId
  req <- parseRequest $ "https://api.vk.com/method/messages.send?" ++ params
  responseBody <$> httpLbs req manager

commonParamList :: Config -> UserId -> [String]
commonParamList conf usId =
  let paramUsId   = "user_id=" ++ show usId
      paramRandom = "random_id=0"
      paramToken  = "access_token=" ++ cBotToken conf
      paramVers   = "v=" ++ vkApiVersion
   in [paramUsId, paramRandom, paramToken, paramVers]

sendKeyb' :: Config -> UserId -> N -> TextOfKeyb -> IO Response
sendKeyb' conf usId n txt = do
  manager <- newTlsManager
  initReq <- parseRequest "https://api.vk.com/method/messages.send"
  let paramUsId   = ("user_id", fromString . show $ usId)
  let paramRandom = ("random_id", "0")
  let paramMsg    = ("message", fromString (show n ++ T.unpack txt))
  let paramKeyb   = ("keyboard", LBS.toStrict . encode $ keyBoard)
  let paramToken  = ("access_token", fromString $ cBotToken conf)
  let paramVers   = ("v", fromString vkApiVersion)
  let params = [paramUsId, paramRandom, paramMsg, paramKeyb, paramToken, paramVers]
  let req = urlEncodedBody params initReq
  responseBody <$> httpLbs req manager

-- clear functions:

isValidResponse' ::
  Response ->
  Result
isValidResponse' json =
  case decode json of
    Nothing ->
      NotSuccess $
        "UNKNOWN RESPONSE:" ++ show json ++ "MESSAGE PROBABLY NOT SENT"
    Just ErrorAnswerMsg {} ->
      NotSuccess $
        "NEGATIVE RESPONSE:" ++ show json ++ "MESSAGE NOT SENT"
    Just _ -> Success

chooseParamsForMsg :: VkAttachMSG -> [ParameterString]
chooseParamsForMsg (StickerMsg idSt) =
  let paramStick = "sticker_id=" ++ show idSt
   in [param]
chooseParamsForMsg (VkAttachMsg txt attachStrings (latStr, longStr)) =
  let paramMsg    = "message=" ++ T.unpack txt
      paramAttach = "attachment=" ++ intercalate "," attachStrings
      paramLat    = "lat=" ++ latStr
      paramLong   = "long=" ++ longStr
   in [paramMsg, paramAttach, paramLat, paramLong]

vkApiVersion :: String
vkApiVersion = "5.85"
