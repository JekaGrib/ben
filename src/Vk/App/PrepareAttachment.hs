{-# LANGUAGE OverloadedStrings #-}

module Vk.App.PrepareAttachment where

import Conf (Config (..))
import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.Except (ExceptT, lift, throwError)
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.String (fromString)
import qualified Data.Text as T
import Logger (LogHandle (..), logDebug)
import Network.HTTP.Client
  ( RequestBody (..),
    httpLbs,
    parseRequest,
    responseBody,
    urlEncodedBody,
  )
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Types
import Vk.Api.Response
import Vk.Oops
  ( PrependAttachmetException (..),
    handleExGetUploadServ,
    handleExGoToUrl,
    handleExLoadToServ,
    handleExSaveOnServ,
    throwAndLogPrepAttEx,
  )
import Vk.Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    getPhotoServer :: UserId -> m Response,
    loadPhotoToServ :: ServerUrl -> PicUrl -> ResponseS -> m Response,
    savePhotoOnServ :: LoadPhotoResp -> m Response,
    getDocServer :: UserId -> TypeInGetServerReq -> m Response,
    loadDocToServ :: ServerUrl -> DocUrl -> ResponseS -> Extention -> m Response,
    saveDocOnServ :: LoadDocResp -> Title -> m Response,
    goToUrl :: Url -> m ResponseS
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  Handle
    conf
    logH
    (getPhotoServer' conf)
    loadPhotoToServ'
    (savePhotoOnServ' conf)
    (getDocServer' conf)
    loadDocToServ'
    (saveDocOnServ' conf)
    goToUrl'

-- logic functions:

getAttachmentString ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  Attachment ->
  ExceptT SomethingWrong m AttachmentString
getAttachmentString _ _ (PhotoAttachment (Photo [])) =
  throwError "Unknown photo attachment, empty sizes"
getAttachmentString h usId (PhotoAttachment (Photo sizes)) = do
  let picUrl = url . head . sortOn (Down . height) $ sizes
  (DocInfo idDoc owner_id) <- lift $ getPhotoDocInfo h usId picUrl
  return $ "photo" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString h usId (DocAttachment doc) = do
  (DocInfo idDoc owner_id) <- lift $ getDocInfo h usId doc
  return $ "doc" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString h usId (AudioMesAttachment aud) = do
  (DocInfo idDoc owner_id) <- lift $ getAudioMsgDocInfo h usId aud
  return $ "doc" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (VideoAttachment (DocInfo idDoc owner_id)) =
  return $ "video" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (AudioAttachment (DocInfo idDoc owner_id)) =
  return $ "audio" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (MarketAttachment (DocInfo idDoc owner_id)) =
  return $ "market" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (WallAttachment (WallInfo idWall owner_id)) =
  return $ "wall" ++ show owner_id ++ "_" ++ show idWall
getAttachmentString _ _ (PollAttachment (DocInfo idDoc owner_id)) =
  return $ "poll" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ usId (StickerAttachment _) =
  throwError $
    "Wrong sticker attachment from user: "
      ++ show usId
      ++ ". Sticker not alone in msg.\n"
getAttachmentString _ _ (UnknownAttachment x) =
  throwError $ "Unknown attachment:" ++ show x ++ "\n"

getPhotoDocInfo ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  Url ->
  m DocInfo
getPhotoDocInfo h usId picUrl = do
  serRespJson <- getPhotoServer h usId `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson
  bsPic <- goToUrl h picUrl `catch` handleExGoToUrl (hLog h)
  loadPhotoJson <-
    loadPhotoToServ h serUrl picUrl bsPic `catch` handleExLoadToServ (hLog h)
  loadPhotoResp <- checkLoadPhotoResponse h loadPhotoJson
  savePhotoJson <-
    savePhotoOnServ h loadPhotoResp `catch` handleExSaveOnServ (hLog h)
  checkSavePhotoResponse h savePhotoJson

getDocInfo ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  Doc ->
  m DocInfo
getDocInfo h usId (Doc docUrl ext title) = do
  serRespJson <-
    getDocServer h usId "doc" `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson
  bsDoc <- goToUrl h docUrl `catch` handleExGoToUrl (hLog h)
  loadDocJson <-
    loadDocToServ h serUrl docUrl bsDoc ext `catch` handleExLoadToServ (hLog h)
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <-
    saveDocOnServ h loadDocResp title `catch` handleExSaveOnServ (hLog h)
  checkSaveDocResponse h saveDocJson

getAudioMsgDocInfo ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  Audio ->
  m DocInfo
getAudioMsgDocInfo h usId (Audio docUrl) = do
  serRespJson <-
    getDocServer h usId "audio_message" `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson
  bsDoc <- goToUrl h docUrl `catch` handleExGoToUrl (hLog h)
  loadDocJson <-
    loadDocToServ h serUrl docUrl bsDoc "ogg"
      `catch` handleExLoadToServ (hLog h)
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <-
    saveDocOnServ h loadDocResp "audio_message"
      `catch` handleExSaveOnServ (hLog h)
  checkSaveDocAuMesResponse h saveDocJson

checkGetUploadServResponse ::
  (MonadCatch m) => Handle m -> Response -> m ServerUrl
checkGetUploadServResponse h json =
  case decode json of
    Just (UploadServerResponse (UploadUrl serUrl)) -> do
      logDebug (hLog h) $ "Got upload server response: " ++ show json
      return serUrl
    _ -> do
      let ex =
            CheckGetUploadServerResponseException $
              "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkLoadDocResponse ::
  (MonadCatch m) => Handle m -> Response -> m LoadDocResp
checkLoadDocResponse h json =
  case decode json of
    Just loadDocResp@(LoadDocResp _) -> do
      logDebug (hLog h) $ "Got load doc to server response: " ++ show json
      return loadDocResp
    _ -> do
      let ex =
            CheckLoadToServResponseException $
              "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkSaveDocResponse ::
  (MonadCatch m) => Handle m -> Response -> m DocInfo
checkSaveDocResponse h json =
  case decode json of
    Just (SaveDocResp (ResponseSDR "doc" docInf)) -> do
      logDebug (hLog h) $ "Got save doc on server response: " ++ show json
      return docInf
    _ -> do
      let ex =
            CheckSaveOnServResponseException $
              "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkSaveDocAuMesResponse ::
  (MonadCatch m) => Handle m -> Response -> m DocInfo
checkSaveDocAuMesResponse h json =
  case decode json of
    Just (SaveDocAuMesResp (ResponseSDAMR "audio_message" docInf)) -> do
      logDebug (hLog h) $ "Got save doc(audio_msg) on server response: " ++ show json
      return docInf
    _ -> do
      let ex =
            CheckSaveOnServResponseException $
              "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkLoadPhotoResponse ::
  (MonadCatch m) => Handle m -> Response -> m LoadPhotoResp
checkLoadPhotoResponse h json =
  case decode json of
    Just loadPhotoResp -> do
      logDebug (hLog h) $ "Got load photo to server response: " ++ show json
      return loadPhotoResp
    _ -> do
      let ex =
            CheckLoadToServResponseException $
              "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkSavePhotoResponse ::
  (MonadCatch m) => Handle m -> Response -> m DocInfo
checkSavePhotoResponse h json =
  case decode json of
    Just (SavePhotoResp [DocInfo id' ownerId]) -> do
      logDebug (hLog h) $ "Got save photo on server response: " ++ show json
      return (DocInfo id' ownerId)
    _ -> do
      let ex =
            CheckSaveOnServResponseException $
              "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

-- IO handle functions:

getDocServer' :: Config -> UserId -> TypeInGetServerReq -> IO Response
getDocServer' conf usId type' = do
  manager <- newTlsManager
  req <-
    parseRequest $
      "https://api.vk.com/method/docs.getMessagesUploadServer?type="
        ++ type'
        ++ "&peer_id="
        ++ show usId
        ++ "&access_token="
        ++ cBotToken conf
        ++ "&v=5.103"
  responseBody <$> httpLbs req manager

loadDocToServ' :: ServerUrl -> DocUrl -> ResponseS -> Extention -> IO Response
loadDocToServ' serUrl docUrl bs ext = do
  manager <- newTlsManager
  initReq <- parseRequest $ T.unpack serUrl
  req <-
    formDataBody
      [ partFileRequestBody "file" (T.unpack docUrl ++ " file." ++ ext) $
          RequestBodyBS bs
      ]
      initReq
  responseBody <$> httpLbs req manager

saveDocOnServ' :: Config -> LoadDocResp -> Title -> IO Response
saveDocOnServ' conf (LoadDocResp file) title = do
  manager <- newTlsManager
  initReq <- parseRequest "https://api.vk.com/method/docs.save"
  let param1 = ("file", fromString file)
  let param2 = ("title", fromString title)
  let param3 = ("access_token", fromString $ cBotToken conf)
  let param4 = ("v", "5.103")
  let params = [param1, param2, param3, param4]
  let req = urlEncodedBody params initReq
  responseBody <$> httpLbs req manager

getPhotoServer' :: Config -> UserId -> IO Response
getPhotoServer' conf usId = do
  manager <- newTlsManager
  req <-
    parseRequest $
      "https://api.vk.com/method/photos.getMessagesUploadServer?peer_id="
        ++ show usId
        ++ "&access_token="
        ++ cBotToken conf
        ++ "&v=5.103"
  res <- httpLbs req manager
  return (responseBody res)

loadPhotoToServ' :: ServerUrl -> PicUrl -> BS.ByteString -> IO Response
loadPhotoToServ' serUrl picUrl bs = do
  manager <- newTlsManager
  initReq <- parseRequest $ T.unpack serUrl
  req <-
    formDataBody
      [partFileRequestBody "photo" (T.unpack picUrl) $ RequestBodyBS bs]
      initReq
  res <- httpLbs req manager
  return (responseBody res)

savePhotoOnServ' :: Config -> LoadPhotoResp -> IO Response
savePhotoOnServ' conf (LoadPhotoResp server hash photo) = do
  manager <- newTlsManager
  initReq <- parseRequest "https://api.vk.com/method/photos.saveMessagesPhoto"
  let param1 = ("server", fromString . show $ server)
  let param2 = ("hash", fromString hash)
  let param3 = ("photo", fromString photo)
  let param4 = ("access_token", fromString $ cBotToken conf)
  let param5 = ("v", "5.103")
  let params = [param1, param2, param3, param4, param5]
  let req = urlEncodedBody params initReq
  res <- httpLbs req manager
  return (responseBody res)

goToUrl' :: Url -> IO ResponseS
goToUrl' urlTxt = do
  manager <- newTlsManager
  req <- parseRequest $ T.unpack urlTxt
  res <- httpLbs req manager
  let bs = LBS.toStrict . responseBody $ res
  return bs
