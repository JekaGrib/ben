{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.App.PrepareAttachment where

import Control.Monad.Catch (MonadCatch(catch))
import Data.Aeson (decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (sortOn)
import Data.Ord (Down(Down))
import Data.String (fromString)
import qualified Data.Text as T
import Network.HTTP.Client
  ( RequestBody(..)
  , httpLbs
  , parseRequest
  , responseBody
  , urlEncodedBody
  )
import Network.HTTP.Client.MultipartFormData (formDataBody, partFileRequestBody)
import Network.HTTP.Client.TLS (newTlsManager)
import Vk.Api.Response
import Vk.Conf (Config(..))
import Vk.Logger (LogHandle(..))
import Vk.Oops
  ( PrependAttachmetException(..)
  , handleExGetUploadServ
  , handleExGoToUrl
  , handleExLoadToServ
  , handleExSaveOnServ
  , throwAndLogPrepAttEx
  )
import Vk.Types

data Handle m =
  Handle
    { hConf :: Config
    , hLog :: LogHandle m
    , getPhotoServer :: UserId -> m Response
    , loadPhotoToServ :: ServerUrl -> PicUrl -> ResponseS -> m Response
    , savePhotoOnServ :: LoadPhotoResp -> m Response
    , getDocServer :: UserId -> TypeInGetServerReq -> m Response
    , loadDocToServ :: ServerUrl -> DocUrl -> ResponseS -> Extention -> m Response
    , saveDocOnServ :: LoadDocResp -> Title -> m Response
    , goToUrl :: Url -> m ResponseS
    }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH = Handle 
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

getSomeAttachmentString ::
     (MonadCatch m)
  => Handle m
  -> UserId
  -> SomeAttachment
  -> m (Either SomethingWrong AttachmentString)
getSomeAttachmentString h usId someAtt =
  case chooseAttType someAtt of
    Left str -> return . Left $ str
    Right att -> getAttachmentString h usId att

getAttachmentString ::
     (Monad m, MonadCatch m)
  => Handle m
  -> UserId
  -> Attachment
  -> m (Either SomethingWrong AttachmentString)
getAttachmentString h usId (PhotoAttachment (Photo sizes)) = do
  let picUrl = url . head . sortOn (Down . height) $ sizes
  serRespJson <- getPhotoServer h usId `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson
  bsPic <- goToUrl h picUrl `catch` handleExGoToUrl (hLog h)
  loadPhotoJson <-
    loadPhotoToServ h serUrl picUrl bsPic `catch` handleExLoadToServ (hLog h)
  loadPhotoResp <- checkLoadPhotoResponse h loadPhotoJson
  savePhotoJson <-
    savePhotoOnServ h loadPhotoResp `catch` handleExSaveOnServ (hLog h)
  (DocInfo idDoc owner_id) <- checkSavePhotoResponse h savePhotoJson
  return $ Right $ "photo" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString h usId (DocAttachment (Doc docUrl ext title)) = do
  serRespJson <-
    getDocServer h usId "doc" `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson
  bsDoc <- goToUrl h docUrl `catch` handleExGoToUrl (hLog h)
  loadDocJson <-
    loadDocToServ h serUrl docUrl bsDoc ext `catch` handleExLoadToServ (hLog h)
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <-
    saveDocOnServ h loadDocResp title `catch` handleExSaveOnServ (hLog h)
  (DocInfo idDoc owner_id) <- checkSaveDocResponse h saveDocJson
  return $ Right $ "doc" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString h usId (AudioMesAttachment (Audio docUrl)) = do
  serRespJson <-
    getDocServer h usId "audio_message" `catch` handleExGetUploadServ (hLog h)
  serUrl <- checkGetUploadServResponse h serRespJson
  bsDoc <- goToUrl h docUrl `catch` handleExGoToUrl (hLog h)
  loadDocJson <-
    loadDocToServ h serUrl docUrl bsDoc "ogg" `catch`
    handleExLoadToServ (hLog h)
  loadDocResp <- checkLoadDocResponse h loadDocJson
  saveDocJson <-
    saveDocOnServ h loadDocResp "audio_message" `catch`
    handleExSaveOnServ (hLog h)
  (DocInfo idDoc owner_id) <- checkSaveDocAuMesResponse h saveDocJson
  return $ Right $ "doc" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (VideoAttachment (DocInfo idDoc owner_id)) =
  return $ Right $ "video" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (AudioAttachment (DocInfo idDoc owner_id)) =
  return $ Right $ "audio" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (MarketAttachment (DocInfo idDoc owner_id)) =
  return $ Right $ "market" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ _ (WallAttachment (WallInfo idWall owner_id)) =
  return $ Right $ "wall" ++ show owner_id ++ "_" ++ show idWall
getAttachmentString _ _ (PollAttachment (DocInfo idDoc owner_id)) =
  return $ Right $ "poll" ++ show owner_id ++ "_" ++ show idDoc
getAttachmentString _ usId (StickerAttachment _) =
  return $
  Left $
  "Wrong sticker attachment from user: " ++
  show usId ++ ". Sticker not alone in msg.\n"


checkGetUploadServResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m ServerUrl
checkGetUploadServResponse h json =
  case decode json of
    Just (UploadServerResponse (UploadUrl serUrl)) -> return serUrl
    _ -> do
      let ex =
            CheckGetUploadServerResponseException $
            "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkLoadDocResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m LoadDocResp
checkLoadDocResponse h json =
  case decode json of
    Just loadDocResp@(LoadDocResp _) -> return loadDocResp
    _ -> do
      let ex =
            CheckLoadToServResponseException $
            "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkSaveDocResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m DocInfo
checkSaveDocResponse h json =
  case decode json of
    Just (SaveDocResp (ResponseSDR "doc" docInf)) -> return docInf
    _ -> do
      let ex =
            CheckSaveOnServResponseException $
            "UNKNOWN RESPONSE:\n" ++ show json
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
            "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkLoadPhotoResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m LoadPhotoResp
checkLoadPhotoResponse h json =
  case decode json of
    Just loadPhotoResp -> return loadPhotoResp
    _ -> do
      let ex =
            CheckLoadToServResponseException $
            "UNKNOWN RESPONSE:\n" ++ show json
      throwAndLogPrepAttEx (hLog h) ex

checkSavePhotoResponse ::
     (Monad m, MonadCatch m) => Handle m -> Response -> m DocInfo
checkSavePhotoResponse h json =
  case decode json of
    Just (SavePhotoResp [DocInfo id' ownerId]) -> return (DocInfo id' ownerId)
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
    "https://api.vk.com/method/docs.getMessagesUploadServer?type=" ++
    type' ++
    "&peer_id=" ++
    show usId ++ "&access_token=" ++ cBotToken conf ++ "&v=5.103"
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
    "https://api.vk.com/method/photos.getMessagesUploadServer?peer_id=" ++
    show usId ++ "&access_token=" ++ cBotToken conf ++ "&v=5.103"
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


-- clear functions:


chooseAttType :: SomeAttachment -> Either SomethingWrong Attachment
chooseAttType sAtt =
  case sAtt of
    SomeAttachment "photo" (Just (Photo [])) _ _ _ _ _ _ _ _ ->
      Left "Unknown photo attachment, empty sizes\n"
    SomeAttachment "photo" (Just photo) _ _ _ _ _ _ _ _ ->
      Right $ PhotoAttachment photo
    SomeAttachment "doc" _ (Just doc) _ _ _ _ _ _ _ -> Right $ DocAttachment doc
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