{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tg.App where

import qualified App
import Conf (Config (..))
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.State (StateT, lift)
import Data.Aeson (decode, encode)
import qualified Data.Text as T
import Logger (LogHandle (..), logDebug, logInfo)
import Network.HTTP.Client
  ( Manager,
    Request,
    RequestBody (RequestBodyLBS),
    httpLbs,
    method,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Tg.Api.Request
  ( CopyMsgJSONBody (..),
    JSONBodyOffset (..),
    JSONBodyTimeOut (..),
    KeyBoard (..),
    KeyButton (..),
    KeybJSONBody (..),
    SendMsgJSONBody (..),
  )
import Tg.Api.Response (Answer (..), From (..), GetUpdResp (..), Message (..), Update (..))
import Tg.Error
  ( TGBotException (..),
    handleExConfUpd,
    handleExGetUpd,
    throwAndLogEx,
  )
import Tg.Types
import Types

data Handle m = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    getUpdates :: m Response,
    getShortUpdates :: m Response,
    confirmUpdates :: UpdateId -> m Response,
    hApp :: App.Handle m MessageId
  }

makeH :: Config -> LogHandle IO -> Handle IO
makeH conf logH =
  Handle
    conf
    logH
    (getUpdates' conf)
    (getShortUpdates' conf)
    (confirmUpdates' conf)
    (makeAppH conf logH)

makeAppH :: Config -> LogHandle IO -> App.Handle IO MessageId
makeAppH conf logH =
  App.Handle
    conf
    logH
    (sendMsg' conf)
    (sendKeyb' conf)
    (copyMsg' conf)
    isValidResponse'

-- logic functions:
startApp :: (MonadCatch m) => Handle m -> m ()
startApp h = do
  logInfo (hLog h) "App started"
  upds <- getShortUpdatesAndCheckResp h
  confirmUpdatesAndCheckResp h upds

run ::
  (MonadCatch m) =>
  Handle m ->
  StateT MapUserN m ()
run h = do
  upds <- lift $ getUpdatesAndCheckResp h
  lift $ confirmUpdatesAndCheckResp h upds
  mapM_ (App.chooseActionOfUpd (hApp h) . isValidUpdate) upds

isValidUpdate :: Update -> IsValidUpdate MessageId
isValidUpdate (UnknownUpdate _) = InvalidUpdate
isValidUpdate (Update _ msg) = case msg of
  Message _ (From usId) (Just txt) -> ValidUpdate usId (TextMsg txt)
  Message msgId (From usId) _ -> ValidUpdate usId (AttachMsg msgId)

getUpdatesAndCheckResp :: (MonadCatch m) => Handle m -> m [Update]
getUpdatesAndCheckResp h = do
  logDebug
    (hLog h)
    ( "Send request to getUpdates: https://api.telegram.org/bot"
        ++ cBotToken (hConf h)
        ++ "/getUpdates"
    )
  json <- getUpdates h `catch` handleExGetUpd (hLog h)
  logDebug (hLog h) ("Get response: " ++ show json)
  checkGetUpdatesResp h json

getShortUpdatesAndCheckResp :: (MonadCatch m) => Handle m -> m [Update]
getShortUpdatesAndCheckResp h = do
  logDebug
    (hLog h)
    ( "Send request to getUpdates: https://api.telegram.org/bot"
        ++ cBotToken (hConf h)
        ++ "/getUpdates"
    )
  json <- getShortUpdates h `catch` handleExGetUpd (hLog h)
  logDebug (hLog h) ("Get response: " ++ show json)
  checkGetUpdatesResp h json

checkGetUpdatesResp ::
  (MonadCatch m) => Handle m -> Response -> m [Update]
checkGetUpdatesResp h json =
  case decode json of
    Nothing ->
      throwAndLogEx (hLog h) . CheckGetUpdatesResponseException $
        "UNKNOWN RESPONSE:" ++ show json
    Just (OkAnswer False) ->
      throwAndLogEx (hLog h) . CheckGetUpdatesResponseException $
        "NEGATIVE RESPONSE:" ++ show json
    Just (OkAnswer True) ->
      throwAndLogEx (hLog h) . CheckGetUpdatesResponseException $
        "UNKNOWN RESULT IN RESPONSE:" ++ show json
    Just (GetUpdResp False _) ->
      throwAndLogEx (hLog h) . CheckGetUpdatesResponseException $
        "NEGATIVE RESPONSE:" ++ show json
    Just (GetUpdResp _ []) -> do
      logInfo (hLog h) "No new updates"
      return []
    Just (GetUpdResp _ upds) -> do
      logInfo (hLog h) "There is new updates list"
      return upds

confirmUpdatesAndCheckResp :: (MonadCatch m) => Handle m -> [Update] -> m ()
confirmUpdatesAndCheckResp _ [] = return ()
confirmUpdatesAndCheckResp h upds = do
  let nextUpdate = extractNextUpdate upds
  checkUpdateId h nextUpdate
  logDebug
    (hLog h)
    ( "Send request to confirmOldUpdates with offset:"
        ++ show nextUpdate
        ++ " https://api.telegram.org/bot"
        ++ cBotToken (hConf h)
        ++ "/getUpdates"
    )
  newJson <-
    confirmUpdates h nextUpdate `catch` handleExConfUpd (hLog h) upds
  logDebug (hLog h) ("Get response: " ++ show newJson)
  checkConfirmUpdatesResponse h nextUpdate upds newJson

checkUpdateId :: (MonadCatch m) => Handle m -> UpdateId -> m ()
checkUpdateId h updId =
  when (updId <= 0) . throwAndLogEx (hLog h) $ ConfirmUpdatesException "Update id not greater then 1"

checkConfirmUpdatesResponse ::
  (MonadCatch m) =>
  Handle m ->
  Offset ->
  [Update] ->
  Response ->
  m ()
checkConfirmUpdatesResponse h offsetArg upds responseJson =
  case decode responseJson of
    Nothing ->
      throwAndLogEx (hLog h) . CheckConfirmUpdatesResponseException $
        "UNKNOWN RESPONSE"
          ++ show responseJson
          ++ "\nUpdates: \n"
          ++ show upds
          ++ "\nPROBABLY NOT CONFIRM with offset: "
          ++ show offsetArg
    Just (Answer False) ->
      throwAndLogEx (hLog h) . CheckConfirmUpdatesResponseException $
        "NEGATIVE RESPONSE:"
          ++ show responseJson
          ++ "\nUpdates: \n"
          ++ show upds
          ++ "\nNOT CONFIRM with offset: "
          ++ show offsetArg
    Just _ -> logInfo (hLog h) "Received updates confirmed"

-- IO methods functions:
getShortUpdates' :: Config -> IO Response
getShortUpdates' conf = do
  manager <- newTlsManager
  req <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken conf ++ "/getUpdates")
  sendReqAndGetRespBody manager req

getUpdates' :: Config -> IO Response
getUpdates' conf = do
  let bodyTimeOut = JSONBodyTimeOut {timeout = 25}
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken conf ++ "/getUpdates")
  let req = addBodyToReq initReq (RequestBodyLBS . encode $ bodyTimeOut)
  sendReqAndGetRespBody manager req

confirmUpdates' :: Config -> UpdateId -> IO Response
confirmUpdates' conf nextUpdate = do
  let bodyOffset = JSONBodyOffset {offset = nextUpdate}
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken conf ++ "/getUpdates")
  let req = addBodyToReq initReq (RequestBodyLBS . encode $ bodyOffset)
  sendReqAndGetRespBody manager req

sendMsg' :: Config -> UserId -> TextOfMsg -> IO Response
sendMsg' conf usId msg = do
  let msgBody = encode (SendMsgJSONBody {chat_id = usId, text = msg})
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken conf ++ "/sendMessage")
  let req = addBodyToReq initReq (RequestBodyLBS msgBody)
  sendReqAndGetRespBody manager req

copyMsg' :: Config -> MessageId -> UserId -> IO Response
copyMsg' conf msgId usId = do
  let msgBody =
        encode
          ( CopyMsgJSONBody
              { chat_idCM = usId,
                from_chat_idCM = usId,
                msg_idCM = msgId
              }
          )
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken conf ++ "/copyMessage")
  let req = addBodyToReq initReq (RequestBodyLBS msgBody)
  sendReqAndGetRespBody manager req

sendKeyb' :: Config -> UserId -> N -> TextOfKeyb -> IO Response
sendKeyb' conf usId n msg = do
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken conf ++ "/sendMessage")
  let keybBody = makeKeybBody usId msg n
  let req = addBodyToReq initReq (RequestBodyLBS . encode $ keybBody)
  sendReqAndGetRespBody manager req

sendReqAndGetRespBody :: Manager -> Request -> IO Response
sendReqAndGetRespBody manager req = responseBody <$> httpLbs req manager

-- clear functions:
isValidResponse' ::
  Response ->
  Result
isValidResponse' json =
  case decode json of
    Nothing ->
      NotSuccess $
        "UNKNOWN RESPONSE:" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"
    Just (Answer False) ->
      NotSuccess $
        "NEGATIVE RESPONSE:" ++ show json ++ "\nMESSAGE NOT SENT"
    Just _ -> Success

extractNextUpdate :: [Update] -> UpdateId
extractNextUpdate = succ . update_id . last

extractUserId :: Update -> UserId
extractUserId = idUser . fromUser . message

addBodyToReq :: Request -> RequestBody -> Request
addBodyToReq initReq reqBody =
  initReq
    { method = "POST",
      requestBody = reqBody,
      requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
    }

makeKeybBody :: UserId -> TextOfKeyb -> N -> KeybJSONBody
makeKeybBody usId msg n =
  KeybJSONBody
    { chat_idKeyb = usId,
      textKeyb = T.concat [T.pack . show $ n, msg],
      reply_markup =
        KeyBoard
          { keyboard =
              [ [KeyButton {textBtn = "1"}],
                [KeyButton {textBtn = "2"}],
                [KeyButton {textBtn = "3"}],
                [KeyButton {textBtn = "4"}],
                [KeyButton {textBtn = "5"}]
              ],
            one_time_keyboard = True
          }
    }
