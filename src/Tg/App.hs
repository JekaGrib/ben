{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tg.App where

import qualified App 
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch (catch))
import Control.Monad.State (StateT, lift)
import Data.Aeson (decode, encode)
import qualified Data.Text as T
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
import Conf (Config (..))
import Logger (LogHandle (..), logDebug, logInfo)
import Tg.Oops
  ( TGBotException (..),
    handleExConfUpd,
--    handleExCopyMsg,
    handleExGetUpd,
--    handleExSendKeyb,
--    handleExSendMsg,
    throwAndLogEx
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
  Message _     (From usId) (Just txt) -> ValidUpdate usId (TextMsg txt)
  Message msgId (From usId) _          -> ValidUpdate usId (AttachMsg msgId)

{-
sendMsgAndCheckResp ::
  (MonadCatch m) => Handle m -> UserId -> TextOfMsg -> m ()
sendMsgAndCheckResp h usId msg = do
  logDebug
    (hLog h)
    ( "Send request to send msg "
        ++ show msg
        ++ " to userId "
        ++ show usId
        ++ ": https://api.telegram.org/bot"
        ++ cBotToken (hConf h)
        ++ "/sendMessage   JSON body : {chat_id = "
        ++ show usId
        ++ ", text = "
        ++ show msg
        ++ "}"
    )
  response <- sendMsg h usId msg `catch` handleExSendMsg (hLog h) usId msg
  logDebug (hLog h) ("Get response: " ++ show response )
  checkSendMsgResponse h usId msg response

copyMsgAndCheckResp ::
  (MonadCatch m) => Handle m -> UserId -> MessageId -> m ()
copyMsgAndCheckResp h usId msgId = do
  let logMsg =
        "Send request to send attachment msg_id: "
          ++ show msgId
          ++ " to userId "
          ++ show usId
          ++ ": https://api.telegram.org/bot"
          ++ cBotToken (hConf h)
          ++ "/copyMessage   JSON body : {chat_id = "
          ++ show usId
          ++ ",from_chat_id = "
          ++ show usId
          ++ ", message_id = "
          ++ show msgId
          ++ "}"
  logDebug (hLog h) logMsg
  response <- copyMsg h usId msgId `catch` handleExCopyMsg (hLog h) usId msgId
  logDebug (hLog h) ("Get response: " ++ show response)
  checkCopyMsgResponse h usId msgId response

sendKeybAndCheckResp ::
  (MonadCatch m) => Handle m -> UserId -> N -> m ()
sendKeybAndCheckResp h usId currN = do
  let infoMsg =
        T.pack $
          " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
  logDebug (hLog h) $
    "Send request to send keyboard with message: "
      ++ show currN
      ++ show infoMsg
      ++ " to userId "
      ++ show usId
      ++ ": https://api.telegram.org/bot"
      ++ cBotToken (hConf h)
      ++ "/sendMessage"
  keybResponse <-
    sendKeyb h usId currN infoMsg `catch` handleExSendKeyb (hLog h) usId
  logDebug (hLog h) ("Get response: " ++ show keybResponse )
  checkSendKeybResponse h usId currN infoMsg keybResponse
-}

getUpdatesAndCheckResp :: (MonadCatch m) => Handle m -> m [Update]
getUpdatesAndCheckResp h = do
  logDebug
    (hLog h)
    ( "Send request to getUpdates: https://api.telegram.org/bot"
        ++ cBotToken (hConf h)
        ++ "/getUpdates"
    )
  json <- getUpdates h `catch` handleExGetUpd (hLog h)
  logDebug (hLog h) ("Get response: " ++ show json )
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
  logDebug (hLog h) ("Get response: " ++ show json )
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
  logDebug (hLog h) ("Get response: " ++ show newJson )
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

{-
checkSendMsgResponse ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  TextOfMsg ->
  Response ->
  m ()
checkSendMsgResponse h usId msg json =
  case decode json of
    Nothing ->
      throwAndLogEx (hLog h)
        . CheckSendMsgResponseException (Msg msg) (ToUserId usId)
        $ "UNKNOWN RESPONSE:" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"
    Just (Answer False) ->
      throwAndLogEx (hLog h)
        . CheckSendMsgResponseException (Msg msg) (ToUserId usId)
        $ "NEGATIVE RESPONSE:" ++ show json ++ "\nMESSAGE NOT SENT"
    Just _ ->
      logInfo
        (hLog h)
        ("Msg " ++ show msg ++ " was sent to user " ++ show usId )

checkCopyMsgResponse ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  MessageId ->
  Response ->
  m ()
checkCopyMsgResponse h usId msgId json =
  case decode json of
    Nothing ->
      throwAndLogEx (hLog h)
        . CheckCopyMsgResponseException (MsgId msgId) (ToUserId usId)
        $ "UNKNOWN RESPONSE:" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"
    Just (Answer False) ->
      throwAndLogEx (hLog h)
        . CheckCopyMsgResponseException (MsgId msgId) (ToUserId usId)
        $ "NEGATIVE RESPONSE:" ++ show json ++ "\nMESSAGE NOT SENT"
    Just _ ->
      logInfo
        (hLog h)
        ( "Attachment msg_id: "
            ++ show msgId
            ++ " was sent to user "
            ++ show usId
        )

checkSendKeybResponse ::
  (MonadCatch m) =>
  Handle m ->
  UserId ->
  N ->
  TextOfKeyb ->
  Response ->
  m ()
checkSendKeybResponse h usId n msg json =
  case decode json of
    Nothing ->
      throwAndLogEx (hLog h) . CheckSendKeybResponseException (ToUserId usId) $
        "UNKNOWN RESPONSE:" ++ show json ++ "\nKEYBOARD PROBABLY NOT SENT"
    Just (Answer False) ->
      throwAndLogEx (hLog h) . CheckSendKeybResponseException (ToUserId usId) $
        "NEGATIVE RESPONSE:" ++ show json ++ "\nKEYBOARD NOT SENT"
    Just _ ->
      logInfo
        (hLog h)
        ( "Keyboard with message: "
            ++ show n
            ++ show msg
            ++ " was sent to user "
            ++ show usId
        )
-}

isValidResponse' ::
  Response ->
  Result
isValidResponse' json =
  case decode json of
    Nothing -> NotSuccess
        $ "UNKNOWN RESPONSE:" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"
    Just (Answer False) -> NotSuccess
        $ "NEGATIVE RESPONSE:" ++ show json ++ "\nMESSAGE NOT SENT"
    Just _ -> Success

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
