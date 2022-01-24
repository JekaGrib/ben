{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.App where

import Control.Monad.Catch (MonadCatch(..))
import Control.Monad.State (StateT, get, lift, modify, replicateM_)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.List (delete)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Network.HTTP.Client
  ( Manager
  , Request
  , RequestBody(RequestBodyLBS)
  , httpLbs
  , method
  , parseRequest
  , requestBody
  , requestHeaders
  , responseBody
  )
import Network.HTTP.Client.TLS (newTlsManager)
import Tg.Api.Request
  ( CopyMsgJSONBody(..)
  , JSONBodyOffset(..)
  , JSONBodyTimeOut(..)
  , KeyBoard(..)
  , KeyButton(..)
  , KeybJSONBody(..)
  , SendMsgJSONBody(..)
  )
import Tg.Api.Response (Answer(..), From(..), Message(..), Update(..))
import Tg.Conf (Config(..))
import Tg.Logger (LogHandle(..), logDebug, logInfo, logWarning)
import Tg.Oops
  ( Msg(..)
  , MsgId(..)
  , TGBotException(..)
  , ToUserId(..)
  , handleExConfUpd
  , handleExCopyMsg
  , handleExGetUpd
  , handleExSendKeyb
  , handleExSendMsg
  , throwAndLogEx
  )
import Tg.TypeSynonym


data Handle m =
  Handle
    { hConf :: Config
    , hLog :: LogHandle m
    , getUpdates :: m LBS.ByteString
    , getShortUpdates :: m LBS.ByteString
    , confirmUpdates :: Integer -> m LBS.ByteString
    , sendMsg :: Integer -> T.Text -> m LBS.ByteString
    , sendKeyb :: Integer -> Int -> T.Text -> m LBS.ByteString
    , copyMsg :: Integer -> Integer -> m LBS.ByteString
    }


-- logic functions:
startApp :: (Monad m, MonadCatch m) => Handle m -> m ()
startApp h = do
  logInfo (hLog h) "App started\n"
  logDebug
    (hLog h)
    ("Send request to getUpdates: https://api.telegram.org/bot" ++
     cBotToken (hConf h) ++ "/getUpdates\n")
  json <- getShortUpdates h `catch` handleExGetUpd (hLog h)
  logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  checkAndConfirmUpdates h json

run ::
     (Monad m, MonadCatch m)
  => Handle m
  -> StateT UsersNs m ()
run h = do
  lift $
    logDebug
      (hLog h)
      ("Send request to getUpdates: https://api.telegram.org/bot" ++
       cBotToken (hConf h) ++ "/getUpdates\n")
  json <- lift $ getUpdates h `catch` handleExGetUpd (hLog h)
  lift $ logDebug (hLog h) ("Get response: " ++ show json ++ "\n")
  lift $ checkAndConfirmUpdates h json
  let upds = extractUpdates json
  mapM_ (chooseActionOfUpd h) upds

chooseActionOfUpd ::
     (Monad m, MonadCatch m)
  => Handle m
  -> Update
  -> StateT UsersNs m ()
chooseActionOfUpd h upd = do
  lift $ logInfo (hLog h) "Analysis update from the list\n"
  case upd of
    UnknownUpdate _ -> do
      lift $ logWarning (hLog h) "There is UNKNOWN UPDATE. Bot will ignore it\n"
      return ()
    Update _ msg -> do
      let msgId = message_id msg
      let usId = extractUserId upd
      lift $
        logInfo
          (hLog h)
          ("Get msg_id: " ++ show msgId ++ " from user " ++ show usId ++ "\n")
      chooseActionOfDbState h msg msgId usId

chooseActionOfDbState ::
     (Monad m, MonadCatch m)
  => Handle m
  -> Message
  -> Integer
  -> Integer
  -> StateT UsersNs m ()
chooseActionOfDbState h msg msgId usId = do
  db <- get
  let nState = lookup usId db
  case nState of
    Just (Left (OpenRepeat oldN)) -> do
      lift $
        logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode\n")
      chooseActionOfButton h msg usId oldN
    Just (Right n) -> do
      let currN = n
      chooseActionOfTryPullTxt h msg msgId usId currN
    Nothing -> do
      let currN = cStartN (hConf h)
      chooseActionOfTryPullTxt h msg msgId usId currN

chooseActionOfTryPullTxt ::
     (Monad m, MonadCatch m)
  => Handle m
  -> Message
  -> Integer
  -> Integer
  -> Int
  -> StateT UsersNs m ()
chooseActionOfTryPullTxt h msg msgId usId currN =
  case tryPullTextMsg msg of
    Just txt -> do
      lift $
        logInfo
          (hLog h)
          ("Msg_id:" ++ show msgId ++ " is text: " ++ show txt ++ "\n")
      chooseActionOfTxt h currN usId txt
    Nothing -> do
      lift $ logInfo (hLog h) ("Msg_id:" ++ show msgId ++ " is attachment\n")
      lift $ replicateM_ currN $ copyMsgAndCheckResp h usId msgId

chooseActionOfButton ::
     (Monad m, MonadCatch m)
  => Handle m
  -> Message
  -> Integer
  -> Int
  -> StateT UsersNs m ()
chooseActionOfButton h msg usId oldN =
  case checkButton msg of
    Just newN -> do
      lift $
        logInfo
          (hLog h)
          ("Change number of repeats to " ++
           show newN ++ " for user " ++ show usId ++ "\n")
      modify (changeDB usId (Right newN))
      let infoMsg =
            T.pack $
            "Number of repeats successfully changed from " ++
            show oldN ++ " to " ++ show newN ++ "\n"
      lift $ sendMsgAndCheckResp h usId infoMsg
    Nothing -> do
      lift $
        logWarning
          (hLog h)
          ("User " ++
           show usId ++
           " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: " ++
           show oldN ++ "\n")
      modify (changeDB usId (Right oldN))
      let infoMsg =
            T.pack $
            "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still " ++
            show oldN ++
            "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later\n"
      lift $ sendMsgAndCheckResp h usId infoMsg

chooseActionOfTxt ::
     (Monad m, MonadCatch m)
  => Handle m
  -> Int
  -> Integer
  -> T.Text
  -> StateT UsersNs m ()
chooseActionOfTxt h currN usId txt =
  case filter (' ' /=) . T.unpack $ txt of
    "/help" -> do
      let infoMsg = T.pack $ cHelpMsg (hConf h)
      lift $ sendMsgAndCheckResp h usId infoMsg
    "/repeat" -> do
      lift $ sendKeybAndCheckResp h usId currN
      lift $
        logInfo (hLog h) ("Put user " ++ show usId ++ " to OpenRepeat mode\n")
      modify (changeDB usId (Left $ OpenRepeat currN))
    _ -> do
      lift $ replicateM_ currN $ sendMsgAndCheckResp h usId txt

sendMsgAndCheckResp ::
     (Monad m, MonadCatch m) => Handle m -> UserId -> T.Text -> m ()
sendMsgAndCheckResp h usId msg = do
  logDebug
    (hLog h)
    ("Send request to send msg " ++
     show msg ++
     " to userId " ++
     show usId ++
     ": https://api.telegram.org/bot" ++
     cBotToken (hConf h) ++
     "/sendMessage   JSON body : {chat_id = " ++
     show usId ++ ", text = " ++ show msg ++ "}\n")
  response <- sendMsg h usId msg `catch` handleExSendMsg (hLog h) usId msg
  logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
  checkSendMsgResponse h usId msg response

copyMsgAndCheckResp ::
     (Monad m, MonadCatch m) => Handle m -> UserId -> MessageId -> m ()
copyMsgAndCheckResp h usId msgId = do
  let logMsg =
        "Send request to send attachment msg_id: " ++
        show msgId ++
        " to userId " ++
        show usId ++
        ": https://api.telegram.org/bot" ++
        cBotToken (hConf h) ++
        "/copyMessage   JSON body : {chat_id = " ++
        show usId ++
        ",from_chat_id = " ++
        show usId ++ ", message_id = " ++ show msgId ++ "}\n"
  logDebug (hLog h) logMsg
  response <- copyMsg h usId msgId `catch` handleExCopyMsg (hLog h) usId msgId
  logDebug (hLog h) ("Get response: " ++ show response ++ "\n")
  checkCopyMsgResponse h usId msgId response

sendKeybAndCheckResp ::
     (Monad m, MonadCatch m) => Handle m -> UserId -> N -> m ()
sendKeybAndCheckResp h usId currN = do
  let infoMsg =
        T.pack $
        " : Current number of repeats your message.\n" ++ cRepeatQ (hConf h)
  logDebug (hLog h) $
    "Send request to send keyboard with message: " ++
    show currN ++
    show infoMsg ++
    " to userId " ++
    show usId ++
    ": https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage\n"
  keybResponse <-
    sendKeyb h usId currN infoMsg `catch` handleExSendKeyb (hLog h) usId
  logDebug (hLog h) ("Get response: " ++ show keybResponse ++ "\n")
  checkSendKeybResponse h usId currN infoMsg keybResponse

checkAndConfirmUpdates ::
     (Monad m, MonadCatch m) => Handle m -> LBS.ByteString -> m ()
checkAndConfirmUpdates h json =
  case decode json of
    Nothing ->
      throwAndLogEx (hLog h) . CheckGetUpdatesResponseException $
      "UNKNOWN RESPONSE:\n" ++ show json
    Just OkAnswer {ok = False} ->
      throwAndLogEx (hLog h) . CheckGetUpdatesResponseException $
      "NEGATIVE RESPONSE:\n" ++ show json
    Just (Answer False _) ->
      throwAndLogEx (hLog h) . CheckGetUpdatesResponseException $
      "NEGATIVE RESPONSE:\n" ++ show json
    Just (OkAnswer True) ->
      throwAndLogEx (hLog h) . CheckGetUpdatesResponseException $
      "Too short response:\n" ++ show json
    Just (Answer True []) -> logInfo (hLog h) "No new updates\n"
    Just _ -> do
      logInfo (hLog h) "There is new updates list\n"
      let nextUpdate = extractNextUpdate json
      logDebug
        (hLog h)
        ("Send request to confirmOldUpdates with offset:" ++
         show nextUpdate ++
         " https://api.telegram.org/bot" ++
         cBotToken (hConf h) ++ "/getUpdates\n")
      emptyJson <-
        confirmUpdates h nextUpdate `catch` handleExConfUpd (hLog h) json
      logDebug (hLog h) ("Get response: " ++ show emptyJson ++ "\n")
      checkConfirmUpdatesResponse h nextUpdate json emptyJson

checkConfirmUpdatesResponse ::
     (Monad m, MonadCatch m)
  => Handle m
  -> Offset
  -> LBS.ByteString
  -> LBS.ByteString
  -> m ()
checkConfirmUpdatesResponse h offsetArg confirmedJson responseJson =
  case decode responseJson of
    Nothing ->
      throwAndLogEx (hLog h) . CheckConfirmUpdatesResponseException $
      "UNKNOWN RESPONSE:\n" ++
      show responseJson ++
      "\nUpdates: \n" ++
      show confirmedJson ++
      "\nPROBABLY NOT CONFIRM with offset: " ++ show offsetArg
    Just OkAnswer {ok = False} ->
      throwAndLogEx (hLog h) . CheckConfirmUpdatesResponseException $
      "NEGATIVE RESPONSE:\n" ++
      show responseJson ++
      "\nUpdates: \n" ++
      show confirmedJson ++ "\nNOT CONFIRM with offset: " ++ show offsetArg
    Just (Answer False _) ->
      throwAndLogEx (hLog h) . CheckConfirmUpdatesResponseException $
      "NEGATIVE RESPONSE:\n" ++
      show responseJson ++
      "\nUpdates: \n" ++
      show confirmedJson ++ "\nNOT CONFIRM with offset: " ++ show offsetArg
    Just _ -> logInfo (hLog h) "Received updates confirmed\n"

checkSendMsgResponse ::
     (Monad m, MonadCatch m)
  => Handle m
  -> UserId
  -> T.Text
  -> LBS.ByteString
  -> m ()
checkSendMsgResponse h usId msg json =
  case decode json of
    Nothing ->
      throwAndLogEx (hLog h) .
      CheckSendMsgResponseException (Msg msg) (ToUserId usId) $
      "UNKNOWN RESPONSE:\n" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"
    Just OkAnswer {ok = False} ->
      throwAndLogEx (hLog h) .
      CheckSendMsgResponseException (Msg msg) (ToUserId usId) $
      "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
    Just (Answer False _) ->
      throwAndLogEx (hLog h) .
      CheckSendMsgResponseException (Msg msg) (ToUserId usId) $
      "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
    Just _ ->
      logInfo
        (hLog h)
        ("Msg " ++ show msg ++ " was sent to user " ++ show usId ++ "\n")

checkCopyMsgResponse ::
     (Monad m, MonadCatch m)
  => Handle m
  -> UserId
  -> MessageId
  -> LBS.ByteString
  -> m ()
checkCopyMsgResponse h usId msgId json =
  case decode json of
    Nothing ->
      throwAndLogEx (hLog h) .
      CheckCopyMsgResponseException (MsgId msgId) (ToUserId usId) $
      "UNKNOWN RESPONSE:\n" ++ show json ++ "\nMESSAGE PROBABLY NOT SENT"
    Just OkAnswer {ok = False} ->
      throwAndLogEx (hLog h) .
      CheckCopyMsgResponseException (MsgId msgId) (ToUserId usId) $
      "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
    Just (Answer False _) ->
      throwAndLogEx (hLog h) .
      CheckCopyMsgResponseException (MsgId msgId) (ToUserId usId) $
      "NEGATIVE RESPONSE:\n" ++ show json ++ "\nMESSAGE NOT SENT"
    Just _ ->
      logInfo
        (hLog h)
        ("Attachment msg_id: " ++
         show msgId ++ " was sent to user " ++ show usId ++ "\n")

checkSendKeybResponse ::
     (Monad m, MonadCatch m)
  => Handle m
  -> UserId
  -> N
  -> T.Text
  -> LBS.ByteString
  -> m ()
checkSendKeybResponse h usId n msg json =
  case decode json of
    Nothing ->
      throwAndLogEx (hLog h) . CheckSendKeybResponseException (ToUserId usId) $
      "UNKNOWN RESPONSE:\n" ++ show json ++ "\nKEYBOARD PROBABLY NOT SENT"
    Just OkAnswer {ok = False} ->
      throwAndLogEx (hLog h) . CheckSendKeybResponseException (ToUserId usId) $
      "NEGATIVE RESPONSE:\n" ++ show json ++ "\nKEYBOARD NOT SENT"
    Just (Answer False _) ->
      throwAndLogEx (hLog h) . CheckSendKeybResponseException (ToUserId usId) $
      "NEGATIVE RESPONSE:\n" ++ show json ++ "\nKEYBOARD NOT SENT"
    Just _ ->
      logInfo
        (hLog h)
        ("Keyboard with message: " ++
         show n ++ show msg ++ " was sent to user " ++ show usId ++ "\n")

-- IO methods functions:
getShortUpdates' :: Handle IO -> IO LBS.ByteString
getShortUpdates' h = do
  manager <- newTlsManager
  req <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  sendReqAndGetRespBody manager req

getUpdates' :: Handle IO -> IO LBS.ByteString
getUpdates' h = do
  let bodyTimeOut = JSONBodyTimeOut {timeout = 25}
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  let req = addBodyToReq initReq (RequestBodyLBS . encode $ bodyTimeOut)
  sendReqAndGetRespBody manager req

confirmUpdates' :: Handle IO -> UpdateId -> IO LBS.ByteString
confirmUpdates' h nextUpdate = do
  let bodyOffset = JSONBodyOffset {offset = nextUpdate}
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/getUpdates")
  let req = addBodyToReq initReq (RequestBodyLBS . encode $ bodyOffset)
  sendReqAndGetRespBody manager req

sendMsg' :: Handle IO -> UserId -> T.Text -> IO LBS.ByteString
sendMsg' h usId msg = do
  let msgBody = encode (SendMsgJSONBody {chat_id = usId, text = msg})
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage")
  let req = addBodyToReq initReq (RequestBodyLBS msgBody)
  sendReqAndGetRespBody manager req

copyMsg' :: Handle IO -> UserId -> MessageId -> IO LBS.ByteString
copyMsg' h usId msgId = do
  let msgBody =
        encode
          (CopyMsgJSONBody
             {chat_idCM = usId, from_chat_idCM = usId, msg_idCM = msgId})
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/copyMessage")
  let req = addBodyToReq initReq (RequestBodyLBS msgBody)
  sendReqAndGetRespBody manager req

sendKeyb' :: Handle IO -> UserId -> N -> T.Text -> IO LBS.ByteString
sendKeyb' h usId n msg = do
  manager <- newTlsManager
  initReq <-
    parseRequest
      ("https://api.telegram.org/bot" ++ cBotToken (hConf h) ++ "/sendMessage")
  let keybBody = makeKeybBody usId msg n
  let req = addBodyToReq initReq (RequestBodyLBS . encode $ keybBody)
  sendReqAndGetRespBody manager req

sendReqAndGetRespBody :: Manager -> Request -> IO LBS.ByteString
sendReqAndGetRespBody manager req = responseBody <$> httpLbs req manager

-- clear functions:
extractNextUpdate :: LBS.ByteString -> UpdateId
extractNextUpdate = succ . update_id . last . result . fromJust . decode

extractUpdates :: LBS.ByteString -> [Update]
extractUpdates = result . fromJust . decode

extractTextMsg :: Update -> T.Text
extractTextMsg = textMsg . message

extractUserId :: Update -> UserId
extractUserId = idUser . fromUser . message

changeDB ::
     UserId
  -> NState
  -> UsersNs
  -> UsersNs
changeDB usId eitherN bd =
  case lookup usId bd of
    Just eitherX -> (:) (usId, eitherN) . delete (usId, eitherX) $ bd
    Nothing -> (:) (usId, eitherN) bd

checkButton :: Message -> Maybe N
checkButton msg =
  case tryPullTextMsg msg of
    Just txt -> checkTextButton txt
    Nothing -> Nothing

checkTextButton :: T.Text -> Maybe N
checkTextButton txt =
  case txt of
    "1" -> Just 1
    "2" -> Just 2
    "3" -> Just 3
    "4" -> Just 4
    "5" -> Just 5
    _ -> Nothing

tryPullTextMsg :: Message -> Maybe T.Text
tryPullTextMsg (TxtMessage _ _ _ _ txt) = Just txt
tryPullTextMsg _ = Nothing

addBodyToReq :: Request -> RequestBody -> Request
addBodyToReq initReq reqBody =
  initReq
    { method = "POST"
    , requestBody = reqBody
    , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
    }

makeKeybBody :: UserId -> T.Text -> N -> KeybJSONBody
makeKeybBody usId msg n =
  KeybJSONBody
    { chat_idKeyb = usId
    , textKeyb = T.concat [T.pack . show $ n, msg]
    , reply_markup =
        KeyBoard
          { keyboard =
              [ [KeyButton {textBtn = "1"}]
              , [KeyButton {textBtn = "2"}]
              , [KeyButton {textBtn = "3"}]
              , [KeyButton {textBtn = "4"}]
              , [KeyButton {textBtn = "5"}]
              ]
          , one_time_keyboard = True
          }
    }



  

