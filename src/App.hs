{-# LANGUAGE OverloadedStrings #-}

module App where

--import Control.Monad (when)
import Control.Monad.Catch (MonadCatch, catch )
import Control.Monad.State (StateT, get, lift, modify, replicateM_)
import qualified Data.Map as Map (insert, lookup)
import qualified Data.Text as T
import Conf (Config (..))
import Logger (LogHandle (..), logInfo, logWarning,logDebug)
import Types
import Oops


data (Attachy a) => Handle m a = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    sendTxtMsg :: UserId -> TextOfMsg -> m Response,
    sendKeyb :: UserId -> N -> TextOfKeyb -> m Response,
    sendAttachMsg :: a -> UserId -> m Response,
    isValidResponse :: Response -> Result
  }


chooseActionOfUpd ::
  (MonadCatch m,Attachy a) =>
  Handle m a ->
  IsValidUpdate a ->
  StateT MapUserN m ()
chooseActionOfUpd h upd = do
  lift $ logInfo (hLog h) "Analysis update from the list"
  case upd of
    InvalidUpdate -> do
      lift $ logWarning (hLog h) "There is UNKNOWN UPDATE. Bot will ignore it"
    InvalidUpdatePlusInfo str -> do
      lift $ logWarning (hLog h) $ "There is UNKNOWN UPDATE. Bot will ignore it" ++ str
    ValidUpdate usId msgType -> do
      lift $
        logInfo
          (hLog h)
          ("Get msg from user " ++ show usId )
      chooseActionOfMapUserN h usId msgType

chooseActionOfMapUserN ::
  (MonadCatch m,Attachy a) =>
  Handle m a ->
  UserId ->
  MsgType a ->
  StateT MapUserN m ()
chooseActionOfMapUserN h usId msgType = do
  mapUN <- get
  let nState = Map.lookup usId mapUN
  case nState of
    Just (Left (OpenRepeat oldN)) -> do
      lift $
        logInfo (hLog h) ("User " ++ show usId ++ " is in OpenRepeat mode")
      chooseActionOfButton h usId msgType oldN
    Just (Right n) -> do
      let currN = n
      chooseActionOfMsgType h usId msgType currN
    Nothing -> do
      let currN = cStartN (hConf h)
      chooseActionOfMsgType h usId msgType currN

chooseActionOfMsgType ::
  (MonadCatch m,Attachy a) =>
  Handle m a ->
  UserId ->
  MsgType a ->
  N ->
  StateT MapUserN m ()
chooseActionOfMsgType h usId msgType currN =
  case msgType of
    TextMsg txt -> do
      lift $
        logInfo
          (hLog h)
          ("Msg has text: " ++ show txt)
      chooseActionOfTxt h currN usId txt
    AttachMsg _ -> do
      lift $ logInfo (hLog h) ("Msg is attachment")
      lift $ replicateM_ currN $ sendMsgAndCheckResp h usId msgType
        



chooseActionOfButton ::
  (MonadCatch m,Attachy a) =>
  Handle m a ->
  UserId ->
  MsgType a ->
  N ->
  StateT MapUserN m ()
chooseActionOfButton h usId msgType oldN =
  case checkButton msgType of
    Just newN -> do
      lift $
        logInfo
          (hLog h)
          ( "Change number of repeats to "
              ++ show newN
              ++ " for user "
              ++ show usId
          )
      modify (changeMapUserN usId (Right newN))
      let infoMsg =
            T.pack $
              "Number of repeats successfully changed from "
                ++ show oldN
                ++ " to "
                ++ show newN
      lift $ sendMsgAndCheckResp h usId (TextMsg infoMsg)
    Nothing -> do
      lift $
        logWarning
          (hLog h)
          ( "User "
              ++ show usId
              ++ " press UNKNOWN BUTTON, close OpenRepeat mode, leave old number of repeats: "
              ++ show oldN
          )
      modify (changeMapUserN usId (Right oldN))
      let infoMsg =
            T.pack $
              "UNKNOWN NUMBER\nI,m ssory, number of repeats has not changed, it is still "
                ++ show oldN
                ++ "\nTo change it you may sent me command \"/repeat\" and then choose number from 1 to 5 on keyboard\nPlease, try again later"
      lift $ sendMsgAndCheckResp h usId (TextMsg infoMsg)

chooseActionOfTxt ::
  (MonadCatch m,Attachy a) =>
  Handle m a ->
  N ->
  UserId ->
  TextOfMsg ->
  StateT MapUserN m ()
chooseActionOfTxt h currN usId txt =
  case filter (' ' /=) . T.unpack $ txt of
    "/help" -> do
      let infoMsg = T.pack $ cHelpMsg (hConf h)
      lift $ sendMsgAndCheckResp h usId (TextMsg infoMsg)
    "/repeat" -> do
      lift $ sendKeybAndCheckResp h usId currN
      lift $
        logInfo (hLog h) ("Put user " ++ show usId ++ " to OpenRepeat mode")
      modify (changeMapUserN usId (Left $ OpenRepeat currN))
    _ -> lift $ replicateM_ currN $ sendMsgAndCheckResp h usId (TextMsg txt)





sendMsgAndCheckResp ::
  (MonadCatch m, Attachy a) => Handle m a -> UserId -> MsgType a -> m ()
sendMsgAndCheckResp h usId msgType = do
  logDebug
    (hLog h)
    ( "Send request to send msg "
        ++ show msgType
        ++ " to userId "
        ++ show usId
    )
  response <- sendMsg h usId msgType `catch` handleExSendMsg (hLog h) usId msgType
  logDebug (hLog h) ("Get response: " ++ show response )
  let result = isValidResponse h response 
  checkSendMsgResponse h usId msgType result

sendMsg :: (MonadCatch m, Attachy a) => Handle m a -> UserId -> MsgType a -> m Response
sendMsg h usId (AttachMsg a) = sendAttachMsg h a usId
sendMsg h usId (TextMsg txt) = sendTxtMsg h usId txt

sendKeybAndCheckResp ::
  (MonadCatch m, Attachy a) => Handle m a -> UserId -> N -> m ()
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
  keybResponse <-
    sendKeyb h usId currN infoMsg `catch` handleExSendKeyb (hLog h) usId
  logDebug (hLog h) ("Get response: " ++ show keybResponse )
  let result = isValidResponse h keybResponse
  checkSendKeybResponse h usId currN infoMsg result 



{-}
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
-}


checkSendMsgResponse ::
  (MonadCatch m, Attachy a) => Handle m a -> 
  UserId ->
  MsgType a ->
  Result ->
  m ()
checkSendMsgResponse h usId msgType res =
  case res of
    NotSuccess str ->
      throwAndLogEx (hLog h) $
        CheckSendMsgResponseException msgType (ToUserId usId) str
    Success ->
      logInfo
        (hLog h)
        ("Msg " ++ show msgType ++ " was sent to user " ++ show usId )


checkSendKeybResponse ::
  (MonadCatch m, Attachy a) => Handle m a -> 
  UserId ->
  N ->
  TextOfKeyb ->
  Result ->
  m ()
checkSendKeybResponse h usId n txt res =
  case res of
    NotSuccess str ->
      throwAndLogEx (hLog h) $ (CheckSendKeybResponseException (ToUserId usId) str :: BotException AttachNotMatter)
    Success ->
      logInfo
        (hLog h)
        ( "Keyboard with message: "
            ++ show n
            ++ show txt
            ++ " was sent to user "
            ++ show usId
        )

{-
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

copyMsg' :: Config -> UserId -> MessageId -> IO Response
copyMsg' conf usId msgId = do
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
-}

changeMapUserN ::
  UserId ->
  NState ->
  MapUserN ->
  MapUserN
changeMapUserN = Map.insert

checkButton :: MsgType a -> Maybe N
checkButton (TextMsg txt) = checkTextButton txt
checkButton _ = Nothing

checkTextButton :: TextOfButton -> Maybe N
checkTextButton txt =
  case txt of
    "1" -> Just 1
    "2" -> Just 2
    "3" -> Just 3
    "4" -> Just 4
    "5" -> Just 5
    _ -> Nothing


