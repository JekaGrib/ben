{-# LANGUAGE OverloadedStrings #-}

module App where

import Conf (Config (..))
import Control.Monad.Catch (MonadCatch, catch)
import Control.Monad.State (StateT, get, lift, modify, replicateM_)
import qualified Data.Map as Map (insert, lookup)
import qualified Data.Text as T
import Logger (LogHandle (..), logDebug, logInfo, logWarning)
import Oops
import Types

data Handle m a = Handle
  { hConf :: Config,
    hLog :: LogHandle m,
    sendTxtMsg :: UserId -> TextOfMsg -> m Response,
    sendKeyb :: UserId -> N -> TextOfKeyb -> m Response,
    sendAttachMsg :: a -> UserId -> m Response,
    isValidResponse :: Response -> Result
  }

chooseActionOfUpd ::
  (MonadCatch m, Attachy a) =>
  Handle m a ->
  IsValidUpdate a ->
  StateT MapUserN m ()
chooseActionOfUpd h upd = do
  lift $ logInfo (hLog h) "Analysis update from the list"
  case upd of
    InvalidUpdate ->
      lift $ logWarning (hLog h) "There is UNKNOWN UPDATE. Bot will ignore it"
    InvalidUpdatePlusInfo str ->
      lift $ logWarning (hLog h) $ "There is UNKNOWN UPDATE. Bot will ignore it" ++ str
    ValidUpdate usId msgType -> do
      lift $
        logInfo
          (hLog h)
          ("Get msg from user " ++ show usId)
      chooseActionOfMapUserN h usId msgType

chooseActionOfMapUserN ::
  (MonadCatch m, Attachy a) =>
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
  (MonadCatch m, Attachy a) =>
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
      lift $ logInfo (hLog h) "Msg is attachment"
      lift $ replicateM_ currN $ sendMsgAndCheckResp h usId msgType

chooseActionOfButton ::
  (MonadCatch m, Attachy a) =>
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
  (MonadCatch m, Attachy a) =>
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
  logDebug (hLog h) ("Get response: " ++ show response)
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
  logDebug (hLog h) ("Get response: " ++ show keybResponse)
  let result = isValidResponse h keybResponse
  checkSendKeybResponse h usId currN infoMsg result

checkSendMsgResponse ::
  (MonadCatch m, Attachy a) =>
  Handle m a ->
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
        ("Msg " ++ show msgType ++ " was sent to user " ++ show usId)

checkSendKeybResponse ::
  (MonadCatch m, Attachy a) =>
  Handle m a ->
  UserId ->
  N ->
  TextOfKeyb ->
  Result ->
  m ()
checkSendKeybResponse h usId n txt res =
  case res of
    NotSuccess str ->
      throwAndLogEx (hLog h) (CheckSendKeybResponseException (ToUserId usId) str :: BotException AttachNotMatter)
    Success ->
      logInfo
        (hLog h)
        ( "Keyboard with message: "
            ++ show n
            ++ show txt
            ++ " was sent to user "
            ++ show usId
        )

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
