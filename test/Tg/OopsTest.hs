{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Tg.OopsTest where

import Tg.Oops



isGetUpdatesException :: TGBotException -> Bool
isGetUpdatesException (GetUpdatesException _) = True
isGetUpdatesException _ = False

isCheckGetUpdatesResponseException :: TGBotException -> Bool
isCheckGetUpdatesResponseException (CheckGetUpdatesResponseException _) = True
isCheckGetUpdatesResponseException _ = False

isConfirmUpdatesException :: TGBotException -> Bool
isConfirmUpdatesException (ConfirmUpdatesException _) = True
isConfirmUpdatesException _ = False

isCheckConfirmUpdatesResponseException :: TGBotException -> Bool
isCheckConfirmUpdatesResponseException (CheckConfirmUpdatesResponseException _) = True
isCheckConfirmUpdatesResponseException _ = False

isSendMsgException :: TGBotException -> Bool
isSendMsgException (SendMsgException _ _ _) = True
isSendMsgException _ = False

isCopyMsgException :: TGBotException -> Bool
isCopyMsgException (CopyMsgException _ _ _) = True
isCopyMsgException _ = False

isCheckSendMsgResponseException :: TGBotException -> Bool
isCheckSendMsgResponseException (CheckSendMsgResponseException _ _ _) = True
isCheckSendMsgResponseException _ = False

isCheckCopyMsgResponseException :: TGBotException -> Bool
isCheckCopyMsgResponseException (CheckCopyMsgResponseException _ _ _) = True
isCheckCopyMsgResponseException _ = False

isSendKeybException :: TGBotException -> Bool
isSendKeybException (SendKeybException _ _) = True
isSendKeybException _ = False

isCheckSendKeybResponseException :: TGBotException -> Bool
isCheckSendKeybResponseException (CheckSendKeybResponseException _ _) = True
isCheckSendKeybResponseException _ = False
