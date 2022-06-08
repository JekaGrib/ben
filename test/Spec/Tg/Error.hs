module Spec.Tg.Error where

import Tg.Error

isGetUpdatesException :: TGBotException -> Bool
isGetUpdatesException GetUpdatesException {} = True
isGetUpdatesException _ = False

isCheckGetUpdatesResponseException :: TGBotException -> Bool
isCheckGetUpdatesResponseException CheckGetUpdatesResponseException {} = True
isCheckGetUpdatesResponseException _ = False

isConfirmUpdatesException :: TGBotException -> Bool
isConfirmUpdatesException ConfirmUpdatesException {} = True
isConfirmUpdatesException _ = False

isCheckConfirmUpdatesResponseException :: TGBotException -> Bool
isCheckConfirmUpdatesResponseException CheckConfirmUpdatesResponseException {} = True
isCheckConfirmUpdatesResponseException _ = False

isSendMsgException :: TGBotException -> Bool
isSendMsgException SendMsgException {} = True
isSendMsgException _ = False

isCopyMsgException :: TGBotException -> Bool
isCopyMsgException CopyMsgException {} = True
isCopyMsgException _ = False

isCheckSendMsgResponseException :: TGBotException -> Bool
isCheckSendMsgResponseException CheckSendMsgResponseException {} = True
isCheckSendMsgResponseException _ = False

isCheckCopyMsgResponseException :: TGBotException -> Bool
isCheckCopyMsgResponseException CheckCopyMsgResponseException {} = True
isCheckCopyMsgResponseException _ = False

isSendKeybException :: TGBotException -> Bool
isSendKeybException SendKeybException {} = True
isSendKeybException _ = False

isCheckSendKeybResponseException :: TGBotException -> Bool
isCheckSendKeybResponseException CheckSendKeybResponseException {} = True
isCheckSendKeybResponseException _ = False
