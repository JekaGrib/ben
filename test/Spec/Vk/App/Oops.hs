
module Spec.Vk.App.Oops where

import Vk.Oops (VKBotException (..))

isGetLongPollServerException :: VKBotException -> Bool
isGetLongPollServerException GetLongPollServerException {} = True
isGetLongPollServerException _ = False

isCheckGetServerResponseException :: VKBotException -> Bool
isCheckGetServerResponseException CheckGetServerResponseException {} = True
isCheckGetServerResponseException _ = False

isGetUpdatesException :: VKBotException -> Bool
isGetUpdatesException GetUpdatesException {} = True
isGetUpdatesException _ = False

isCheckGetUpdatesResponseException :: VKBotException -> Bool
isCheckGetUpdatesResponseException CheckGetUpdatesResponseException {} = True
isCheckGetUpdatesResponseException _ = False

isSendMsgException :: VKBotException -> Bool
isSendMsgException SendMsgException {} = True
isSendMsgException _ = False

isCheckSendMsgResponseException :: VKBotException -> Bool
isCheckSendMsgResponseException CheckSendMsgResponseException {} = True
isCheckSendMsgResponseException _ = False

isSendKeybException :: VKBotException -> Bool
isSendKeybException SendKeybException {} = True
isSendKeybException _ = False

isCheckSendKeybResponseException :: VKBotException -> Bool
isCheckSendKeybResponseException CheckSendKeybResponseException {} = True
isCheckSendKeybResponseException _ = False
