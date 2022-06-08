module Spec.Error where

import Error


isSendMsgException :: BotException a -> Bool
isSendMsgException SendMsgException {} = True
isSendMsgException _ = False

isCheckSendMsgResponseException :: BotException a -> Bool
isCheckSendMsgResponseException CheckSendMsgResponseException {} = True
isCheckSendMsgResponseException _ = False

isSendKeybException :: BotException a -> Bool
isSendKeybException SendKeybException {} = True
isSendKeybException _ = False

isCheckSendKeybResponseException :: BotException a -> Bool
isCheckSendKeybResponseException CheckSendKeybResponseException {} = True
isCheckSendKeybResponseException _ = False
