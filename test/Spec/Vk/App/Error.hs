module Spec.Vk.App.Error where

import Vk.Error (VKBotException (..))

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
