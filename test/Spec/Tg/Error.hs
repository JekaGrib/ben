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
