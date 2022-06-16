module Spec.Types where

import Logger (Priority (..))
import Spec.Tg.Types
import Spec.Vk.Types
import Tg.Types
import Types

data MockAction a
  = SENDMSG UserId TextOfMsg
  | SENDAttachMSG UserId a
  | SENDKEYB UserId N TextOfKeyb
  | LOG Priority
  | LOGMSG Priority String
  | TgMock TgMockAction
  | VkMock VkMockAction
  deriving (Eq, Show)
