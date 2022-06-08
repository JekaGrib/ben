module Spec.Types where

import Logger (Priority (..))
import Types
import Tg.Types
import Spec.Tg.Types

data MockAction a
  = SENDMSG UserId TextOfMsg
  | SENDAttachMSG UserId a
  | SENDKEYB UserId N TextOfKeyb
  | LOG Priority
  | LOGMSG Priority String
  | TgMock TgMockAction
  deriving (Eq, Show)
