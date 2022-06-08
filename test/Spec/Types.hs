module Spec.Types where

import Logger (Priority (..))
import Types

data MockAction a
  = SENDMSG UserId TextOfMsg
  | SENDAttachMSG UserId a
  | SENDKEYB UserId N TextOfKeyb
  | LOG Priority
  | LOGMSG Priority String
  deriving (Eq, Show)
