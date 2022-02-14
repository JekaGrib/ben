{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module TgTest.Types where

import Tg.Logger ( Priority(..))
import Tg.Types

data MockAction
  = GOTUPDATES
  | SENDMSG UserId TextOfMsg
  | COPYMSG UserId MessageId
  | CONFIRMUPDATES Offset
  | SENDKEYB UserId N TextOfKeyb
  | LOG Priority
  | LOGMSG Priority String
  deriving (Eq, Show)

