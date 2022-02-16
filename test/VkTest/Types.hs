{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module VkTest.Types where


import Vk.Logger ( Priority(..))
import Vk.Types

data MockAction
  = GOTSERVER
  | SENDMSG UserId MSG
  | SENDKEYB UserId N TextOfKeyb
  | LOG Priority
  | LOGMSG Priority String
  deriving (Eq, Show)

