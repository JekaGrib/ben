module Spec.Tg.Types where

import Tg.Types
import Types

data TgMockAction
  = GOTUPDATES
  | CONFIRMUPDATES UpdateId
  deriving (Eq, Show)
