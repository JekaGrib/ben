{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tg.Types where

import Data.Aeson (FromJSON, ToJSON)

newtype UpdateId
  = UpdateId Integer
  deriving newtype (Eq, Show, ToJSON, FromJSON, Ord, Enum, Num)
