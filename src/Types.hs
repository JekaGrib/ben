{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Text as T
import Data.Typeable (Typeable)

data Messenger = VK | TG

instance Show Messenger where
  show VK = "VK"
  show TG = "telegram"

data ValidUpdate a
  = ValidUpdate UserId (MsgType a)
  | InvalidUpdatePlusInfo String
  | InvalidUpdate
  deriving (Eq, Show)

class (Show a, Eq a, Typeable a) => Attachy a

instance Attachy Integer

data MsgType a = AttachMsg a | TextMsg TextOfMsg
  deriving (Eq, Show)

data AttachNotMatter = AttachNotMatter
  deriving (Eq, Show)

instance Attachy AttachNotMatter

data Result = Success | NotSuccess String
  deriving (Eq, Show)

type N = Int

type NState = Either OpenRepeat N

type MapUserN = Map UserId NState

newtype OpenRepeat
  = OpenRepeat N
  deriving (Eq, Show)

newtype UserId
  = UserId Integer
  deriving newtype (Eq, Show, ToJSON, FromJSON, Ord, Enum, Num)

newtype MessageId
  = MessageId Integer
  deriving newtype (Eq, Show, ToJSON, FromJSON, Ord, Enum, Num, Attachy)

type Response = LBS.ByteString

type TextOfMsg = T.Text

type TextOfKeyb = T.Text

type TextOfButton = T.Text
