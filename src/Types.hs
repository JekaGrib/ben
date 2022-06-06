{-# LANGUAGE ExistentialQuantification #-}

module Types where

import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Text as T
import Data.Typeable (Typeable)

data (Attachy a) => IsValidUpdate a = 
  ValidUpdate UserId (MsgType a)
  | InvalidUpdatePlusInfo String
  | InvalidUpdate 
  deriving (Eq, Show)

class (Show a, Eq a, Typeable a) => Attachy a 

instance Attachy Integer

data (Attachy a) => MsgType a = AttachMsg a | TextMsg TextOfMsg 
  deriving (Eq, Show)

data AttachNotMatter = AttachNotMatter
  deriving (Eq, Show)

instance Attachy AttachNotMatter

data Result = Success | NotSuccess String
  deriving (Eq, Show)


type N = Int

type UserId = Integer

type NState = Either OpenRepeat N

type MapUserN = Map UserId NState

type MessageId = Integer



type Response = LBS.ByteString

type TextOfMsg = T.Text

type TextOfKeyb = T.Text

type TextOfButton = T.Text

newtype OpenRepeat
  = OpenRepeat N
  deriving (Eq, Show)


newtype ToUserId
  = ToUserId UserId
  deriving (Eq, Show)

newtype MsgId
  = MsgId MessageId
  deriving (Eq, Show)

