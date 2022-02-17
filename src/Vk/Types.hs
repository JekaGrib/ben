{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}

module Vk.Types where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Text as T
import Vk.Api.Response ( ServerInfo(..),Update(..) )

type N = Int

type UserId = Integer

type NState = Either OpenRepeat N

type MapUserN = Map UserId NState

type MessageId = Integer

type UpdateId = Integer

type Offset = Integer

type ServerAndMapUserN = (ServerInfo, MapUserN)
type UpdatesAndServer = ([Update],ServerInfo)

type StickerId = Integer

type GroupId = Integer

type Response = LBS.ByteString

type ResponseS = BS.ByteString

type TextOfMsg = T.Text

type TextOfKeyb = T.Text

type AttachmentString = String

type Url = T.Text

type ServerUrl = T.Text

type DocUrl = T.Text

type PicUrl = T.Text

type TypeInGetServerReq = String

type Extention = String

type Title = String

type LatLong = (String, String)

type ParameterString = String

type SomethingWrong = String

type Counter = Int

newtype OpenRepeat =
  OpenRepeat N
  deriving (Eq, Show)

data MSG
  = TextMsg TextOfMsg
  | AttachmentMsg TextOfMsg [AttachmentString] LatLong
  | StickerMsg StickerId
  deriving (Eq, Show)

newtype ToUserId =
  ToUserId UserId
  deriving (Eq, Show)