module Vk.Types where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Types

type UpdateId = Integer

type Offset = Integer

type StickerId = Integer

type GroupId = Integer

type ResponseS = BS.ByteString

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

data VkAttachMSG
  = VkAttachMsg TextOfMsg [AttachmentString] LatLong
  | StickerMsg StickerId
  deriving (Eq, Show)

instance Attachy VkAttachMSG

newtype ToUserId
  = ToUserId UserId
  deriving (Eq, Show)
