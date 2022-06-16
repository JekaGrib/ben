{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Vk.Types where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString as BS
import Data.Configurator ()
import Data.Configurator.Types (Configured)
import qualified Data.Text as T
import Types

newtype UpdateId = UpdateId Integer deriving newtype (Eq, Show, ToJSON, FromJSON, Ord, Enum, Num)

newtype StickerId = StickerId Integer deriving newtype (Eq, Show, ToJSON, FromJSON, Ord, Enum, Num)

newtype GroupId = GroupId Integer deriving newtype (Eq, Show, ToJSON, FromJSON, Ord, Enum, Num, Read, Configured)

type ResponseS = BS.ByteString

type AttachmentString = String

type Url = T.Text

type ServerUrl = Url

type DocUrl = Url

type PicUrl = Url

type TypeInGetServerReq = String

type Extention = String

type Title = String

type LatLong = (String, String)

type ParameterString = String

type SomethingWrong = String

data VkAttachMSG
  = VkAttachMsg TextOfMsg [AttachmentString] LatLong
  | StickerMsg StickerId
  deriving (Eq, Show)

instance Attachy VkAttachMSG
