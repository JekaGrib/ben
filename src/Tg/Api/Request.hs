{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.Api.Request where

import Api (optionsEraseSuffix, optionsSnakeCase, optionsSnakeCasePreEraseSuffix)
import Data.Aeson
  ( ToJSON (toEncoding, toJSON),
    genericToEncoding,
    genericToEncoding,
    genericToJSON,
  )
import GHC.Generics (Generic)
import Tg.Types
import Types

newtype JSONBodyOffset = JSONBodyOffset
  { offset :: UpdateId
  }
  deriving (Generic, Show)

instance ToJSON JSONBodyOffset

newtype JSONBodyTimeOut = JSONBodyTimeOut
  { timeout :: Integer
  }
  deriving (Generic, Show)

instance ToJSON JSONBodyTimeOut

data SendMsgJSONBody = SendMsgJSONBody
  { chatId :: UserId,
    text :: TextOfMsg
  }
  deriving (Generic, Show)

instance ToJSON SendMsgJSONBody where
  toJSON = genericToJSON optionsSnakeCase
  toEncoding = genericToEncoding optionsSnakeCase

data CopyMsgJSONBody = CopyMsgJSONBody
  { chatIdCM :: UserId,
    fromChatIdCM :: UserId,
    messageIdCM :: MessageId
  }
  deriving (Generic, Show)

instance ToJSON CopyMsgJSONBody where
  toJSON = genericToJSON (optionsSnakeCasePreEraseSuffix "CM")
  toEncoding = genericToEncoding (optionsSnakeCasePreEraseSuffix "CM")

data KeybJSONBody = KeybJSONBody
  { chatIdKeyb :: UserId,
    textKeyb :: TextOfKeyb,
    replyMarkupKeyb :: KeyBoard
  }
  deriving (Generic, Show)

instance ToJSON KeybJSONBody where
  toJSON = genericToJSON (optionsSnakeCasePreEraseSuffix "Keyb")
  toEncoding = genericToEncoding (optionsSnakeCasePreEraseSuffix "Keyb")

data KeyBoard = KeyBoard
  { keyboard :: [[KeyButton]],
    oneTimeKeyboard :: Bool
  }
  deriving (Generic, Show)

instance ToJSON KeyBoard where
  toJSON = genericToJSON optionsSnakeCase
  toEncoding = genericToEncoding optionsSnakeCase

newtype KeyButton = KeyButton
  { textBtn :: TextOfButton
  }
  deriving (Generic, Show)

instance ToJSON KeyButton where
  toJSON = genericToJSON (optionsEraseSuffix "Btn")
  toEncoding = genericToEncoding (optionsEraseSuffix "Btn")
