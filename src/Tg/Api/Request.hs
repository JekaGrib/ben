{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.Api.Request where

import Data.Aeson
  ( (.=),
    ToJSON (toEncoding, toJSON),
    defaultOptions,
    genericToEncoding,
    object,
    pairs,
    Options,
    defaultOptions,
    genericToJSON,
    genericToEncoding,
    fieldLabelModifier
  )
import GHC.Generics (Generic)
import Tg.Types
import Types
import Data.Char (isUpper,toLower)
import Data.List (isSuffixOf)

newtype JSONBodyOffset = JSONBodyOffset
  { offset :: UpdateId
  }
  deriving (Generic, Show)

instance ToJSON JSONBodyOffset where
  toEncoding = genericToEncoding defaultOptions

newtype JSONBodyTimeOut = JSONBodyTimeOut
  { timeout :: Integer
  }
  deriving (Generic, Show)

instance ToJSON JSONBodyTimeOut where
  toEncoding = genericToEncoding defaultOptions

data SendMsgJSONBody = SendMsgJSONBody
  { chat_id :: UserId,
    text :: TextOfMsg
  }
  deriving (Generic, Show)

instance ToJSON SendMsgJSONBody where
  toEncoding = genericToEncoding defaultOptions

data CopyMsgJSONBody = CopyMsgJSONBody
  { chatIdCM :: UserId,
    fromChatIdCM :: UserId,
    messageIdCM :: MessageId
  }
  deriving (Generic, Show)

instance ToJSON CopyMsgJSONBody where
  toJSON     = genericToJSON     (optionsSnakeCasePreEraseSuffix "CM")
  toEncoding = genericToEncoding (optionsSnakeCasePreEraseSuffix "CM")

optionsSnakeCasePreEraseSuffix :: String -> Options
optionsSnakeCasePreEraseSuffix suffix = 
  defaultOptions
    { fieldLabelModifier = fromCamelToSnake . eraseSuffix suffix
    }

fromCamelToSnake :: String -> String
fromCamelToSnake = foldr (\x acc -> if isUpper x then '_':(toLower x):acc else x:acc) []

eraseSuffix :: String -> String -> String
eraseSuffix suffix str = if isSuffixOf suffix str then take (length str - length suffix) str else str


data KeybJSONBody = KeybJSONBody
  { chat_idKeyb :: UserId,
    textKeyb :: TextOfKeyb,
    reply_markup :: KeyBoard
  }
  deriving (Generic, Show)

instance ToJSON KeybJSONBody where
  toJSON (KeybJSONBody a b c) =
    object ["chat_id" .= a, "text" .= b, "reply_markup" .= c]
  toEncoding (KeybJSONBody a b c) =
    pairs ("chat_id" .= a <> "text" .= b <> "reply_markup" .= c)

data KeyBoard = KeyBoard
  { keyboard :: [[KeyButton]],
    one_time_keyboard :: Bool
  }
  deriving (Generic, Show)

instance ToJSON KeyBoard where
  toJSON (KeyBoard a b) = object ["keyboard" .= a, "one_time_keyboard" .= b]
  toEncoding (KeyBoard a b) =
    pairs ("keyboard" .= a <> "one_time_keyboard" .= b)

newtype KeyButton = KeyButton
  { textBtn :: TextOfButton
  }
  deriving (Generic, Show)

instance ToJSON KeyButton where
  toJSON (KeyButton a) = object ["text" .= a]
  toEncoding (KeyButton a) = pairs ("text" .= a)
