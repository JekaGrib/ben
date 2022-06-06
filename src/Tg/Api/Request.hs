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
  )
import GHC.Generics (Generic)
import Tg.Types
import Types


newtype JSONBodyOffset = JSONBodyOffset
  { offset :: Offset
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
  { chat_idCM :: UserId,
    from_chat_idCM :: UserId,
    msg_idCM :: MessageId
  }
  deriving (Generic, Show)

instance ToJSON CopyMsgJSONBody where
  toJSON (CopyMsgJSONBody a b c) =
    object ["chat_id" .= a, "from_chat_id" .= b, "message_id" .= c]
  toEncoding (CopyMsgJSONBody a b c) =
    pairs ("chat_id" .= a <> "from_chat_id" .= b <> "message_id" .= c)

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
