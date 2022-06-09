{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Vk.Api.Request where

import Api (optionsEraseSuffix, optionsSnakeCase)
import Data.Aeson
  ( ToJSON (toEncoding, toJSON),
    genericToEncoding,
    genericToJSON,
  )
import qualified Data.Text as T
import GHC.Generics (Generic)

data KeyBoard = KeyBoard
  { oneTime :: Bool,
    buttons :: [[Button]],
    inline :: Bool
  }
  deriving (Generic, Show)

instance ToJSON KeyBoard where
  toJSON = genericToJSON optionsSnakeCase
  toEncoding = genericToEncoding optionsSnakeCase

data Button = Button
  { action :: Action,
    color :: T.Text
  }
  deriving (Generic, Show, ToJSON)

data Action = Action
  { typeA :: T.Text,
    labelA :: T.Text
  }
  deriving (Generic, Show)

instance ToJSON Action where
  toJSON = genericToJSON $ optionsEraseSuffix "A"
  toEncoding = genericToEncoding $ optionsEraseSuffix "A"

keyBoard :: KeyBoard
keyBoard =
  KeyBoard
    True
    [[button "1"], [button "2"], [button "3"], [button "4"], [button "5"]]
    False

button :: T.Text -> Button
button txt = Button (Action "text" txt) "positive"
