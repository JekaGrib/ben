{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.Api.Request where

import Data.Aeson
  ( ToJSON(toEncoding, toJSON)
  , (.=)
  , defaultOptions
  , genericToEncoding
  , object
  , pairs
  )
import qualified Data.Text as T
import GHC.Generics (Generic)

data KeyBoard =
  KeyBoard
    { one_time :: Bool
    , buttons :: [[Button]]
    , inline :: Bool
    }
  deriving (Generic, Show)

instance ToJSON KeyBoard where
  toEncoding = genericToEncoding defaultOptions

data Button =
  Button
    { action :: Action
    , color :: T.Text
    }
  deriving (Generic, Show)

instance ToJSON Button where
  toEncoding = genericToEncoding defaultOptions

data Action =
  Action
    { typeA :: T.Text
    , label :: T.Text
    }
  deriving (Generic, Show)

instance ToJSON Action where
  toJSON (Action a b) = object ["type" .= a, "label" .= b]
  toEncoding (Action a b) = pairs ("type" .= a <> "label" .= b)

keyBoard :: KeyBoard
keyBoard =
  KeyBoard
    True
    [[button "1"], [button "2"], [button "3"], [button "4"], [button "5"]]
    False

button :: T.Text -> Button
button txt = Button (Action "text" txt) "positive"