{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.Api.Request where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T

kB = KeyBoard True [[button1],[button2],[button3],[button4],[button5]] False

button1 = Button action1 "positive"
button2 = Button action2 "positive"
button3 = Button action3 "positive"
button4 = Button action4 "positive"
button5 = Button action5 "positive"

action1 = Action "text" "1"
action2 = Action "text" "2"
action3 = Action "text" "3"
action4 = Action "text" "4"
action5 = Action "text" "5"

data KeyBoard = KeyBoard
  { one_time  :: Bool,
    buttons   :: [[Button]],
    inline    :: Bool
    } deriving (Generic, Show)

instance ToJSON KeyBoard where
    toEncoding = genericToEncoding defaultOptions

data Button = Button
  { action  :: Action,
    color   :: T.Text
    } deriving (Generic, Show)

instance ToJSON Button where
    toEncoding = genericToEncoding defaultOptions

data Action = Action
  { typeA  :: T.Text,
    label  :: T.Text
    } deriving (Generic, Show)

instance ToJSON Action where
  toJSON (Action typeA  label ) =
    object ["type" .= typeA , "label" .= label]
  toEncoding (Action typeA  label ) =
    pairs ("type" .= typeA  <> "label" .= label)


