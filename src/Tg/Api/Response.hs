{-# LANGUAGE DeriveGeneric #-}

module Tg.Api.Response where

import Api (optionsEraseSuffix, optionsSnakeCasePreEraseSuffix)
import Control.Applicative ((<|>), liftA2)
import Data.Aeson ((.:), FromJSON (parseJSON), genericParseJSON, withObject)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Tg.Types
import Types

data GetUpdResp
  = GetUpdResp Bool [Update]
  | OkAnswer Bool

instance FromJSON GetUpdResp where
  parseJSON =
    liftA2
      (<|>)
      (withObject "GetUpdResp" (\v -> GetUpdResp <$> v .: "ok" <*> v .: "result"))
      (withObject "OkAnswer" (\v -> OkAnswer <$> v .: "ok"))

newtype Answer = Answer
  { okA :: Bool
  }
  deriving (Generic)

instance FromJSON Answer where
  parseJSON = genericParseJSON (optionsEraseSuffix "A")

data Update
  = Update UpdateId Message
  | UnknownUpdate UpdateId
  deriving (Eq, Show)

instance FromJSON Update where
  parseJSON =
    liftA2
      (<|>)
      ( withObject
          "Update"
          (\v -> Update <$> v .: "update_id" <*> v .: "message")
      )
      (withObject "UnknownUpdate" (\v -> UnknownUpdate <$> v .: "update_id"))

data Message = Message
  { messageIdMsg :: MessageId,
    fromMsg :: From,
    textMsg :: Maybe T.Text
  }
  deriving (Generic, Eq, Show)

instance FromJSON Message where
  parseJSON = genericParseJSON (optionsSnakeCasePreEraseSuffix "Msg")

newtype From = From
  { idUser :: UserId
  }
  deriving (Generic, Eq, Show)

instance FromJSON From where
  parseJSON = genericParseJSON (optionsEraseSuffix "User")
