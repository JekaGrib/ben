{-# LANGUAGE OverloadedStrings #-}

module Tg.Api.Response where

import Control.Applicative ((<|>), liftA2)
import Data.Aeson ((.:), (.:?), FromJSON (parseJSON), withObject)
import qualified Data.Text as T
import Tg.Types
import Types

data GetUpdResp
  = GetUpdResp
      { ok :: Bool,
        result :: [Update]
      }
  | OkAnswer {okOA :: Bool}

instance FromJSON GetUpdResp where
  parseJSON =
    liftA2
      (<|>)
      (withObject "GetUpdResp" (\v -> GetUpdResp <$> v .: "ok" <*> v .: "result"))
      (withObject "OkAnswer" (\v -> OkAnswer <$> v .: "ok"))

newtype Answer = Answer
  { okA :: Bool
  }

instance FromJSON Answer where
  parseJSON = withObject "Answer" (\v -> Answer <$> v .: "ok")

data Update
  = Update
      { update_id :: UpdateId,
        message :: Message
      }
  | UnknownUpdate
      { update_id :: UpdateId
      }
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
  { message_id :: MessageId,
    fromUser :: From,
    textMsg :: Maybe T.Text
  }
  deriving (Eq, Show)

instance FromJSON Message where
  parseJSON =
    withObject
      "Message"
      ( \v ->
          Message <$> v .: "message_id" <*> v .: "from"
            <*> v .:? "text"
      )

newtype From = From
  { idUser :: UserId
  }
  deriving (Eq, Show)

instance FromJSON From where
  parseJSON =
    withObject "From" $ \v ->
      From <$> v .: "id"
