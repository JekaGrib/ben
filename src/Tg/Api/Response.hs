{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.Api.Response where

import Control.Applicative ((<|>), liftA2)
import Data.Aeson (FromJSON(parseJSON), (.:), withObject)
import qualified Data.Text as T
import Tg.Types


data GetUpdResp
  = GetUpdResp
      { ok :: Bool
      , result :: [Update]
      }

instance FromJSON GetUpdResp where
  parseJSON = withObject "GetUpdResp" (\v -> GetUpdResp <$> v .: "ok" <*> v .: "result")

data Answer
  = Answer
      { okA :: Bool
      }

instance FromJSON Answer where
  parseJSON = withObject "Answer" (\v -> Answer <$> v .: "ok" )


data Update
  = Update
      { update_id :: UpdateId
      , message :: Message
      }
  | UnknownUpdate
      { update_id :: UpdateId
      }
  deriving (Eq,Show)

instance FromJSON Update where
  parseJSON =
    liftA2
      (<|>)
      (withObject
         "Update"
         (\v -> Update <$> v .: "update_id" <*> v .: "message"))
      (withObject "UnknownUpdate" (\v -> UnknownUpdate <$> v .: "update_id"))

data Message
  = TxtMessage
      { message_id :: MessageId
      , fromUser :: From
      , chat ::  Maybe Chat
      , date ::  Maybe Integer
      , textMsg :: T.Text
      }
  | Message
      { message_id :: Integer
      , fromUser :: From
      , chat ::  Maybe Chat
      , date :: Maybe Integer
      }
  deriving (Eq,Show)


instance FromJSON Message where
  parseJSON =
    liftA2
      (<|>)
      (withObject
         "TxtMessage"
         (\v ->
            TxtMessage <$> v .: "message_id" <*> v .: "from" <*> v .: "chat" <*>
            v .: "date" <*>
            v .: "text"))
      (withObject
         "Message"
         (\v ->
            Message <$> v .: "message_id" <*> v .: "from" <*> v .: "chat" <*>
            v .: "date"))

data From =
  From
    { idUser :: UserId
    , is_bot :: Maybe Bool
    , first_name ::  Maybe T.Text
    , last_name ::  Maybe T.Text
    , language_code ::  Maybe T.Text
    }
  deriving (Eq,Show)

instance FromJSON From where
  parseJSON =
    withObject "From" $ \v ->
      From <$> v .: "id" <*> v .: "is_bot" <*> v .: "first_name" <*>
      v .: "last_name" <*>
      v .: "language_code"

data Chat =
  Chat
    { id1 :: Integer
    , first_nameChat :: T.Text
    , last_nameChat :: T.Text
    , typ :: T.Text
    }
  deriving (Eq,Show)

instance FromJSON Chat where
  parseJSON =
    withObject "Chat" $ \v ->
      Chat <$> v .: "id" <*> v .: "first_name" <*> v .: "last_name" <*>
      v .: "type"