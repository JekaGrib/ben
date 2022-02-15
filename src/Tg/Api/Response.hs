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
    | OkAnswer {okOA :: Bool}

instance FromJSON GetUpdResp where
  parseJSON = 
    liftA2
      (<|>)
      (withObject "GetUpdResp" (\v -> GetUpdResp <$> v .: "ok" <*> v .: "result"))
      (withObject "OkAnswer" (\v -> OkAnswer <$> v .: "ok" ))

newtype Answer
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
      , textMsg :: T.Text
      }
  | Message
      { message_id :: MessageId
      , fromUser :: From
      }
  deriving (Eq,Show)


instance FromJSON Message where
  parseJSON =
    liftA2
      (<|>)
      (withObject
         "TxtMessage"
         (\v ->
            TxtMessage <$> v .: "message_id" <*> v .: "from"  <*>
            v .: "text"))
      (withObject
         "Message"
         (\v ->
            Message <$> v .: "message_id" <*> v .: "from" ))

data From =
  From
    { idUser :: UserId
    }
  deriving (Eq,Show)

instance FromJSON From where
  parseJSON =
    withObject "From" $ \v ->
      From <$> v .: "id" 
