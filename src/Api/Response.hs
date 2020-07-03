{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Response where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text                      as T
import           Control.Applicative

data Answer 
    = Answer 
        { ok :: Bool,
          result :: [Update] }
    | NotOkAnswer
        { ok :: Bool}

data Update
    = Update 
        { update_id :: Int,
          message :: Message }
    | UnknownUpdate 
        { update_id :: Int }

data Message = Message { message_id :: Int,
                         fromUser :: From,
                         chat :: Chat,
                         date :: Int,
                         textMsg :: T.Text
                       }

data From = From { idUser :: Int,
                   is_bot :: Bool,
                   first_name :: T.Text,
                   last_name :: T.Text,
                   language_code :: T.Text }

data Chat = Chat { id1 :: Int,
                   first_nameChat :: T.Text,
                   last_nameChat :: T.Text,
                   typ :: T.Text }

instance FromJSON Answer where
    parseJSON (Object v) = (Answer
        <$> v .: "ok"
        <*> v .: "result") <|> ( NotOkAnswer
        <$> v .: "ok")

instance FromJSON Update where
    parseJSON (Object v) = (Update
        <$> v .: "update_id"
        <*> v .: "message") <|> ( UnknownUpdate
        <$> v .: "update_id")

instance FromJSON Message where
    parseJSON = withObject "Message" $ \v -> Message
        <$> v .: "message_id"
        <*> v .: "from"
        <*> v .: "chat"
        <*> v .: "date"
        <*> v .: "text"

instance FromJSON From where
    parseJSON = withObject "From" $ \v -> From
        <$> v .: "id"
        <*> v .: "is_bot"
        <*> v .: "first_name"
        <*> v .: "last_name"
        <*> v .: "language_code"

instance FromJSON Chat where
    parseJSON = withObject "Chat" $ \v -> Chat
        <$> v .: "id"
        <*> v .: "first_name"
        <*> v .: "last_name"
        <*> v .: "type"

