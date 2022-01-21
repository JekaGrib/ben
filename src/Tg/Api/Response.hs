{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.Api.Response where

import           Data.Aeson
import qualified Data.Text                      as T
import           Control.Applicative


data Answer 
  = Answer 
    { ok :: Bool,
      result :: [Update] }
  | OkAnswer
    { ok :: Bool}

instance FromJSON Answer where
  parseJSON = liftA2 (<|>)
    (withObject "Answer" (\v -> Answer
      <$> v .: "ok"
      <*> v .: "result")) 
    (withObject "OkAnswer" (\v -> OkAnswer
      <$> v .: "ok"))


data Update
    = Update 
        { update_id :: Int,
          message :: Message }
    | UnknownUpdate 
        { update_id :: Int }

instance FromJSON Update where
  parseJSON = liftA2 (<|>)
      (withObject "Update" (\v -> Update
        <$> v .: "update_id"
        <*> v .: "message"))
      (withObject "UnknownUpdate" (\v -> UnknownUpdate
        <$> v .: "update_id"))


data Message 
  = TxtMessage 
    { message_id :: Int,
      fromUser :: From,
      chat :: Chat,
      date :: Int,
      textMsg :: T.Text }
  | Message 
    { message_id :: Int,
      fromUser :: From,
      chat :: Chat,
      date :: Int}

instance FromJSON Message where
  parseJSON = liftA2 (<|>)
      (withObject "TxtMessage" (\v -> TxtMessage
        <$> v .: "message_id"
        <*> v .: "from"
        <*> v .: "chat"
        <*> v .: "date"
        <*> v .: "text"))
      (withObject "Message" (\v -> Message
        <$> v .: "message_id"
        <*> v .: "from"
        <*> v .: "chat"
        <*> v .: "date"))


data From = From { idUser :: Int,
                   is_bot :: Bool,
                   first_name :: T.Text,
                   last_name :: T.Text,
                   language_code :: T.Text }

instance FromJSON From where
    parseJSON = withObject "From" $ \v -> From
        <$> v .: "id"
        <*> v .: "is_bot"
        <*> v .: "first_name"
        <*> v .: "last_name"
        <*> v .: "language_code"


data Chat = Chat { id1 :: Int,
                   first_nameChat :: T.Text,
                   last_nameChat :: T.Text,
                   typ :: T.Text }

instance FromJSON Chat where
    parseJSON = withObject "Chat" $ \v -> Chat
        <$> v .: "id"
        <*> v .: "first_name"
        <*> v .: "last_name"
        <*> v .: "type"

