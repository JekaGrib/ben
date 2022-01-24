{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Vk.Oops where

import           Vk.Api.Response
import qualified Data.Text                      as T
import           Control.Monad.Catch
import Vk.TypeSynonym

data VKBotException 
  = DuringGetLongPollServerException String
  | CheckGetServerResponseException String 
  | DuringGetUpdatesException String
  | CheckGetUpdatesResponseException String
  | DuringSendMsgException MSG ToUserId String
  | CheckSendMsgResponseException MSG ToUserId String
  | DuringSendKeybException ToUserId String
  | CheckSendKeybResponseException ToUserId String
  | DuringGetTimeException String
  | DuringPullConfigException String
  | DuringParseConfigException String
    deriving (Eq,Show)

instance Exception VKBotException 

data ToUserId      = ToUserId   UserId               deriving (Eq,Show)

data MSG = TextMsg T.Text | AttachmentMsg T.Text [String] (Maybe Geo) | StickerMsg StickerId 
  deriving (Eq,Show)

