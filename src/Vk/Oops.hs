{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Vk.Oops where

import           Vk.Logger
import           Vk.Api.Response
import           Vk.Api.Request
import           Network.HTTP.Client            ( urlEncodedBody, parseRequest, responseBody, httpLbs, method, requestBody, requestHeaders, RequestBody(..) )
import           Network.HTTP.Client.TLS        (newTlsManager)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString                as BS
import qualified Data.Text                      as T
import           Data.Maybe                     ( fromJust )
import           Control.Monad
import           Control.Monad.State
import           Data.List
import           Control.Monad.Catch
import qualified Control.Exception              as E
import           Network.HTTP.Client.MultipartFormData
import           Data.Binary.Builder
import qualified System.IO                      as S
import           Data.String                    ( fromString )
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

