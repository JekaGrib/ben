{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.Types where

import Vk.Api.Response (ServerInfo(..),Photo,Doc,Audio,DocInfo,StickerInfo,WallInfo)
import qualified Data.Text                      as T
import Data.Map (Map)
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString                as BS

type N = Int
type UserId = Integer
type NState = Either OpenRepeat N
type MapUserN = Map UserId NState
type MessageId = Integer
type UpdateId = Integer
type Offset   = Integer
type ServerAndMapUserN = (ServerInfo,MapUserN)
type StickerId = Integer
type GroupId = Integer
type Response = LBS.ByteString
type ResponseS = BS.ByteString
type TextOfMsg = T.Text
type TextOfKeyb = T.Text
type AttachmentString = String
type Url = T.Text
type ServerUrl = T.Text
type DocUrl = T.Text
type PicUrl = T.Text
type TypeInGetServerReq = String
type Extention = String
type Title = String
type LatLong = (String,String)
type ParameterString = String

newtype OpenRepeat =
  OpenRepeat N
  deriving (Eq, Show)

data MSG = TextMsg TextOfMsg | AttachmentMsg TextOfMsg [AttachmentString] LatLong | StickerMsg StickerId 
  deriving (Eq,Show)

data Attachment = PhotoAttachment  Photo 
    | DocAttachment Doc 
    | AudioMesAttachment Audio
    | VideoAttachment DocInfo 
    | StickerAttachment StickerInfo 
    | AudioAttachment DocInfo 
    | MarketAttachment DocInfo 
    | WallAttachment WallInfo 
    | PollAttachment DocInfo 
     deriving (Eq, Show)

data ToUserId      = ToUserId   UserId               deriving (Eq,Show)