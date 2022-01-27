{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.TypeSynonym where

import Vk.Api.Response (ServerInfo(..),Geo,Photo,Doc,Audio,DocInfo,StickerInfo,WallInfo)
import qualified Data.Text                      as T
import Data.Map (Map)


type N = Int
type UserId = Integer
type NState = Either OpenRepeat N
--type UserN  = (UserId,NState)
--type UsersNs = [UserN]
type MapUserN = Map UserId NState
type MessageId = Integer
type UpdateId = Integer
type Offset   = Integer
type ServerAndMapUserN = (ServerInfo,MapUserN)
type StickerId = Integer
type GroupId = Integer

newtype OpenRepeat =
  OpenRepeat N
  deriving (Eq, Show)

data MSG = TextMsg T.Text | AttachmentMsg T.Text [String] (Maybe Geo) | StickerMsg StickerId 
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