{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.Types where

import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import qualified Data.Text as T

type N = Int

type UserId = Integer

type NState = Either OpenRepeat N

type MapUserN = Map UserId NState

type MessageId = Integer

type UpdateId = Integer

type Offset = Integer

type Response = LBS.ByteString

type TextOfMsg = T.Text

type TextOfKeyb = T.Text

type TextOfButton = T.Text

newtype OpenRepeat =
  OpenRepeat N
  deriving (Eq, Show)

newtype Msg =
  Msg T.Text
  deriving (Eq, Show)

newtype ToUserId =
  ToUserId UserId
  deriving (Eq, Show)

newtype MsgId =
  MsgId MessageId
  deriving (Eq, Show)
