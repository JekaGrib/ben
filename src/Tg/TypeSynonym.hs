{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.TypeSynonym where

import qualified Data.Text as T
import Data.Map (Map)


type N = Int
type UserId = Integer
type NState = Either OpenRepeat N
--type UserN  = (UserId,NState)
type MapUserN = Map UserId NState
type MessageId = Integer
type UpdateId = Integer
type Offset   = Integer

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