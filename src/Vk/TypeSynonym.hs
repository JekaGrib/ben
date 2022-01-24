{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Vk.TypeSynonym where

import Vk.Api.Response (ServerInfo(..))

type N = Int
type UserId = Integer
type NState = Either OpenRepeat N
type UserN  = (UserId,NState)
type UsersNs = [UserN]
type MessageId = Integer
type UpdateId = Integer
type Offset   = Integer
type ServerAndUsersNs = (ServerInfo,UsersNs)
type StickerId = Integer
type GroupId = Integer

newtype OpenRepeat =
  OpenRepeat N
  deriving (Eq, Show)
