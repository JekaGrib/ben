{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tg.TypeSynonym where

type N = Int
type UserId = Integer
type NState = Either OpenRepeat N
type UserN  = (UserId,NState)
type UsersNs = [UserN]
type MessageId = Integer
type UpdateId = Integer
type Offset   = Integer

newtype OpenRepeat =
  OpenRepeat N
  deriving (Eq, Show)
