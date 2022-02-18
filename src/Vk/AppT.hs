{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC  -Wall  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}


module Vk.AppT where

import Control.Monad.State ( StateT, gets,  modify,MonadState)
import Vk.Api.Response (ServerInfo)
import Vk.Types (MapUserN)


type AppT = StateT (ServerInfo,MapUserN) 

class (MonadState (s1,s2) m) => MonadStateTwo s1 s2 m | m -> s1 s2 where
  get1 :: m s1
  get2 :: m s2
  put1 :: s1 -> m ()
  put2 :: s2 -> m ()
  modify1 :: (s1 -> s1) -> m ()
  modify2 :: (s2 -> s2) -> m ()
  
instance (Monad m) => MonadStateTwo ServerInfo MapUserN (StateT (ServerInfo,MapUserN) m) where
  get1 = gets fst
  get2 = gets snd
  put1 s1 = modify (\(_,s2) -> (s1,s2))
  put2 s2 = modify (\(s1,_) -> (s1,s2))
  modify1 f = modify (\(s1,s2) -> (f s1,s2))
  modify2 f = modify (\(s1,s2) -> (s1,f s2))
  
