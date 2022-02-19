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
import Vk.Api.Response (ServerInfo(..))
import Vk.Types (MapUserN)


type AppT = StateT (TryServer,MapUserN) 

class (MonadState (s1,s2) m) => MonadStateTwo s1 s2 m | m -> s1 s2 where
  get1 :: m s1
  get2 :: m s2
  put1 :: s1 -> m ()
  put2 :: s2 -> m ()
  modify1 :: (s1 -> s1) -> m ()
  modify2 :: (s2 -> s2) -> m ()
  
instance (Monad m) => MonadStateTwo TryServer MapUserN (StateT (TryServer,MapUserN) m) where
  get1 = gets fst
  get2 = gets snd
  put1 s1 = modify (\(_,s2) -> (s1,s2))
  put2 s2 = modify (\(s1,_) -> (s1,s2))
  modify1 f = modify (\(s1,s2) -> (f s1,s2))
  modify2 f = modify (\(s1,s2) -> (s1,f s2))
  
data TryServer = TryServer {tryNum ::Int, servInf :: ServerInfo}
 deriving (Eq,Show)

instance Ord TryServer where
  compare a b = compare (tryNum a) (tryNum b)

nextTry :: TryServer -> TryServer
nextTry (TryServer num sI) = TryServer (succ num) sI

resetTry :: TryServer -> TryServer
resetTry (TryServer _ sI) = TryServer 1 sI

changeServInfo :: ServerInfo -> TryServer -> TryServer
changeServInfo sI (TryServer num _) = TryServer num sI

changeTs :: Integer -> TryServer -> TryServer
changeTs ts (TryServer num sI) = TryServer num sI{tsSI=ts}

firstTry :: ServerInfo -> TryServer 
firstTry sI = TryServer 1 sI

secondTry :: ServerInfo -> TryServer 
secondTry sI = TryServer 2 sI

thirdTry :: ServerInfo -> TryServer 
thirdTry sI = TryServer 3 sI

fourthTry :: ServerInfo -> TryServer 
fourthTry sI = TryServer 4 sI