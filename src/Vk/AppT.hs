{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Vk.AppT where

import Control.Monad.State (StateT, MonadState(get,put), gets)
import Types (MapUserN)
import Vk.Api.Response (ServerInfo (..))

type AppT m = StateT AppState m

instance (Monad m) => MonadState MapUserN (StateT AppState m) where
  get = gets mapUserN
  put map = modify (\AppState server _ -> AppState server map)

data AppState = AppState
  { tryServer :: TryServer,
    mapUserN :: MapUserN
  }  

data TryServer = TryServer {tryNum :: Int, servInf :: ServerInfo}
  deriving (Eq, Show)

instance Ord TryServer where
  compare a b = compare (tryNum a) (tryNum b)

nextTry :: TryServer -> TryServer
nextTry (TryServer num sI) = TryServer (succ num) sI

resetTry :: TryServer -> TryServer
resetTry (TryServer _ sI) = TryServer 1 sI

changeServInfo :: ServerInfo -> TryServer -> TryServer
changeServInfo sI (TryServer num _) = TryServer num sI

changeTs :: Integer -> TryServer -> TryServer
changeTs ts (TryServer num sI) = TryServer num sI {tsSI = ts}

firstTry :: ServerInfo -> TryServer
firstTry = TryServer 1

secondTry :: ServerInfo -> TryServer
secondTry = TryServer 2

thirdTry :: ServerInfo -> TryServer
thirdTry = TryServer 3

fourthTry :: ServerInfo -> TryServer
fourthTry = TryServer 4
