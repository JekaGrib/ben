{-# LANGUAGE FlexibleInstances #-}

module Vk.AppT where

import qualified App
import Control.Monad.State (StateT, gets, modify)
import Types (MapUserN)
import Vk.Api.Response (ServerInfo (..))

type AppT m = StateT AppState m

data AppState = AppState
  { tryServer :: TryServer,
    mapUserN :: MapUserN
  }

instance (Monad m) => App.HasUserMap (StateT AppState m) where
  getUserMap = gets mapUserN
  setUserMap mapUN = modify (\appState -> appState {mapUserN = mapUN})
  modifyUserMap f = modify (\appState -> appState {mapUserN = f (mapUserN appState)})

modifyTryServer :: Monad m => (TryServer -> TryServer) -> AppT m ()
modifyTryServer f =
  modify $ \appState ->
    appState {tryServer = f (tryServer appState)}

getTryServer :: Monad m => AppT m TryServer
getTryServer = gets tryServer

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
