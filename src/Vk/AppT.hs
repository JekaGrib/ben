{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Vk.AppT where

import Control.Monad.State (MonadState, StateT, gets, modify)
import Data.Bifunctor (first,second)
import Vk.Api.Response (ServerInfo (..))
import Vk.Types (MapUserN)

type AppT = StateT (TryServer, MapUserN)

class (MonadState (s1, s2) m) => MonadStateTwo s1 s2 m | m -> s1 s2 where
  get1 :: m s1
  get2 :: m s2
  put1 :: s1 -> m ()
  put2 :: s2 -> m ()
  modify1 :: (s1 -> s1) -> m ()
  modify2 :: (s2 -> s2) -> m ()

instance (Monad m) => MonadStateTwo TryServer MapUserN (StateT (TryServer, MapUserN) m) where
  get1 = gets fst
  get2 = gets snd
  put1 s1 = modify (\(_, s2) -> (s1, s2))
  put2 s2 = modify (\(s1, _) -> (s1, s2))
  modify1 f = modify (first f)
  modify2 f = modify (second f)

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
