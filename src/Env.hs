{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type EnvState k v = Map k v

newtype Env k v a = Env (State (EnvState k v) a) deriving (Functor,Monad)

bind :: Ord k => k -> v -> Env k v ()
bind k v = Env $ do
  m <- get
  put (Map.insert k v m)

unbind :: Ord k => k -> Env k v ()
unbind k = Env $ do
  m <- get
  put (Map.delete k m)

lookup :: Ord k => k -> Env k v (Maybe v)
lookup k = Env $ do
  m <- get
  return $ Map.lookup k m

evalEnv :: Env k v a -> a
evalEnv (Env m) = evalState m Map.empty

runEnv :: Env k v a -> EnvState k v -> (a,EnvState k v)
runEnv (Env m) st = runState m st

