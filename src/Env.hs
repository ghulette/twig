{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type Id = String
type Env a = Map Id a

empty :: Env a
empty = Map.empty

bind :: Eq a => Id -> a -> Env a -> Maybe (Env a)
bind k x m =
  -- If k is already bound, make sure we are rebinding it to the same value
  case lookup k m of
    Just x' -> do
      guard (x' == x)
      return m
    Nothing -> do
      return $ Map.insert k x m

unbind :: Id -> Env a -> Env a
unbind = Map.delete

lookup :: Id -> Env a -> Maybe a
lookup = Map.lookup


-- Monad for Env operations

newtype EnvState v a = EnvState (StateT (Env v) Maybe a)
  deriving (Functor,Monad,MonadPlus)

bindM :: Eq v => Id -> v -> EnvState v ()
bindM k x = EnvState $ do
  m <- get
  m' <- lift (bind k x m)
  put m'

unbindM :: Id -> EnvState v ()
unbindM k = EnvState $ do
  m <- get
  let m' = unbind k m
  put m'

lookupM :: Id -> EnvState v v
lookupM k = EnvState $ do
  m <- get
  lift (lookup k m)

runEnvState :: EnvState v a -> Maybe (a,Env v)
runEnvState (EnvState m) = runStateT m empty

evalEnvState :: EnvState v a -> Maybe a
evalEnvState m = liftM fst $ runEnvState m
