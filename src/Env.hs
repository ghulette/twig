{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type Id = String
type Env a = Map Id a

fromList :: [(Id,a)] -> Env a
fromList = Map.fromList

empty :: Env a
empty = Map.empty

bind :: Id -> a -> Env a -> Env a
bind = Map.insert

-- If k is already bound, make sure we are rebinding it to the same value
uniqueBind :: Eq a => Id -> a -> Env a -> Maybe (Env a)
uniqueBind k x m =
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

bindM :: Id -> v -> EnvState v ()
bindM k x = EnvState $ do
  m <- get
  put (bind k x m)

uniqueBindM :: Eq v => Id -> v -> EnvState v ()
uniqueBindM k x = EnvState $ do
  m <- get
  m' <- lift (uniqueBind k x m)
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
