{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type EnvState k v = Map k v

newtype Env k v a = Env (StateT (EnvState k v) Maybe a) 
  deriving (Functor,Monad,MonadPlus)

bind :: (Ord k,Eq v) => k -> v -> Env k v ()
bind k x = Env $ do
  m <- get
  -- If k is already bound, make sure we are rebinding it to the same value
  m' <- case Map.insertLookupWithKey (\_ a _ -> a) k x m of
    (Just x',m') -> do
      guard (x' == x)
      return m'
    (Nothing,m') -> return m'
  put m'

unbind :: Ord k => k -> Env k v ()
unbind k = Env $ do
  m <- get
  let m' = Map.delete k m
  put m'

lookup :: Ord k => k -> Env k v v
lookup k = Env $ do
  m <- get
  v <- lift $ Map.lookup k m
  return v

runEnv :: Env k v a -> EnvState k v -> Maybe (a,EnvState k v)
runEnv (Env m) st = runStateT m st

evalEnv :: Env k v a -> Maybe a
evalEnv m = liftM fst $ runEnv m Map.empty
