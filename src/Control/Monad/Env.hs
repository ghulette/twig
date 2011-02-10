{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             GeneralizedNewtypeDeriving #-}

module Control.Monad.Env where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

type Env k e = Map k e

newtype EnvT k e m a = EnvT {envState :: StateT (Map k e) m a}
  deriving (Monad,Functor,MonadTrans,MonadIO)

class Monad m => MonadEnv k e m | m -> k, m -> e where
  load :: k -> m (Maybe e)
  store :: k -> e -> m ()
  reset :: m ()

instance (Monad m,Ord k) => MonadEnv k e (EnvT k e m) where
  load x = EnvT $ do
    s <- get
    let r = Map.lookup x s
    return r
  store x v = EnvT $ do
    s <- get
    let s' = Map.insert x v s
    put s'
  reset = EnvT $ do
    put Map.empty

evalEnvT :: (Monad m,Ord k) => EnvT k e m a -> m a
evalEnvT m = evalStateT (envState m) Map.empty
