{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             GeneralizedNewtypeDeriving #-}

module Control.Monad.Supply where

import Control.Monad.State

newtype SupplyT s m a = SupplyT {supplyState :: StateT [s] m a}
  deriving (Monad,Functor,MonadTrans,MonadIO)

-- instance Monad m => Monad (SupplyT s m) where
--   return x = SupplyT (return x)
--   (SupplyT mx) >>= f = SupplyT $ do
--     x <- mx
--     supplyState (f x)
--
-- instance MonadTrans (SupplyT s) where
--   lift = SupplyT . lift
-- 
-- instance (MonadIO m) => MonadIO (SupplyT s m) where
--   liftIO = lift . liftIO

class Monad m => MonadSupply s m | m -> s where
  supply :: m s

instance Monad m => MonadSupply s (SupplyT s m) where
  supply = SupplyT $ do
    (x:xs) <- get -- This will fail if the supply is empty!
    put xs
    return x

evalSupplyT :: Monad m => SupplyT s m a -> [s] -> m a
evalSupplyT m sup = evalStateT (supplyState m) sup

runSupplyT :: Monad m => SupplyT s m a -> [s] -> m (a,[s])
runSupplyT m sup = runStateT (supplyState m) sup
