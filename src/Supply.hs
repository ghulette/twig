{-# LANGUAGE GeneralizedNewtypeDeriving,FlexibleInstances #-}

module Supply where

import Control.Monad.State
import Data.Monoid

newtype Supply s a = Supply (State [s] a)
  deriving (Functor,Monad)

evalSupply :: [s] -> Supply s a -> a
evalSupply ss m = fst (runSupply ss m)

runSupply :: [s] -> Supply s a -> (a,[s])
runSupply ss (Supply m) = runState m ss

supply :: Supply s s
supply = Supply $ do
  (x:xs) <- get
  put xs
  return x

-- Actually any pair of a monad and a monoid gives rise to a new monoid, i.e.
--   instance (Monad m,Monoid a) => Monoid (m a)
-- but we can't write it like that because it conflicts with existing
-- instances provided by Data.Monoid.
instance (Monoid a) => Monoid (Supply s a) where
  mempty = return mempty
  m1 `mappend` m2 = do
    x1 <- m1
    x2 <- m2
    return (x1 `mappend` x2)
