{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Supply where

import Control.Monad.State

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

supplies :: Int -> Supply s [s]
supplies n = Supply $ do
  xs <- get
  let xs' = take n xs
  put (drop n xs)
  return xs'
