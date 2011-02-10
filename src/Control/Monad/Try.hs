{-# LANGUAGE MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             GeneralizedNewtypeDeriving,
             UndecidableInstances #-}

module Control.Monad.Try where

import Control.Monad.Identity
import Control.Monad.Writer
import Control.Monad.State

class Monad m => MonadTry m where
  try :: m a -> m a

instance (MonadTry m,Monoid w) => MonadTry (WriterT w m) where
  try f = WriterT $ do
    (x,_) <- runWriterT f
    return (x,mempty)

instance Monoid w => MonadTry (Writer w) where
  try f = do
    let (x,_) = runWriter f
    return x

instance MonadTry m => MonadTry (StateT s m) where
  try f = StateT $ \s -> do
    (x,_) <- runStateT f s
    return (x,s)

instance MonadTry Identity where
  try f = f
