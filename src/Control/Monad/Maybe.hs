{-# LANGUAGE FlexibleInstances,MultiParamTypeClasses,UndecidableInstances #-}

module Control.Monad.Maybe
( MaybeT
, runMaybeT
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer

newtype MaybeT m a = MaybeT {runMaybeT :: m (Maybe a)}

instance Functor m => Functor (MaybeT m) where
  fmap f = MaybeT . fmap (fmap f) . runMaybeT

instance Monad m => Monad (MaybeT m) where
  return = MaybeT . return . Just
  x >>= f = MaybeT $ do 
    mx <- runMaybeT x
    case mx of
      Just x  -> runMaybeT $ f x
      Nothing -> return Nothing

instance Monad m => MonadPlus (MaybeT m) where
  mzero = MaybeT $ return Nothing
  x `mplus` y = MaybeT $ do
    mx <- runMaybeT x
    case mx of
      Just _  -> return mx
      Nothing -> runMaybeT y

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just
  
instance (MonadIO m) => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

instance (MonadState s m) => MonadState s (MaybeT m) where
  get = lift get
  put = lift . put

instance (MonadReader r m) => MonadReader r (MaybeT m) where
  ask = lift ask
  local f = MaybeT . local f . runMaybeT

instance (MonadWriter w m) => MonadWriter w (MaybeT m) where
  tell = lift . tell
  listen m = MaybeT $ do
    (mx,w) <- listen (runMaybeT m)
    case mx of
      Just x -> return (Just (x,w))
      Nothing -> return Nothing
  pass m = MaybeT $ do
    mvf <- runMaybeT m
    case mvf of
      Just (v,f) -> pass (return (Just v,f))
      Nothing -> return Nothing
