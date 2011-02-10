{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Monad.WSE (WSE,run,fetch,bind,reset,supply,tell) where

import Data.Monoid
import Control.Monad.Trans
import Control.Monad.Env (EnvT,evalEnvT)
import qualified Control.Monad.Env as Env
import Control.Monad.Identity
import Control.Monad.Supply (SupplyT,runSupplyT)
import qualified Control.Monad.Supply as Supply
import Control.Monad.Writer (WriterT,runWriterT)
import qualified Control.Monad.Writer as Writer

newtype WSE w s k a = WSE (WriterT w (SupplyT s (EnvT k s Identity)) a)
  deriving (Functor,Monad)

run :: (Monoid w,Ord k) => [s] -> WSE w s k a -> (a,w,[s])
run ss (WSE m) = (x,w,ss')
  where m1 = runWriterT m
        m2 = runSupplyT m1 ss
        m3 = evalEnvT m2
        ((x,w),ss') = runIdentity m3

fetch :: (Monoid w,Ord k) => k -> WSE w s k (Maybe s)
fetch x = WSE $ lift $ lift $ Env.fetch x

bind :: (Monoid w,Ord k) => k -> s -> WSE w s k ()
bind x v = WSE $ lift $ lift $ Env.bind x v

reset :: (Monoid w,Ord k) => WSE w s k ()
reset = WSE $ lift $ lift $ Env.reset

supply :: (Monoid w,Ord k) => WSE w s k s
supply = WSE $ lift $ Supply.supply

tell :: (Monoid w,Ord k) => w -> WSE w s k ()
tell w = WSE $ Writer.tell w
