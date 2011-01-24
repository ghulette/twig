module Iso.Writer where

import Data.Monoid
import Control.Monad

data Writer m a = Writer m a

instance Functor (Writer m) where
  fmap f (Writer m x) = Writer m (f x)

instance Monoid m => Monad (Writer m) where
  return = pure
  (Writer m1 x) >>= f = Writer (m1 `mappend` m2) x'
    where Writer m2 x' = f x


newtype MaybeWriter m a = MaybeWriter (Maybe (Writer m a))

instance Functor (MaybeWriter m) where
  fmap f (MaybeWriter x) = MaybeWriter $ fmap (fmap f) x

instance Monoid m => Monad (MaybeWriter m) where
  return x = MaybeWriter $ return (return x)
  (MaybeWriter mbx) >>= f = MaybeWriter $ do
    Writer m1 x <- mbx
    let MaybeWriter mbx' = f x
    Writer m2 x' <- mbx'
    return $ Writer (m1 `mappend` m2) x'
