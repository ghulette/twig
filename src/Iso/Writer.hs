module Iso.Writer where

import Data.Monoid
import Control.Monad

data Writer m a = Writer m a

instance Functor (Writer m) where
  fmap f (Writer m x) = Writer m (f x)

instance Monoid m => Monad (Writer m) where
  return = Writer mempty
  (Writer m1 x) >>= f = Writer (m1 `mappend` m2) x'
    where Writer m2 x' = f x
