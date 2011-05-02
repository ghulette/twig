{-# OPTIONS_GHC -fno-warn-orphans #-}

module MonadWriter where

import Data.Monoid
import Control.Monad

instance Monoid m => Monad ((,)m) where
  return x = (mempty,x)
  (m1,x) >>= f = let (m2,x') = f x in (m1 `mappend` m2,x')
