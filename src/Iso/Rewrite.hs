module Iso.Rewrite where

import Data.Monoid
import Iso.MaybeWriter

type Rewrite m a = a -> MaybeWriter m a

seqn :: Monoid m => Rewrite m a -> Rewrite m a -> Rewrite m a
seqn r1 r2 x = do
  y <- r1 x
  z <- r2 y
  return z

choice :: Rewrite m a -> Rewrite m a -> Rewrite m a
choice = undefined