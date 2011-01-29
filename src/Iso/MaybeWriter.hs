module Iso.MaybeWriter where

import Data.Monoid
import Control.Monad
import Control.Monad.Instances

-- Standard writer monad, does this exist somewhere in the standard 
instance Monoid m => Monad ((,) m) where
  return = (,) mempty
  (m1,x) >>= f = let (m2,x') = f x in (m1 `mappend` m2,x')


newtype MaybeWriter m a = MaybeWriter (Maybe (m,a))

instance (Show m, Show a) => Show (MaybeWriter m a) where
  show (MaybeWriter Nothing) = "Failed"
  show (MaybeWriter (Just (m,x))) = (show x) ++ "\n" ++  (show m)

instance Functor (MaybeWriter m) where
  fmap f (MaybeWriter x) = MaybeWriter (fmap (fmap f) x)

instance Monoid m => Monad (MaybeWriter m) where
  return x = MaybeWriter (return (return x))
  (MaybeWriter mbx) >>= f = MaybeWriter $ do
    (m1,x) <- mbx
    let MaybeWriter mbx' = f x
    (m2,x') <- mbx'
    return $ (m1,x) >> (m2,x')

write :: Monoid m => m -> MaybeWriter m ()
write m = MaybeWriter (Just (m,()))

failure :: MaybeWriter m a
failure = MaybeWriter Nothing
