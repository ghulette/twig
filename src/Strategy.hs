module Strategy where

import Control.Monad

type Strategy m a b = a -> Maybe (a,b -> m b)

success :: Monad m => Strategy m a b
success x = Just (x,return)

failure :: Monad m => Strategy m a b
failure _ = Nothing

test :: Monad m => Strategy m a b -> Strategy m a b
test s x = 
  case s x of Just _ -> Just (x,return)
              Nothing -> Nothing

neg :: Monad m => Strategy m a b -> Strategy m a b
neg s x = 
  case s x of Just _ -> Nothing
              Nothing -> Just (x,return)

seqn :: Monad m => Strategy m a b -> Strategy m a b -> Strategy m a b
seqn s1 s2 x =
  case s1 x of 
    Just (x',g1) -> 
      case s2 x' of
        Just (x'',g2) -> Just (x'',g1 >=> g2)
        Nothing -> Nothing
    Nothing -> Nothing

choice :: Monad m => Strategy m a b -> Strategy m a b -> Strategy m a b
choice s1 s2 x =
  case s1 x of 
    Just (x',g) -> Just (x',g)
    Nothing -> s2 x
