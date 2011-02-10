module Strategy where

import Data.Monoid

type Strategy m a = a -> Maybe (a,m)

success :: Monoid m => Strategy m a
success x = Just (x,mempty)

failure :: Monoid m => Strategy m a
failure _ = Nothing

test :: Monoid m => Strategy m a -> Strategy m a
test s x = 
  case s x of 
    Just _ -> Just (x,mempty)
    Nothing -> Nothing

neg :: Monoid m => Strategy m a -> Strategy m a
neg s x = 
  case s x of
    Just _ -> Nothing
    Nothing -> Just (x,mempty)

seqn :: Monoid m => Strategy m a -> Strategy m a -> Strategy m a
seqn s1 s2 x =
  case s1 x of 
    Nothing -> Nothing
    Just (x',g1) -> 
      case s2 x' of
        Nothing -> Nothing
        Just (x'',g2) -> Just (x'',g1 `mappend` g2)

    

choice :: Monoid m => Strategy m a -> Strategy m a -> Strategy m a
choice s1 s2 x =
  case s1 x of 
    Nothing -> s2 x
    Just (x',g) -> Just (x',g)
