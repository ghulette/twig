module Strategy where

import Data.Monoid

type Strategy a m = a -> Maybe (a,m)

success :: Monoid m => Strategy a m
success x = Just (x,mempty)

failure :: Monoid m => Strategy a m
failure _ = Nothing

test :: Monoid m => Strategy a m -> Strategy a m
test s x = 
  case s x of 
    Just _ -> Just (x,mempty)
    Nothing -> Nothing

neg :: Monoid m => Strategy a m -> Strategy a m
neg s x = 
  case s x of
    Just _ -> Nothing
    Nothing -> Just (x,mempty)

seqn :: Monoid m => Strategy a m -> Strategy a m -> Strategy a m
seqn s1 s2 x =
  case s1 x of 
    Nothing -> Nothing
    Just (x',g1) -> 
      case s2 x' of
        Nothing -> Nothing
        Just (x'',g2) -> Just (x'',g1 `mappend` g2)

    

choice :: Monoid m => Strategy a m -> Strategy a m -> Strategy a m
choice s1 s2 x =
  case s1 x of 
    Nothing -> s2 x
    Just (x',g) -> Just (x',g)
