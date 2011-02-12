module Strategy where

import Control.Monad
import Data.Monoid
import Term
import Util (mapOne,mapSome,mapAll)
import qualified Util as Util

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

path :: Monoid m => Int -> Strategy Term m -> Strategy Term m
path 0 s t = s t
path i s t = do
  let ts = children t
  (ts',m) <- Util.path i s ts
  return (t `withChildren` ts',m)

branchAll :: Monoid m => Strategy Term m -> Strategy Term m
branchAll s t = do
  let ts = children t
  (ts',m) <- mapAll s ts
  return (t `withChildren` ts',m)

branchOne :: Monoid m => Strategy Term m -> Strategy Term m
branchOne s t = do
  let ts = children t
  (ts',m) <- mapOne s ts
  return (t `withChildren` ts',m)

branchSome :: Monoid m => Strategy Term m -> Strategy Term m
branchSome s t = do
  let ts = children t
  (ts',m) <- mapSome s ts
  return (t `withChildren` ts',m)

congruence :: Monoid m => [Strategy Term m] -> Strategy Term m
congruence ss t = do
  let ts = children t
  guard (length ts == length ss)
  mts' <- sequence (zipWith ($) ss ts)
  let (ts',ms) = unzip mts'
  return (t `withChildren` ts',mconcat ms)
