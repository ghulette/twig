module Strategy where

import Control.Monad
import Data.Monoid
import Data.Tree
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

-- This is wrong
dual :: Monoid m => Strategy a m -> Strategy a m
dual s x = 
  case s x of
    Just (y,_) -> s y
    Nothing -> Nothing

branch :: ([Tree a] -> Maybe ([Tree a],b)) -> Tree a -> Maybe (Tree a,b)
branch f (Node x xs) = do
  (xs',m) <- f xs
  return (Node x xs',m)

path :: Monoid m => Int -> Strategy (Tree a) m -> Strategy (Tree a) m
path 0 s = s
path i s = branch (Util.path i s)

branchAll :: Monoid m => Strategy (Tree a) m -> Strategy (Tree a) m
branchAll s = branch (mapAll s)

branchOne :: Monoid m => Strategy (Tree a) m -> Strategy (Tree a) m
branchOne s = branch (mapOne s)

branchSome :: Monoid m => Strategy (Tree a) m -> Strategy (Tree a) m
branchSome s = branch (mapSome s)

congruence :: Monoid m => [Strategy (Tree a) m] -> Strategy (Tree a) m
congruence ss (Node x xs) = do
  guard (length xs == length ss)
  mts' <- sequence (zipWith ($) ss xs)
  let (xs',ms) = unzip mts'
  return (Node x xs',mconcat ms)
