module Util where

import Data.Monoid
import Data.List (intercalate)

jn :: [String] -> String
jn = intercalate "\n"

-- | Path functions are indexed starting at 1
path :: Monoid m => Int -> (a -> Maybe (a,m)) -> [a] -> Maybe ([a],m)
path i _ _ | i < 1 = Nothing
path _ _ [] = Nothing
path 1 f (x:xs) = do
  (x',m) <- f x
  return (x':xs,m)
path i f (x:xs) = do
  (xs',m) <- path (i-1) f xs
  return (x:xs',m)

mapAll :: Monoid m => (a -> Maybe (a,m)) -> [a] -> Maybe ([a],m)
mapAll _ [] = Just ([],mempty)
mapAll f (x:xs) = case f x of
  Just (x',m1) -> case mapAll f xs of
    Just (xs',m2) -> Just (x':xs',m1 `mappend` m2)
    Nothing -> Nothing
  Nothing -> Nothing

mapOne :: Monoid m => (a -> Maybe (a,m)) -> [a] -> Maybe ([a],m)
mapOne _ [] = Nothing
mapOne f (x:xs) = case f x of
  Just (x',m) -> Just (x':xs,m)
  Nothing -> case mapOne f xs of
    Just (xs',m) -> Just (x:xs',m)
    Nothing -> Nothing

mapSome :: Monoid m => (a -> Maybe (a,m)) -> [a] -> Maybe ([a],m)
mapSome _ [] = Nothing
mapSome f (x:xs) = case f x of
  Just (x',m1) -> case mapSome f xs of
    Just (xs',m2) -> Just (x':xs',m1 `mappend` m2)
    Nothing -> Just (x':xs,m1)
  Nothing -> case mapSome f xs of
    Just (xs',m2) -> Just (x:xs',m2)
    Nothing -> Nothing
