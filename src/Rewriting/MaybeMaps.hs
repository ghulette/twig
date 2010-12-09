module Rewriting.MaybeMaps where

mapAll :: (a -> Maybe a) -> [a] -> Maybe [a]
mapAll _ [] = Just []
mapAll f (x:xs) = case f x of
  Just x' -> case mapAll f xs of
    Just xs' -> Just (x':xs')
    Nothing -> Nothing
  Nothing -> Nothing

mapOne :: (a -> Maybe a) -> [a] -> Maybe [a]
mapOne _ [] = Nothing
mapOne f (x:xs) = case f x of
  Just x' -> Just (x':xs)
  Nothing -> case mapOne f xs of
    Just xs' -> Just (x:xs')
    Nothing -> Nothing

mapSome :: (a -> Maybe a) -> [a] -> Maybe [a]
mapSome _ [] = Nothing
mapSome f (x:xs) = case f x of
  Just x' -> case mapSome f xs of
    Just xs' -> Just (x':xs')
    Nothing -> Just (x':xs)
  Nothing -> case mapSome f xs of
    Just xs' -> Just (x:xs')
    Nothing -> Nothing
