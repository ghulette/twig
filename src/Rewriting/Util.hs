module Rewriting.Util where

withDefault :: Maybe a -> a -> a
withDefault (Just x) _ = x
withDefault Nothing y = y

update :: (a -> Maybe a) -> a -> a
update f x = f x `withDefault` x

path :: Int -> (a -> Maybe a) -> [a] -> Maybe [a]
path i _ _ | i < 1 = Nothing
path _ _ [] = Nothing
path 1 f (x:xs) = do
  x' <- f x
  return (x':xs)
path i f (x:xs) = do
  xs' <- path (i-1) f xs
  return (x:xs')

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
