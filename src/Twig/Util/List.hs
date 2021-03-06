module Twig.Util.List 
( MaybeMap
, MaybeMapM
, permute
, group
, path
, mapAll
, mapOne
, mapSome
, pathM
, mapAllM
, mapOneM
, mapSomeM
) where

import Control.Monad (guard)

type MaybeMap a = (a -> Maybe a) -> [a] -> Maybe [a]
type MaybeMapM m a = (a -> Maybe (m a)) -> [a] -> Maybe (m [a])

permute :: [a] -> [Int] -> Maybe [a]
permute _ [] = Just []
permute es (n:ns) = do
  guard (n < length es)
  let e = es !! n
  es' <- permute es ns
  return (e:es')

group :: [a] -> [Int] -> [[a]]
group [] []     = []
group _ []      = undefined -- Too few groups
group [] _      = undefined -- Too few elements
group xs (n:ns) = take n xs : group (drop n xs) ns

-- Apply a function to a given element of the list.
path :: Int -> MaybeMap a
path i _ _ | i < 1 = Nothing
path _ _ [] = Nothing
path 1 f (x:xs) = do
  x' <- f x
  return (x':xs)
path i f (x:xs) = do
  xs' <- path (i-1) f xs
  return (x:xs')

-- Apply a function to every element of a list - if any element fails, the whole
-- map fails.
mapAll :: MaybeMap a
mapAll _ [] = Just []
mapAll f (x:xs) = case f x of
  Just x' -> case mapAll f xs of
    Just xs' -> Just (x':xs')
    Nothing -> Nothing
  Nothing -> Nothing

-- Apply a function to exactly one element of a list - if no one element
-- succeeds, the whole map fails. If more than one element would succeed, this
-- function chooses the first one in the list.
mapOne :: MaybeMap a
mapOne _ [] = Nothing
mapOne f (x:xs) = case f x of
  Just x' -> Just (x':xs)
  Nothing -> case mapOne f xs of
    Just xs' -> Just (x:xs')
    Nothing -> Nothing

-- Apply a function to one or more elements of a list - if no elements succeed,
-- the whole map fails.
mapSome :: MaybeMap a
mapSome _ [] = Nothing
mapSome f (x:xs) = case f x of
  Just x' -> case mapSome f xs of
    Just xs' -> Just (x':xs')
    Nothing -> Just (x':xs)
  Nothing -> case mapSome f xs of
    Just xs' -> Just (x:xs')
    Nothing -> Nothing

pathM :: Monad m => Int -> MaybeMapM m a
pathM i _ _ | i < 1 = Nothing
pathM _ _ [] = Nothing
pathM 1 f (x:xs) = case f x of
  Just mx' -> Just $ do 
    x' <- mx'
    return (x':xs)
  Nothing -> Nothing
pathM i f (x:xs) = case pathM (i-1) f xs of
  Just mxs' -> Just $ do 
    xs' <- mxs'
    return (x:xs')
  Nothing -> Nothing

mapAllM :: Monad m => MaybeMapM m a
mapAllM _ [] = Just (return [])
mapAllM f (x:xs) = case f x of
  Just mx' -> case mapAllM f xs of
    Just mxs' -> Just $ do 
      x' <- mx'
      xs' <- mxs'
      return (x':xs')
    Nothing -> Nothing
  Nothing -> Nothing

mapOneM :: Monad m => MaybeMapM m a
mapOneM _ [] = Nothing
mapOneM f (x:xs) = case f x of
  Just mx' -> Just $ do
    x' <- mx'
    return (x':xs)
  Nothing -> case mapOneM f xs of
    Just mxs' -> Just $ do
      xs' <- mxs'
      return (x:xs')
    Nothing -> Nothing

mapSomeM :: Monad m => MaybeMapM m a
mapSomeM _ [] = Nothing
mapSomeM f (x:xs) = case f x of
  Just mx' -> case mapSomeM f xs of
    Just mxs' -> Just $ do
      x' <- mx'
      xs' <- mxs'
      return (x':xs')
    Nothing -> Just $ do
      x' <- mx'
      return (x':xs)
  Nothing -> case mapSomeM f xs of
    Just mxs' -> Just $ do
      xs' <- mxs'
      return (x:xs')
    Nothing -> Nothing
