module Util where

import Data.Monoid

withDefault :: Maybe a -> a -> a
withDefault (Just x) _ = x
withDefault Nothing y = y

update :: (a -> Maybe a) -> a -> a
update f x = f x `withDefault` x

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

pathM :: Monad m => Int -> (a -> Maybe (m a)) -> [a] -> Maybe (m [a])
pathM i _ _ | i < 1 = Nothing
pathM _ _ [] = Nothing
pathM 1 f (x:xs) = 
  case f x of
    Just mx' -> Just $ do 
      x' <- mx'
      return (x':xs)
    Nothing -> Nothing
pathM i f (x:xs) = 
  case pathM (i-1) f xs of
    Just mxs' -> Just $ do 
      xs' <- mxs'
      return (x:xs')
    Nothing -> Nothing

mapAllM :: Monad m => (a -> Maybe (m a)) -> [a] -> Maybe (m [a])
mapAllM _ [] = Just (return [])
mapAllM f (x:xs) = 
  case f x of
    Just mx' -> 
      case mapAllM f xs of
        Just mxs' -> Just $ do 
          x' <- mx'
          xs' <- mxs'
          return (x':xs')
        Nothing -> Nothing
    Nothing -> Nothing

mapOneM :: Monad m => (a -> Maybe (m a)) -> [a] -> Maybe (m [a])
mapOneM _ [] = Nothing
mapOneM f (x:xs) = 
  case f x of
    Just mx' -> Just $ do
      x' <- mx'
      return (x':xs)
    Nothing -> 
      case mapOneM f xs of
        Just mxs' -> Just $ do
          xs' <- mxs'
          return (x:xs')
        Nothing -> Nothing

mapSomeM :: Monad m => (a -> Maybe (m a)) -> [a] -> Maybe (m [a])
mapSomeM _ [] = Nothing
mapSomeM f (x:xs) = 
  case f x of
    Just mx' -> 
      case mapSomeM f xs of
        Just mxs' -> Just $ do
          x' <- mx'
          xs' <- mxs'
          return (x':xs')
        Nothing -> Just $ do
          x' <- mx'
          return (x':xs)
    Nothing -> 
      case mapSomeM f xs of
        Just mxs' -> Just $ do
          xs' <- mxs'
          return (x:xs')
        Nothing -> Nothing
