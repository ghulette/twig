module Rewriting.Util where

zappM :: Monad m => [a -> m b] -> [a] -> m [b]
zappM [] _ = return []
zappM _ [] = return []
zappM (f:fs) (x:xs) = do
  x' <- f x
  xs' <- zappM fs xs
  return (x':xs')

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

mapAllM :: Monad m => (a -> m (Maybe a)) -> [a] -> m (Maybe [a])
mapAllM _ [] = return (Just [])
mapAllM f (x:xs) = do
  mx' <- f x
  case mx' of
    Nothing -> return Nothing
    Just x' -> do 
      mxs' <- mapAllM f xs
      case mxs' of
        Just xs' -> return (Just (x':xs'))
        Nothing  -> return Nothing

mapOneM :: Monad m => (a -> m (Maybe a)) -> [a] -> m (Maybe [a])
mapOneM _ [] = return Nothing
mapOneM f (x:xs) = do
  mx' <- f x
  case mx' of
    Just x' -> return (Just (x':xs))
    Nothing -> do
      mxs' <- mapOneM f xs
      case mxs' of
        Just xs' -> return (Just (x:xs'))
        Nothing  -> return Nothing

mapSomeM :: Monad m => (a -> m (Maybe a)) -> [a] -> m (Maybe [a])
mapSomeM _ [] = return Nothing
mapSomeM f (x:xs) = do
  mx' <- f x
  mxs' <- mapSomeM f xs
  case (mx',mxs') of
    (Just x',Just xs') -> return (Just (x':xs'))
    (Just x',Nothing)  -> return (Just (x':xs))
    (Nothing,Just xs') -> return (Just (x:xs'))
    (Nothing,Nothing)  -> return Nothing
