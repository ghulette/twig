module Strategy where

import Control.Monad.Try

type Strategy m a = a -> m (Maybe a)

success :: Monad m => Strategy m a
success = return . Just

failure :: Monad m => Strategy m a
failure _ = return Nothing

test :: MonadTry m => Strategy m a -> Strategy m a
test s x = do
  mbx' <- try (s x)
  case mbx' of Just _ -> return (Just x)
               Nothing -> return Nothing
  
neg :: MonadTry m => Strategy m a -> Strategy m a
neg s x = do
  mbx' <- try (s x)
  case mbx' of Just _ -> return Nothing
               Nothing -> return (Just x)

seqn :: Monad m => Strategy m a -> Strategy m a -> Strategy m a
seqn s1 s2 x = do
  mbx' <- s1 x
  case mbx' of Just x' -> s2 x'
               Nothing -> return Nothing

choice :: MonadTry m => Strategy m a -> Strategy m a -> Strategy m a
choice s1 s2 x = do
  mbx' <- try (s1 x)
  case mbx' of Just _  -> s1 x
               Nothing -> s2 x
