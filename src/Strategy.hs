module Strategy where

type Strategy m a = a -> m (Maybe a)

success :: Monad m => Strategy m a
success = return . Just

failure :: Monad m => Strategy m a
failure _ = return Nothing

-- These need "try" or something like that
-- test :: Strategy a -> Strategy a
-- test s x = undefined
--   
-- neg :: Strategy a -> Strategy a
-- neg s x = undefined

-- seqn :: Strategy a -> Strategy a -> Strategy a
-- seqn s1 s2 = runKleisli $ (Kleisli s1) >>> (Kleisli s2)

-- MonadPlus is not well defined
-- choice :: Strategy a -> Strategy a -> Strategy a
-- choice s1 s2 = runKleisli $ (Kleisli s1) <+> (Kleisli s2)

seqn :: Monad m => Strategy m a -> Strategy m a -> Strategy m a
seqn s1 s2 t = do
  mbt' <- s1 t
  case mbt' of
    Just t' -> s2 t'
    Nothing -> return Nothing
