module Rewriting.Error where

instance Monad (Either a) where
  return = Right
  (Left x) >>= _ = Left x
  (Right x) >>= f = f x

data Error = RuntimeError String deriving Eq

instance Show Error where
  show (RuntimeError msg) = "Runtime error: " ++ msg

evalError :: String -> Either Error a
evalError msg = Left (RuntimeError msg)
