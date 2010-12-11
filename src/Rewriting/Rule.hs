module Rewriting.Rule (Rule (..),apply) where

import Control.Monad
import Rewriting.Term

-- Rule datatype

data Rule = Rule Term Term deriving Eq

instance Show Rule where
  show (Rule t1 t2) = (show t1) ++ " -> " ++ (show t2)

-- Match against outermost term and rewrite
apply :: Rule -> Term -> Maybe Term
apply (Rule p q) x = do
  e <- x `match` p
  subst e q

type Env a = [(String,a)]

-- LHS should not contain variables, maybe enforce this in the types?
match :: Term -> Term -> Maybe (Env Term)
match (Const x1 ts1) (Const x2 ts2) = do 
  guard (x1 == x2)
  env <- matchList ts1 ts2
  return env
match t (Var x) = return [(x,t)]
match _ _ = Nothing

appendNoDups :: (Eq a, Eq b) => [(a,b)] -> [(a,b)] -> Maybe [(a,b)]
appendNoDups xs [] = Just xs
appendNoDups xs ((y,t):ys) = 
  case lookup y xs of
    Just t' -> do
      guard (t == t')
      xs' <- appendNoDups xs ys
      return ((y,t):xs')
    Nothing -> do
      xs' <- appendNoDups xs ys
      return ((y,t):xs')

concatNoDups :: (Eq a, Eq b) => [[(a,b)]] -> Maybe [(a,b)]
concatNoDups = foldM appendNoDups []

matchList :: [Term] -> [Term] -> Maybe (Env Term)
matchList [] [] = Just []
matchList ts1 ts2 = do
  guard (length ts1 == length ts2)
  bindings <- mapM (\(t1,t2) -> match t1 t2) (zip ts1 ts2)
  noDups <- concatNoDups bindings
  return noDups

subst :: (Env Term) -> Term -> Maybe Term
subst e (Var x) = lookup x e
subst e (Const x ts) = do
  ts' <- mapM (subst e) ts
  return $ Const x ts'
