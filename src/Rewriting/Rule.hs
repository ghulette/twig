module Rewriting.Rule (Rule (..),apply) where

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
match (Const x1 ts1) (Const x2 ts2) | x1 == x2 = matchList ts1 ts2
match t (Var x) = Just [(x,t)]
match _ _ = Nothing

matchList :: [Term] -> [Term] -> Maybe (Env Term)
matchList [] [] = Just []
matchList ts1 ts2 | length ts1 /= length ts2 = Nothing
matchList ts1 ts2 = do
  bindings <- mapM (\(t1,t2) -> match t1 t2) (zip ts1 ts2)
  let env = concat bindings
  return env

subst :: (Env Term) -> Term -> Maybe Term
subst e (Var x) = lookup x e
subst e (Const x ts) = do
  ts' <- mapM (subst e) ts
  return $ Const x ts'
