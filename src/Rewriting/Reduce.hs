module Rewriting.Reduce where

import Rewriting.Term

type Env = [(String,Term)]

empty :: Env
empty = []

extend :: String -> Term -> Env -> Env
extend x t e = (x,t) : e

rewrite :: Rule -> Term -> Maybe Term
rewrite (Rule p q) x = do
  e <- match p x empty
  subst e q

-- LHS should not contain variables, maybe enforce this in the types?
match :: Term -> Term -> Env -> Maybe Env
match t (Var x) e = Just $ extend x t e
match (Const x1 ts1) (Const x2 ts2) e | x1 == x2 = matchList ts1 ts2 e
match _ _ _ = Nothing

matchList :: [Term] -> [Term] -> Env -> Maybe Env
matchList [] [] env = Just env
matchList ts1 ts2 _ | length ts1 /= length ts2 = Nothing
matchList ts1 ts2 env = do
  envs' <- mapM (\(t1,t2) -> match t1 t2 env) (zip ts1 ts2)
  return $ concat envs'

subst :: Env -> Term -> Maybe Term
subst e (Var x) = lookup x e
subst e (Const x ts) = do
  ts' <- mapM (subst e) ts
  return $ Const x ts'
