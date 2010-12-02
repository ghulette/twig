module Rewriting.Reduce where

import Rewriting.Term
import Rewriting.Environment

fixMaybe :: (a -> Maybe a) -> a -> a
fixMaybe f x = case f x of Just x' -> fixMaybe f x'
                           Nothing -> x

reduceOnce :: [Rule] -> Term -> Maybe Term
reduceOnce [] t = Nothing
reduceOnce (r:rs) t = 
  case rewrite r t of Just t' -> Just t'
                      Nothing -> reduceOnce rs t

reduce :: [Rule] -> Term -> Term
reduce rs t = fixMaybe (reduceOnce rs) t

rewrite :: Rule -> Term -> Maybe Term
rewrite (Rule p q) x = do
  e <- x `match` p
  subst e q

-- LHS should not contain variables, maybe enforce this in the types?
match :: Term -> Term -> Maybe Env
match (Const x1 ts1) (Const x2 ts2) | x1 == x2 = matchList ts1 ts2
match t (Var x) = Just [(x,t)]
match _ _ = Nothing

matchList :: [Term] -> [Term] -> Maybe Env
matchList [] [] = Just []
matchList ts1 ts2 | length ts1 /= length ts2 = Nothing
matchList ts1 ts2 = do
  bindings <- mapM (\(t1,t2) -> match t1 t2) (zip ts1 ts2)
  let env = concat bindings
  return env

subst :: Env -> Term -> Maybe Term
subst e (Var x) = fetch x e
subst e (Const x ts) = do
  ts' <- mapM (subst e) ts
  return $ Const x ts'
