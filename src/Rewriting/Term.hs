module Rewriting.Term where

import Control.Monad
import Data.List (intercalate)

-- Terms

data Term = Var String
          | Const String [Term]
          deriving Eq

instance Show Term where
  show (Var x) = "\'" ++ x
  show (Const k []) = k
  show (Const k ts) = k ++ "(" ++ (intercalate "," tss) ++ ")"
    where tss = map show ts

children :: Term -> [Term]
children (Var _) = undefined
children (Const _ ts) = ts

-- Rules

data Rule = Rule Term Term deriving Eq

instance Show Rule where
  show (Rule t1 t2) = (show t1) ++ " -> " ++ (show t2)

-- Environment

type Env = [(String,Term)]

empty :: Env
empty = []

extend :: String -> Term -> Env -> Env
extend x t e = (x,t) : e

fetch :: String -> Env -> Maybe Term
fetch = lookup

-- Rule application

rep :: (Term -> Maybe Term) -> Term -> Maybe Term
rep s t = case s t of Just t' -> rep s t'
                      Nothing -> Just t

applyTopDown :: Rule -> Term -> Maybe Term
applyTopDown r t = apply r t `mplus` branchOne (applyTopDown r) t

branchAll :: (Term -> Maybe Term) -> Term -> Maybe Term
branchAll _ (Var _) = undefined
branchAll _ (Const x []) = Just (Const x []) -- always succeeds for const
branchAll s (Const x ts) = 
  case mapM s ts of Just ts' -> Just (Const x ts')
                    Nothing -> Nothing

branchOne :: (Term -> Maybe Term) -> Term -> Maybe Term
branchOne _ (Var _) = undefined
branchOne _ (Const _ []) = Nothing -- always fail for const
branchOne s (Const x ts) = 
  case one s ts of Just ts' -> Just (Const x ts')
                   Nothing -> Nothing

one :: (a -> Maybe a) -> [a] -> Maybe [a]
one _ [] = Nothing
one f (x:xs) = 
  case f x of Just x' -> Just (x':xs)
              Nothing -> x `plus` (one f xs)
  where h `plus` (Just t) = Just (h:t)
        _ `plus` Nothing   = Nothing

-- Match against outermost term and rewrite
apply :: Rule -> Term -> Maybe Term
apply (Rule p q) x = do
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

-- Rule combinators

success :: Term -> Maybe Term
success x = Just x

failure :: Term -> Maybe Term
failure _ = Nothing

test :: (Term -> Maybe Term) -> Term -> Maybe Term
test s x = case s x of Just _ -> Just x
                       Nothing -> Nothing

neg :: (Term -> Maybe Term) -> Term -> Maybe Term
neg s x = case s x of Just _ -> Nothing
                      Nothing -> Just x

seqn :: (Term -> Maybe Term) -> (Term -> Maybe Term) -> Term -> Maybe Term
seqn = (>=>)

choice :: (Term -> Maybe Term) -> (Term -> Maybe Term) -> Term -> Maybe Term
choice s1 s2 x = s1 x `mplus` s2 x

