module Rewriting.Term
(Term (..)
,children
,withChildren
,isLeaf
,isConst
,Rule (..)
,apply
,success
,failure
,test
,neg
,seqn
,choice
,branchAll
,branchOne
,branchSome
,congruence
,path
,root
)where

import Control.Monad
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

-- Terms

data Term = Var String
          | Const String [Term]
          deriving Eq

instance Show Term where
  show (Var x) = "\'" ++ x
  show (Const k []) = k
  show (Const k ts) = k ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

children :: Term -> [Term]
children (Var _) = undefined
children (Const _ ts) = ts

withChildren :: Term -> [Term] -> Term
withChildren (Var _) _ = undefined
withChildren (Const x _) ts = Const x ts

isLeaf :: Term -> Bool
isLeaf (Var _) = undefined
isLeaf (Const _ []) = True
isLeaf (Const _ _) = False

isConst :: Term -> Bool
isConst (Var _) = False
isConst (Const _ []) = True
isConst (Const _ ts) = any isConst ts

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


-- Utility functions

repeatMaybe :: (a -> Maybe a) -> a -> Maybe a
repeatMaybe f x = case f x of Just x' -> repeatMaybe f x'
                              Nothing -> Just x

catMaybeList :: a -> Maybe [a] -> Maybe [a]
catMaybeList _ Nothing = Nothing
catMaybeList x (Just xs) = Just (x:xs)

-- This is very ineffcient, rewrite
changeOne :: (a -> Maybe a) -> [a] -> Maybe [a]
changeOne _ [] = Nothing
changeOne f (x:xs) = 
  case f x of Just x' -> Just (x':xs)
              Nothing -> x `catMaybeList` (changeOne f xs)

-- This is very ineffcient, rewrite
changeSome :: (a -> Maybe a) -> [a] -> Maybe [a]
changeSome f xs = 
  let
    def = map fromMaybe xs
    xs' = map f xs
  in
    if length xs' > 0 
      then Just (zipWith ($) def xs') 
      else Nothing


-- Rule application

applyTopDown :: Rule -> Term -> Maybe Term
applyTopDown r = choice (apply r) (branchOne (applyTopDown r))

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
test s x = 
  case s x of Just _ -> Just x
              Nothing -> Nothing

neg :: (Term -> Maybe Term) -> Term -> Maybe Term
neg s x = 
  case s x of Just _ -> Nothing
              Nothing -> Just x

seqn :: (Term -> Maybe Term) -> (Term -> Maybe Term) -> Term -> Maybe Term
seqn = (>=>)

choice :: (Term -> Maybe Term) -> (Term -> Maybe Term) -> Term -> Maybe Term
choice s1 s2 x = s1 x `mplus` s2 x

-- always succeeds for leaf
branchAll :: (Term -> Maybe Term) -> Term -> Maybe Term
branchAll _ (Var _) = undefined
branchAll s (Const x ts) = 
  case mapM s ts of 
    Just ts' -> Just (Const x ts')
    Nothing -> Nothing

-- always fails for leaf
branchOne :: (Term -> Maybe Term) -> Term -> Maybe Term
branchOne _ (Var _) = undefined
branchOne _ (Const _ []) = Nothing
branchOne s (Const x ts) = 
  case changeOne s ts of 
    Just ts' -> Just (Const x ts')
    Nothing -> Nothing

-- always fails for leaf
branchSome :: (Term -> Maybe Term) -> Term -> Maybe Term
branchSome _ (Var _) = undefined
branchSome _ (Const _ []) = Nothing
branchSome s (Const x ts) = 
  case changeSome s ts of 
    Just ts' -> Just (Const x ts')
    Nothing -> Nothing

congruence :: [Term -> Maybe Term] -> Term -> Maybe Term
congruence _ (Var _) = undefined
congruence rs (Const _ ts) | length rs /= length ts = Nothing
congruence rs (Const x ts) = 
  case sequence (zipWith ($) rs ts) of
    Just ts' -> Just (Const x ts')
    Nothing -> Nothing

-- Path of 0 returns original term.  Path of 1 returns the first child, path
-- of 2 returns the second, and so on.  Path fails if the index exceeds the
-- number of children.
path :: Int -> Term -> Maybe Term
path _ (Var _) = undefined
path 0 t = Just t
path i (Const _ ts) | i > 0 && i <= length ts = Just (ts !! (i-1))
path _ (Const _ _) = Nothing

root :: String -> Term -> Maybe Term
root _ (Var _) = undefined
root x (Const y ts) | x == y = Just (Const y ts)
root _ _ = Nothing
