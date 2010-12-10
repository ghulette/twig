module Rewriting.Term
( Term (..)
, children
, withChildren
, isLeaf
, isConst
, success
, failure
, test
, neg
, seqn
, choice
, branchAll
, branchOne
, branchSome
, congruence
, path
, root
) where

import Control.Monad
import Data.List (intercalate)
import Rewriting.Util

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


-- Combinators

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

type MaybeMap a = (a -> Maybe a) -> [a] -> Maybe [a]

branch :: MaybeMap Term -> (Term -> Maybe Term) -> Term -> Maybe Term
branch _ _ (Var _) = undefined
branch mapf s t = do
  let ts = children t
  ts' <- mapf s ts
  return (t `withChildren` ts')

branchAll :: (Term -> Maybe Term) -> Term -> Maybe Term
branchAll = branch mapAll

branchOne :: (Term -> Maybe Term) -> Term -> Maybe Term
branchOne = branch mapOne

branchSome :: (Term -> Maybe Term) -> Term -> Maybe Term
branchSome = branch mapSome

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
