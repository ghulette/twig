module Rewriting.Term
( Term (..)
, children
, withChildren
, isLeaf
, isConst
) where

import Data.List (intercalate)

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
