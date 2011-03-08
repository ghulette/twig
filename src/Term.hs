module Term
( Term (..)
, children
, withChildren
, isLeaf
) where

import Data.List (intercalate)

-- Terms

data Term = Term String [Term] deriving Eq

instance Show Term where
  show (Term f []) = f
  show (Term f ts) = f ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

children :: Term -> [Term]
children (Term _ ts) = ts

withChildren :: Term -> [Term] -> Term
withChildren (Term x _) ts = Term x ts

isLeaf :: Term -> Bool
isLeaf (Term _ []) = True
isLeaf (Term _ _) = False
