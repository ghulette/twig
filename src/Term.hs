module Term
( Term (..)
, children
, withChildren
, isLeaf
, tupleConstructor
) where

import Data.List (intercalate)

-- Terms

data Term = Term String [Term] deriving Eq

instance Show Term where
  show (Term f []) = f
  show (Term f ts) | f == tupleConstructor = 
    "{" ++ (intercalate "," (map show ts)) ++ "}"
  show (Term f ts) = 
    f ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

tupleConstructor :: String
tupleConstructor = "Tuple"

children :: Term -> [Term]
children (Term _ ts) = ts

withChildren :: Term -> [Term] -> Term
withChildren (Term x _) ts = Term x ts

isLeaf :: Term -> Bool
isLeaf (Term _ []) = True
isLeaf (Term _ _) = False

isTuple :: Term -> Bool
isTuple (Term f _) = f == tupleConstructor
