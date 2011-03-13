module Term
( Term (..)
, withChildren
, isLeaf
, isTuple
, tupleConstructor
) where

import Data.List (intercalate)

-- Terms

data Term = Term
  { constr :: String
  , tag :: String
  , children :: [Term]
  } deriving Eq

instance Show Term where
  show (Term c x []) = x ++ " : " ++ c
  show (Term c _ ts) | c == tupleConstructor = 
    "{" ++ (intercalate "," (map show ts)) ++ "}"
  show (Term c x ts) = 
    x ++ ":" ++ c ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

tupleConstructor :: String
tupleConstructor = "Tuple"

withChildren :: Term -> [Term] -> Term
withChildren (Term c x _) ts = Term c x ts

isLeaf :: Term -> Bool
isLeaf (Term _ _ []) = True
isLeaf (Term _ _ _) = False

isTuple :: Term -> Bool
isTuple (Term c _ _) = c == tupleConstructor
