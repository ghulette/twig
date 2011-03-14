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
  { root :: String
  , ident :: Maybe String 
  , children :: [Term] 
  } deriving Eq

instance Show Term where
  show (Term f _ []) = f
  show (Term f _ ts) | f == tupleConstructor = 
    "{" ++ (intercalate "," (map show ts)) ++ "}"
  show (Term f _ ts) = 
    f ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

tupleConstructor :: String
tupleConstructor = "Tuple"

withChildren :: Term -> [Term] -> Term
withChildren (Term f x _) ts = Term f x ts

isLeaf :: Term -> Bool
isLeaf (Term _ _ []) = True
isLeaf (Term _ _ _) = False

isTuple :: Term -> Bool
isTuple (Term f _ _) = f == tupleConstructor
