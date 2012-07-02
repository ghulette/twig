module Twig.Term
( Term (..)
, tupleConstructor
, children
, tuple
, withChildren
, isLeaf
, isTuple
, size
, flatSize
, flatten
) where

import Data.List (intercalate)

-- Terms

data Term = Term String [Term] deriving Eq

instance Show Term where
  show (Term f []) = f
  show (Term f ts) | f == tupleConstructor = 
    "(" ++ (intercalate "," (map show ts)) ++ ")"
  show (Term f ts) = 
    f ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

tupleConstructor :: String
tupleConstructor = "Tuple"

tuple :: [Term] -> Term
tuple = Term tupleConstructor

constructor :: Term -> String
constructor (Term t _) = t

children :: Term -> [Term]
children (Term _ ts) = ts

withChildren :: Term -> [Term] -> Term
withChildren (Term t _) ts = Term t ts

isLeaf :: Term -> Bool
isLeaf (Term _ []) = True
isLeaf (Term _ _) = False

isTuple :: Term -> Bool
isTuple t = constructor t == tupleConstructor

size :: Term -> Int
size t | isTuple t = length (children t)
       | otherwise = 1

-- flatten and flatSize both remove tuple constructors at the top level
-- E.g., (x,y) -> [x,y], size 2
-- ((x,y),z) -> [x,y,z], size 2
-- (foo(x),(y,z)) -> [foo(x),y,z], size 3

flatSize :: Term -> Int
flatSize t | isTuple t = sum (map size (children t))
           | otherwise = 1

flatten :: Term -> [Term]
flatten t | isTuple t = concatMap flatten (children t)
          | otherwise = [t]
