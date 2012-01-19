module Twig.Term
( Term (..)
, tupleConstructor
, children
, tuple
, withChildren
, size
, isLeaf
, isTuple
, tuplify
, untuplify
, toList
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

size :: Term -> Int
size t | isTuple t = length (children t)
       | otherwise = 1

isLeaf :: Term -> Bool
isLeaf (Term _ []) = True
isLeaf (Term _ _) = False

isTuple :: Term -> Bool
isTuple t = constructor t == tupleConstructor

tuplify :: Term -> Term
tuplify t = if isTuple t then t else Term tupleConstructor [t]

untuplify :: Term -> Term
untuplify t = case t of
  (Term x [t']) | x == tupleConstructor -> untuplify t'
  _ -> t 

toList :: Term -> [Term]
toList t | isTuple t = children t
toList t | otherwise = [t]
