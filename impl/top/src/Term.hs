module Term
( Term (..)
, tupleConstructor
, children
, isLeaf
, isTuple
, pathM
, allM
, oneM
, someM
, congruenceM
) where

import Data.List (intercalate)
import qualified Util
import Control.Monad (guard)

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

children :: Term -> [Term]
children (Term _ ts) = ts

isLeaf :: Term -> Bool
isLeaf (Term _ []) = True
isLeaf (Term _ _) = False

isTuple :: Term -> Bool
isTuple (Term f _) = f == tupleConstructor

updateM :: Monad m => ([Term] -> Maybe (m [Term])) -> Term -> Maybe (m Term)
updateM f (Term t ts) = do
  mts <- f ts
  return (mts >>= \ts' -> return $ Term t ts')

pathM :: Monad m => Int -> (Term -> Maybe (m Term)) -> Term -> Maybe (m Term)
pathM i f = updateM (Util.pathM i f)

allM :: Monad m => (Term -> Maybe (m Term)) -> Term -> Maybe (m Term)
allM f = updateM (Util.mapAllM f)

oneM :: Monad m => (Term -> Maybe (m Term)) -> Term -> Maybe (m Term)
oneM f = updateM (Util.mapOneM f)

someM :: Monad m => (Term -> Maybe (m Term)) -> Term -> Maybe (m Term)
someM f = updateM (Util.mapSomeM f)

congruenceM :: Monad m => [Term -> Maybe (m Term)] -> Term -> Maybe (m Term)
congruenceM fs (Term t ts) = do
  guard $ length ts == length fs
  mts <- sequence (zipWith ($) fs ts)
  return (sequence mts >>= \ts' -> return $ Term t ts')