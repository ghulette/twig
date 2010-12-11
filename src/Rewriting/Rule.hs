module Rewriting.Rule (Rule (..),apply) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Rewriting.Term

-- Environment type for variable bindings

type Env a = Map String a

emptyEnv :: Env a
emptyEnv = Map.empty

singletonEnv :: String -> a -> Env a
singletonEnv = Map.singleton

lookupEnv :: String -> Env a -> Maybe a
lookupEnv = Map.lookup

insertEnv :: Eq a => String -> a -> Env a -> Maybe (Env a)
insertEnv k x t = 
  case insertLookup k x t of
    (Just x',t') -> do
      guard (x' == x)
      return t'
    (Nothing,t') -> return t'
  where
    insertLookup = Map.insertLookupWithKey (\_ a _ -> a)

unionEnv :: Eq a => Env a -> Env a -> Maybe (Env a)
unionEnv t1 t2 = foldM ins t1 (Map.toList t2)
  where ins = \t (k,x) -> insertEnv k x t

unionsEnv :: Eq a => [Env a] -> Maybe (Env a)
unionsEnv = foldM unionEnv Map.empty


-- Rule datatype

data Rule = Rule Term Term deriving Eq

instance Show Rule where
  show (Rule t1 t2) = (show t1) ++ " -> " ++ (show t2)

-- Match against outermost term and rewrite
apply :: Rule -> Term -> Maybe Term
apply (Rule p q) x = do
  e <- x `match` p
  subst e q

-- LHS should not contain variables, maybe enforce this in the types?
match :: Term -> Term -> Maybe (Env Term)
match (Const x1 ts1) (Const x2 ts2) = do 
  guard (x1 == x2)
  matchList ts1 ts2
match t (Var x) = return (singletonEnv x t)
match _ _ = Nothing

matchList :: [Term] -> [Term] -> Maybe (Env Term)
matchList [] [] = Just emptyEnv
matchList ts1 ts2 = do
  guard (length ts1 == length ts2)
  bindings <- mapM (\(t1,t2) -> match t1 t2) (zip ts1 ts2)
  env <- unionsEnv bindings
  return env

subst :: (Env Term) -> Term -> Maybe Term
subst e (Var x) = lookupEnv x e
subst e (Const x ts) = do
  ts' <- mapM (subst e) ts
  return $ Const x ts'
