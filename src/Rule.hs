module Rule (Rule (..),TermPattern (..),apply,match,build) where

import Control.Monad
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Term

-- Terms

data TermPattern = Var String
                 | Const String [TermPattern]
                 deriving Eq

instance Show TermPattern where
  show (Var x) = "\'" ++ x
  show (Const k []) = k
  show (Const k ts) = k ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

-- Variable bindings

type Env = Map String Term

emptyEnv :: Env
emptyEnv = Map.empty

singletonEnv :: String -> Term -> Env
singletonEnv = Map.singleton

lookupEnv :: String -> Env -> Maybe Term
lookupEnv = Map.lookup

insertEnv :: String -> Term -> Env -> Maybe Env
insertEnv k x t = 
  case Map.insertLookupWithKey (\_ a _ -> a) k x t of
    (Just x',t') -> do
      guard (x' == x)
      return t'
    (Nothing,t') -> return t'

unionEnv :: Env -> Env -> Maybe Env
unionEnv t1 t2 = foldM ins t1 (Map.toList t2)
  where ins = \t (k,x) -> insertEnv k x t

unionsEnv :: [Env] -> Maybe Env
unionsEnv = foldM unionEnv Map.empty


-- Rule datatype

data Rule = Rule TermPattern TermPattern deriving Eq

instance Show Rule where
  show (Rule t1 t2) = (show t1) ++ " -> " ++ (show t2)

-- Match against outermost term and rewrite
apply :: Rule -> Term -> Maybe Term
apply (Rule p q) x = do
  e <- p `match` x
  build e q

match :: TermPattern -> Term -> Maybe Env
match (Const x1 ts1) (Term x2 ts2) = do 
  guard (x1 == x2)
  matchList ts1 ts2
match (Var x) t = do
  return (singletonEnv x t)

matchList :: [TermPattern] -> [Term] -> Maybe Env
matchList [] [] = Just emptyEnv
matchList ts1 ts2 = do
  guard (length ts1 == length ts2)
  bindings <- mapM (\(t1,t2) -> match t1 t2) (zip ts1 ts2)
  env <- unionsEnv bindings
  return env

build :: Env -> TermPattern -> Maybe Term
build e (Var x) = lookupEnv x e
build e (Const x ts) = do
  ts' <- mapM (build e) ts
  return $ Term x ts'
