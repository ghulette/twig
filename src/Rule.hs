module Rule (Rule (..),TermBindings,apply,match,build) where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Term

-- Environment for variable bindings

type TermBindings a = Map String a

emptyEnv :: TermBindings a
emptyEnv = Map.empty

singletonEnv :: String -> a -> TermBindings a
singletonEnv = Map.singleton

lookupEnv :: String -> TermBindings a -> Maybe a
lookupEnv = Map.lookup

insertEnv :: Eq a => String -> a -> TermBindings a -> Maybe (TermBindings a)
insertEnv k x t = 
  case insertLookup k x t of
    (Just x',t') -> do
      guard (x' == x)
      return t'
    (Nothing,t') -> return t'
  where
    insertLookup = Map.insertLookupWithKey (\_ a _ -> a)

unionEnv :: Eq a => TermBindings a -> TermBindings a -> Maybe (TermBindings a)
unionEnv t1 t2 = foldM ins t1 (Map.toList t2)
  where ins = \t (k,x) -> insertEnv k x t

unionsEnv :: Eq a => [TermBindings a] -> Maybe (TermBindings a)
unionsEnv = foldM unionEnv Map.empty


-- Rule datatype

data Rule = Rule Term Term deriving Eq

instance Show Rule where
  show (Rule t1 t2) = (show t1) ++ " -> " ++ (show t2)

-- Match against outermost term and rewrite
apply :: Rule -> Term -> Maybe Term
apply (Rule p q) x = do
  e <- x `match` p
  build e q

-- LHS should not contain variables, maybe enforce this in the types?
match :: Term -> Term -> Maybe (TermBindings Term)
match (Const x1 ts1) (Const x2 ts2) = do 
  guard (x1 == x2)
  matchList ts1 ts2
match t (Var x) = return (singletonEnv x t)
match _ _ = Nothing

matchList :: [Term] -> [Term] -> Maybe (TermBindings Term)
matchList [] [] = Just emptyEnv
matchList ts1 ts2 = do
  guard (length ts1 == length ts2)
  bindings <- mapM (\(t1,t2) -> match t1 t2) (zip ts1 ts2)
  env <- unionsEnv bindings
  return env

build :: (TermBindings Term) -> Term -> Maybe Term
build e (Var x) = lookupEnv x e
build e (Const x ts) = do
  ts' <- mapM (build e) ts
  return $ Const x ts'
