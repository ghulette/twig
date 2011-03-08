module Rule (Rule (..),TermPattern (..),apply,match,build) where

import Control.Monad
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Term
import Env (Env)
import qualified Env as Env

-- Terms

data TermPattern = Var String
                 | Const String [TermPattern]
                 deriving Eq

instance Show TermPattern where
  show (Var x) = "\'" ++ x
  show (Const k []) = k
  show (Const k ts) = k ++ "(" ++ (intercalate "," (map show ts)) ++ ")"


-- Rule datatype

data Rule = Rule TermPattern TermPattern deriving Eq

instance Show Rule where
  show (Rule t1 t2) = (show t1) ++ " -> " ++ (show t2)

-- Match against outermost term and rewrite
apply :: Rule -> Term -> Env String Term Term
apply (Rule p q) x = do
  p `match` x
  build q

match :: TermPattern -> Term -> Env String Term Term
match (Const y tps) (Term x ts) = do 
  guard (x == y)
  ts' <- mapM (\(t1,t2) -> match t1 t2) (zip tps ts)
  return (Term x ts')
match (Var x) t = do
  Env.bind x t
  return t

build :: TermPattern -> Env String Term Term
build (Var x) = do
  t <- Env.lookup x
  return t
build (Const x tps) = do
  ts <- mapM build tps
  return $ Term x ts
