module Twig.Pattern 
( Pattern (..)
, match
, build
) where

import Control.Monad (guard)
import Data.List (intercalate)
import Twig.Term
import Twig.Env (Env)
import qualified Twig.Env as Env


data Pattern = Var String
             | Const String [Pattern]
             deriving Eq

instance Show Pattern where
  show (Var x)      = "\'" ++ x
  show (Const k []) = k
  show (Const k ts) = k ++ "(" ++ (intercalate "," (map show ts)) ++ ")"

match :: Pattern -> Term -> Maybe (Env Term)
match (Const p ps) (Term t ts) = do
  guard (p == t)
  guard (length ps == length ts)
  es <- mapM (uncurry match) (zip ps ts)
  e <- Env.unionsUnique es
  return e
match (Var x) t = do
  return (Env.singleton x t)

build :: Env Term -> Pattern -> Maybe Term
build env (Var x) = Env.lookup x env
build env (Const x pats) = do
  ts <- mapM (build env) pats
  return $ Term x ts
