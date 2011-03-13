module Rule (Rule (..),TermPattern (..),apply,match,build) where

import Control.Monad
import Data.List (intercalate)
import Term
import Env (Env,EnvState)
import qualified Env as Env


-- Term patterns

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
apply :: Rule -> Term -> Maybe (Term,Env Term)
apply (Rule p q) x = Env.runEnvState $ do
  _ <- p `match` x
  build q

match :: TermPattern -> Term -> EnvState Term Term
match (Const p ps) (Term c x ts) = do
  guard (p == c)
  guard (length ps == length ts)
  ts' <- mapM (uncurry match) (zip ps ts)
  return (Term c x ts')
match (Var x) t = do
  Env.uniqueBindM x t
  return t

build :: TermPattern -> EnvState Term Term
build (Var x) = do
  t <- Env.lookupM x
  return t
build (Const k tps) = do
  ts <- mapM build tps
  return $ Term k "ok" ts
