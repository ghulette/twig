{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, DisambiguateRecordFields #-}

module Twig.RuleExpr 
( RuleExpr (..)
, Proc (..)
, Id
, Strategy
, Trace
, EvalException (..)
, run
) where

import Control.Exception
import Data.Typeable (Typeable)
import Data.List (foldl')
import Control.Monad (when,guard)
import Twig.Pattern (Pattern,match,build)
import Twig.Env (Env)
import qualified Twig.Env as Env
import Twig.Term
import Twig.Block
import Twig.Util.List (MaybeMap)
import qualified Twig.Util.List as L


-- Runtime exceptions

data EvalException = RuntimeException String deriving (Typeable,Show)

instance Exception EvalException

runtimeErr :: String -> a
runtimeErr msg = throw (RuntimeException msg)


-- Expression types

type Id = String
data Proc = Proc [Id] RuleExpr deriving Show
type Strategy a = Term -> Maybe (a,Term)
type Trace a = Term -> Term -> String -> Maybe a

data EvalState a = EvalState
  { defs  :: Env Proc
  , env   :: Env (Strategy a)
  , trace :: Trace a
  }


-- Environment

bindVars :: Env (Strategy a) -> [(Id,Strategy a)] -> Env (Strategy a)
bindVars = foldl' $ flip $ uncurry Env.bind  

-- Expressions

data RuleExpr = Call Id [RuleExpr]
              | Rule Pattern Pattern String
              | Success
              | Failure
              | Test RuleExpr
              | Neg RuleExpr
              | Fix Id RuleExpr
              | VarExpr Id
              | Seq RuleExpr RuleExpr
              | LeftChoice RuleExpr RuleExpr
              | Path Int RuleExpr
              | BranchOne RuleExpr
              | BranchAll RuleExpr
              | BranchSome RuleExpr
              | Permute Int [Int]
              | Fan [RuleExpr]
              | Congruence [RuleExpr]
              deriving (Show)

eval :: Block a => RuleExpr -> EvalState a -> Strategy a
eval (Rule lhs rhs txt) st t = do
  bindings <- match lhs t
  t' <- build bindings rhs
  blk <- (trace st) t t' txt
  return (blk,t')
eval Success _ t = 
  Just (identity (flatSize t),t)
eval Failure _ _ = 
  Nothing
eval (Test e) st t = 
  case eval e st t of
    Just _ -> Just (identity (flatSize t),t)
    Nothing -> Nothing
eval (Neg e) st t = 
  case eval e st t of
    Just _ -> Nothing
    Nothing -> Just (identity (flatSize t),t)
eval (Seq e1 e2) st t =
  case eval e1 st t of
    Nothing -> Nothing
    Just (m1,t') -> 
      case eval e2 st t' of
        Just (m2,t'') -> Just (m1 `seqn` m2,t'')
        Nothing -> Nothing
eval (LeftChoice e1 e2) st t =
  case eval e1 st t of
    Just (t',m) -> Just (t',m)
    Nothing -> 
      case eval e2 st t of
        Just (t',m) -> Just (t',m)
        Nothing -> Nothing
eval (Path 0 e) st t = 
  eval e st t -- #0(s) just applies s to root
eval (Path i e) st t = 
  branch (L.path i) (eval e st) t
eval (BranchOne e) st t = 
  branch L.mapOne (eval e st) t
eval (BranchAll e) st t =
  branch L.mapAll (eval e st) t
eval (BranchSome e) st t =
  branch L.mapSome (eval e st) t
eval (Permute 1 ns) _ t = do
  ts <- L.permute [t] (map pred ns) -- L.permute is 0-based
  let t' = tuple ts
  let tw = flatSize t
  let plcs = [1..tw]
  let plcGrps = [plcs]
  plcGrps' <- L.permute plcGrps (map pred ns)
  let plcs' = concat plcGrps'
  return (permute tw plcs',t')
eval (Permute w ns) _ t = do
  guard (isTuple t)
  let ts = children t
  guard (length ts == w)
  let grps = map flatSize ts
  let grpw = sum grps
  let plcs = [1..grpw]
  let plcGrps = L.group plcs grps
  plcGrps' <- L.permute plcGrps (map pred ns)
  let plcs' = concat plcGrps'
  ts' <- L.permute ts (map pred ns) -- L.permute is 0-based
  let t' = t `withChildren` ts'
  return (permute grpw plcs',t')
eval (Fan es) st t = do
  let fan = take (length es) (repeat 1)
  eval (Permute 1 fan `Seq` Congruence es) st t
eval (Congruence es) st t = 
  congruence (map (\e -> eval e st) es) t
eval (Fix x e) st t = f t
  where f = eval e st'
        st' = st {env = Env.bind x f (env st)}
eval (VarExpr x) st t =
  case Env.lookup x (env st) of
    Nothing -> eval (Call x []) st t
    Just s -> s t
eval (Call x args) st t =
  case Env.lookup x (defs st) of
    Nothing -> runtimeErr $ "Variable " ++ x ++ " not in scope"
    Just (Proc params e) -> do
      when (length args < length params) $ runtimeErr "Not enough args"
      when (length args > length params) $ runtimeErr "Too many args"
      let ss = map (\arg -> eval arg st) args
      let st' = st {env = bindVars (env st) (zip params ss)}
      eval e st' t

branch :: Block b => MaybeMap (b,Term) -> Strategy b -> Strategy b
branch mapf s t = do
  guard (isTuple t)
  let ts = children t
  let ms = map (identity . flatSize) ts
  let mts = zip ms ts
  mts' <- mapf (s . snd) mts
  let (ms',ts') = unzip mts'
  return (foldl1 par ms',t `withChildren` ts')

congruence :: Block b => [Strategy b] -> Strategy b
congruence ss t = do
  guard (isTuple t)
  let ts = children t
  guard (length ss == length ts)
  mts' <- sequence (zipWith ($) ss ts)
  let (ms',ts') = unzip mts'
  return (foldl1 par ms',t `withChildren` ts')

run :: Block a => Id -> Env Proc -> Trace a -> Strategy a
run entry defs trace t = 
  case Env.lookup entry defs of 
    Just (Proc [] e) -> do
      (t',ms) <- eval e (EvalState defs Env.empty trace) t
      return (t',ms)
    Just (Proc _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
