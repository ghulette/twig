{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE NamedFieldPuns, RecordWildCards, DisambiguateRecordFields #-}

module Twig.RuleExpr 
( RuleExpr (..)
, Proc (..)
, Id
, Strategy
, EvalException (..)
, BlockBuilder
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
import Twig.Util.List


-- Runtime exceptions

data EvalException = RuntimeException String deriving (Typeable,Show)

instance Exception EvalException

runtimeErr :: String -> a
runtimeErr msg = throw (RuntimeException msg)


-- Expression types

type Id = String
data Proc = Proc [Id] RuleExpr
type Strategy a = Term -> Maybe (a,Term)

type BlockBuilder a = Int -> Int -> String -> a

data EvalState a = EvalState 
  { defs :: Env Proc
  , env :: Env (Strategy a)
  , mkBlock :: BlockBuilder a
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
              | Path Integer RuleExpr
              | BranchOne RuleExpr
              | BranchAll RuleExpr
              | BranchSome RuleExpr
              | Congruence [RuleExpr]

eval :: Block a => RuleExpr -> EvalState a -> Strategy a
eval (Rule lhs rhs trc) st t = do
  bindings <- match lhs t
  t' <- build bindings rhs
  let blk = (mkBlock st) (size t) (size t') trc
  return (blk,t')
eval Success _ t = 
  Just (identity (size t),t)
eval Failure _ _ = 
  Nothing
eval (Test e) st t = 
  case eval e st t of
    Just _ -> Just (identity (size t),t)
    Nothing -> Nothing
eval (Neg e) st t = 
  case eval e st t of
    Just _ -> Nothing
    Nothing -> Just (identity (size t),t)
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
  branch (path (fromInteger i)) (eval e st) t
eval (BranchOne e) st t = 
  branch mapOne (eval e st) t
eval (BranchAll e) st t =
  branch mapAll (eval e st) t
eval (BranchSome e) st t =
  branch mapSome (eval e st) t
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
  let mts = zip (repeat (identity 1)) ts
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

-- reduce :: Eq a => (a -> a) -> a -> a
-- reduce f x = case f x of x' | x' == x -> x'
--                          x' -> reduce f x'
-- 
-- normalize :: RuleExpr -> RuleExpr
-- normalize (Seq a (LeftChoice b c)) = LeftChoice (Seq a b) (Seq a c)
-- normalize (Seq (LeftChoice a b) c) = LeftChoice (Seq a c) (Seq b c)
-- normalize (Test e) = Test (normalize e)
-- normalize (Neg e) = Neg (normalize e)
-- normalize (Fix x e) = Fix x (normalize e)
-- normalize (Path i e) = Path i (normalize e)
-- normalize (BranchOne e) = BranchOne (normalize e)
-- normalize (BranchAll e) = BranchAll (normalize e)
-- normalize (BranchSome e) = BranchSome (normalize e)
-- normalize (Congruence es) = Congruence (map normalize es)
-- normalize x = x

run :: Block a => Id -> Env Proc -> BlockBuilder a -> Strategy a
run entry defs mkblk t = 
  case Env.lookup entry defs of 
    Just (Proc [] e) -> do
      (t',ms) <- eval e (EvalState defs Env.empty mkblk) t
      return (t',ms)
    Just (Proc _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
