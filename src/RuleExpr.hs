{-# LANGUAGE DeriveDataTypeable #-}

module RuleExpr where

import Control.Exception
import Data.Typeable (Typeable)
import Data.List (foldl')
import Control.Monad (guard,when)
import Data.Monoid
import Pattern (Pattern,match,build)
import Env (Env)
import qualified Env
import Term
import Util
import StringSub
import Supply


type Id = String

-- Runtime exceptions

data EvalException = RuntimeException String deriving (Typeable,Show)

instance Exception EvalException

runtimeErr :: String -> a
runtimeErr msg = throw (RuntimeException msg)


-- Values

type Trace = Supply Id [String]
data Proc = Proc [Id] RuleExpr

type AbstractStrategy w a = a -> Maybe (a,w)
type Strategy = AbstractStrategy Trace Term


-- Environment

bindVars :: Env Strategy -> [(Id,Strategy)] -> Env Strategy
bindVars = foldl' $ flip $ uncurry Env.bind

makeRuleEnv :: [(Id,Proc)] -> Env Proc
makeRuleEnv = Env.fromList

-- Expressions

data RuleExpr = Call Id [RuleExpr]
              | Rule Pattern Pattern Trace
              | Success
              | Failure
              | Test RuleExpr
              | Neg RuleExpr
              | Fix Id RuleExpr
              | VarExpr Id
              | Seq RuleExpr RuleExpr
              | LeftChoice RuleExpr RuleExpr
              | Choice RuleExpr RuleExpr
              | Path Integer RuleExpr
              | BranchOne RuleExpr
              | BranchAll RuleExpr
              | BranchSome RuleExpr
              | Congruence [RuleExpr]

eval :: RuleExpr -> Env Proc -> Env Strategy -> Strategy
eval (Rule lhs rhs m) _ _ t = do
  bindings <- match lhs t
  t' <- build bindings rhs
  let m' = fmap (fmap (stringSub bindings)) $ m
  return (t',m')
eval Success _ _ t = Just (t,mempty)
eval Failure _ _ _ = Nothing
eval (Test e) defs env t = 
  case eval e defs env t of
    Just _ -> Just (t,mempty)
    Nothing -> Nothing
eval (Neg e) defs env t = 
  case eval e defs env t of
    Just _ -> Nothing
    Nothing -> Just (t,mempty)
eval (Seq e1 e2) defs env t =
  case eval e1 defs env t of
    Nothing -> Nothing
    Just (t',m1) -> 
      case eval e2 defs env t' of
        Just (t'',m2) -> Just (t'',m1 `mappend` m2)
        Nothing -> Nothing
eval (LeftChoice e1 e2) defs env t =
  case eval e1 defs env t of
    Just (t',m) -> Just (t',m)
    Nothing -> 
      case eval e2 defs env t of
        Just (t',m) -> Just (t',m)
        Nothing -> Nothing
eval (Choice e1 e2) defs env t =
  -- Probably we don't want non-det choice for Twig, but it is interesting
  -- as a general rewriting expression.
  case (eval e1 defs env t,eval e2 defs env t) of
    (Just (x,m1),Just (x',_)) -> 
      if x == x' 
        then Just (x,m1) -- Note: arbitrarily choose first trace!
        else runtimeErr "Non-confluence"
    (Just (t',m),Nothing) -> Just (t',m)
    (Nothing,Just (t',m)) -> Just (t',m)
    (Nothing,Nothing) -> Nothing
eval (Path 0 e) defs env t = 
  -- #0(s) just applies s to root
  eval e defs env t 
eval (Path i e) defs env t = do
  let ts = children t
  let s = eval e defs env
  (ts',m) <- path (fromInteger i) s ts
  return (t `withChildren` ts',m)
eval (BranchOne e) defs env t = do
  let ts = children t
  (ts',m) <- mapOne (eval e defs env) ts
  return (t `withChildren` ts',m)
eval (BranchAll e) defs env t = do
  let ts = children t
  (ts',m) <- mapAll (eval e defs env) ts
  return (t `withChildren` ts',m)
eval (BranchSome e) defs env t = do
  let ts = children t
  (ts',m) <- mapSome (eval e defs env) ts
  return (t `withChildren` ts',m)
eval (Congruence es) defs env t = do
  let ts = children t
  let rs = map (\e -> eval e defs env) es
  guard $ length ts == length rs
  mts' <- sequence (zipWith ($) rs ts)
  let (ts',ms) = unzip mts'
  return (t `withChildren` ts',mconcat ms)
eval (Fix x e) defs env t = s t
  where s = eval e defs env'
        env' = Env.bind x s env
eval (VarExpr x) defs env t =
  case Env.lookup x env of
    Nothing -> eval (Call x []) defs env t
    Just s -> s t
eval (Call x args) defs env t =
  case Env.lookup x defs of
    Nothing -> runtimeErr $ "Variable " ++ x ++ " not in scope"
    Just (Proc params e) -> do
      when (length args < length params) $ runtimeErr "Not enough args"
      when (length args > length params) $ runtimeErr "Too many args"
      let ss = map (\arg -> eval arg defs env) args
      let env' = bindVars env (zip params ss)
      eval e defs env' t

run :: Id -> Env Proc -> Strategy
run entry defs t = 
  case Env.lookup entry defs of 
    Just (Proc [] e) -> do
      (t',ms) <- eval e defs Env.empty t
      return (t',ms)
    Just (Proc _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
