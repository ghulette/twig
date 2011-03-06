{-# LANGUAGE DeriveDataTypeable #-}

module RuleExpr where

import Control.Exception
import Data.Typeable (Typeable)
import Control.Monad (guard,when)
import Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import Rule
import Term
import Util


-- Runtime exceptions

data EvalException = RuntimeException String deriving (Typeable,Show)

instance Exception EvalException

runtimeErr :: String -> a
runtimeErr msg = throw (RuntimeException msg)


-- Values

type Id = String
type Trace = String
type Strategy = Term -> Maybe (Term,[Trace])
data Proc = Proc [Id] RuleExpr deriving (Eq,Show)


-- Environment

data RuleEnv = RuleEnv 
  { envProcs :: Map Id Proc
  , envVars :: Map Id Strategy
  }

buildEnv :: [(Id,Proc)] -> RuleEnv
buildEnv xs = RuleEnv (Map.fromList xs) Map.empty 

lookupProc :: Id -> RuleEnv -> Maybe Proc
lookupProc x = Map.lookup x . envProcs

lookupVar :: Id -> RuleEnv -> Maybe Strategy
lookupVar x = Map.lookup x . envVars

bindVars :: RuleEnv -> [(Id,Strategy)] -> RuleEnv
bindVars (RuleEnv procs _) = RuleEnv procs . Map.fromList


-- Expressions

data RuleExpr = RuleCall Id [RuleExpr]
              | RuleVar Id
              | RuleLit Rule Trace
              | Match Term
              | Build Term
              | Success
              | Failure
              | Test RuleExpr
              | Neg RuleExpr
              | Seq RuleExpr RuleExpr
              | LeftChoice RuleExpr RuleExpr
              | Choice RuleExpr RuleExpr
              | Path Integer RuleExpr
              | BranchAll RuleExpr
              | BranchOne RuleExpr
              | BranchSome RuleExpr
              | Congruence [RuleExpr]
              deriving (Eq,Show)

eval :: RuleExpr -> RuleEnv -> Strategy
eval (RuleLit rule m) _ t = do
  t' <- apply rule t
  return (t',[m])
eval (Match pat) _ t = undefined
eval (Build pat) _ t = undefined
eval Success _ t = Just (t,mempty)
eval Failure _ _ = Nothing
eval (Test e) env t = 
  case eval e env t of
    Just _ -> Just (t,mempty)
    Nothing -> Nothing
eval (Neg e) env t = 
  case eval e env t of
    Just _ -> Nothing
    Nothing -> Just (t,mempty)
eval (Seq e1 e2) env t =
  case eval e1 env t of
    Nothing -> Nothing
    Just (t',m1) -> 
      case eval e2 env t' of
        Just (t'',m2) -> Just (t'',m1 `mappend` m2)
        Nothing -> Nothing
eval (LeftChoice e1 e2) env t =
  case eval e1 env t of
    Just (t',m) -> Just (t',m)
    Nothing -> 
      case eval e2 env t of
        Just (t',m) -> Just (t',m)
        Nothing -> Nothing
eval (Choice e1 e2) env t =
  -- Probably we don't want non-det choice for Twig, but it is interesting
  -- as a general rewriting expression.
  case (eval e1 env t,eval e2 env t) of
    (Just (x,m1),Just (x',_)) -> 
      if x == x' 
        then Just (x,m1) -- Note: arbitrarily choose first trace!
        else runtimeErr "Non-confluence"
    (Just (t',m),Nothing) -> Just (t',m)
    (Nothing,Just (t',m)) -> Just (t',m)
    (Nothing,Nothing) -> Nothing
eval (Path 0 e) env t = 
  eval e env t -- #0(s) just applies s to root
eval (Path i e) env t = do
  let ts = children t
  (ts',m) <- path (fromInteger i) (eval e env) ts
  return (t `withChildren` ts',m)
eval (BranchAll e) env t = do
  let ts = children t
  (ts',m) <- mapAll (eval e env) ts
  return (t `withChildren` ts',m)
eval (BranchOne e) env t = do
  let ts = children t
  (ts',m) <- mapOne (eval e env) ts
  return (t `withChildren` ts',m)
eval (BranchSome e) env t = do
  let ts = children t
  (ts',m) <- mapSome (eval e env) ts
  return (t `withChildren` ts',m)
eval (Congruence es) env t = do
  let ts = children t
  let rs = map (\e -> eval e env) es
  guard $ length ts == length rs
  mts' <- sequence (zipWith ($) rs ts)
  let (ts',ms) = unzip mts'
  return (t `withChildren` ts',mconcat ms)
eval (RuleVar x) env t = 
  case lookupVar x env of
    Nothing -> eval (RuleCall x []) env t
    Just s -> s t
eval (RuleCall x args) env t =
  case lookupProc x env of
    Nothing -> runtimeErr $ "Variable " ++ x ++ " not in scope"
    Just (Proc params e) -> do
      when (length args < length params) $ runtimeErr "Not enough args"
      when (length args > length params) $ runtimeErr "Too many args"
      let ss = map (\arg -> eval arg env) args
      let env' = bindVars env (zip params ss)
      eval e env' t

run :: Id -> RuleEnv -> Strategy
run entry env t = 
  case lookupProc entry env of 
    Just (Proc [] e) -> eval e env t
    Just (Proc _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
