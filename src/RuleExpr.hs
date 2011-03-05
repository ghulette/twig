{-# LANGUAGE DeriveDataTypeable #-}

module RuleExpr where

import Control.Exception
import Control.Monad (guard,when)
import Data.Monoid
import Data.Typeable (Typeable)
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


-- Expressions

type Id = String
type Trace = String
type Strategy = Term -> Maybe (Term,[Trace])
data RuleProc = RuleProc [Id] RuleExpr deriving (Eq,Show)
type RuleEnv = Map Id RuleProc

buildRuleEnv :: [(Id,[Id],RuleExpr)] -> RuleEnv
buildRuleEnv = Map.fromList . (map (\x -> (procId x,proc x)))
  where procId (x,_,_) = x
        proc (_,ps,e) = RuleProc ps e

lookupRule :: Id -> RuleEnv -> Maybe RuleProc
lookupRule = Map.lookup

data RuleExpr = RuleLit Rule Trace
              | Call Id [RuleExpr]
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

eval :: RuleEnv -> RuleExpr -> Strategy
eval _ (RuleLit s m) t = do
  t' <- apply s t
  return (t',[m])
eval _ Success t = Just (t,mempty)
eval _ Failure _ = Nothing
eval env (Test s) t = 
  case eval env s t of
    Just _ -> Just (t,mempty)
    Nothing -> Nothing
eval env (Neg s) t = 
  case eval env s t of
    Just _ -> Nothing
    Nothing -> Just (t,mempty)
eval env (Seq s1 s2) t = 
  case eval env s1 t of
    Nothing -> Nothing
    Just (t',m1) -> 
      case eval env s2 t' of
        Just (t'',m2) -> Just (t'',m1 `mappend` m2)
        Nothing -> Nothing
eval env (LeftChoice s1 s2) t =
  case eval env s1 t of
    Just (t',m) -> Just (t',m)
    Nothing -> 
      case eval env s2 t of
        Just (t',m) -> Just (t',m)
        Nothing -> Nothing
eval env (Choice s1 s2) t =
  -- Probably we don't want non-det choice for Twig, but it is interesting
  -- as a general rewriting expression.
  case (eval env s1 t,eval env s2 t) of
    (Just (x,m1),Just (x',_)) -> 
      if x == x' 
        then Just (x,m1) -- What do we do with the second trace here?
        else runtimeErr "Non-confluence"
    (Just (t',m),Nothing) -> Just (t',m)
    (Nothing,Just (t',m)) -> Just (t',m)
    (Nothing,Nothing) -> Nothing
eval env (Path 0 s) t = 
  eval env s t -- #0(s) just applies s to root
eval env (Path i s) t = do
  let ts = children t
  (ts',m) <- path (fromInteger i) (eval env s) ts
  return (t `withChildren` ts',m)
eval env (BranchAll s) t = do
  let ts = children t
  (ts',m) <- mapAll (eval env s) ts
  return (t `withChildren` ts',m)
eval env (BranchOne s) t = do
  let ts = children t
  (ts',m) <- mapOne (eval env s) ts
  return (t `withChildren` ts',m)
eval env (BranchSome s) t = do
  let ts = children t
  (ts',m) <- mapSome (eval env s) ts
  return (t `withChildren` ts',m)
eval env (Congruence ss) t = do
  let ts = children t
  let rs = map (eval env) ss
  guard $ length ts == length rs
  mts' <- sequence (zipWith ($) rs ts)
  let (ts',ms) = unzip mts'
  return (t `withChildren` ts',mconcat ms)
eval env (Call x args) t =
  case lookupRule x env of
    Nothing -> 
      runtimeErr $ "Variable " ++ x ++ " not in scope"
    Just (RuleProc params s) -> do
      when (length args < length params) $ runtimeErr "Not enough args"
      when (length args > length params) $ runtimeErr "Too many args"
      --let env' = localRuleEnv env (zip3 params (repeat []) args)
      eval env s t

run :: Id -> RuleEnv -> Term -> Maybe (Term,[Trace])
run entry env t = 
  case lookupRule entry env of 
    Just (RuleProc [] s) -> eval env s t
    Just (RuleProc _ _) -> runtimeErr (entry ++ " cannot have args")
    Nothing -> runtimeErr (entry ++ " is not defined")
